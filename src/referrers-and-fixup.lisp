(in-package :cl-store-faster)

;; References are used to handle circularity (in lists, non-specialized
;; vectors, structure-classes and standard-classes).  References are
;; created during serialization and deserialization.

;; We use references when the underlying common lisp structure allows sharing
;; and when it makes sense to save memory in the restored image (that is objects
;; that are tagged):
;;  (and number (not fixnum) (not `single-float')) will be de-duplicated
;;  `structure-class'es and slot-values thereof
;;  `standard-class'es and slot-values thereof
;;  `cons'es
;;
;; During serialization, we record every (referrable) object we see.
;; This is done during an explicit reference pass through the data.
;; Then, if we have seen an object multiple times we assign it a
;; reference id.  Then we begin the serialization process with that
;; information.  While the reference assignment pass is hard to
;; parallelize the rest of the storage can then in principle be easily
;; parallized (with just some contention over when we decide to
;; serialize the reference objects).

;; The nice thing about this explicit reference scheme is that it also
;; allows the restoration to be parallelized (though we would need to
;; add some file features to allow that).  Say some bookmarks at the
;; end of the file (with some rule about where object starts are with
;; respect to the buffers that we flush during ;; serialization 

;; A few things I've tried to speed this up, I tried precompiling the dispatch
;; during the reference step to avoid the dispatch tree, but that just made things
;; slower.  I tried parallelizing the reference pass but there is way too much
;; contention for the reference hash table and we need eq hashing, so I cannot
;; just plug in a lockless hashtable.

(defvar *track-references* t
  "If you let this to NIL, then every object will be stored anew, and
 there will be no circular reference detection.  It's a huge
 performance win (you can hit hundreds of MB/sec instead of 10s of
 MB/sec, but you need to make sure your data is safe to serialize and
 you don't care about EQL checks of data..")

(declaim (inline references-vector make-references))
(defstruct references
  "During deserialization this array grows as we restore references.
 In a parallel restore scenario this would have to work differently
 (we'd want to store the number of references in the file and then
 fill this vector up with fix-ups at the beginning)"
  (vector (make-array nil) :type simple-vector))

;; During the storing phase, we pass around a references hash table
;; which is an EQL hash-table which maps objects to reference ids.
;; Nominally we could have multiple reference hash tables if we wanted
;; to parallelize the storage operation more by reducing contention.

(declaim (inline check/store-reference))
(defun check/store-reference (object storage references &optional (add-new-reference t))
  (when references
    (check/store-reference& object storage references add-new-reference)))

(declaim (notinline check/store-reference&))
(defun check/store-reference& (object storage references &optional (add-new-reference t))
  "Used during the storage phase both during the reference counting
 step and the serialization step.  This function returns T if this
 object has already been written out, in which case the caller should
 NOT write OBJECT out to storage.  If NIL, then you must write the
 OBJECT out.  If ADD-NEW-REFERENCE is T, in the case where this
 function returns NIL, we will generate a new reference id for this
 object so it can be used in the future.  The only case where
 ADD-NEW-REFERENCE should be NIL is if you are explicitly
 dis-allowing (for performance reasons) circularity, as we optionally
 do during cons serialization."
  (declare (optimize speed safety))
  (if storage	     ; we are in the storage phase, writing things out
      (let ((ref-idx (gethash object references)))
	(declare (type (or null fixnum) ref-idx))
	;; When ref-idx is positive, it's a note that we have already written out the
	;; actual value, so we can just store the reference id. If it is negative,
	;; it means we must write out the ref-idx and the object as this is the first time
	;; it has appeared in the output.
	(if ref-idx
	    (cond
	      ((>= ref-idx 0)
	       #+debug-csf (format t "Storing a reference (#~A) which is to a ~A~%"
				   ref-idx (type-of object))
	       (store-reference ref-idx storage)
	       t)
	      (t
	       #+debug-csf (format t "Storing reference definition (#~A) for next object: ~A~%"
				   (- ref-idx) (type-of object))
	       (setf ref-idx (- ref-idx))
	       (store-reference-id-for-following-object ref-idx storage)
	       (setf (gethash object references) ref-idx)
	       nil))))
      (let ((number-of-times-referenced (gethash object references 0)))
	(declare (type fixnum number-of-times-referenced))
	;; This is the reference collection pass.  We store the number of times
	;; an object is referenced as 1 or 2, where 2 means anything more than 1.
	;; The logic below is unnecessarily complex, speed this up.
	(cond
	  ((and (zerop number-of-times-referenced) add-new-reference)
	   (setf (gethash object references) 1)
	   nil)
	  (#+debug-csf (>= number-of-times-referenced 1)
           #-debug-csf (= number-of-times-referenced 1)
           #+debug-csf (the fixnum (incf (the fixnum (gethash object references))))
	   #-debug-csf(setf (gethash object references) 2)
	   t)
	  #-debug-csf((= number-of-times-referenced 2) t)
	  (t nil)))))

;; RESTORE PHASE

(defun grow-references-vector (references ref-id)
  (let* ((vec (references-vector references))
	 (len (length vec)))
    (setf (references-vector references)
	  (adjust-array vec
			(max (the fixnum (* 2 len))
			     (the fixnum (1+ ref-id)))))))

(declaim (inline ensure-references-vector))
(defun ensure-references-vector (references ref-id)
  "Return / resize references-vector which can hold ref-id"
  (let* ((vec (references-vector references))
	 (len (length vec)))
    (if (<= len ref-id)
	(setf (references-vector references) (grow-references-vector references ref-id))
	vec)))

(declaim (inline update-reference))
(defun update-reference (ref-id value references)
  "Used during RESTORE"
  (declare (optimize speed safety) (type fixnum ref-id))
  #+debug-csf
  (let ((*print-circle* t))
    (format t "Updating reference id ~A to ~S~%" ref-id value))
  (let ((vec (ensure-references-vector references ref-id)))
    (locally (declare (optimize (speed 3) (safety 0)))
      (setf (svref vec ref-id) value))))

(defun invalid-referrer (ref-idx)
  (cerror "skip" (format nil "reference index ~A does not refer to anything!" ref-idx)))

(declaim (inline get-reference))
(defun get-reference (ref-id references)
  (declare (optimize speed safety))
  (let ((actual-object (svref (references-vector references) ref-id)))
  #+debug-csf (format t "Resolving reference ~A to a ~A~%"
		      ref-id (if actual-object (type-of actual-object) 'invalid-object))
    (or actual-object (invalid-referrer ref-id))))

(declaim (inline restore-referrer))
(defun restore-referrer (storage references)
  (get-reference (restore-object storage references) references))

(declaim (inline restore-referrer-ub8))
(defun restore-referrer-ub8 (storage references)
  (get-reference (restore-ub8 storage) references))

(declaim (inline restore-referrer-ub16))
(defun restore-referrer-ub16 (storage references)
  (get-reference (restore-ub16 storage) references))

(declaim (inline restore-referrer-ub32))
(defun restore-referrer-ub32 (storage references)
  (get-reference (restore-ub32 storage) references))

;; During restoring, we cannot always construct objects before we have
;; restored a bunch of other information (for example building displaced
;; arrays).  So we need to be able to fix-up references to the not yet built
;; object (which may have been restored while determining how to build the
;; object).

(declaim (inline fixup-p make-fixup fixup-list fixup-ref-id))
(defstruct fixup
  (list nil :type list)
  (ref-id -1 :type fixnum))

(defun fixup (fixup new-value references)
  (declare (optimize speed safety))
  "Resolve a delayed object construction.  Returns new-value."
  #+debug-csf (format t "Executing ~A fixups for reference id ~A of type ~A~%"
		      (length (fixup-list fixup)) (fixup-ref-id fixup)
		      (type-of new-value))
  (mapc (lambda (func)
	  (funcall (the function func) new-value))
	(fixup-list fixup))
  (update-reference (fixup-ref-id fixup) new-value references))

(defmacro with-delayed-reference/fixup ((ref-id references) &body body)
  "When we know an object is going to be referred to multiple times,
 we place it in the *references* array immediately before we even start
 building it because it may not be buildable without restoring other objects
 that might refer to it.  So we always stick a fixup in the references array
 first for any of those newly created objects to hang their requests to be
 notified of the final object once it is constructed. BODY must eventually yield
 the fully constructed object.  Not hygenic, "
  (let ((fixup (gensym))
	(num (gensym)))
    `(let* ((,num ,ref-id)
	    (,fixup (make-fixup :ref-id ,num)))
       (declare (dynamic-extent ,fixup))
       (update-reference ,num ,fixup ,references)
       #+debug-csf(format t "Created a fixup: ~A~%" ,fixup)
       (fixup ,fixup (progn ,@body) references))))

(defmacro restore-object-to (place storage references &optional tag)
  "If you are deserializing an object which contains slots (for
 example an array, a list, hash-table, or structure-object or a
 standard-object) which may point to other lisp objects which have yet
 to be fully reified, then please update your slots with this macro
 which will handle circularity fixups for you.

 Note that we capture any parameters of place so you may safely use this
 in loops or with references to variables whose values may be updated later"
  (let* ((restored (gensym))
	 (new-object (gensym))
	 (variables-to-capture (cdr place))
	 (names (loop repeat (length variables-to-capture) collect (gensym))))
    `(let ((,restored (restore-object ,storage ,references ,@(when tag (list tag)))))
       (if (fixup-p ,restored)
	   (push
	    (let (,@(mapcar #'list names variables-to-capture))
	      (lambda (,new-object)
		(setf (,(first place) ,@names) ,new-object)))
	    (fixup-list ,restored))
	   (setf ,place ,restored)))))

(defmacro maybe-store-reference-instead ((obj storage references &optional (add-new-reference t))
					 &body body)
  "Objects may be seen multiple times during serialization,
 so where object equality after deserialization is expected (pretty
 much every object except numbers) or not determinable (double-floats,
 complex, ratios, bignum), we record objects along with reference ids
 that we can refer to later in the serialization to point to the
 original object.  The counting of objects is done explicitly in the
 writing phase, so there is nothing to do in the reading phase except
 to plunk objects into the right place in the *references* array."
  `(or (check/store-reference ,obj ,storage ,references ,add-new-reference)
       (progn
	 ,@body)))

(declaim (notinline store-reference))
(defun store-reference (ref-index storage)
  "We store references as the minimum possible size we can"
  (declare (type (and (integer 0) fixnum) ref-index)
	   (type (not null) storage))
  (when storage
    #+debug-csf (format t "Writing reference ~A~%" ref-index)
    (typecase ref-index
      ((unsigned-byte 8)
       (with-write-storage (storage :offset offset :reserve-bytes 2 :sap sap)
	 (storage-write-ub16! storage (+ +referrer-ub8-code+ (ash ref-index 8))
			      :offset offset :sap sap)))
      ((unsigned-byte 16)
       (with-write-storage (storage :offset offset :reserve-bytes 3 :sap sap)
	 (storage-write-byte! storage +referrer-ub16-code+ :offset offset :sap sap)
	 (storage-write-ub16! storage ref-index :offset (incf offset) :sap sap)))
      ((unsigned-byte 32)
       (with-write-storage (storage :offset offset :reserve-bytes 5 :sap sap)
	 (storage-write-byte! storage +referrer-ub32-code+ :offset offset :sap sap)
	 (storage-write-ub32! storage ref-index :offset (incf offset) :sap sap)))
      (t
       (storage-write-byte storage +referrer-code+)
       (store-tagged-unsigned-fixnum ref-index storage)))))

(declaim (notinline store-reference-id-for-following-object))
(defun store-reference-id-for-following-object (ref-index storage)
  (declare (type (and (integer 0) fixnum) ref-index)
	   (type (not null) storage))
  (with-write-storage (storage :offset offset :over-reserve-bytes 5 :sap sap)
    #+debug-csf (format t "Writing reference follows ~A~%" ref-index)
    (typecase ref-index
      ((unsigned-byte 8)
       (storage-write-ub16! storage (+ +record-reference-ub8-code+ (ash ref-index 8))
			    :offset offset :sap sap)
       (setf (storage-offset storage) (+ 2 offset)))
      ((unsigned-byte 16)
       (storage-write-byte! storage +record-reference-ub16-code+ :offset offset :sap sap)
       (storage-write-ub16! storage ref-index :offset (incf offset) :sap sap)
       (setf (storage-offset storage) (+ 2 offset)))
      ((unsigned-byte 32)
       (storage-write-byte! storage +record-reference-ub32-code+ :offset offset :sap sap)
       (storage-write-ub32! storage ref-index :offset (incf offset) :sap sap)
       (setf (storage-offset storage) (+ 4 offset)))
      (t
       (storage-write-byte! storage +record-reference-code+ :offset offset :sap sap)
       (setf (storage-offset storage) (+ 1 offset))
       (store-tagged-unsigned-fixnum ref-index storage)))))

(declaim (notinline restore-reference-id-for-following-object))
(defun restore-reference-id-for-following-object (ref-id storage references)
  "Object may not reified before other objects refer to it"
  (with-delayed-reference/fixup (ref-id references)
    (restore-object storage references)))

(defun restore-reference-id-ub8 (storage references)
  (restore-reference-id-for-following-object (restore-ub8 storage) storage references))

(defun restore-reference-id-ub16 (storage references)
  (let ((ref-id (restore-ub16 storage)))
    #+debug-csf(format t "Restoring reference id ~A~%" ref-id)
    (restore-reference-id-for-following-object ref-id storage references)))

(defun restore-reference-id-ub32 (storage references)
  (let ((ref-id (restore-ub32 storage)))
    #+debug-csf(format t "Restoring reference id ~A~%" ref-id)
    (restore-reference-id-for-following-object ref-id storage references)))

(defun restore-reference-id (storage references)
  (restore-reference-id-for-following-object (restore-object storage references)
					     storage references))
