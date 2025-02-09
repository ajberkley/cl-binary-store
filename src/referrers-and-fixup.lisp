(in-package :cl-binary-store)

#+allegro
(eval-when (:compile-toplevel)
  (setf declared-fixnums-remain-fixnums-switch t)
  (declaim (optimize (speed 3) (safety 1)
		     (space 0) (debug 0) (compilation-speed 0))))

;; References are used to handle both circularity and the maintenance
;; of equality of objects.  For example if you have two references to
;; the same object in your data you do not want them to be restored as
;; separate objects!  Or if you have a circular list we need to detect
;; the circularity so we can store and restore it.

;; We use references when the underlying common lisp structure allow
;; sharing transparently to the user (double-floats, complex, ratio,
;; or bignums) or if the objects were #'eq originally.  That is, in
;; addition to maintaining #'eq-uality, (and number (not fixnum)
;; (not `single-float')) will be de-duplicated during serialization.
;;
;; During the initial phase of serialization, we do an explicit
;; reference counting pass through the data and record (almost) every
;; (referrable) object we see (there are some small exceptions --- the
;; contents of specialized vectors and arrays, the symbol-names of
;; uninterned symbols, (complex double-float), (complex
;; single-float)).  Then, if we have seen an object multiple times we
;; keep it around in a hash-table for the storage pass where we will
;; assign it a sequential reference id and emit a code that says "the
;; next object should be assigned a new reference id" when we store
;; objects that we know will be multiply referenced.  We also add a
;; note in the file of the total number of references the file
;; contains which helps restore speed.  The counting is implicit ---
;; so the restoration side keeps a count as it sees objects registered
;; as referrable.

;; The other complexity handled in this file is that an object may be
;; referred to during deserialization *before* it has been fully
;; created.  This is rare, but can happen with displaced-arrays for
;; example.  To handle this we put a placeholder object in the reference
;; vector during restore and anyone who finds a reference to that object
;; can register a "fix-up" which we will call once the object is fully
;; constructed to resolve the object.  This allows circular list building
;; among other things.  See `restore-object-to'.

(declaim (inline references-vector make-references references-ref-id))
(defstruct references
  "During deserialization this array contains all the references we
 have seen so far, and a running count ref-id of assigned ids.  Nominally
 the array size is hinted at the start of restore, but the code allows it
 to grow if needed."
  (vector (make-array 0) :type simple-vector)
  (ref-id 0 :type fixnum)) ;; ref-ids run from 1 to infinity; they are incf'ed from here

(declaim (inline check-reference))
(defun check-reference (object references &optional (add-new-reference t))
  "Returns T if OBJECT has already been seen and updates its reference count.
 If OBJECT has not been seen, and ADD-NEW-REFERENCE is T, then adds it to
 references and returns NIL.  If ADD-NEW-REFERENCE is NIL, just returns NIL.
 This should *ONLY* be called during the reference counting phase, that is when
 storage is nil."
  (when references
    (if add-new-reference
        (let ((number-of-times-referenced (gethash object references 0)))
          (declare (type fixnum number-of-times-referenced))
          ;; We store the number of times an object is referenced as 1 or 2, where 2 means anything
          ;; more than 1 (except if debug-cbs is in *features* then we keep track of the exact
          ;; number). The logic below is unnecessarily complex, clean this up with clear brain.
	  ;; When :info-cbs is in features, we do a complete count of occurences.
          (cond
            ((zerop number-of-times-referenced)
             (setf (gethash object references) 1)
             nil)
            (#+info-cbs (>= number-of-times-referenced 1) ;; do actual reference counting
             #-info-cbs (= number-of-times-referenced 1)
             #+info-cbs (the fixnum (incf (the fixnum (gethash object references))))
             #-info-cbs(setf (gethash object references) 2)
             t)
            #-info-cbs((= number-of-times-referenced 2) t)
            (t nil)))
        (gethash object references))))

(declaim (inline store-new-reference-indicator))
(defun store-new-reference-indicator (storage)
  "Write an indicator that we should assign a reference id to the next object; that is place
 it in the restore reference-vector (and increment the ref-id counter)."
  (with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
    (set-sap-ref-8 sap offset +new-reference-indicator-code+)))

(declaim (inline referenced-already))
(defun referenced-already (object storage references assign-new-reference-id)
  "Returns T if OBJECT is in REFERENCES and writes out a reference to it to storage.
 Otherwise returns NIL if it is not a reference at all.  This should
 *ONLY* be called during the actual storage phase, not the reference
 counting phase."
  (declare (type write-storage storage) (optimize (speed 3) (safety 1)))
  (when references
    (let ((ref-idx (gethash object references)))
      ;; When ref-idx is positive, it's a note that we have already written out the
      ;; actual value, so we can just store the reference id. If it is negative,
      ;; it means we must write out the ref-idx and the object as this is the first time
      ;; it has appeared in the output.
      (cond
	((eq ref-idx t)
	 ;; Assign a reference id
	 (let ((new-ref-id (funcall (the function assign-new-reference-id))))
	   (declare (type fixnum new-ref-id))
	   #+dribble-cbs (format t "Storing reference definition (#~A) for next object: ~A~%"
				 new-ref-id (type-of object))
	   (setf (gethash object references) new-ref-id))
	 ;; We know the reference id here, so we could write it out, but it wastes a lot
	 ;; of space, so until we want to do parallel store and restore leave it implicit
	 ;; for the reader.
	 (store-new-reference-indicator storage)
	 nil)
	((typep ref-idx 'fixnum)
	 #+dribble-cbs (format t "Storing a reference (#~A) which is to a ~A~%"
			       ref-idx (type-of object))
	 (store-reference ref-idx storage)
	 t)
	(t nil)))))

(declaim (inline check/store-reference))
(defun check/store-reference (object storage references assign-new-reference-id &key (add-new-reference t))
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
  (declare (optimize (speed 3) (safety 1)))
  (if storage	     ; we are in the storage phase, writing things out
      (referenced-already object storage references assign-new-reference-id)
      (check-reference object references add-new-reference)))

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
  (declare (optimize (speed 3) (safety 1)) (type fixnum ref-id))
  #+debug-cbs
  (let ((*print-circle* t))
    (format t "Updating reference id ~A to ~S~%" ref-id value))
  (let ((vec (ensure-references-vector references ref-id)))
    (locally (declare (optimize (speed 3) (safety 0)))
      (setf (svref vec ref-id) value))))

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
  (declare (optimize (speed 3) (safety 1)))
  "Resolve a delayed object construction.  Returns new-value."
  #+debug-cbs (format t "Executing ~A fixups for reference id ~A of type ~A~%"
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
       (fixup ,fixup (progn ,@body) references))))

(defmacro restore-object-to (place restore-object &optional tag)
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
    `(let ((,restored (funcall (the function ,restore-object) ,@(when tag (list tag)))))
       (if (fixup-p ,restored)
	   (push
	    (let (,@(mapcar #'list names variables-to-capture))
	      (lambda (,new-object)
		(setf (,(first place) ,@names) ,new-object)))
	    (fixup-list ,restored))
	   (setf ,place ,restored)))))

(defmacro maybe-store-reference-instead ((obj storage references assign-new-reference-id 
					  &key (add-new-reference t))
					 &body body)
  "Objects may be seen multiple times during serialization,
 so where object equality after deserialization is expected (pretty
 much every object except numbers) or not determinable (double-floats,
 complex, ratios, bignum), we record objects along with reference ids
 that we can refer to later in the serialization to point to the
 original object.  The counting of objects is done explicitly in the
 writing phase, so there is nothing to do in the reading phase except
 to plunk objects into the right place in the *references* array."
  `(or (check/store-reference ,obj ,storage ,references ,assign-new-reference-id
			      :add-new-reference ,add-new-reference)
       (progn
	 ,@body)))

(declaim (inline restore-new-reference-indicator))
(defun restore-new-reference-indicator (references restore-object)
  (let ((ref-id (incf (references-ref-id references))))
    (setf (svref (references-vector references) ref-id)
	  (with-delayed-reference/fixup (ref-id references)
	    (funcall (the function restore-object))))))
			
(declaim (notinline store-reference))
(defun store-reference (ref-index storage)
  "Write a reference id to the output which will be resolved at restore time to an object.  The
 basic-codespace implementation here reserves 6 bits of the codespace for reference ids which
 makes these pretty cheap."
  (declare (type (and (integer 1) fixnum) ref-index)
	   (type (not null) storage))
  (when storage
    #+dribble-cbs (format t "Writing reference ~A~%" ref-index)
    (cond
      ((<= +reference-direct-min-ref-id+ ref-index +reference-direct-max-ref-id+)
       (with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
	 (set-sap-ref-8 sap offset (encode-reference-direct ref-index))))
      ((<= ref-index +reference-one-byte-max-ref-id+)
       (with-write-storage (storage :offset offset :reserve-bytes 2 :sap sap)
	 ;;(format t "~16,'0b~%" (encode-reference-one-byte ref-index))
	 (set-sap-ref-16 sap offset (encode-reference-one-byte ref-index))))
      ((<= ref-index +reference-two-byte-max-ref-id+)
       (multiple-value-bind (tag-byte second-two-bytes)
	   (encode-reference-two-bytes ref-index)
	 ;;(format t "~16,'0b~8,'0b~%" second-two-bytes tag-byte)
	 (with-write-storage (storage :offset offset :reserve-bytes 3 :sap sap)
	   (set-sap-ref-8 sap offset tag-byte)
	   (set-sap-ref-16 sap (incf offset) second-two-bytes))))
      (t
       (with-write-storage (storage :offset offset :reserve-bytes 1 :sap sap)
	 (set-sap-ref-8 sap offset +tagged-reference-code+))
       (when storage
	 (store-tagged-unsigned-fixnum (encode-reference-tagged ref-index) storage))))))

(declaim (#-debug-cbs inline #+debug-cbs notinline restore-reference))
(defun restore-reference (ref-id references)
  "The reference has already been calculated in the dispatch code for us.
 If we are actually restoring the next object, it may not be re-ified before
 someone refers to it, so we have to store a fixup for those other objects
 to hang their reference onto."
  (declare (optimize (speed 3) (safety 1)) (type (and (integer 0) fixnum) ref-id))
  (let* ((vec (references-vector references))
         (len (length vec)))
    (if (>= ref-id len)
        (progn
          (cerror "Use NIL" 'invalid-input-data :format-control "Invalid data, reference to non-existent id ~A" :format-arguments (list ref-id))
          nil)
        (locally (declare (optimize (speed 3) (safety 0)))
          (svref vec ref-id)))))
