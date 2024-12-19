(in-package :cl-store-faster)

;; To enable debugging execute the below line and recompile everything
;; (pushnew :debug-csf *features*)
;; To disable debugging execute the below and recompile everything
;; (setf *features* (delete :debug-csf *Features*))

;; Referrers are used to handle circularity (in lists, non-specialized
;; vectors, structure-classes and standard-classes).  Referrers are
;; implicitly created during serialization and deserialization.

;; We use referrers when the underlying common lisp structure allows sharing
;; and when it makes sense to save memory in the restored image (that is objects
;; that are tagged):
;;  (and number (not fixnum) (not `single-float')) will be de-duplicated
;;  `structure-class'es and slot-values thereof
;;  `standard-class'es and slot-values thereof
;;  `cons'es

;; There are two methods that referrers are handled.
;;  1) Implicitly
;;      We assign a reference-id to every object we see during store
;;      or restore.
;;  2) Explicitly
;;      We record every object we see during store.  If we see an
;;      object multiple times then we assign it a reference id.  Then
;;      we begin the serialization process with that information.
;;      While the reference assignment pass is hard to parallelize the
;;      rest of the storage can then be parallelized.  It also allows
;;      the restoration to be parallelized (though we would need to
;;      add some file features to allow that).  The simplest probably
;;      is to add a set of bookmarks at the end of the buffer (probably
;;      recorded as we flush buffers to disk / file).
;;  3) No references allowed
;;      While nominally this would be super fast, is it worth the small
;;      complexity?

;; I'd like to pre-compile these modes.  So probably the restore and store
;; functions need to take information about this.  We'll probably have to move
;; away from simple functions to some hidden parameters.  If we inline them
;; into compiled trampolines we should get very fast performance.

;; For (2), while we are doing the reference id pass, we are having to
;; do the main dispatch work.  Why don't we compile at the same time a
;; dispatch list?  The closure would cost at a minimum 8 bytes
;; (widetag), reference id (8 bytes), function address (8 bytes) for
;; each dispatch decision.  If there was no reference id (or we were
;; working with no references allowed?) we would store the closure
;; without it (as the inline function would be specialized to not have
;; the information).  Is a closure needed or should we just store the
;; dispatch indices in a specialized array?  If we did that we'd be
;; down to 1 byte each.  But then we'd have to make a decision about
;; reference or not at each object.  Nominally that's a fast path as
;; references don't have very often.  OK, I like that.  It avoids
;; slowing us down.  Also if we occasionally stored an offset into the data
;; we could then parallelize the writing later.

;; OK, so let's work on an explicit referencing pass?  First step is
;; to re-use the store functions to do the object walking.  I think we
;; can just replace STORE-OBJECT with a passed in function with no
;; side-effects... but we need to be careful not to do extra work in
;; the serializers --- we need to NO-OP out the basic storage
;; functions (I suppose we can pass in a STORAGE which is NIL and make
;; sure everything is INLINED which will allow the compiler to elide
;; those paths).  Even if things aren't inlined, the test is trivial
;; and predictable.  OK, so lets do that.

;; STEP 1:
;;  Let's do a first SERIAL pass to identify references (this can be
;;  parallelized later with the cost of a fixup pass which can also
;;  be parallelized).

;; With the idea that we need to also potentially compute the dispatch
;; list in the STORE-OBJECT call

(defstruct referrer)

(defparameter *references* nil
  "An EQL hash-table that is locally bound during store or restore.

 Used to store objects we restore which may be referenced later
 (or in a circular manner).
 For example if we store a circular list:
  #1=(cons #1 #1)
 To store this, we first see the cons, write out a +cons-code+,
 and store the cons in *references* pointing at referrer index 1.
 Then we see a reference to referrer index 1, which we can
 immediately resolve.  There is no complexity here requiring
 delayed fix-ups or anything.

 When we are reading though, we see the cons, and immediately
 allocate (cons nil nil).  The same goes for structures and classes,
 where we pre-allocate the objects as soon as we see their types.

 This hash-table maps values -> reference-indices when writing, and
 reference-indices -> values when reading, as such ONLY store non
 fixnum objects in here.")

(defun check/store-reference (object storage &optional (add-new-reference t)
			      &aux (ht *references*) (ref-idx (gethash object ht)))
  "Returns T if we have written a short-hand reference out to the OBJECT, in which case the
 caller should NOT write OBJECT out to storage.  If NIL, then you must write the OBJECT out.
 If ADD-NEW-REFERENCE is T, in the case where this function returns NIL, we will generate a
 new reference id for this object so it can be used in the future.  The only case where
 ADD-NEW-REFERENCE should be NIL is if you are explicitly dis-allowing (for performance reasons)
 circularity."
  (declare (optimize speed safety) (type (or null fixnum) ref-idx))
  (cond
    (storage
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
	    (setf (gethash object ht) ref-idx)
	    nil))))
    (t
     ;; first reference collection pass, no ref-idx assigned yet,
     ;; just keeping track of how many times an object is referenced
     ;; eventually HT will be thread local, but for now this is fine.
     (let ((number-of-times-referenced
	     (when (or ref-idx add-new-reference)
	       (setf (gethash object ht) (+ 1 (or ref-idx 0))))))
       #+debug-csf
       (when number-of-times-referenced
	 (let ((*print-circle* t))
	   (format t "Reference of ~S now has ~A references~%"
		   object number-of-times-referenced)))
       ;; If we have seen this reference before, don't do work on it
       (and number-of-times-referenced (> number-of-times-referenced 1))))))

;; RESTORATION WORK

(declaim (inline update-reference))
(defun update-reference (ref-id value)
  "Used during RESTORE"
  #+debug-csf
  (let ((*print-circle* t))
    (format t "Updating reference id ~A to ~S~%" ref-id value))
  (if *references-already-fixed*
      value
      (let ((references *references*))
	(setf (aref
	       (if (array-in-bounds-p references ref-id)
		   (setf *references*
			 (adjust-array references
				       (max (* 2 (length references)) (1+ ref-id))))
		   references)
	       ref-id)
	      value))))

(defun invalid-referrer (ref-idx)
  (cerror "skip" (format nil "reference index ~A does not refer to anything!" ref-idx)))

(declaim (inline get-reference))
(defun get-reference (ref-id)
  (let ((actual-object (aref *references* ref-id)))
  #+debug-csf (format t "Resolving reference ~A to a ~A~%"
		      ref-id (if actual-object (type-of actual-object) 'invalid-object))
    (or actual-object (invalid-referrer ref-id))))

(declaim (inline restore-referrer))
(defun restore-referrer (storage)
  "Used during RESTORE"
  (get-reference (restore-object storage)))

(declaim (inline restore-referrer-ub8))
(defun restore-referrer-ub8 (storage)
  (get-reference (restore-ub8 storage)))

(declaim (inline restore-referrer-ub16))
(defun restore-referrer-ub16 (storage)
  (get-reference (restore-ub16 storage)))

(declaim (inline restore-referrer-ub32))
(defun restore-referrer-ub32 (storage)
  (get-reference (restore-ub32 storage)))

;; During restoring, we cannot always construct objects before we have
;; restored a bunch of other information (for example building displaced
;; arrays).  So we need to be able to fix-up references to the not yet built
;; object (which may have been restored while determining how to build the
;; object).

(declaim (inline fixup-p make-fixup fixup-list fixup-ref-id))
(defstruct fixup
  (list nil :type list)
  (ref-id -1 :type fixnum))

(defun fixup (fixup new-value)
  (declare (optimize speed safety))
  "Resolve a delayed object construction.  Returns new-value."
  #+debug-csf (format t "Executing ~A fixups for reference id ~A of type ~A~%"
		      (length (fixup-list fixup)) (fixup-ref-id fixup)
		      (type-of new-value))
  (mapc (lambda (func)
	  (funcall (the function func) new-value))
	(fixup-list fixup))
  (update-reference (fixup-ref-id fixup) new-value))

(defun add-reference/fixup (value/fix-up ref-id)
  (unless (array-in-bounds-p *references* ref-id)
    (setf *references*
	  (adjust-array *references*
			(max (* 2 (length *references*)) (1+ ref-id)))))
  (setf (aref *references* ref-id) value/fix-up))

(defmacro with-delayed-reference/fixup (ref-id &body body)
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
       (add-reference/fixup ,fixup ,num)
       #+debug-csf(format t "Created a fixup: ~A~%" ,fixup)
       (fixup ,fixup (progn ,@body)))))

(defmacro restore-object-to (place storage &optional tag)
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
    `(let ((,restored (restore-object ,storage ,@(when tag (list tag)))))
       (if (fixup-p ,restored)
	   (push
	    (let (,@(mapcar #'list names variables-to-capture))
	      (lambda (,new-object)
		(setf (,(first place) ,@names) ,new-object)))
	    (fixup-list ,restored))
	   (setf ,place ,restored)))))

(defmacro maybe-store-reference-instead ((obj storage) &body body)
  "Objects may be seen multiple times during serialization,
 so where object equality after deserialization is expected (pretty
 much every object except numbers) or not determinable (double-floats,
 complex, ratios, bignum), we record objects along with reference ids
 that we can refer to later in the serialization to point to the
 original object.  The counting of objects is done explicitly in the
 writing phase, so there is nothing to do in the reading phase except
 to plunk objects into the right place in the *references* array."
  `(or (check/store-reference ,obj ,storage)
       (progn
	 ,@body)))

(declaim (inline store-reference))
(defun store-reference (ref-index storage)
  "We store references as the minimum possible size we can"
  (declare (type (and (integer 0) fixnum) ref-index)
	   (type (not null) storage))
  (ensure-enough-room storage 4)
  (let ((offset (storage-offset storage))
	(array (storage-store storage)))
    #+debug-csf (format t "Writing reference ~A~%" ref-index)
    (typecase ref-index
      ((unsigned-byte 8)
       (store-ub8 +referrer-ub8-code+ storage nil)
       (store-ub8 ref-index storage nil))
      ((unsigned-byte 16)
       (store-ub8 +referrer-ub16-code+ storage nil)
       (store-ub16 ref-index storage nil))
      ((unsigned-byte 32)
       (store-ub8 +referrer-ub32-code+ storage nil)
       (store-ub32 ref-index storage nil))
      (t
       (setf (aref array offset) +referrer-code+)
       (setf (storage-offset storage) (+ offset 1))
       (store-tagged-unsigned-integer ref-index storage)))))

(defun store-reference-id-for-following-object (ref-index storage)
  (declare (type (and (integer 0) fixnum) ref-index)
	   (type (not null) storage))
  (ensure-enough-room storage 4)
  (let ((offset (storage-offset storage))
	(array (storage-store storage)))
    (typecase ref-index
      ((unsigned-byte 8)
       (store-ub8 +record-reference-ub8-code+ storage nil)
       (store-ub8 ref-index storage nil))
      ((unsigned-byte 16)
       (store-ub8 +record-reference-ub16-code+ storage nil)
       (store-ub16 ref-index storage nil))
      ((unsigned-byte 32)
       (store-ub8 +record-reference-ub32-code+ storage nil)
       (store-ub32 ref-index storage nil))
      (t
       (setf (aref array offset) +record-reference-code+)
       (setf (storage-offset storage) (+ offset 1))
       (store-tagged-unsigned-integer ref-index storage)))))

(declaim (inline restore-reference-id-for-following-object))
(defun restore-reference-id-for-following-object (ref-id storage)
  "Object may not reified before other objects refer to it"
  (with-delayed-reference/fixup ref-id
    (restore-object storage)))

(defun restore-reference-id-ub8 (storage)
  (restore-reference-id-for-following-object (restore-ub8 storage) storage))

(defun restore-reference-id-ub16 (storage)
  (restore-reference-id-for-following-object (restore-ub16 storage) storage))

(defun restore-reference-id-ub32 (storage)
  (restore-reference-id-for-following-object (restore-ub32 storage) storage))

(defun restore-reference-id (storage)
  (restore-reference-id-for-following-object (restore-object storage) storage))
