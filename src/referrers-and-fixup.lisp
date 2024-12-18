(in-package :cl-store-faster)


;; (pushnew :debug-csf *features*)
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

(declaim (inline store-reference))
(defun store-reference (ref-index storage)
  "We store references as the minimum possible size we can"
  (declare (type (and (integer 0) fixnum) ref-index)
	   (type (not null) storage))
  #+debug-csf (format t "Storing reference #~A~%" ref-index)
  (ensure-enough-room storage 4)
  (let ((offset (storage-offset storage))
	(array (storage-store storage)))
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
  #+debug-csf (format t "Storing reference for following object #~A~%" ref-index)
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
  (setf (aref *references* ref-id)
	(restore-object storage)))

(defun restore-reference-id-ub8 (storage)
  (restore-reference-id-for-following-object (restore-ub8 storage) storage))

(defun restore-reference-id-ub16 (storage)
  (restore-reference-id-for-following-object (restore-ub16 storage) storage))

(defun restore-reference-id-ub32 (storage)
  (restore-reference-id-for-following-object (restore-ub32 storage) storage))

(defun restore-reference-id (storage)
  (restore-reference-id-for-following-object (restore-object storage) storage))

(defun check/store-reference (value storage &aux (ht *references*) (ref-idx (gethash value ht)))
  (declare (optimize speed safety) (type (or null fixnum) ref-idx))
  (cond
    (storage
     (if ref-idx
	 ;; We will have to write a reference tag out in front of
	 ;; this object if we have not stored a reference for it
	 ;; yet.  The way we signal that is that the ref-idx
	 ;; is negative if we haven't written it out.
	 (cond
	   ((>= ref-idx 0)
	    (store-reference ref-idx storage)
	    t)
	   (t
	    #+debug-csf (format t "Updating reference id from ~A to ~A for a ~A~%"
				ref-idx (- ref-idx) (type-of value))
	    (setf ref-idx (- ref-idx))
	    (store-reference-id-for-following-object ref-idx storage)
	    (setf (gethash value ht) ref-idx)
	    nil))
	 (unless *do-explicit-reference-pass*
	   (let ((assigned-ref-idx (hash-table-count ht)))
	     #+debug-csf
 	     (let ((*print-circle* t))
	       (format t "Assigning reference id ~A to ~S (~A)~%" ref-idx value
		       (type-of value)))
	     (setf (gethash value ht) assigned-ref-idx)
	     nil))))
    (t
     ;; first reference collection pass, no ref-idx assigned yet,
     ;; just keeping track of how many times an object is referenced
     ;; eventually HT will be thread local, but for now this is fine.
     (setf (gethash value ht) (+ 1 (or ref-idx 0)))
     #+debug-csf
     (let ((*print-circle* t))
       (format t "Reference ~A now has ~A references~%" value (gethash value ht 0)))
     nil)))

;; RESTORATION WORK

(declaim (notinline record-reference))
(defun record-reference (value &aux (refs *references*))
  "This is used during RESTORE.  Here we keep track of a global count of
 references"
  ;; TODO DO NOT USE A GLOBAL HERE, TOO SLOW, PASS IT IN VIA CALLS SO THAT
  ;; IT CAN BE COMPILE TIME INLINED
  (if *references-already-fixed*
      (values value -1)
      (let ((len (length refs)))
	#+debug-csf
	(let ((*print-circle* t))
	  (format t "Recording reference id ~A as ~S ~%" len (if value value :delayed)))
	(vector-push-extend value refs)
	(values value len))))

(declaim (inline update-reference))
(defun update-reference (ref-id value)
  "Used during RESTORE"
  #+debug-csf
  (let ((*print-circle* t))
    (format t "Updating reference id ~A to ~S~%" ref-id value))
  (values (if *references-already-fixed* value (setf (aref *references* ref-id) value))))

(defun invalid-referrer (ref-idx)
  (cerror "skip" (format nil "reference index ~A does not refer to anything!" ref-idx)))

(declaim (inline get-reference))
(defun get-reference (ref-id)
  (or (aref *references* ref-id) (invalid-referrer ref-id)))

(declaim (inline restore-referrer))
(defun restore-referrer (storage)
  "Used during RESTORE"
  (get-reference (restore-object storage)))

(declaim (inline restore-referrer-ub8))
(defun restore-referrer-ub8 (storage)
  #+debug-csf (format t "REFERRER UB8~%")
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
  (list nil)
  (ref-id -1 :type fixnum))

(defun fixup (fixup new-value)
  (declare (optimize speed safety))
  "Resolve a delayed object construction.  Returns new-value."
  (mapc (lambda (func)
	  (funcall (the function func) new-value))
	(fixup-list fixup))
  (update-reference (fixup-ref-id fixup) new-value))

(defmacro with-delayed-reference/fixup (&body body)
  "If you cannot construct the object you are deserializing at the
 beginning of the deserialization routine because you need to load more
 information, AND there is a chance that the information you load may
 contain a reference back to the as-yet-constructed object you are building,
 then you must wrap your code with this magic.  BODY must yield the fully
 constructed object"
  (let ((fixup (gensym)))
    `(let ((,fixup (make-fixup)))
       (declare (dynamic-extent ,fixup))
       (setf (fixup-ref-id ,fixup) (nth-value 1 (record-reference ,fixup)))
       #+debug-csf(format t "Fixup is now ~A~%" ,fixup)
       (fixup ,fixup (progn ,@body)))))

(defmacro with-delayed-reference (&body body)
  "If you cannot construct the object you are deserializing at the
 beginning of the deserialization routine because you need to load
 more information, then you must wrap your code with this to keep
 referrer ids correct.  BODY must return the final object.  IF there
 is a chance of deserializing an object during BODY that may contain a
 reference to this not yet constructed object, then you must use
 WITH-DELAYED-REFERENCE/FIXUP instead."
  (let ((ref-id (gensym)))
    `(let ((,ref-id (nth-value 1 (record-reference nil))))
       (update-reference ,ref-id (progn ,@body)))))

(defmacro restore-object-to (place storage)
  "If you are deserializing an object which contains slots (for example
 an array, a list, or structure-object or a standard-object) which may
 point to other lisp objects which have yet to be fully reified, then
 please update your slots with this macro which will handle circularity
 fixups for you.

 Note that we capture any parameters of place so you may safely use this
 in loops or with references to variables whose values may be updated later"
  (let* ((restored (gensym))
	 (new-object (gensym))
	 (variables-to-capture (cdr place))
	 (names (loop repeat (length variables-to-capture) collect (gensym))))
    `(let ((,restored (restore-object ,storage)))
       (if (fixup-p ,restored)
	   (push
	    (let (,@(mapcar #'list names variables-to-capture))
	      (lambda (,new-object)
		(setf (,(first place) ,@names) ,new-object)))
	    (fixup-list ,restored))
	   (setf ,place ,restored)))))

(defmacro maybe-store-reference-instead ((obj storage) &body body)
  "Objects may occur multiple times during serialization or
 deserialization, so where object equality is expected (pretty much
 every object except numbers) or not determinable (double-floats,
 complex, ratios, bignum), we store references to objects we
 serialize so we can write a shorter reference to them later.  The
 counting of objects is done implicitely by matching of
 'maybe-store-reference-instead in store routines with the use of
 'with-delayed-reference, 'with-delayed-reference/fixup, or record-reference
 in the restore routines."
  `(or (check/store-reference ,obj ,storage)
       (progn
	 ,@body)))
