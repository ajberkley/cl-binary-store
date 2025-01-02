(in-package :cl-binary-store)

;; A codespace is a description of a binary file format and the code that
;; stores and restores from it.  A codespace is built through the imperative
;; `define-codespace' micro-domain specific language.

;; Within a `define-codespace' top-level, one is working within your
;; own namespace, and using a declarative language to describe how to
;; serialize and deserialize things.  You can define global variables
;; with register-store-state, and explicit reference tracking
;; hash-tables with register-references.  There are a three globally bound
;; names within a define-codespace:
;; TRACK-REFERENCES is bound to *track-references*
;; OBJ within a defstore is the object you should store
;; CODE within a defrestore is the tag code that has been read.

;; To debug this stuff you want to inspect *codespaces*.
;; Find your codepsace, dump the source code stored in the -source-code slot
;; into a file, compile it to a named function, and then put that named function
;; into the codespace compiled function slots and you will have full debuggability.

#+allegro
(eval-when (:compile-toplevel)
  (setf declared-fixnums-remain-fixnums-switch t)
  (declaim (optimize (speed 3) (safety 1)
		     (space 0) (debug 0) (compilation-speed 0))))

(defvar *codespaces* (make-hash-table :test 'eql)
  "a map from magic/version-number -> `codespace'")

(defvar *current-codespace* nil "a `codespace' bound by store based on
 *write-version*.  This is also bound during restore operations once we
 know the format of the data stream or is set to *read-version*.")

(defvar *current-codespace/compile-time* nil
  "nil or a `codespace'.  This is bound while compiling each codespace.")


(defun invalid (&rest rest)
  (declare (ignore rest))
  (error "Not compiled yet"))

(defstruct codespace
  (name "default" :type string)
  (magic-number 0 :type fixnum)
  (ref-tables (make-hash-table :test 'eql)) ; Maps name -> ref-table
  (store-infos (make-hash-table :test 'equal)) ; Maps type -> `store-info'
  (restore-infos (make-hash-table :test 'eql)) ; Maps code -> `restore-info'
  (store-global-state-info (make-hash-table :test 'eql)) ; Maps name -> `global-state'
  (restore-global-state-info (make-hash-table :test 'eql)) ; Maps name -> `global-state'
  (restore-objects-source-code nil) ; the source code that was compiled to restore-objects
  (store-objects-source-code nil) ; the source code that was compiled to make store-objects
  (restore-objects #'invalid :type function)
  (store-objects #'invalid :type function))

(defun deep-copy-codespace (target source-codespace)
  (setf (codespace-ref-tables target) (codespace-ref-tables source-codespace))
  (setf (codespace-store-infos target) (codespace-store-infos source-codespace))
  (setf (codespace-restore-infos target) (codespace-restore-infos source-codespace))
  (setf (codespace-store-global-state-info target)
	(codespace-store-global-state-info source-codespace))
  (setf (codespace-restore-global-state-info target)
	(codespace-restore-global-state-info source-codespace)))


(defvar *track-references* t
  "If you let this to NIL, then every object will be stored anew, and
 there will be no circular reference detection.  It's a huge
 performance win (you can hit hundreds of MB/sec instead of 10s of
 MB/sec, but you need to make sure your data is safe to serialize and
 you don't care about EQL checks of data.")

(defvar *version-being-read* nil
  "During restore this is bound to any magic number found previous to
 this in the file.")

(defvar *write-end-marker* nil
  "If T, once all objects are stored an end marker will be written to the output.
 This will trigger the end of restore (for use in cases where there
 isn't an obvious end of file)")

(defun build-restore-objects (codespace)
  "Builds the body of a function that reads tag bytes and dispatches them through a
 big case statement built by make-read-dispatch-table."
  `(let* ((references-vector (make-array 2048 :initial-element nil))
	  (references (make-references :vector references-vector))
	  (*version-being-read* (codespace-magic-number *current-codespace*))
	  ,@(build-global-state-let-bindings codespace :restore t))
     (declare (dynamic-extent references references-vector))
     ,(build-global-state-declarations codespace :restore t)
     (labels ((restore-object2 (&optional (code (restore-ub8 storage)))
		(let ((restore-object #'restore-object))
                  ,(make-read-dispatch-table codespace 'code)))
	      (restore-object (&optional (code (restore-ub8 storage)))
		(let ((restore-object #'restore-object2))
                  ,(make-read-dispatch-table codespace 'code))))
       (declare (inline restore-object2)) ;; inline one level
       (let ((objects
               (loop
		 with object and ignore/eof
		 for code = (maybe-restore-ub8 storage)
		 while code
		 do #+debug-cbs (format t "Read code ~A (offset ~A max ~A)~%" code
					(read-storage-offset storage) (read-storage-max storage))
                    (setf (values object ignore/eof) (restore-object code))
		 #+debug-cbs (format t "Got object type ~A, ignore/eof ~S~%" (type-of object)
				     ignore/eof)
		 until (eq ignore/eof :end)
		 unless (eq ignore/eof :ignore)
                   collect object)))
	 (apply #'values objects)))))

(defun build-store-objects (codespace)
  `(let* ((track-references *track-references*)
	  ,@(build-global-state-let-bindings codespace :store t))
     ,(build-global-state-declarations codespace :store t)
     ,(build-reference-tables
       codespace 'track-references
       `(progn
	  #+debug-cbs (when track-references (format t "Starting reference counting pass on ~A objects~%" (length stuff)))
	  (labels ((store-object2 (obj)
		     (let ((store-object #'store-object)
			   (storage nil)
			   (assign-new-reference-id nil))
		       ,(store-object/phase codespace 'obj 'store-info-reference-phase-code)))
		   (store-object (obj)
		     (let ((store-object #'store-object2)
			   (assign-new-reference-id nil)
			   (storage nil))
		       ,(store-object/phase codespace 'obj 'store-info-reference-phase-code))))
	    (declare (inline store-object2)) ;; inline one level deep
	    (when track-references
	      (dolist (elt stuff)
		(store-object elt))))
	  #+debug-cbs (when track-references (format t "Finished reference counting pass~%"))
	  #+debug-cbs (when track-references
			,(build-map-reference-tables codespace ''analyze-references-hash-table))
          ;; Now clean up the references table: delete anyone who has no references
          #+debug-cbs (when track-references (format t "Generating real reference hash-table~%"))
	  ;; We do not assign reference ids.  They are assigned as objects are
	  ;; written, in order as we are keeping the implicit numbering scheme, on
	  ;; reading
	  (let ((max-ref-id 0))
	    (declare (type fixnum max-ref-id))
            (when track-references
	      ,(replacing-reference-tables
	       codespace
	       'old-ht 'new-ht
	       '(maphash (lambda (k v)
			   (when (> (the fixnum v) 1)
			     (setf (gethash k new-ht) t)
			     (the fixnum (incf max-ref-id))))
		 old-ht)))
            #+debug-cbs
	    (when track-references
              ,(build-map-reference-tables codespace
					   `(lambda (table-name table)
                                              (format t "~A: there are ~A actual references~%"
						      table-name
						      (hash-table-count table)))))
            #+debug-cbs (format t "Starting actual storage phase~%")
            (let ((ref-id 0))
	      (declare (type fixnum ref-id))
	      (labels ((assign-new-reference-id ()
			 #+dribble-cbs
			 (format t "Assigning new reference id! (ref-id is ~A)~%" ref-id)
			 (the fixnum (incf ref-id)))
		       (store-object2 (obj) ;; inline one deep
			 (let ((store-object #'store-object)
			       (assign-new-reference-id #'assign-new-reference-id))
			   ,(store-object/phase codespace 'obj 'store-info-storage-phase-code)))
                       (store-object (obj)
			 (let ((store-object #'store-object2)
			       (assign-new-reference-id #'assign-new-reference-id))
			   ,(store-object/phase codespace 'obj 'store-info-storage-phase-code))))
		(declare (inline store-object2) (inline assign-new-reference-id))
		(when (>= max-ref-id 2048) ;; if we would have to expand the references vector
		  #+debug-cbs (format t "Writing total reference count ~A to file~%" (1+ ref-id))
		  (write-reference-count (1+ max-ref-id) #'store-object))
		(dolist (elt stuff)
		  (store-object elt))
		(when *write-end-marker* (store-object (make-end-marker)))))
	    (flush-write-storage storage))))))

(defun analyze-references-hash-table (table-name references)
  (declare (ignorable table-name references))
  #+info-cbs(let ((types (make-hash-table :test 'equal))
                   (individual-reference-counts (make-hash-table :test 'equal))
                   (max-refed (make-hash-table :test 'equal))
                   (total-references-used 0)
                   (total-unique-multiply-referenced-objects 0))
	       (declare (type fixnum total-references-used total-unique-multiply-referenced-objects))
	       (maphash (lambda (k v)
                          (let ((type (type-of k)))
                            (incf (gethash type types 0) v)
                            (when (> v 1)
			      (incf total-references-used v)
			      (incf total-unique-multiply-referenced-objects))
                            (push v (gethash type individual-reference-counts))
                            (when (< (car (gethash type max-refed (cons 0 nil))) v)
			      (setf (gethash type max-refed) (cons v k)))))
	                references)
	       (format t "~A: Total ~A references (out of ~A objects) emitted to file with ~A ~
                            reference ids allocated~%" table-name total-references-used
                            (hash-table-count references)  total-unique-multiply-referenced-objects)
               (when (not (zerop total-references-used))
		 (let (data)
                   (format t "Reference types are:~%")
                   (maphash (lambda (type total-count)
                              (push (cons type total-count) data))
                            types)
                   (setf data (sort data #'> :key #'cdr))
                   (map nil (lambda (d)
                              (let ((individual-counts (gethash (car d) individual-reference-counts)))
				(format t "~A of type ~A (~A unique)~%     ~
                                    avg ref count ~,3f / min ~,3f / max ~,3f / frac>1 ~,3f~
                                    ~A"
					(cdr d) (car d) (length individual-counts)
					(/ (cdr d) (length individual-counts) 1d0)
					(loop for i fixnum in individual-counts minimizing i)
					(loop for i fixnum in individual-counts maximizing i)
					(/ (count-if (lambda (x) (> x 1)) individual-counts)
                                           (length individual-counts) 1d0)
					(let ((obj (gethash (car d) max-refed)))
                                          (let ((*print-circle* t))
                                            (if (> (car obj) 1)
                                                (format nil "~%     most-refed ~A times: ~S~%" (car obj) (cdr obj))
                                                (format nil "~%")))))))
			data)))))

(defun compile-codespace (codespace)
  (let ((store-objects-source-code
	  `(lambda (storage &rest stuff)
	     (declare (optimize (speed 3) (safety 1)) (type write-storage storage)
		      (dynamic-extent stuff))
	     ,(macroexpand (build-store-objects codespace)))))
    (setf (codespace-store-objects-source-code codespace) store-objects-source-code)
    (setf (codespace-store-objects codespace) (compile nil store-objects-source-code)))
  (let ((restore-objects-source-code
	  `(lambda (storage)
	     (declare (optimize (speed 3) (safety 1))
		      (type read-storage storage))
	     ,(macroexpand (build-restore-objects codespace)))))
    (setf (codespace-restore-objects-source-code codespace) restore-objects-source-code)
    (setf (codespace-restore-objects codespace) (compile nil restore-objects-source-code))
    codespace))


(defmacro define-codespace ((name magic-number &key inherits-from) &body body)
  "Creates and registers a codespace into *codespaces*.  Within this environment
 there are a three pre-defined symbols:
 TRACK-REFERENCES is bound to *track-references*
 OBJ within a defstore is the object you should store
 CODE within a defrestore is the tag code that has been read."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((codespace (make-codespace :magic-number ,magic-number :name ,name)))
       ,(when inherits-from
	  `(deep-copy-codespace codespace (gethash ,inherits-from *codespaces*)))
       (macrolet ((get-current-codespace/compile-time () 'codespace))
	 ,@body)
       (when (gethash ,magic-number *codespaces*)
	 (format t "WARNING: redefining code-space ~A~%" ,magic-number))
       (setf (gethash ,magic-number *codespaces*) (compile-codespace codespace)))))

(defstruct restore-info
  "Information about a defrestore statement"
  (restore-function-dispatch-code nil :type (or (unsigned-byte 8) list))
  (restore-function-source-code nil))
  
(defstruct store-info
  "Information about a defstore statement"
  (type nil)
  (reference-phase-code nil)
  (storage-phase-code nil))

(defstruct global-state
  "Something that is instantiated at the start of the store process, regardless
 of whether track-references is true or not.  Like OBJECT-INFO, and LIST-LENGTHS."
  (name nil)
  (construction-code nil)
  (type nil)
  (dynamic-extent nil))

(defstruct ref-table
  "A ref-table is a hash table which is used solely to track references.  It will be nil
 instantiated unless *track-references* is T.  After the reference counting phase, only
 elements in it that are multiply referenced will be retained and have their value set to
 T"
  (name nil)
  (construction-code nil))

(defun register-references& (current-codespace/compile-time table-name construction-code)
  (let* ((new-ref-table (make-ref-table :name table-name :construction-code construction-code))
	 (ref-tables (codespace-ref-tables current-codespace/compile-time))
         (pre-existing (gethash table-name ref-tables)))
    (when (and pre-existing (not (equalp pre-existing new-ref-table)))
      (cerror "REPLACE IT" (format nil "Already extant reference table ~A" table-name))
      (remhash pre-existing ref-tables))
    (setf (gethash table-name ref-tables) new-ref-table))
  (values))

(defmacro register-references (table-name construction-code)
  `(register-references& (get-current-codespace/compile-time) ',table-name ',construction-code))

(defun build-reference-tables (codespace track-references &rest body)
  "Wrap body with defined reference tables"
  (let ((let-bindings nil))
    (maphash (lambda (table-name ref-table)
               (push (list table-name `(when ,track-references
                                         ,(ref-table-construction-code ref-table)))
                     let-bindings))
             (codespace-ref-tables codespace))
    `(let (,@let-bindings)
       (declare (dynamic-extent ,@(mapcar #'first let-bindings)))
       ,@body)))

(defmacro with-reference-tables (track-references &rest body)
  `(with-reference-tables& (get-current-codespace/compile-time) ,track-references ,@body))

(defun build-map-reference-tables (codespace func)
  (let ((code nil))
    (maphash (lambda (table-name ref-table)
               (declare (ignorable ref-table))
               (push `(funcall ,func ',table-name ,table-name) code))
             (codespace-ref-tables codespace))
    `(progn ,@code)))

(defun replacing-reference-tables (codespace old-ht new-ht body)
  (let ((code nil))
    (maphash (lambda (table-name ref-table)
               (push `(setf ,table-name
			    (let ((,old-ht ,table-name)
				  (,new-ht ,(ref-table-construction-code ref-table)))
			      (progn ,body
				     ,new-ht)))
		     code))
             (codespace-ref-tables codespace))
    `(progn ,@code)))

(defun register-store-state& (codespace name construction-code type dynamic-extent)
  (setf (gethash name (codespace-store-global-state-info codespace))
	(make-global-state :name name :construction-code construction-code :type type
			  :dynamic-extent dynamic-extent)))

(defun register-restore-state& (codespace name construction-code type dynamic-extent)
  (setf (gethash name (codespace-restore-global-state-info codespace))
	(make-global-state :name name :construction-code construction-code :type type
			  :dynamic-extent dynamic-extent)))

(defmacro register-global-state (name construction-code &key type dynamic-extent documentation
							  store restore)
  (declare (ignore documentation))
  `(progn
     ,(when store
	`(register-store-state& (get-current-codespace/compile-time) ',name ',construction-code ',type ',dynamic-extent))
     ,(when restore
	`(register-restore-state& (get-current-codespace/compile-time) ',name ',construction-code ',type ',dynamic-extent))))

(defun build-global-state-let-bindings (codespace &key store restore)
  (assert (not (and store restore)))
  (loop for global-state being the
	hash-values of (if store
			   (codespace-store-global-state-info codespace)
			   (codespace-restore-global-state-info codespace))
	 collect (list (global-state-name global-state)
		       (global-state-construction-code global-state))))

(defun build-global-state-declarations (codespace &key store restore)
  (assert (not (and store restore)))
  `(declare
    ,@(loop for global-state being the
	    hash-values of
			(if store
			    (codespace-store-global-state-info codespace)
			    (codespace-restore-global-state-info codespace))
	    for type = (global-state-type global-state)
	    for dynamic-extent = (global-state-dynamic-extent global-state)
	    for name = (global-state-name global-state)
	    when type
	      collect `(type ,type ,name)
	    when dynamic-extent
	      collect `(dynamic-extent ,name))))

(defun update-store-info
    (codespace type store-function-signature
     &key (call-during-reference-phase nil call-during-reference-phase-provided-p)
       check-for-ref-in write-phase-code override)
  (labels ((maybe-wrap-code-with-ref-check-for-store-phase (code)
             (if check-for-ref-in
                 `(unless (referenced-already obj storage ,check-for-ref-in assign-new-reference-id)
                    ,@(when write-phase-code
                        `((store-ub8/no-tag ,write-phase-code storage)))
                    ,code)
                 code))
           (maybe-wrap-code-with-ref-check-for-ref-phase (code)
             (if check-for-ref-in
                 `(unless (check-reference obj ,check-for-ref-in t)
                    ,code)
                 code)))
    (let* ((write-phase-code store-function-signature)
           (reference-phase-code (if call-during-reference-phase-provided-p
                                     call-during-reference-phase
                                     write-phase-code))
           (si (make-store-info
                :type type
                :reference-phase-code (maybe-wrap-code-with-ref-check-for-ref-phase reference-phase-code)
                :storage-phase-code (maybe-wrap-code-with-ref-check-for-store-phase write-phase-code)))
	   (store-info (codespace-store-infos codespace)))
      (unless (or (null (gethash type store-info))
                  (equalp (gethash type store-info) si)
		  override)
        (cerror "REPLACE IT" (format nil "Replacing already existing store code for type ~A" type)))
      (setf (gethash type store-info) si))))

(defmacro delete-restore (code)
  "In define-codespace that has inherited another codespace, delete store capability for a type"
  `(remhash ',code (codespace-restore-infos (get-current-codespace/compile-time))))

(defmacro delete-store (type)
  `(remhash ',type (codespace-store-infos (get-current-codespace/compile-time))))

(defun delete-codespace (codespace)
  (remhash codespace *codespaces*))

(defmacro defstore
    (type store-function-signature
     &key (call-during-reference-phase nil call-during-reference-phase-provided-p)
       check-for-ref-in write-phase-code override)
  `(update-store-info (get-current-codespace/compile-time) ',type ',store-function-signature
		      ,@(if call-during-reference-phase-provided-p
			    `(:call-during-reference-phase ',call-during-reference-phase))
		      :check-for-ref-in ',check-for-ref-in
		      :write-phase-code ',write-phase-code
		      :override ,override))

(defun update-restore-info (current-codespace/compile-time code restore-function-signature)
  (when (constantp code) ;; maybe be a defconstant or a direct number
    (setf code (eval code)))
  (let ((ri (make-restore-info :restore-function-source-code restore-function-signature))
	(restore-info (codespace-restore-infos current-codespace/compile-time)))
    (unless (or (null (gethash code restore-info))
                (equalp (gethash code restore-info) ri))
      (cerror "REPLACE IT" (format nil "Replacing already existing restore code for code ~A" code)))
    (setf (gethash code restore-info) ri)))

(defmacro defrestore (code restore-function-signature)
  `(update-restore-info (get-current-codespace/compile-time) ',code ',restore-function-signature))

(defun store-object/phase (codespace obj store-info-accessor)
  ;; This assumes that the caller has defined OBJ, STORAGE, STORE-OBJECT, and the various
  ;; tables in *ref-tables*.  I don't have the energy to make this all hygenic.
  `(etypecase ,obj
     ,@(strict-subtype-ordering
	(let ((type-dispatch-table nil))
	  (maphash (lambda (type store-info)
                     (push (list type
                                 (funcall store-info-accessor store-info))
                           type-dispatch-table))
		   (codespace-store-infos codespace))
	  type-dispatch-table)
	:key #'first)))
  
(defun make-read-dispatch-table (codespace code-to-dispatch-on)
  ;; Assumes this is in a context where STORAGE, REFERENCES, and RESTORE-OBJECT are defined
  (assert (eq code-to-dispatch-on 'code))
  (let ((code nil))
    (maphash (lambda (dispatch-code restore-info)
               (push (list dispatch-code
                           ;; #+info-cbs `(incf (aref *dispatch-counter* ,dispatch-code))
                           (restore-info-restore-function-source-code restore-info)) code))
             (codespace-restore-infos codespace))
    (let ((numeric-dispatch-codes (sort (remove-if-not #'numberp code :key #'first) #'< :key #'first)))
      `(cond
	 ,@(loop for source-code in (remove-if #'numberp code :key #'first)
		 collect (list (first source-code) (second source-code)))
	 (t (case ,code-to-dispatch-on
	      ,@numeric-dispatch-codes
	      (otherwise
	       (error 'simple-error :format-control "Unknown code ~A found in stream"
				    :format-arguments (list ,code-to-dispatch-on)))))))))

(defun store-objects (storage &rest stuff)
  "Store all the objects in stuff to storage.  Do not call this directly without let'ing
 *current-codespace* to a valid entry in *codespaces*.  Prefer the functions in user.lisp
 which do this for you based on *write-version* and *read-version*."
  (declare (dynamic-extent stuff) (type write-storage storage))
  (let ((codespace *current-codespace*))
    (assert codespace nil "Unknown codespace to store with... is *write-version* not correct?")
    (apply (codespace-store-objects codespace) storage stuff)))

(defun restore-objects (storage)
  "Read data from storage until we run into an end of data signal, or an +end-action-code+.
 If you want to call this directly, you should let *current-codespace* to a codespace, as is
 done in the user facing functions in user.lisp which choose it based on *write-version* and
 *read-version*."
  (declare (type read-storage storage))
  (let ((codespace *current-codespace*))
    (assert codespace nil
	    "Unknown codespace to restore objects with... is *read-version* not correct?")
    (funcall (codespace-restore-objects codespace) storage)))
