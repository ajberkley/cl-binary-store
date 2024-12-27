(in-package :cl-binary-store)

;; A codespace is the collection of defstore / defrestore definitions.
;; It defines the data structure written on disk.  So far, we use a
;; single byte as the first dispatch mechanism for an object, so the
;; codespace defines a mapping from that byte to a deserialization
;; routine or a serialization routine.  So, for example maybe all #s >
;; 128 could be a single instruction using the high bit as a tag, so
;; for example one could encode 0-127 by using the high bit thus
;; achieving high density for small numbers.  But that might not be
;; the best use of the codespace for some other application, thuse we
;; allow codespaces to be switched in and out based on what the user
;; requests or what is requested within the deserialized data.  The
;; version/magic number specifies a codespace.

(defvar *codespaces* (make-hash-table :test 'eql)
  "a map from magic/version-number -> `codespace'")

(defvar *current-codespace* nil "a `codespace' bound by store based on
 *write-version*.  This is also bound during restore operations once we
 know the format of the data stream, or else it is bound to *default-codespace*")

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
  (restore-objects #'invalid :type function)
  (store-objects #'invalid :type function)
  (report-dispatch-counts #'invalid :type function)) ;; not implemented

(defun deep-copy-codespace (target source-codespace)
  (setf (codespace-ref-tables target) (codespace-ref-tables source-codespace))
  (setf (codespace-store-infos target) (codespace-store-infos source-codespace))
  (setf (codespace-restore-infos target) (codespace-restore-infos source-codespace)))

(defun build-restore-objects ()
  "Returns all the elements in storage.  If a single element just
     returns it, otherwise returns a list of all elements restored."
  `(let* ((references-vector (make-array 2048))
	  (references (make-references :vector references-vector))
	  (*version-being-read* nil))
     (declare (dynamic-extent references references-vector))
     (labels ((read-dispatch (code storage restore-object)
		,(make-read-dispatch-table 'code))
	      (restore-object (&optional (tag (restore-ub8 storage)))
                (read-dispatch tag storage #'restore-object)))
       (let ((objects
               (loop
		 with object and ignore/eof
		 for code = (maybe-restore-ub8 storage)
		 while code
		 do #+debug-cbs (format t "Read code ~A (offset ~A max ~A)~%" code
					(storage-offset storage) (storage-max storage))
                    (setf (values object ignore/eof) (restore-object code))
		 #+debug-cbs (format t "Got object type ~A, ignore/eof ~S~%" (type-of object)
				     ignore/eof)
		 until (eq ignore/eof :end)
		 unless (eq ignore/eof :ignore)
                   collect object)))
	 (apply #'values objects)))))

(defun build-store-objects ()
  `(let* ((track-references *track-references*)
          (slot-info (make-hash-table :test 'eql))
	  (*slot-info* slot-info))
     (declare (dynamic-extent slot-info))
     ,(with-reference-tables 'track-references
	#+debug-cbs `(format t "Starting reference counting pass on ~A objects~%" (length stuff))
	`(labels ((store-object2 (obj)
		    (let ((store-object #'store-object)
			  (storage nil))
		      (store-object/reference-phase obj)))
		  (store-object (obj)
		    (let ((store-object #'store-object2)
			  (storage nil))
		      (store-object/reference-phase obj))))
	   (declare (inline store-object2)) ;; inline one level deep
	   (when track-references
	     (dolist (elt stuff)
	       (store-object elt))))
	#+debug-cbs `(format t "Finished reference counting pass~%")
	`(let ((ref-id 0))
           (declare (type fixnum ref-id))
           (when track-references
             (map-reference-tables #'analyze-references-hash-table) ;; debugging only
             ;; Now clean up the references table: delete anyone who has no references
             #+debug-cbs `(format t "Generating real reference hash-table~%")
             (map-reference-tables
	      (lambda (table-name table)
		(declare (ignore table-name))
		(maphash (lambda (k v)
        	           (if (> (the fixnum v) 1)
        		       (setf (gethash k table) (- (incf ref-id))) ; signal it needs writing
			       (remhash k table)))
			 table)))
             #+debug-cbs
             `(map-reference-tables (lambda (table-name table)
                                      (format t "~A: there are ~A actual references~%"
                                              table-name
                                              (hash-table-count table)))))
           #+debug-cbs `(format t "Starting actual storage phase~%")
           (labels ((store-object2 (obj) ;; inline one deep
		      (let ((store-object #'store-object))
			(store-object/storage-phase obj)))
                    (store-object (obj)
		      (let ((store-object #'store-object2))
			(store-object/storage-phase obj))))
             (declare (inline store-object2))
             (when (>= ref-id 2048) ;; if we would have to expand the references vector
	       #+debug-cbs `(format t "Writing total reference count ~A to file~%" (1+ ref-id))
	       (write-reference-count (1+ ref-id) #'store-object))
             (dolist (elt stuff)
	       (store-object elt))
	     (when *write-end-marker* (store-object (make-end-marker)))))
	`(flush-write-storage storage))))

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
  ;; First build the code for the store-object phases
  (unwind-protect
       (progn
	 (setf *current-codespace/compile-time* codespace)
	 (setf (codespace-store-objects codespace)
	       (compile nil
			`(lambda (storage &rest stuff)
			   (declare (optimize speed safety) (type storage storage)
				    (dynamic-extent stuff))
			   ,(build-store-objects))))
	 (setf (codespace-restore-objects codespace)
	       (compile nil `(lambda (storage)
			       ,(build-restore-objects)))))
    (setf *current-codespace/compile-time* nil))
  codespace)

(defmacro define-codespace ((name magic-number &key inherits-from) &body body)
  "Creates and registers a codespace into *codespaces*."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unwind-protect
	  (progn
	    (setf *current-codespace/compile-time*
		  (let ((codespace (make-codespace :magic-number ,magic-number :name ,name)))
		    ,(when inherits-from
		       `(deep-copy-codespace codespace (gethash ,inherits-from *codespaces*)))
		    codespace))
	    ,@body
	    (when (gethash ,magic-number *codespaces*)
	      (format t "WARNING: redefining code-space ~A~%" ,magic-number))
	    (setf (gethash ,magic-number *codespaces*)
		  (compile-codespace *current-codespace/compile-time*)))
       (setf *current-codespace/compile-time* nil))))

(defstruct restore-info
  (restore-function-code nil))
  
(defstruct store-info
  (type nil)
  (reference-phase-code nil)
  (storage-phase-code nil))

(defstruct ref-table
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
  `(register-references& *current-codespace/compile-time* ',table-name ',construction-code))
  
(defun with-reference-tables (track-references &rest body)
  "Wrap body with defined reference tables"
  (assert *current-codespace/compile-time*)
  (assert (codespace-ref-tables *current-codespace/compile-time*))
  (let ((let-bindings nil))
    (maphash (lambda (table-name ref-table)
               (push (list table-name `(when ,track-references
                                         ,(ref-table-construction-code ref-table)))
                     let-bindings))
             (codespace-ref-tables *current-codespace/compile-time*))
    `(let (,@let-bindings)
       (declare (dynamic-extent ,@(mapcar #'first let-bindings)))
       ,@body)))
  
(defmacro map-reference-tables (func)
  (let ((code nil))
    (maphash (lambda (table-name ref-table)
               (declare (ignorable ref-table))
               (push `(funcall ,func ',table-name ,table-name) code))
             (codespace-ref-tables *current-codespace/compile-time*))
    `(progn ,@code)))

(defun update-store-info
    (codespace type store-function-signature
     &key (call-during-reference-phase nil call-during-reference-phase-provided-p)
       check-for-ref-in write-phase-code)
  (labels ((maybe-wrap-code-with-ref-check-for-store-phase (code)
             (if check-for-ref-in
                 `(unless (referenced-already obj storage ,check-for-ref-in)
                    ,@(when write-phase-code
                        `((store-ub8 ,write-phase-code storage nil)))
                    ,code)
                 code))
           (maybe-wrap-code-with-ref-check-for-ref-phase (code)
             (if check-for-ref-in
                 `(unless (check-reference obj ,check-for-ref-in t)
                    ,code)
                 code)))
    (loop for param in (cdr store-function-signature)
          do (unless (or (member param '(obj storage store-object)) (gethash param (codespace-ref-tables codespace)))
               (error (format nil "While parsing DEFSTORE for ~A, ~A is an unknown parameter of DEFSTORE~%~
                                   it must be one of OBJ, STORAGE, STORE-OBJECT or a reference table name from~%~
                                   REGISTER-REFERENCES" type param))))
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
                  (equalp (gethash type store-info) si))
        (cerror "REPLACE IT" (format nil "Replacing already existing store code for type ~A" type)))
      (setf (gethash type store-info) si))))

(defmacro defstore
    (type store-function-signature
     &key (call-during-reference-phase nil call-during-reference-phase-provided-p)
       check-for-ref-in write-phase-code)
  `(update-store-info *current-codespace/compile-time* ',type ',store-function-signature
		      ,@(if call-during-reference-phase-provided-p
			    `(:call-during-reference-phase ',call-during-reference-phase))
		      :check-for-ref-in ',check-for-ref-in
		      :write-phase-code ',write-phase-code))

(defun update-restore-info (current-codespace/compile-time code restore-function-signature)
  (let ((code (eval code)))
    (loop for param in (cdr restore-function-signature)
          do (unless (member param '(storage references restore-object))
               (error (format nil "While parsing DEFRESTORE for code ~A, found unknown param ~A, it must be one~%~
                                 of STORAGE, REFERENCES, or RESTORE-OBJECT" code param))))
    (let ((ri (make-restore-info :restore-function-code restore-function-signature))
	  (restore-info (codespace-restore-infos current-codespace/compile-time)))
      (unless (or (null (gethash code restore-info))
                  (equalp (gethash code restore-info) ri))
        (cerror "REPLACE IT" (format nil "Replacing already existing restore code for code ~A" code)))
      (setf (gethash code restore-info) ri))))

(defmacro defrestore (code restore-function-signature)
  `(update-restore-info *current-codespace/compile-time* ,code ',restore-function-signature))

(defun store-object/phase (obj store-info-accessor)
  ;; This assumes that the caller has defined OBJ, STORAGE, STORE-OBJECT, and the various
  ;; tables in *ref-tables*.  I don't have the energy to make this all hygenic.
  `(etypecase ,obj
     ,@(strict-subtype-ordering
	(let ((type-dispatch-table nil))
	  (maphash (lambda (type store-info)
                     (push (list type
                                 (funcall store-info-accessor store-info))
                           type-dispatch-table))
		   (codespace-store-infos *current-codespace/compile-time*))
	  type-dispatch-table)
	:key #'first)))
  
(defmacro store-object/storage-phase (obj)
  (declare (ignorable obj))
  (assert (string= (symbol-name obj) "OBJ"))
  (store-object/phase obj 'store-info-storage-phase-code))

(defmacro store-object/reference-phase (obj)
  (declare (ignorable obj))
  (assert (string= (symbol-name obj) "OBJ"))
  (store-object/phase obj 'store-info-reference-phase-code))

;; NOT IMPLEMENTED YET
;; #+info-cbs
;; (declaim (type (simple-array fixnum (256)) *dispatch-counter*))
;; #+info-cbs
;; (defparameter *dispatch-counter* (make-array 256 :element-type 'fixnum :initial-element 0))
;; #+info-cbs
;; (defun report-dispatch-counts ()
;;   (let ((res nil))
;;     (maphash (lambda (dispatch-code restore-info)
;;                (push (cons (aref *dispatch-counter* dispatch-code)
;;                            (format nil "#~A (count ~A) ~A"
;;                                    dispatch-code
;;                                    (aref *dispatch-counter* dispatch-code)
;;                                    (restore-info-restore-function-code restore-info)))
;;                      res))
;;              (codespace-restore-infos *current-codespace/compile-time*))
;;     (format t "~{~A~%~}" (mapcar #'cdr (sort res #'> :key #'car)))
;;     (values)))

(defun make-read-dispatch-table (code-to-dispatch-on)
  ;; Assumes this is in a context where STORAGE, REFERENCES, and RESTORE-OBJECT are defined
  (let ((code nil))
    (maphash (lambda (dispatch-code restore-info)
               (push (list dispatch-code
                           #+info-cbs `(incf (aref *dispatch-counter* ,dispatch-code))
                           (restore-info-restore-function-code restore-info)) code))
             (codespace-restore-infos *current-codespace/compile-time*))
    (setf code (sort code #'< :key #'first))
    `(case ,code-to-dispatch-on
       ,@code
       (otherwise
        (error 'simple-error :format-control "Unknown code ~A found in stream"
                             :format-arguments (list ,code-to-dispatch-on))))))

(defun store-objects (storage &rest stuff)
  (declare (dynamic-extent stuff) (type storage storage))
  (let ((codespace *current-codespace*))
    (assert codespace nil "Unknown codespace to store with... is *write-version* not correct?")
    (apply (codespace-store-objects codespace) storage stuff)))

(defun restore-objects (storage)
  ;; Starts with whatever the current-codespace is, may change the
  ;; codespace when we hit a magic number.
  (declare (type storage storage))
  (let ((codespace *current-codespace*))
    (assert codespace)
    (funcall (codespace-restore-objects codespace) storage)))
