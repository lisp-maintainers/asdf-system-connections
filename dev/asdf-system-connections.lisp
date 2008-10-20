(in-package #:asdf)

(eval-when (:compile-toplevel :load-toplevel)
  (export '(map-system-connections
	    defsystem-connection)))

;;; ---------------------------------------------------------------------------
;;; not particularly rich person's system interconnection facility
;;; ---------------------------------------------------------------------------

(defclass system-connection (system)
  ((systems-required :initarg :systems-required :reader systems-required)
   (been-loaded? :accessor been-loaded? :initform nil)))

;;; ---------------------------------------------------------------------------

(defun map-system-connections (fn)
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (typep (cdr v) 'system-connection)
               (funcall fn (cdr v))))
           *defined-systems*))

;;; ---------------------------------------------------------------------------

(defmacro defsystem-connection (name &body options)
  (let ((requires (getf options :requires))
        (class (getf options :class 'system-connection)))
    (remf options :requires)
    (remf options :class)
    `(progn
       (defsystem ,name
         :class ,class
         :systems-required ,requires
         ,@options) 
       (values ',name))))

;;; ---------------------------------------------------------------------------

(defun load-connected-systems ()
  (map-system-connections 
   (lambda (connection)
     (when (and (required-systems-loaded-p connection)
                (not (system-loaded-p (component-name connection)))
                (not (been-loaded? connection)))
       (setf (been-loaded? connection) t)
       (asdf:oos 'asdf:load-op (component-name connection) :force t)))))

#+Test
(load-connected-systems)

(defun required-systems-loaded-p (connection)
  (every (lambda (system)
           (system-loaded-p system))
         (systems-required connection)))

;;; ---------------------------------------------------------------------------

(defun system-loaded-p (system-name)
  (let ((system (find-system-in-memory system-name)))
    (when system
      (gethash 'load-op (asdf::component-operation-times system)))))

;;; ---------------------------------------------------------------------------

(defmethod perform :around ((operation load-op) (system system))
  (call-next-method)
  (load-connected-systems))

;;; ---------------------------------------------------------------------------

(defmethod operation-done-p :around ((o load-op) (c system))
  (let ((it (find-system-in-memory c)))
    (if (typep it 'system-connection)
      (been-loaded? it)
      (call-next-method))))

;;; ---------------------------------------------------------------------------

(defun find-system-in-memory (system-name)
  (let* ((name (coerce-name system-name))
        (system (gethash name *defined-systems*)))
    (when system
      (cdr system))))

(pushnew :asdf-system-connections *features*)
