(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(map-system-connections defsystem-connection)))

;;; ---------------------------------------------------------------------------
;;; not particularly rich person's system interconnection facility
;;; ---------------------------------------------------------------------------

(defclass system-connection (system)
  ((systems-required :initarg :systems-required :reader systems-required)))

;;; ---------------------------------------------------------------------------

(defvar *system-connections* (make-hash-table :test 'equal))

(defmacro defsystem-connection (name &body options)
  (let ((requires (getf options :requires))
        (depends-on (getf options :depends-on))
        (class (getf options :class 'system-connection))
        (connections (gensym "CONNECTIONS")))
    (remf options :requires)
    (remf options :class)
    `(progn
       (defsystem ,name
         :class ,class
         :depends-on ,(append requires
                              depends-on)
         :systems-required ,requires
         ,@options)
       ,@(mapcar (lambda (r)
                   (flet ((system-name (s)
                            (etypecase s
                              (string s)
                              (symbol (string-downcase (symbol-name s)))
                              (asdf:component (asdf:component-name s)))))
                     (let* ((r    (system-name r))
                            (name (system-name name))
                            (requires
                              (sort (mapcar #'system-name requires) #'string<)))
                       `(let ((,connections
                                (append (gethash ',r *system-connections*)
                                        (list (cons (remove ',r ',requires
                                                            :test #'string=)
                                                    ',name)))))
                          (setf (gethash ',r *system-connections*)
                                ,connections)))))
                 requires)
       (values ',name))))

;;; ---------------------------------------------------------------------------

(defun load-connected-systems (operation component)
  (let ((connections (gethash (typecase component
                                (string component)
                                (symbol (string-downcase (symbol-name component)))
                                (asdf:component (asdf:component-name component)))
                              *system-connections*)))
    (loop :for (prerequisites . connection) :in connections
          :do (when (and (not (component-loaded-p connection))
                         (every #'component-loaded-p prerequisites))
                (asdf:oos operation connection)))))

;;; ---------------------------------------------------------------------------

(defmethod operate :after ((operation t) (component t) &key &allow-other-keys)
  (when (or (eq 'asdf:load-op operation)
            (typep operation 'asdf:load-op))
    (loop :for dep :in (system-depends-on
                        (etypecase component
                          (system
                           component)
                          ((or string symbol)
                           (find-system component))))
          :do (load-connected-systems 'asdf:load-op dep))
    (load-connected-systems 'asdf:load-op component)))

;;; ---------------------------------------------------------------------------

(pushnew :asdf-system-connections *features*)
