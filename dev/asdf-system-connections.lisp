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
  (let* ((component (etypecase component
                      (system
                       component)
                      ((or string symbol)
                       (find-system component))
                      (list
                       (find-system
                        (ecase (car component)
                          (:version (second component))
                          (:feature (third component)))))))
         (deps (system-depends-on component))
         (connections (gethash (component-name component)
                               *system-connections*)))
    (loop :for dep :in deps
          :do (load-connected-systems 'asdf:load-op dep))
    (loop :for (prerequisites . connection) :in connections
          :do (when (and (not (component-loaded-p connection))
                         (every #'component-loaded-p prerequisites))
                (dolist (prereq prerequisites)
                  (dolist (dep (system-depends-on (find-system prereq)))
                    (load-connected-systems operation dep)))
                (asdf:oos operation connection)))))

;;; ---------------------------------------------------------------------------

(defmethod operate :after ((operation t) (component t) &key &allow-other-keys)
  (when (or (eq 'asdf:load-op operation)
            (typep operation 'asdf:load-op))
    (load-connected-systems 'asdf:load-op component)))

;;; ---------------------------------------------------------------------------

(pushnew :asdf-system-connections *features*)
