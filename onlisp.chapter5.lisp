;;
;; Paul Graham. On Lisp
;; Chapter 5
;;

(defvar *!equivs* (make-hash-table))

(defun !unsafe (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

(def! #'remove-if #'delete-if)

(funcall (!unsafe #'remove-if) #'oddp somelist)

(defun memoize (fn)
  "Memoize function calls."
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      (#'identity)))

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

;; some smart usage of compose, as `not' is just a
;; common lisp function.
(defun complement0 (pred) (compose #'not pred))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))

(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda () (self (cdr lst)))))))
    #'self))

;; Examples of #'lrec
;;
;; copy-list
(lrec #'(lambda (x f) (cons x (funcall f))))

;; remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))

;; find-if, for some function fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))

;; some, for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))
