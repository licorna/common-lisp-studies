;;
;;
;;  Let Over Lambda. Chapter 1
;;
;;

(defun mkstr (&rest args)
  "Writes the object representation of each element of args into the string."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Makes a symbol from a string."
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  "Groups source list into sublists of size n."
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  "Flattens a list, this is, splices deep lists contents into main list."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))
