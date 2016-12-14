;;
;; 6.2 Compiling Networks
;;
;; The idea of the following code is to treat nodes as closures, instead
;; of data structures (defstruct). The data stored in the nodes is now
;; stored in the bindings within the closures. We don't need run-node any
;; more. It is implicit in the nodes themselves.


(defvar *nodes* (make-hash-table))

(defun defnode% (name conts &optional yes no)
  "Defnode will put something in the *nodes* hash by name `name'.
Two modes of operation are working here. First we start the traversal
with (funcall (gethash 'people *nodes*)). The interesting part is that
after casing read we will (funcall) the lambda in the *nodes* hash
pointed by yes or no, which is hierachical in nature. The relation
between them is by means of the yes symbol: in the first case it is
'people (which exists and for which the yes option points to 'male).
'male is also defined as a node in the tree."
  (setf (gethash name *nodes*)
        (if yes
            #'(lambda ()
                (format t "~A~%>>  " conts)
                (case (read)
                  (yes (funcall (gethash yes *nodes*)))
                  (t (funcall (gethash no *nodes*)))))
            #'(lambda () conts))))

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he american?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)

;; There is no run-node in this version. To start the traversal, we just
;; have to do:
;;
;; (funcall (gethash 'people *nodes*))


;;
;; New version of the program ;)
;;

;; *nodes* is not a hash anymore.
(defvar *nodes* nil)

(defun defnode (&rest args)
  "Defnode only push stuff into the *nodes* list."
  (push args *nodes*)
  args)

(defun compile-net (root)
  "This will create a net from the assoc list *nodes*, by destructing its
contents and creating functions to follow on both yes and no cases. In the case
of the node not being a list (it has a 'yes option), the net will ask the user
for a response (eq (read)). Both yes and no functions are also nets that have
been compile in a recursive way."
  (let ((node (assoc root *nodes*))) ;; sets node to the value of root in *nodes*
    (if (null node)
        nil
        (let ((conts (second node)) ;; gets the value of conts, yes and no
              (yes (third node))
              (no (fourth node)))
          (if yes ;; if not a leaf
              (let ((yes-fn (compile-net yes)) ;; go down on yes and no
                    (no-fn (compile-net no)))
                #'(lambda ()
                    (format t "~A~%>> " conts)
                    (funcall (if (eq (read) 'yes)
                                 yes-fn
                                 no-fn))))
              #'(lambda () conts))))))
