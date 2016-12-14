;;
;; Paul Graham. On Lisp
;; Chapter 6
;;

(defstruct somestruct title firstname lastname)

(defstruct node contents yes no)
;; clueless about this `defstruct'

(defvar *nodes* (make-hash-table))
;; hash table to hold nodes

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes yes
                   :no no)))

(defun my-run-node (name)
  "This is my version of run-node, compare with next from Graham."
  (let ((got-node (gethash name *nodes*)))
    (if (atomp got-node)
        (return-name-in-node)
        (let ((response (ask-question got-node)))
          (cond ((eq "yes" response) (run-node (got-node :left)))
                ((eq "no" response) (run-node) (got-node :right))
                (t (error "wrong response")))))))

;; and this is Graham's. I think mine is better because in Graham's
;; version he states that if the node has a yes component, it means
;; it is not a leaf, but that's contextual to trees, not the nodes
;; in this algo. In my case, I check if this branch is a leaf, in
;; which case I return the response (name is Lincoln.). My error
;; was in using atomp to check if this was a leaf, but I was only
;; proto-programming.
(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
           (format t "~A~%>> " (node-contents n))
           (case (read)
             (yes (run-node (node-yes n)))
             (t (run-node (node-no n))))))))
