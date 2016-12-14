;;
;;
;; Common Lisp Studies
;; Let Over Lambda - Chapter 2
;;
;;

(defmacro nlet (n letargs &rest body)
  "Common Lisp Implementation of Scheme's nlet."
  `(labels ((,n ,(mapcar #'car letargs)               ;; nlet es similar a let pero ademas permite bindear el nombre de una funcion al cuerpo del let
              ,@body))                                ;; Se define un `labels' con el nombre de `,n'
     (,n ,@(mapcar #'cadr letargs))))


(defun flatten (ls)
  "Esta es la supuesta version de Paul Graham de OnLisp."
  (labels ((mklist (x) (if (listp x) x (list x))))    ;; labels es como let, pero para funciones, aqui mklist se define como una funcion que vuelve listas
                                                      ;; cosas que no son listas
    (mapcan #'(lambda (x)                             ;; mapcan evalua cada elemento de la lista y los resultados son listas concatenadas
                (if (atom x)                          ;; si este elemento es un atomo
                    (mklist x)                        ;; -> hago una lista de este elemento, una lista de un solo atomo (y la devuelvo)
                    (flatten x))) ls)))               ;; en caso contrario (el elemento es una lista), aplano esa lista
                                                      ;; todas las listas resultantes seran concatenadas al final en una sola lista, por mapcan

(defmacro defmacro/g! (name args &rest body)      ;; macro definition with name defmacro/g! with parameters name, args and body[&rest]
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p       ;; let with syms defined as all the symbols in body which are not g! symbols
                              (flatten body)))))
    `(defmacro ,name ,args                        ;; macro definition with name and args evaluated
       (let ,(mapcar                              ;; will let from syms, with each definition being the gensym'd name of every symbol
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))                                ;; macro will evaluate the body
