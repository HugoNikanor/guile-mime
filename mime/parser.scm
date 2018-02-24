(define-module (mime parser)
  #:use-module (ice-9 peg)
  #:export (parse-line))

(define (parse-line str)
  (keyword-flatten
   '(content-type extension)
   (peg:tree (match-pattern entry str))))

(define-peg-pattern NL none "\n")
(define-peg-pattern TAB none "\t")
(define-peg-pattern SPC none " ")

(define-peg-pattern WS none (or TAB SPC))

(define-peg-pattern content-type all
  (and (peg "[-.+a-zA-Z]+")
       "/"
       (peg "[-.+a-zA-Z0-9]+")))

(define-peg-pattern extension all
  (+ (peg "[a-zA-Z0-9]")))

(define-peg-pattern entry all
  (or (and content-type
           (+ WS)
           (+ (and extension (ignore (* SPC)))))
      content-type))

