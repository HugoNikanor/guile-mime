(add-to-load-path (dirname (current-filename)))

(define-module (mime)
  #:use-module (mime parser)

  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:use-module ((rnrs io ports) #:select (port-eof?))

  #:export (build-mime-table mime-table mimetype))

(define-syntax ->>
  (syntax-rules ()
    ((->> obj)
     obj)
    ((->> obj (func args ...) rest ...)
     (->> (func args ... obj) rest ...))
    ((->> obj func rest ...)
     (->> (func obj) rest ...))))

(define (build-mime-table file-path)
  (->> file-path
       open-input-file
       port->line-stream
       (stream-remove comment/empty?)
       (stream-map parse-line)
       (stream-fold into-hash! (make-hash-table 400))))

(define mime-table (make-parameter (build-mime-table "/etc/mime.types")))

(define (mimetype extension)
  (hash-ref (mime-table) extension))

(define (port->line-stream port)
  "A stream promising to return the contents of port as string,
one line per element"
  (stream-unfold
   read-line
   (negate port-eof?)
   identity
   port))

(define (stream-remove pred stream)
  "(stream-filter (negate pred) stream)"
  (stream-filter (negate pred)
                 stream))

(define (comment/empty? line)
  "Returns true if a string is empty, or starts with an octophorpe."
  (or (string-null? line)
      (eqv? #\# (string-ref line 0))))

;; Adds a record on the form returned from parse-line in
;; (mime parser) to a hash table.
(define into-hash!
  (match-lambda* ((hash-table
                   ('entry ('content-type content-type)
                           ('extension extensions) ...))
                  (for-each (cut hash-set! hash-table <> content-type)
                            extensions)
                  hash-table)))
