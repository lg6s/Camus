#lang racket/base
(require racket/string)
(require racket/list)
(require racket/file)
(require racket/cmdline)
(require "camus-core.rkt")

; camus

(define (main)
  (case (vector-length (current-command-line-arguments))
    ((0) (displayln "Camus v20180710\nusage: camus [filename]"))
    ((1)               
     (let* ((filename (vector-ref (current-command-line-arguments) 0))
            (outputfilename (string-append filename ".html"))
            (filecontent (file->string filename)))
       (toHTML/file filename)))))
(main)
