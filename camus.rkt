#lang racket/base
(require racket/string)
(require racket/list)
(require racket/file)
(require "camus-core.rkt")

; camus

(define (main)
  (displayln "Camus v0.1 beta 20171030\nplz input file name.")
  (let* ((filename (symbol->string (read)))
         (outputfilename (string-append filename ".html"))
         (filecontent (file->string filename)))
    (toHTML/file filename)))

(main)
