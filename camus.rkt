#lang racket/base
(require racket/string)
(require racket/list)
(require racket/file)
(require "camus-core.rkt")

; camus

(displayln "Camus v0.1 beta\nplz input file name.")
(let* ((filename (symbol->string (read)))
       (outputfilename (string-append filename ".html"))
       (filecontent (file->string filename)))
  (begin
    (displayln "no include? [\"yes\" for not doing include; \"no\" for the opposite]")
    (let ((noinclude (read)))
      (when (equal? noinclude 'yes)
        (begin
            (set!/flag 'noInclude #t)
            (displayln "INCLUDE directive will now be processed into <a> links."))))
    (toHTML/file filename)))
