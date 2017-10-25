#lang racket/base
(require racket/string)
(require racket/list)
(require racket/file)
(provide (all-defined-out))

; camus-core

#|
(define flags/toHTML
  '((noInclude . #f)))
(define (getflag/toHTML flagn)  (cdr (assoc flagn flags/toHTML)))
(define (flagset?/toHTML flagn)  (cdr (assoc flagn flags/toHTML)))
(define (set!/flag flagn val)
  (let-values ([(before after) (splitf-at flags/toHTML (λ (x) (not (equal? (car x) flagn))))])
    (set! flags/toHTML (append before (cons (cons flagn val) (cdr after))))))
(define (extflag/toHTML flagn) (set! flags/toHTML (cons (cons flagn #f) flags/toHTML)))

|#

(define linelevelp/toHTML
  `(("INCLUDE" .
     ,(λ (fstres strl k)
        (toHTML/strlist_k
         (append
          (string-split (file->string (attribute/linelevel fstres)) "\n")
          (cdr strl))
         k)))
    ("LINK" .
     ,(λ (fstres strl k)
        (begin
          (toHTML/file (attribute/linelevel fstres))
          (toHTML/strlist_k
           (cdr strl)
           (appendstr/k
            (string-append
             "<a href=\"" (attribute/linelevel fstres) ".html\">"
             (content/linelevel fstres)
             "</a>\n")
            k)))))
    (DEFAULT .
       ,(λ (fstres strl k)
         (toHTML/strlist_k
          (cdr strl)
          (appendstr/k (toHTML/linelevel (car fstres)) k))))
    ))
(define (getlinelevelp/toHTML tagn)
  (assoc tagn linelevelp/toHTML))


(define inlinep/toHTML
  `(("LINK" .
     ,(λ (fstres str res tagstack)
        (begin
          (toHTML/file (attribute/inline1 fstres))
          (toHTML/inline_i
           (substring str (length/inline1 fstres))
           (string-append
            res
            "<a href=\"" (attribute/inline1 fstres) ".html\">")
           (cons "a" tagstack)))))
    (DEFAULT .
      ,(λ (fstres str res tagstack)
         (toHTML/inline_i
          (substring str (length/inline1 fstres))
          (string-append
           res
           "<" (tagname/inline1 fstres)
           (toHTML/attribute (attribute/inline1 fstres)) ">")
          (cons (tagname/inline1 fstres) tagstack))))
    ))
(define (getinlinep/toHTML tagn)
  (assoc tagn inlinep/toHTML))

;  --------------------------------------------------------------------------

(define normalTextRegexp_r "(?:[^][:\\\\.]|\\\\\\[|\\\\\\]|\\\\\\:|\\\\\\.)")
(define normalTextRegexp/nospc_r "(?:[^][:\\\\. ]|\\\\\\[|\\\\\\]|\\\\\\:|\\\\\\.)")
(define normalTextRegexp/ll_r "(?:[^][:\\\\]|\\\\\\[|\\\\\\]|\\\\\\:|\\\\\\.)")
(define tagTextRegexp/inline1_r "(?:[^ \\.|])")
(define normalTextRegexp/inline1_r "(?:[^|]|\\\\\\|)")
(define normalTextRegexp/inline2_r "(?:[^|]|\\\\\\|)")
(define blockLevelRegexp_r
  (string-append
   "^(?:(" normalTextRegexp/nospc_r "+))"
   "(?:\\[(" normalTextRegexp_r "*)\\])?"
   "\\.([ \\\t]*)$"))
(define lineLevelRegexp_r
  (string-append
   "^(?:(" normalTextRegexp/nospc_r "+))"
   "(?:\\[(" normalTextRegexp/ll_r "*)\\])?"
   ": (" normalTextRegexp/ll_r "*)([ \\\t]*)$"))
(define blockLevelRegexp (regexp blockLevelRegexp_r))
(define lineLevelRegexp (regexp lineLevelRegexp_r))
(define isDirectiveRegexp #rx"^([ABCDEFGHIJKLMNOPQRSTUVWXYZ_]+.*)$")
(define escapeRegexp #rx"\\\\([^\\\\])")
(define inline2Regexp
  (regexp
   (string-append
    "^\\|([^ |]+)\\|("
    normalTextRegexp/inline2_r
    "*)\\|")))
(define inline1Regexp
  (regexp (string-append
           "^\\|(" tagTextRegexp/inline1_r "+)"
           "(?:\\[(" normalTextRegexp/inline1_r "+)\\])?"
           "\\. "
           )))

(define tagname/blocklevel cadr)
(define attribute/blocklevel caddr)
(define tagname/linelevel cadr)
(define attribute/linelevel caddr)
(define content/linelevel cadddr)
(define tagname/inline2 cadr)
(define content/inline2 caddr)
(define tagname/inline1 cadr)
(define attribute/inline1 caddr)
(define (length/inline1 matchres) (string-length (car matchres)))
(define (length/inline2 matchres) (string-length (car matchres)))

(define (toHTML/escape str)
  (regexp-replace* escapeRegexp str "\\1"))
(define (toHTML/attribute attrstr)
  (if attrstr
      (string-append " " (regexp-replace* #rx"," (toHTML/escape attrstr) " "))
      ""))

(define (toHTML/linelevel_r fstres)
  (let* ((tagname (tagname/linelevel fstres))
         (attribute (attribute/linelevel fstres))
         (content (content/linelevel fstres))
         (tagname_esc (toHTML/escape tagname)))
  (string-append
   "<" tagname_esc (toHTML/attribute attribute) ">"
   content
   "</" tagname_esc ">")))
(define (toHTML/linelevel str)
  (let ((parse-result (regexp-match lineLevelRegexp str)))
    (if parse-result
        (toHTML/linelevel_r parse-result)
        #f)))

(define (toHTML/blocklevel_r fstres)
  (let* ((tagname (tagname/blocklevel fstres))
         (attribute (attribute/blocklevel fstres))
         (tagname_esc (toHTML/escape tagname)))
    (if (string=? tagname "end")
        (string-append "\n</" (if attribute (toHTML/escape attribute) "") ">")
        (string-append "\n<" tagname_esc (toHTML/attribute attribute) ">"))))
(define (toHTML/blocklevel str)
  (let ((parse-result (regexp-match blockLevelRegexp str)))
    (if parse-result
        (toHTML/blocklevel_r parse-result)
        #f)))


; ---------------------------------------------------

(define (toHTML/inlinep tagn)
  (let ((assocres (getinlinep/toHTML tagn)))
    (if assocres (cdr assocres) (cdr (getinlinep/toHTML 'DEFAULT)))))

(define (toHTML/inline_i str res tagstack)
  (if (string=? str "") res
      (let ((pivot (string-ref str 0)))
        (case pivot
          ((#\\) (toHTML/inline_i
                  (substring str 2)
                  (string-append res (make-string 1 (string-ref str 1)))
                  tagstack))
          ((#\|)
           (let ((inline2match (regexp-match inline2Regexp str))
                 (inline1match (regexp-match inline1Regexp str)))
             (cond (inline2match
                    (let ((tagname (tagname/inline2 inline2match))
                          (content (content/inline2 inline2match)))
                      (toHTML/inline_i
                       (substring str (length/inline2 inline2match))
                       (string-append res "<" tagname ">" content "</" tagname ">")
                       tagstack)))
                   (inline1match
                    ((toHTML/inlinep (tagname/inline1 inline1match))
                     inline1match str res tagstack))
                   (else
                         (toHTML/inline_i
                          (substring str 1)
                          (string-append
                           res
                           (if (null? tagstack) "|"
                               (string-append "</" (car tagstack) ">")))
                          (if (null? tagstack) tagstack (cdr tagstack)))))))
          (else
                (toHTML/inline_i
                 (substring str 1)
                 (string-append res (make-string 1 (string-ref str 0)))
                 tagstack))))))
(define (toHTML/inline str)
  (toHTML/inline_i str "" '()))
(define (isTag? str) (or (toHTML/blocklevel str) (toHTML/linelevel str)))
(define (toHTML/singl str) (or (toHTML/blocklevel str) (toHTML/linelevel str)))
(define (appendstr/k str k) (λ (rststr) (string-append (k rststr) str)))
(define (empty?/string str) (string=? (string-trim str) ""))


;--------------------------------------------------


(define (toHTML/linelevelp tagn)
  (let ((assocres (getlinelevelp/toHTML tagn)))
    (if assocres (cdr assocres) (cdr (getlinelevelp/toHTML 'DEFAULT)))))

(define (toHTML/strlist_k strl k)
  (if (null? strl) (k "")
      (let ([fst (car strl)] [rst (cdr strl)])
        (let ([fstres/blocklevel_r (regexp-match blockLevelRegexp fst)]
              [fstres/linelevel_r (regexp-match lineLevelRegexp fst)])
          (cond
            (fstres/blocklevel_r
             (toHTML/strlist_k rst (appendstr/k (toHTML/blocklevel_r fstres/blocklevel_r) k)))
            (fstres/linelevel_r
             ((toHTML/linelevelp (tagname/linelevel fstres/linelevel_r)) 
              fstres/linelevel_r strl k))
            (else
             (let-values
                 ([(paragraph afterp)
                   (splitf-at rst (λ (x) (and (not (isTag? x))
                                               (not (empty?/string x)))))])
               (toHTML/strlist_k
                afterp
                (appendstr/k
                 (string-append
                  "<p>\n"
                  (apply string-append
                         (map (λ (s) (string-append (toHTML/inline s) "\n"))
                              (if (empty?/string fst) paragraph (cons fst paragraph))))
                  "</p>\n")
                 k)))))))))
(define (toHTML str)
  (toHTML/strlist_k (string-split str "\n") (λ (x) x)))
(define (toHTML/file filename)
  (let ((outputfilename (string-append filename ".html"))
        (filecontent (file->string filename)))
    (with-output-to-file outputfilename
      (λ () (displayln (toHTML filecontent)))
      #:exists 'replace)))
