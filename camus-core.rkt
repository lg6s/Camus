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

;; v20180711: no explicit block-level <html> & <body> tag anymore.
(define *html-result-header* "<html>\n")
(define *html-result-footer* "</body></html>\n")
;; v20180712: HEADER
;; block-level HEADER tags should:
;; 1. only occur once.
;; 2. be the first block-level tag of the document.
;; the empty lines before & after HEADER is ignored.
(define *header-done?* #f)
(define *first-body-tag-done?* #f)
;; v20180711: ESCAPE
(define *process-flag* #t)
(define (do-not-process) (set! *process-flag* #f))
(define (process) (set! *process-flag* #t))

;; this is to prevent loops.
(define *working-set* '())
(define (inWorkingSet filename)
  (for/or ([i (in-list *working-set*)]) (string=? filename i)))
(define (regWorkingSet filename)
  (set! *working-set* (cons filename *working-set*)))

(define (mkLineLevelP/toHTML name f)
  (cons name
        (λ (fstres strl k)
          (let-values (((result reststrl) (f fstres strl)))
            (toHTML/strlist_k
             reststrl
             (appendstr/k result k))))))
(define linelevelp/toHTML
  `(,(mkLineLevelP/toHTML "INCLUDE"
      (λ (fstres strl)
        (values ""
                (append
                 (string-split (file->string (attribute/linelevel fstres)) "\n")
                 (cdr strl)))))
    ,(mkLineLevelP/toHTML
      "LINK"
      (λ (fstres strl)
        (let ((filename (attribute/linelevel fstres)))
          (when (not (inWorkingSet filename))
            (begin (regWorkingSet filename)
                   (toHTML/file filename)))
          (values (string-append "<a href=\"" filename ".html\">"
                                 (content/linelevel fstres)
                                 "</a>\n")
                  (cdr strl)))))
    ,(mkLineLevelP/toHTML
      "INCLUDE-RAW"
      (λ (fstres strl)
        (values (file->string (attribute/linelevel fstres))
                (cdr strl))))
    ,(mkLineLevelP/toHTML
      "LINK-RAW"
      (λ (fstres strl)
        (values (string-append "<a href=\"" (attribute/linelevel fstres) "\">"
                               (content/linelevel fstres)
                               "</a>\n")
                (cdr strl))))
    ,(mkLineLevelP/toHTML "REFER"
      (λ (fstres strl)
        (values (string-append "<a href=\"#" (attribute/linelevel fstres) "\">"
                               (content/linelevel fstres)
                               "</a>\n")
                (cdr strl))))
    ,(mkLineLevelP/toHTML "LABEL"
      (λ (fstres strl)
        (values (string-append
                 "<a name=\"" (attribute/linelevel fstres) "\">"
                 (toHTML/inline (content/linelevel fstres))
                 "</a>\n")
                (cdr strl))))
    ,(mkLineLevelP/toHTML
      "IMG"
      (λ (fstres strl)
        (values (string-append
                 "<img src=\"" (attribute/linelevel fstres) "\" />\n")
                (cdr strl))))
    ,(mkLineLevelP/toHTML 'DEFAULT
      (λ (fstres strl)
        (values (toHTML/linelevel (car fstres))
                (cdr strl))))
    ))
(define (getlinelevelp/toHTML tagn)
  (assoc tagn linelevelp/toHTML))


;  --------------------------------------------------------------------------

(define normalTextRegexp_r "(?:[^][:\\\\.]|\\\\\\[|\\\\\\]|\\\\\\:|\\\\\\.)")
(define normalTextRegexp/blockattr_r ".")
(define normalTextRegexp/nospc_r "(?:[^][:\\\\. ]|\\\\\\[|\\\\\\]|\\\\\\:|\\\\\\.)")
(define normalTextRegexp/llattr_r "(?:[^][:\\\\]|\\\\\\[|\\\\\\]|\\\\\\:|\\\\\\.)")
(define normalTextRegexp/ll_r "(?:.)")
(define tagTextRegexp/inline1_r "(?:[^][ \\.|])")
(define normalTextRegexp/inline1_r "(?:[^|]|\\\\\\|)")
(define normalTextRegexp/inline2_r "(?:[^|]|\\\\\\|)")
(define blockLevelRegexp_r
  (string-append
   "^(?:(" normalTextRegexp/nospc_r "+))"
   "(?:\\[(" normalTextRegexp/blockattr_r "*)\\])?"
   "\\.([ \\\t]*)$"))
(define lineLevelRegexp_r
  (string-append
   "^(?:(" normalTextRegexp/nospc_r "+))"
   "(?:\\[(" normalTextRegexp/llattr_r "*)\\])?"
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
   (toHTML/inline content)
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
    (cond
      ((string=? tagname "END")
       (if attribute
           (cond
             ((string=? attribute "HEADER")
              (begin (set! *header-done?* #t) "</head><body>"))
             (else
              (begin (when (and (not *first-body-tag-done?*) *header-done?*) (set! *first-body-tag-done?* #t))
                     (string-append "\n</" (if attribute (toHTML/escape attribute) "") ">"))))
           (begin (display "WARNING: invalid END tag. ignoring.\n" (current-error-port)) "")))
      ((string=? tagname "ESCAPE")
       (begin (do-not-process) ""))
      ((string=? tagname "HEADER")
       (if *header-done?*
           (begin (display "WARNING: invalid HEADER position. ignoring.\n" (current-error-port))
                  "")
           "<head>"))
      (else
       (begin (when (and (not *first-body-tag-done?*) *header-done?*) (set! *first-body-tag-done?* #t))
              (string-append "\n<" tagname_esc (toHTML/attribute attribute) ">"))))))
(define (toHTML/blocklevel str)
  (let ((parse-result (regexp-match blockLevelRegexp str)))
    (if parse-result
        (toHTML/blocklevel_r parse-result)
        #f)))


;;; toHTML/inline ------------------------------------------------------

(define (mkInlineP/toHTML name f)
  (cons name
        (λ (fstres str res tagstack)
          (let-values (((result nexttagstack) (f fstres tagstack)))
            (toHTML/inline_i
             (substring str (length/inline1 fstres))
             (string-append res result)
             nexttagstack)))))
(define inlinep/toHTML
  `(
    ,(mkInlineP/toHTML "LINK"
      (λ (fstres tagstack)
        (toHTML/file (attribute/inline1 fstres))
        (values (string-append "<a href=\"" (attribute/inline1 fstres) ".html\">")
                (cons "a" tagstack))))
    ,(mkInlineP/toHTML "REFER"
      (λ (fstres tagstack)
        (values (string-append "<a href=\"#" (attribute/inline1 fstres) "\">")
                (cons "a" tagstack))))
    ,(mkInlineP/toHTML "LABEL"
      (λ (fstres tagstack)
        (values (string-append "<a name=\"" (attribute/inline1 fstres) "\">")
                (cons "a" tagstack))))
    ,(mkInlineP/toHTML
      "IMG"
      (λ (fstres tagstack)
        (values (string-append "<img src=\"" (attribute/inline1 fstres) "\">")
                (cons "img" tagstack))))
    ,(mkInlineP/toHTML 'DEFAULT
      (λ (fstres tagstack)
        (values (string-append "<" (tagname/inline1 fstres)
                               (toHTML/attribute (attribute/inline1 fstres)) ">")
                (cons (tagname/inline1 fstres) tagstack))))
    ))
(define (getinlinep/toHTML tagn)
  (assoc tagn inlinep/toHTML))

;; v20180712: type-2 inline tags.
;; tagname * conent -> string.
(define (mkInline2P/toHTML tagname f)
  (cons tagname f))
(define inline2p/toHTML
  `(
    ,(mkInline2P/toHTML
      "IMG"
      (λ (tagname content)
        (string-append "<img src=\"" content "\" />")))
    ,(mkInline2P/toHTML
      'DEFAULT
      (λ (tagname content)
        (if (string=? "" content)
            (string-append "<" tagname " />")
            (string-append "<" tagname ">" content "</" tagname ">"))))))
(define (getinline2p/toHTML tagn)
  (assoc tagn inline2p/toHTML))

(define (toHTML/inlinep tagn)
  (let ((assocres (getinlinep/toHTML tagn)))
    (if assocres (cdr assocres) (cdr (getinlinep/toHTML 'DEFAULT)))))
(define (toHTML/inline2p tagn)
  (let ((assocres (getinline2p/toHTML tagn)))
    (if assocres (cdr assocres) (cdr (getinline2p/toHTML 'DEFAULT)))))

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
             (cond
               ;; inline type 2: |TAG|content|
               (inline2match
                (let ((tagname (tagname/inline2 inline2match))
                      (content (content/inline2 inline2match)))
                  ;; v20180712: CAMUS tags for inline type 2.
                  (toHTML/inline_i
                   (substring str (length/inline2 inline2match))
                   (string-append res ((toHTML/inline2p (tagname/inline2 inline2match)) tagname content))
                   tagstack)))
               ;; inline type 1: |TAG[ATTR]. content|
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
          (if fstres/blocklevel_r
              ;; block-level.
              ;; v20180711: ESCAPE is ended with block level tag so every block level
              ;; tag should be checked to see if there's an end tag.
              (if (and (string=? "END" (tagname/blocklevel fstres/blocklevel_r))
                       (string=? "ESCAPE" (attribute/blocklevel fstres/blocklevel_r)))
                  ;; ESCAPE. and end[ESCAPE]. does not map to any HTML tags.
                  (begin (process) (toHTML/strlist_k rst k))
                  (toHTML/strlist_k rst (appendstr/k
                                         (if *process-flag*
                                             (toHTML/blocklevel_r fstres/blocklevel_r)
                                             (string-append fst "\n"))
                                         k)))
              ;; line-level.
              (begin
                (if *process-flag*
                    (cond
                      ;; line-level tag detected.
                      (fstres/linelevel_r
                       (begin (when (and *header-done?* (not *first-body-tag-done?*))
                                (set! *first-body-tag-done?* #t))
                              ((toHTML/linelevelp (tagname/linelevel fstres/linelevel_r)) 
                               fstres/linelevel_r strl k)))
                      ((empty?/string fst)
                       (let-values (((empty-lines rst) (splitf-at strl empty?/string)))
                         (cond
                           ((or (not *header-done?*) (not *first-body-tag-done?*))
                            (toHTML/strlist_k rst k)) ;; ignored.
                           (else
                            (toHTML/strlist_k rst (appendstr/k (if (= 1 (length empty-lines)) "<br />" "<br /><br />") k))))))
                      (else
                       ;; normal text.
                       (begin (when (and *header-done?* (not *first-body-tag-done?*))
                                (set! *first-body-tag-done?* #t))
                              (toHTML/strlist_k
                               rst
                               (appendstr/k (string-append (toHTML/inline fst) "\n") k)))))
                    (toHTML/strlist_k rst (appendstr/k (string-append fst "\n") k)))))))))
(define (toHTML str)
  (toHTML/strlist_k (string-split str "\n") (λ (x) x)))
(define (toHTML/file filename)
  (let ((outputfilename (string-append filename ".html"))
        (filecontent (file->string filename)))
    (with-output-to-file outputfilename
      (λ ()
        (displayln *html-result-header*)
        (displayln (toHTML filecontent))
        (displayln *html-result-footer*))
      #:exists 'replace)))
