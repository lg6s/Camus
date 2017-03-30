#lang racket/base
(require racket/string)
(require racket/list)

; camus

(define blockLevelRegexp #rx"^([^ \t[]*)(\\[(.*)\\])?\\.$")
(define lineLevelRegexp #rx"^([^:]+?)(\\[(.*?)\\])?\\: (.*)$")
(define inlineRegexp
  #rx"(.*?)\\|([^] \t\\|[.0123456789]+)(\\[(.*?)\\])?\\. (.*)\\|(.*)")
(define inlineRegexp-short
  #rx"(.*?)\\|([^] \t\\|0123456789]+)(\\[(.*?)\\])?\\|([^\\|]*)\\|(.*)")
(define isDirectiveRegexp
  #rx"^[^abcdefghijklmnopqrstuvwxyz]+$")

(define (isDirective? str) (regexp-match? isDirectiveRegexp str))

(define (getTagname-blockLevel matchres) (cadr matchres))
(define (getAttribute-blockLevel matchres) (cadddr matchres))
(define (getTagname-lineLevel matchres) (cadr matchres))
(define (getAttribute-lineLevel matchres) (cadddr matchres))
(define (getContent-lineLevel matchres) (car (cddddr matchres)))
(define (getPrestr-inline matchres) (cadr matchres))
(define (getTagname-inline matchres) (caddr matchres))
(define (getAttribute-inline matchres) (car (cddddr matchres)))
(define (getContent-inline matchres) (cadr (cddddr matchres)))
(define (getPoststr-inline matchres) (caddr (cddddr matchres)))

(define (toHTML-attribute str)
  (regexp-replace* #rx"," str " "))

(define (toHTML-no-camus str) str)
(define (toHTML-inline-asDirective matchres)
  (let ((prestr (getPrestr-inline matchres))
        (tagname (getTagname-inline matchres))
        (attr (getAttribute-inline matchres))
        (content (getContent-inline matchres))
        (outro (getPoststr-inline matchres)))
    (cond ((string=? tagname "END")
           ; rebuild the whole string...
           (string-append
            prestr "|" tagname (if attr attr "") "|" content "|" (toHTML-inline outro)))
          ((string=? tagname "NO-CAMUS")
           (string-append
            prestr (toHTML-no-camus content) (toHTML-inline outro)))
          ; todo: add support for the INCL directive.
          ((string=? tagname "LINK")
           (if attr
               (string-append
                prestr "<a href=\"" content "\">" (toHTML-inline attr) "</a>" outro)
               (string-append
                prestr "<a href=\"" content "\" />" outro)))
          ((string=? tagname "IMG")
           (string-append
            prestr "<img src=\"" content "\" " (toHTML-attribute attr) " />" outro))
          (#T
           ; rebuild the whole string...
           (string-append
            prestr "|" tagname "|" content "|" (toHTML-inline outro)))
          )))
(define (toHTML-inline str)
  (cond  ((regexp-match? inlineRegexp-short str)
          (let* ((matchres (regexp-match inlineRegexp-short str))
                 (prestr (getPrestr-inline matchres))
                 (tagname (getTagname-inline matchres))
                 (attr (getAttribute-inline matchres))
                 (content (getContent-inline matchres))
                 (outro (getPoststr-inline matchres)))
            (if (isDirective? tagname)
                (toHTML-inline-asDirective matchres)
                (toHTML-inline
                 (string-append
                  prestr
                  "<" tagname (if attr (string-append " " (toHTML-attribute attr)) "") ">"
                  content
                  "</" tagname ">"
                  outro)))))
         ((regexp-match? inlineRegexp str)
          (let* ((matchres (regexp-match inlineRegexp str))
                 (prestr (getPrestr-inline matchres))
                 (tagname (getTagname-inline matchres))
                 (attr (getAttribute-inline matchres))
                 (content (getContent-inline matchres))
                 (outro (getPoststr-inline matchres)))
            #|
            there's some dirty workaround here because I want this:
            some text |strong. some texts |em. some other| texts| some other texts
            will be rendered as:
            some text <strong>some texts <em>some other</em> texts</strong> some other texts
            so there's the new rule:
            1. inline tags written like |strong|something| will match the nearest |.
            2. inline tags written like |strong. some other texts| will match the furthest |.
            so we have two regexp for matching inline tags...
            |#
            (if (isDirective? tagname)
                (toHTML-inline-asDirective matchres)
                (toHTML-inline
                 (string-append
                  prestr
                  "<" tagname (if attr (string-append " " (toHTML-attribute attr)) "") ">"
                  content
                  "</" tagname ">"
                  outro)))))
         (#t
          str)))
(define (toHTML-singl str)
  (cond ((regexp-match? lineLevelRegexp str)
         (let* ((matchres (regexp-match lineLevelRegexp str))
                (tagname (getTagname-lineLevel matchres))
                (attribute (getAttribute-lineLevel matchres))
                (content (getContent-lineLevel matchres)))
           (if (isDirective? tagname)
               (toHTML-inline-asDirective
                (list #f ; placeholder only.
                      "" ; prestr
                      tagname
                      "" ; placeholder.
                      attribute
                      content
                      "" ; no outro.
                      ))
               (string-append
                "<" tagname ">" content "</" tagname ">"))))
        ((or (regexp-match? inlineRegexp str)
             (regexp-match? inlineRegexp-short str))
         (toHTML-inline str))
        (#T str)))
(define (toHTML-multi-stated strlist st)
  (cond ((null? strlist) "")
        ((string=? (car strlist) "END.")
         (string-append
          "</" (car st) ">\n"
          (toHTML-multi-stated (cdr strlist) (cdr st))))
        ((string=? (car strlist) "NO-CAMUS.")
         (let-values (([left right] (splitf-at (cdr strlist) (λ (l) (not (string=? l "END."))))))
           (string-append
            (string-append* (map (λ (t) (string-append t "\n")) left))
            (toHTML-multi-stated (cdr right) st))))
        ((regexp-match? blockLevelRegexp (car strlist))
         (let* ((matchres (regexp-match blockLevelRegexp (car strlist)))
                (tagname (getTagname-blockLevel matchres))
                (attribute (getAttribute-blockLevel matchres)))
           (string-append
            "<" tagname (if attribute
                            (string-append " " (toHTML-attribute attribute))
                            "") ">\n"
                            (toHTML-multi-stated (cdr strlist) (cons tagname st))
                            )))
        (#T (string-append (toHTML-singl (car strlist)) "\n"
                           (toHTML-multi-stated (cdr strlist) st)))))
(define (toHTML str)
  (toHTML-multi-stated (string-split str "\n") '()))

