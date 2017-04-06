#lang racket/base
(require racket/string)
(require racket/list)

; camus

(define blockLevelRegexp #rx"^(?:([^][.]*))(?:\\[(.*)\\])?\\.")
(define lineLevelRegexp #rx"^(?:([^][:]+))(?:\\[(.*)\\])?: (.*)$")
(define isDirectiveRegexp
  #rx"^([ABCDEFGHIJKLMNOPQRSTUVWXYZ_]+.*)$")
(define inlineTag-tier1-Regexp
  #rx"^([^][|.\t ]+)(?:\\[(.*)\\])?\\. ")
(define inlineTag-tier2-Regexp
  #rx"^([^ \t|]+)\\|((?:&&|&\\||[^|])+)\\|(.*)")
(define inlineTag-tier2-Regexp-noOutro
  #rx"^([^ \t|]+)\\|((?:&&|&\\||[^|])+)\\|")

(define (isDirective? str) (regexp-match? isDirectiveRegexp str))

(define (getTagname-blockLevel matchres) (cadr matchres))
(define (getAttribute-blockLevel matchres) (caddr matchres))
(define (getTagname-lineLevel matchres) (cadr matchres))
(define (getAttribute-lineLevel matchres) (caddr matchres))
(define (getContent-lineLevel matchres) (cadddr matchres))
(define (getTagname/inlineTag-tier2 matchres) (cadr matchres))
(define (getContent/inlineTag-tier2 matchres) (caddr matchres))
(define (getOutro/inlineTag-tier2 matchres) (cadddr matchres))
(define (getTagname/inlineTag-tier1 matchres) (cadr matchres))
(define (getAttr/inlineTag-tier1 matchres) (caddr matchres))

(define (toHTML-attribute str)
  (regexp-replace* #rx"," str " "))

(define (toHTML-Directive directiveType attr content)
  (cond ((string=? directiveType "LINK")
         (if content
             (string-append "<a href=\"" attr "\">" content "</a>")
             (string-append "<a href=\"" attr "\" />")))
        ((string=? directiveType "IMG")
         (if content
             (string-append "<img src=\"" attr "\" alt=\"" content "\" />")
             (string-append "<img src=\"" attr "\" />")))
        ((string=? directiveType "NO-CAMUS")
         content)
        ))

(define (countTier2Tag rst)
  (define (countTier2Tag_ rst c pos)
    (if (char=? (string-ref rst 0) #\|)
        (if (= c 2) (+ pos 1)
            (countTier2Tag_ (substring rst 1) (+ c 1) (+ 1 pos)))
        (countTier2Tag_ (substring rst 1) c (+ 1 pos))))
  (countTier2Tag_ rst 0 0))
(define (splitContent-stated rst c c2)
  (if (string=? rst "") #f
      (let ((pivot (string-ref rst 0)))
        (cond ((char=? pivot #\&) (splitContent-stated (substring rst 2) (+ c 2) c2))
              ((char=? pivot #\|)
               (let ((r (substring rst 1)))
                 (cond ((regexp-match? inlineTag-tier2-Regexp r)
                        (let ((count3res (countTier2Tag rst)))
                          (splitContent-stated (substring rst count3res)
                                               (+ c count3res)
                                               c2)))
                       ((regexp-match? inlineTag-tier1-Regexp r)
                        (let ((l (string-length (car (regexp-match inlineTag-tier1-Regexp r)))))
                          (splitContent-stated (substring rst l) (+ c l) (+ c2 1))))
                       (#t
                        (if (= c2 0) c
                            (splitContent-stated (substring rst 1) (+ c 1) (- c2 1)))))))
              (#t (splitContent-stated (substring rst 1) (+ c 1) c2))))))
(define (splitContent rst)
  (let ((splitPos (splitContent-stated rst 0 0)))
    (if splitPos
        (values (substring rst 0 splitPos) (substring rst (+ 1 splitPos)))
        (values "" rst))))

(define (toHTML-inline-stated str st)
  (cond
    ((equal? st 'normal)
     (cond
       ((string=? str "") "")
       ((char=? #\& (string-ref str 0))
        (cond ((char=? #\& (string-ref str 1))
               (string-append "&" (toHTML-inline-stated (substring str 2) st)))
              ((char=? #\|
                       (string-ref str 1))
               (string-append "|" (toHTML-inline-stated (substring str 2) st)))))
       ((char=? #\| (string-ref str 0))
        (toHTML-inline-stated (substring str 1) 'tag))
       (#t
        (string-append (substring str 0 1)
                       (toHTML-inline-stated (substring str 1) st)))))
    #|
    |sdfsf|sdfsfsfdsf|A -> |, sdfsf|sdfsfsfdsf|A -> sdfsf, sdfsfsfdsf, A
    |A[B]. C|D -> |, A[B]. C|D -> A, B, C, D
    |#
    ((equal? st 'tag)
     (cond ((regexp-match? inlineTag-tier2-Regexp str)
            (let* ((matchres (regexp-match inlineTag-tier2-Regexp str))
                   (tagname (getTagname/inlineTag-tier2 matchres))
                   (content (getContent/inlineTag-tier2 matchres))
                   (outro (getOutro/inlineTag-tier2 matchres)))
              (if (isDirective? tagname)
                  (string-append (toHTML-Directive tagname #f content)
                                 (toHTML-inline-stated outro 'normal))
                  (string-append
                   "<" tagname ">" content "</" tagname ">"
                   (toHTML-inline-stated outro 'normal)))))
           ((regexp-match? inlineTag-tier1-Regexp str)
            (let* ((matchres (regexp-match inlineTag-tier1-Regexp str))
                   (tagname (getTagname/inlineTag-tier1 matchres))
                   (attr (getAttr/inlineTag-tier1 matchres))
                   (rst (substring str (string-length (car matchres)))))
              (let-values (((content outro) (splitContent rst)))
                (if (isDirective? tagname)
                    (string-append (toHTML-Directive tagname attr content)
                                   (toHTML-inline-stated outro 'normal))
                    (string-append
                     "<" tagname (if attr (string-append " " attr) "") ">"
                     (toHTML-inline-stated content 'normal)
                     "</" tagname ">"
                     (toHTML-inline-stated outro 'normal)))
                )))
           (#t str)
           ))))
(define (toHTML-inline str)
  (toHTML-inline-stated str 'normal))

(define (toHTML-lineLevel str)
  (if (regexp-match? lineLevelRegexp str)
      (let* ((matchres (regexp-match lineLevelRegexp str))
             (tagname (getTagname-lineLevel matchres))
             (attribute (getAttribute-lineLevel matchres))
             (content (getContent-lineLevel matchres)))
        (if (isDirective? tagname)
            (toHTML-Directive tagname attribute content)
            (string-append
             "<" tagname (if attribute (string-append " " attribute) "") ">"
             (toHTML-inline content) "</" tagname ">")))
      (toHTML-inline str)))

(define (toHTML-blockLevel strlist st)
  (cond ((null? strlist) "\n")
        ((string=? (car strlist) "END.")
         (string-append
          "</" (car st) ">\n"
          (toHTML-blockLevel (cdr strlist) (cdr st))))
        ((regexp-match? blockLevelRegexp (car strlist))
         (let* ((matchres (regexp-match blockLevelRegexp (car strlist)))
                (tagname (getTagname-blockLevel matchres))
                (attribute (getAttribute-blockLevel matchres)))
           (string-append
            "<" tagname
            (if attribute (string-append " " (toHTML-attribute attribute)) "")
            ">\n" (toHTML-blockLevel (cdr strlist) (cons tagname st)))))
        (#T (string-append (toHTML-lineLevel (car strlist)) "\n"
                           (toHTML-blockLevel (cdr strlist) st)))))

(define (toHTML str)
  (toHTML-blockLevel (string-split str "\n") '()))
