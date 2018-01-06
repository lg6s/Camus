#lang racket

;;; camus.
;;; generation 2
;;; (c) Tset'ien Lin 2017-2018



;; you can always use \[any character you want] to represent [any character you want]
;; ``literally'', so this:
;;     \t\h\i\s \i\s \a \t\e\s\t
;; should be the same as:
;;     this is a test
;; in output.
;; so when you write something like this:
;;     \|this-looks-like-a-tag-but-it-is-not|blah|
;; its output will be:
;;     \|this-looks-like-a-tag-but-it-is-not|blah|
;; but not:
;;     <this-looks-like-a-tag-but-it-is-not>blah</this-looks-like-a-tag-but-it-is-not>
;; this works the same way for other type of tags.
(define (convert-escape str)
  (regexp-replace* #rx"\\\\(.+?)" str
                   (λ (all escaped-char) escaped-char)))

(define (convert-attribute attribute)
  (let* ((escape-reserved-convert
          (regexp-replace* #rx"(.*?)([^\\]+?)(,)" attribute
                           (λ (all before critical comma)
                             (string-append before critical " ")))))
    (convert-escape escape-reserved-convert)))

;; you cannot nest linetag.
;; e.g.:
;;     h3: h4: not possible
;; will not be converted to:
;;     <h3><h4>not possible</h4></h3>
(define linetag-regexp #rx"^[ \t\b]*([^]:[\\]+)(?:\\[((?:(?:[^:])|\\\\:)+)\\])?: (.*)$")
(define (convert-linetag line)
  (let ((match-result (regexp-match linetag-regexp line)))
    (if match-result
        (let* ((tagname (cadr match-result))
               (content (cadddr match-result))
               (has-attribute? (caddr match-result))
               (converted-attribute
                (if has-attribute? (convert-attribute has-attribute?) "")))
          (string-append
           "<" tagname converted-attribute ">"
           (convert-inline content)
           "</" tagname ">"))
        (convert-escape (convert-inline line)))))

(define blocktag-header-regexp #rx"^[ \t\b]*([^]:[\\]+)(?:\\[((?:(?:[^][])|\\\\\\[|\\\\\\])+)\\])?\\{[ \t\b]*$")
(define blocktag-tail-regexp #rx"^[ \t\b]*\\}[ \t\b]*$")
(define (blocktag-tagname match-res) (cadr match-res))
(define (convert-blocktag-header line)
  (let ((match-result (regexp-match blocktag-header-regexp line)))
    (if match-result
        (let* ((tagname (blocktag-tagname match-result))
               (has-attribute? (caddr match-result))
               (converted-attribute
                (if has-attribute? (string-append " " (convert-attribute has-attribute?)) "")))
          (string-append
           "<" tagname converted-attribute ">"))
        (convert-escape (convert-inline line)))))

;; in camus generation 1 you have:
;;     blahblahblahblahblahbalhbalbhalhb
;;     blhablahblahblablahblahblah
;;     blhablahblahblablahblahblahblhablahblahblablahblahblah
;;
;;     blhablahblahblablahblahblahblhablahblahblablahblahblah
;;     blhablahblahblablahblahblahblhablahblahblablahbl
;;
;; to be converted into to different paragraph. but now that is not the case.
;; you need to specify the p tag explicitly like this:
;;     p{
;;        blahblahbhal...
;;     }
;;     p{
;;        blahblahblah...
;;     }

;; to handle inline tags correctly we need a recursive descent parser.
(define (convert-inline line)
  (define (convert-inline-inner rest tag-stack result)
    ;; type1:  |tag-name[tag-attr]. tag-content|
    (define type1-header-regexp
      #rx"^\\|((?:[^][.]|\\\\\\.|\\\\\\[|\\\\\\])*)(?:\\[((?:[^][.]|\\\\\\.|\\\\\\[|\\\\\\])*)\\])?\\. (.*)")
    (define (type1-rest match-res) (cadddr match-res))
    (define (type1-tagname match-res) (cadr match-res))
    (define (type1-attr match-res) (caddr match-res))
    (define type2-regexp
      ;; type2: |tag-name|contnet|.
      ;; note: you cannot include any tag inside a type2 tag.
      #rx"^\\|((?:[^ \t|]|\\\\\\|)*)\\|((?:(?:[^|])|(?:\\\\\\|))*)\\|(.*)$")
    (define (type2-tagname match-res) (cadr match-res))
    (define (type2-tagcontent match-res) (caddr match-res))
    (define (type2-rest match-res) (cadddr match-res))
    (define before-normaltext-regexp #rx"^((?:(?:[^|])|(?:\\\\\\|))+)")
    (if (string=? "" rest) result
    (let ((before-match-result (regexp-match before-normaltext-regexp rest)))
      (if before-match-result
          (convert-inline-inner (substring rest (string-length (car before-match-result)))
                                 tag-stack
                                 (string-append result (car before-match-result)))
          (let ((type1? (regexp-match type1-header-regexp rest))
                (type2? (regexp-match type2-regexp rest)))
            (cond (type2?
                   (convert-inline-inner
                    (type2-rest type2?) tag-stack
                    (string-append result "<" (type2-tagname type2?) ">"
                                   (type2-tagcontent type2?)
                                   "</" (type2-tagname type2?) ">")))
                  (type1?
                   (convert-inline-inner
                    (type1-rest type1?)
                    (cons (type1-tagname type1?) tag-stack)
                    (string-append result "<" (type1-tagname type1?)
                                   (if (type1-attr type1?)
                                       (string-append " " (convert-attribute (type1-attr type1?)))
                                       "")
                                   ">")))
                  (#t
                   (convert-inline-inner
                    (substring rest 1)
                    (if (null? tag-stack) tag-stack (cdr tag-stack))
                    (string-append result (if (null? tag-stack) "|"
                                              (string-append "</" (car tag-stack) ">")))))))))))
  (convert-escape (convert-inline-inner line '() "")))
(define (convert-document doc)
  (define (convert-document-inner doclist tag-stack result)
    (if (null? doclist) result
        (let* ((pivot (car doclist)))
          (let ((is-blocktag-header? (regexp-match blocktag-header-regexp pivot))
                (is-blocktag-tail? (regexp-match blocktag-tail-regexp pivot))
                (is-linetag? (regexp-match linetag-regexp pivot)))
            (cond
              (is-blocktag-header?
               (convert-document-inner (cdr doclist)
                                       (cons (blocktag-tagname is-blocktag-header?) tag-stack)
                                       (string-append result (convert-blocktag-header pivot))))
              (is-blocktag-tail?
               (convert-document-inner (cdr doclist)
                                       (if (null? tag-stack) tag-stack (cdr tag-stack))
                                       (string-append result
                                        (if (null? tag-stack) pivot
                                            (string-append "</" (car tag-stack) ">")))))
              (is-linetag?
               (convert-document-inner (cdr doclist)
                                       tag-stack
                                       (string-append result (convert-linetag pivot))))
              (#t
               (convert-document-inner (cdr doclist)
                                       tag-stack
                                       (string-append result (convert-inline pivot)))))))))
  (let ((lines (string-split doc "\n"))) (convert-document-inner lines '() "")))