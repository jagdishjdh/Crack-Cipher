#lang racket

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         unzip
         zip
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;helper fum=nctions of cipher-monogram
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (unzip zip-list)
    (cond[(null? zip-list) '()]
         [else (cons (caar zip-list) (unzip (cdr zip-list)))]))

(define (is-zero-25 a)
  (and (>= a 0) (<= a 25)))

(define (zip l1 l2)
  (match (cons l1 l2)
    [(cons l '())  '()]
    [(cons '() l)  '()]
    [(cons (cons a r1) (cons b r2)) (cons (cons a b) (zip r1 r2))]))

(define (pack l)
  (match l
    ['() '()]
    [(cons a '()) (list (cons a 1))]
    [(cons a rest) (let([done (pack rest)])
                     (match done
                       [(cons (cons x y) rest) (if (equal? a x)
                                                   (cons (cons x (+ y 1)) rest)
                                                   (cons (cons a 1) done))]))]))
(define (pack-zip-list l)
  (match l
    ['() '()]
    [(cons a '()) (list a)]
    [(cons (cons a frq) other) (let([done (pack-zip-list other)])
                     (match done
                       [(cons (cons x y) rest) (if (equal? a x)
                                                   (cons (cons x (+ y frq)) rest)
                                                   (cons (cons a frq) done))]))]))

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define (cipher-monograms ciphertext)
  (define (add1 l pos)
    (cond [(= pos 0) (cons (+ (car l) 1) (cdr l))]
          [else (cons (car l) (add1 (cdr l) (sub1 pos)))]))

  (define (modify-frq-list frq-list char-list)
    (cond[(null? char-list) frq-list]
         [else (let* ([pos (- (char->integer (car char-list)) utils:CIPHER-BEGIN)]
                      [new-frq-list (if(is-zero-25 pos)
                                       (add1 frq-list pos)
                                       frq-list)])
                 (modify-frq-list new-frq-list (cdr char-list)))]))
  
  (define frq-list (build-list 26 (lambda (x) 0)))
  (define cipher-letter-list (build-list 26 (lambda (x) (integer->char (+ x utils:CIPHER-BEGIN))) ))
  (set! frq-list (modify-frq-list frq-list (string->list ciphertext)))
  (unzip (sort (zip cipher-letter-list  frq-list) > #:key cdr)))
  
;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!

(define (cipher-bigrams-frq cipher-word-list)
  (define (extract-bigrams word)
    (cond [(< (string-length word) 2) '()]
          [else (cons (substring word 0 2) (extract-bigrams (substring word 1)))]))
  
  (sort (pack (sort (append* (map extract-bigrams cipher-word-list))
                    string>?))
        > #:key cdr))

(define (cipher-bigrams cipher-word-list)
  (unzip (cipher-bigrams-frq cipher-word-list)))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (define (extract bigram)
    ;; will give list of monograms out of bigrams depending on mode;;
    (match mode
      ['predecessor (list (string-ref bigram 0))]
      ['successor   (list (string-ref bigram 1))]
      ['both        (if (char=? (string-ref bigram 0) (string-ref bigram 1))
                        (list (string-ref bigram 0))
                        (list (string-ref bigram 0) (string-ref bigram 1)))]))
  (sort (pack (sort
               (append* (map extract cipher-bigrams-list))
               char>?))
        > #:key cdr))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-bigram-frq-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (define (extract bigram)
    ;; will give list of monograms out of zipped-bigrams depending on mode;;
    (match mode
      ['predecessor (list (cons (string-ref (car bigram) 0) (cdr bigram)))]
      ['successor   (list (cons (string-ref (car bigram) 1) (cdr bigram)))]
      ['both        (if (char=? (string-ref (car bigram) 0) (string-ref (car bigram) 1))
                        (list (cons (string-ref (car bigram) 0) (cdr bigram)))
                        (list (cons (string-ref (car bigram) 0) (cdr bigram))
                              (cons (string-ref (car bigram) 1) (cdr bigram))))]))
  (sort (pack-zip-list (sort
                        (append* (map extract cipher-bigram-frq-list))
                        char>? #:key car))
        > #:key cdr))

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
    (define (extract-bigrams word)
    (cond [(< (string-length word) 3) '()]
          [else (cons (substring word 0 3) (extract-bigrams (substring word 1)))]))
  
  (sort (pack (sort (append* (map extract-bigrams cipher-word-list))
                    string>?))
        > #:key cdr))

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
    (define (extract-bigrams word)
    (cond [(< (string-length word) 4) '()]
          [else (cons (substring word 0 4) (extract-bigrams (substring word 1)))]))
  
  (sort (pack (sort (append* (map extract-bigrams cipher-word-list))
                    string>?))
        > #:key cdr))

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
(define (cipher-common-words-single cipher-word-list)
  (define (take-single word)
    (cond[(= (string-length word) 1) (list word)]
         [else '()]))
  (unzip (sort (pack (sort (append* (map take-single cipher-word-list))
                           string>?))
               > #:key cdr)))

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  (define (take-triple word)
    (cond[(= (string-length word) 3) (list word)]
         [else '()]))
  (unzip
   (sort (pack (sort (append* (map take-triple cipher-word-list))
                           string>?))
               > #:key cdr)))

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  '())
;(define (cipher-third-last-letter cipher-word-list)
;  (define (third-last word)
;    (cond[(>= (string-length word) 1) (list (string-ref word (- (string-length word) 1)))]
;         [else '()]))
;  (sort (pack (sort (append* (map third-last cipher-word-list))
;                           char>?))
;               > #:key cdr))
