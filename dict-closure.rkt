#lang racket

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt")
         (prefix-in strat: "strategies.rkt")
         "list-comprehension.rkt")

(provide dictionary-closure)
;(define key (build-list 26 (lambda (_) #\_)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pack l)
  (match l
    ['() '()]
    [(cons a '()) (list (list a))]
    [(cons a rest) (let([done (pack rest)])
                     (match done
                       [(cons (cons x y) rest) (if (= (length a) (length x))
                                                   (cons (cons a (cons x y)) rest)
                                                   (cons (list a) done))]))]))
(define mydictionary (pack (sort (map  string->list  utils:dictionary) < #:key length )))

(define (zip l1 l2)
  (match (cons l1 l2)
    [(cons l '())  '()]
    [(cons '() l)  '()]
    [(cons (cons a r1) (cons b r2)) (cons (cons a b) (zip r1 r2))]))

(define (partially-consistent? zip-list);;(zip pw dw)
  (match zip-list
    ['() #t]
    [(cons (cons a b) rest) (if (char-upper-case? a)
                                (if (char=? a b) (partially-consistent? (cdr zip-list)) #f)
                                (partially-consistent? (cdr zip-list)))]))

(define (fully-consistent? cons-zip-list);;(list (cons (cons pw dw) (cons pw dw)))
  (match cons-zip-list
    ['() #t]
    [(cons (cons (cons c1 d1) (cons c2 d2)) rest)
             (if (char=? c1 c2)
                 (if (char=? d1 d2) (fully-consistent? (cdr cons-zip-list)) #f)
                 (if (char=? d1 d2) #f (fully-consistent? (cdr cons-zip-list))))]))
  

    
   
(define (get-matches  psword  equal-letter-dict-list match-list)
  (if (> (length match-list) 1) match-list
      (match equal-letter-dict-list
        ['() match-list]
        [(cons a rest) (if (match? psword  a)
                           (get-matches psword rest (cons a match-list))
                           (get-matches psword rest match-list))])))

(define (get-substitutions dw-list pw-list)
  (match (cons dw-list pw-list)
    [(cons '() '()) '()]
    [(cons (cons d r1) (cons p r2))
           (if (char=? d p) (get-substitutions r1 r2)
               (cons (cons d p) (get-substitutions r1 r2)))]))

(define (list-of-length len lst) ;;assums that lst is list of list  of list;;
  (match lst
    ['() '()]
    [(cons a rest) (if (= (length (car a)) len) a
                       (list-of-length len rest))]))
    
(define (plaintext? str)
  (equal? str (string-upcase str)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;main body of dictionary closure;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dictionary-closure key)
  (define (apply-dic cwl key)
    (if(null? cwl) key
       (let ([partial-word (utils:decrypt key (car cwl))])
         (if (plaintext? partial-word)
             (begin ;(display partial-word) (displayln "-> skipping this one")
                    (apply-dic (cdr cwl) key))
             (let ([match-list (get-matches  (string->list partial-word)
                                             (list-of-length (string-length partial-word) mydictionary)
                                             '())])
               (cond [(null? match-list) #f]
                     [(= (length match-list) 1) ;(display (car match-list) (display "  ")
                                                ;(display partial-word)
                                                ;(displayln "-> Unique completion")
                                  (let ([new-key (utils:add-substitution
                                                  (get-substitutions (car match-list)
                                                                     (string->list (car cwl)))
                                                  key)])
                                    (utils:show-key new-key)
                                    (displayln "  ")
                                    (apply-dic  utils:cipher-word-list new-key))]
                     [else ;(display partial-word)
                           ;(displayln "---->multiple complitions")
                           (apply-dic (cdr cwl) key)]))))))
       
  (apply-dic utils:cipher-word-list key))

(define (match? pswl dicl )
  (let ([one-one-map (zip pswl dicl)])
    (if (partially-consistent? one-one-map) ;;getting zipped list
        (fully-consistent? (lc (cons x y) : x <- one-one-map
                               y <- one-one-map
                               @(not (or (equal? x y) (char-upper-case? (car x))))))
        #f)))

;(define t (current-inexact-milliseconds))
;(dictionary-closure  key)
;(- (current-inexact-milliseconds) t)
