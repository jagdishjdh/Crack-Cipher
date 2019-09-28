#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in algo: "dict-closure.rkt"))

(provide secret-word-enumeration)
;(define key (build-list 26 (lambda (_) #\_)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define six-letter-dict (foldr (lambda (x y) (if (= (string-length x) 6) (cons x y) y)) '() utils:dictionary))
;;assuming initially partial key was consistent
(define (match? pswl dicl  key)
  (match (cons pswl dicl)
    [(cons '() '()) #t]
    [(cons (cons #\_ r1) (cons dw r2)) (if (index-of key dw)
                                           #f
                                           (match? r1 r2 (cons dw key)))]
    [(cons (cons pw r1) (cons dw r2)) (if (char=? pw dw)
                                          (match? r1 r2 key)
                                          #f)]))
                                           
   
(define (get-all-matches  psword  six-letter-dict-list  key)
  (match six-letter-dict-list
    ['() '()]
    [(cons a rest) (if (match? psword  a  key)
                       (cons a (get-all-matches psword (cdr six-letter-dict-list) rest))
                       (get-all-matches psword (cdr six-letter-dict-list) rest))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main body of secret word enueration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (secret-word-enumeration key-after-dictionary-closure) ;; Returns a key or false (#f)
  (define key key-after-dictionary-closure)
  (define partial-secret-word (map char-upcase (take  key  6)))
  (define match-list (get-all-matches  partial-secret-word  (map  string->list  six-letter-dict) key))
  (match match-list
    ['() #f]
    [(cons a '()) (utils:encryption-key (list->string a))]
    [else key]))