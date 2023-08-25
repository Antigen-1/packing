#lang racket/base
;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require racket/contract)
(provide (contract-out (kws-and-vals->table (-> (listof keyword?) list? any))
                       (subtable (-> kwargs-table/c (listof keyword?) any))
                       (apply-packed-arguments (-> procedure? kwargs-table/c list? any))
                       (make-packing-procedure (-> (-> any/c any/c any) any))))

;;association lists
;;-----
(define (cars->assoc-list cars proc)
  (map (lambda (c) (cons c (proc c))) cars))
(define (cars-and-cdrs->assoc-list cars cdrs)
  (map cons cars cdrs))
(define (assoc-list->cars-and-cdrs lst)
  (for/fold ((cars null) (cdrs null))
            ((pair (in-list lst)))
    (values (cons (car pair) cars) (cons (cdr pair) cdrs))))
;;-----

;;by-keyword arguments and immutable hash tables
;;-----
(define kwargs-table/c (hash/c keyword? any/c #:immutable #t))

(define (make-table pairs)
  (make-immutable-hasheq pairs))
(define (retrieve-keyword-argument table kw)
  (hash-ref table kw))

(define (kws-and-vals->table kws vals) (make-table (cars-and-cdrs->assoc-list kws vals)))
(define (subtable table kws)
  (make-table (cars->assoc-list kws (lambda (kw) (retrieve-keyword-argument table kw)))))
(define (apply-packed-arguments func table rest)
  (let ((sorted (sort (hash->list table) keyword<? #:key car)))
    (call-with-values (lambda () (assoc-list->cars-and-cdrs sorted))
                      (lambda (kws vals) (keyword-apply func kws vals rest)))))
;;-----

;;wrapper
;;-----
(define (make-packing-procedure func)
  (make-keyword-procedure (lambda (kws vals . rest) (func (kws-and-vals->table kws vals) rest))))
;;-----

(module* test racket/base
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (require rackunit racket/match racket/date racket/port (submod ".."))

  (define (orignal-function tbl lst)
    (with-output-to-string
      (lambda ()
        (match* (tbl lst)
          (((hash-table ('#:name name) ('#:date date)) (list greeting))
           (displayln (format "~a: ~a" name greeting))
           (apply-packed-arguments
            (lambda (#:date date) (displayln (date->string date)))
            (subtable tbl '(#:date)) null))))))
  (define wrapped-function (make-packing-procedure orignal-function))

  (define date (current-date))

  (check-true (string=? (format "~a: ~a\n~a\n" "Antigen-1" "Hello!" (date->string date))
                        (wrapped-function #:name "Antigen-1" #:date date "Hello!"))))
