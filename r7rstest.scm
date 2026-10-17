;;;;"r7rstest.scm":  Test R7RS-small correctness of Scheme implementations.
;; Copyright (C) 1991, 1992, 1993, 1994, 1995, 2000, 2003, 2004, 2006, 2007 Free Software Foundation, Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;;;"r7rstest.scm":  Test R7RS-small correctness of Scheme implementations.
;;; Author (upstream): Aubrey Jaffer.
;;; Provenance: edited copy of Aubrey Jaffer's r4rstest.scm (SCM 5f6
;;; distribution); see README.md for the upstream checksum and links.
;;; Licence: GPL-3.0-or-later (see LICENSE); the copyright notices
;;; above are retained in this derivative.
;;; Normative contract: R7RS-small; report citations appear at changed
;;; sites. Run with any R7RS-small Scheme, e.g.
;;; `chibi-scheme r7rstest.scm' or `gosh -q -r7 r7rstest.scm'.
;;;
;;; The body below is the edited upstream script; the import makes
;;; base and optional-library bindings available in R7RS mode.
;;; Upstream R5RS spellings exact->inexact/inexact->exact are written
;;; with their R7RS-small base names inexact/exact (6.2.6).
;;; The suite requires every library listed below; a missing library
;;; fails at load time. The r7rs-optional? flag affects failure
;;; reporting only, never loading. Every import here is exercised:
;;; (scheme process-context) supplies get-environment-variable for
;;; r7rs-tmpdir below.
(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme file)
        (scheme char)
        (scheme lazy)
        (scheme inexact)
        (scheme complex)
        (scheme load)
        (scheme process-context))

(define cur-section '())(define errs '())
(define SECTION (lambda args
		  (display "SECTION") (write args) (newline)
		  (set! cur-section args) #t))
(define record-error (lambda (e) (set! errs (cons (list cur-section e) errs))))

(define test
  (lambda (expect fun . args)
    (write (cons fun args))
    (display "  ==> ")
    ((lambda (res)
      (write res)
      (newline)
      (cond ((not (equal? expect res))
	     (record-error (list res expect (cons fun args)))
	     (display " BUT EXPECTED ")
	     (write expect)
	     (newline)
	     #f)
	    (else #t)))
     (if (procedure? fun) (apply fun args) (car args)))))
(define (report-errs)
  (newline)
  (if (null? errs) (display "Passed all tests")
      (begin
	(display "errors were:")
	(newline)
	(display "(SECTION (got expected (call)))")
	(newline)
	(for-each (lambda (l) (write l) (newline))
		  errs)))
  (newline))

;;; R7RS-small port notes. Provenance: edited copy of
;;; r4rstest.scm (Aubrey Jaffer, GPL-3.0-or-later); see README.md for
;;; the upstream checksum and links.
;;; R7RS-small is the normative contract (report citations appear at
;;; changed sites). The suite requires all imported libraries (see the
;;; import above); r7rs-optional? affects failure reporting only and
;;; cannot gate loading. Tests exercising (scheme char), (scheme lazy),
;;; (scheme file)/(scheme read), (scheme load), or (scheme inexact)/
;;; (scheme complex) run with r7rs-optional? set; the summary reports
;;; required/optional failures separately. The original
;;; SECTION/test/report-errs harness is kept; counters and a summary
;;; line are added, and only the final report signals failure via
;;; `error` (scheme base) so runners exit nonzero after all suites run.
;;; The final report exits nonzero only on required failures; optional
;;; failures never gate the exit code.
(define r7rs-total 0)
(define r7rs-optional? #f)
(define r7rs-optional-fails 0)
(define r7rs-reported 0)
(define r7rs-orig-test test)
;;; dynamic-wind so an error escaping the thunk cannot leave the flag
;;; set, which would misreport later required failures as optional and
;;; suppress the nonzero exit.
(define (r7rs-optional-thunk thunk)
  (let ((outer-optional? r7rs-optional?))
    (dynamic-wind
      (lambda () (set! r7rs-optional? #t))
      thunk
      (lambda () (set! r7rs-optional? outer-optional?)))))
(set! test
      (lambda (expect fun . args)
        (set! r7rs-total (+ r7rs-total 1))
        (let ((ok? (apply r7rs-orig-test expect fun args)))
          (cond ((not ok?)
                 (cond (r7rs-optional?
                        (set! r7rs-optional-fails
                              (+ r7rs-optional-fails 1))))
                 #f)
                (else #t)))))
(define r7rs-report-errs
  (lambda ()
    (let* ((failed (length errs))
           (new-errs (if (> failed r7rs-reported)
                         (list-tail (reverse errs) r7rs-reported)
                         '()))
           (passed (max 0 (- r7rs-total failed)))
           (req-failed (- failed r7rs-optional-fails)))
      (cond ((not (null? new-errs))
             (display "errors since last report:")
             (newline)
             (display "(SECTION (got expected (call)))")
             (newline)
             (for-each (lambda (l) (write l) (newline))
                       (reverse new-errs))))
      (set! r7rs-reported failed)
      (display "r7rs-summary: total ")
      (write r7rs-total)
      (display " passed ")
      (write passed)
      (display " failed ")
      (write failed)
      (display " (required-failed ")
      (write req-failed)
      (display " optional-failed ")
      (write r7rs-optional-fails)
      (display ")")
      (newline)
      #t)))
(define r7rs-final-report
  (lambda ()
    (r7rs-report-errs)
    (let ((req-failed (- (length errs) r7rs-optional-fails)))
      (display "r7rs-complete: required-failed ")
      (write req-failed)
      (newline)
      (cond ((> req-failed 0)
             (error "r7rstest: required failures" req-failed))))
    #t))

(SECTION 2 1);; test that all symbol characters are supported.
'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)

(SECTION 3 4)
(define disjoint-type-functions
  (list boolean? char? null? number? pair? procedure? string? symbol? vector?))
(define type-examples
  (list
   #t #f #\a '() 9739 '(test) record-error "test" "" 'test '#() '#(a b c) ))
(define i 1)
(for-each (lambda (x) (display (make-string i #\space))
		  (set! i (+ 3 i))
		  (write x)
		  (newline))
	  disjoint-type-functions)
(define type-matrix
  (map (lambda (x)
	 (let ((t (map (lambda (f) (f x)) disjoint-type-functions)))
	   (write t)
	   (write x)
	   (newline)
	   t))
       type-examples))
(set! i 0)
(define j 0)
;;; R7RS-small 6.4: for-each needs proper lists; the datum '(t . t)
;;; below is a dotted-pair element, not an improper list structure.
(for-each (lambda (x y)
	    (set! j (+ 1 j))
	    (set! i 0)
	    (for-each (lambda (f)
			(set! i (+ 1 i))
			(cond ((and (= i j))
			       (cond ((not (f x)) (test #t f x))))
			      ((f x) (test #f f x)))
			(cond ((and (= i j))
			       (cond ((not (f y)) (test #t f y))))
			      ((f y) (test #f f y))))
		      disjoint-type-functions))
	  (list #t #\a '() 9739 '(test) record-error "test" 'car '#(a b c))
	  (list #f #\newline '() -3252 '(t . t) car "" 'nil '#()))
(SECTION 4 1 2)
(test '(quote a) 'quote (quote 'a))
(test '(quote a) 'quote ''a)
(SECTION 4 1 3)
(test 12 (if #f + *) 3 4)
(SECTION 4 1 4)
(test 8 (lambda (x) (+ x x)) 4)
(define reverse-subtract
  (lambda (x y) (- y x)))
(test 3 reverse-subtract 7 10)
(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(test 10 add4 6)
(test '(3 4 5 6) (lambda x x) 3 4 5 6)
(test '(5 6) (lambda (x y . z) z) 3 4 5 6)
(SECTION 4 1 5)
(test 'yes 'if (if (> 3 2) 'yes 'no))
(test 'no 'if (if (> 2 3) 'yes 'no))
(test '1 'if (if (> 3 2) (- 3 2) (+ 3 2)))
(SECTION 4 1 6)
(define x 2)
(test 3 'define (+ x 1))
(set! x 4)
(test 5 'set! (+ x 1))
(SECTION 4 2 1)
(test 'greater 'cond (cond ((> 3 2) 'greater)
			   ((< 3 2) 'less)))
(test 'equal 'cond (cond ((> 3 3) 'greater)
			 ((< 3 3) 'less)
			 (else 'equal)))
(test 2 'cond (cond ((assv 'b '((a 1) (b 2))) => cadr)
		     (else #f)))
(test 'composite 'case (case (* 2 3)
			 ((2 3 5 7) 'prime)
			 ((1 4 6 8 9) 'composite)))
(test 'consonant 'case (case (car '(c d))
			 ((a e i o u) 'vowel)
			 ((w y) 'semivowel)
			 (else 'consonant)))
(test #t 'and (and (= 2 2) (> 2 1)))
(test #f 'and (and (= 2 2) (< 2 1)))
(test '(f g) 'and (and 1 2 'c '(f g)))
(test #t 'and (and))
(test #t 'or (or (= 2 2) (> 2 1)))
(test #t 'or (or (= 2 2) (< 2 1)))
(test #f 'or (or #f #f #f))
(test #f 'or (or))
(test '(b c) 'or (or (memq 'b '(a b c)) (+ 3 0)))
(SECTION 4 2 2)
(test 6 'let (let ((x 2) (y 3)) (* x y)))
(test 35 'let (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))
(test 70 'let* (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))
(test #t 'letrec (letrec ((even?
			   (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
			  (odd?
			   (lambda (n) (if (zero? n) #f (even? (- n 1))))))
		   (even? 88)))
(set! x 34)
(test 5 'let (let ((x 3)) (define x 5) x))
(test 34 'let x)
(test 6 'let (let () (define x 6) x))
(test 34 'let x)
(test 34 'let (let ((x x)) x))
(test 7 'let* (let* ((x 3)) (define x 7) x))
(test 34 'let* x)
(test 8 'let* (let* () (define x 8) x))
(test 34 'let* x)
(test 9 'letrec (letrec () (define x 9) x))
(test 34 'letrec x)
(test 10 'letrec (letrec ((x 3)) (define x 10) x))
(test 34 'letrec x)
(define (s x) (if x (let () (set! s x) (set! x s))))
(SECTION 4 2 3)
(set! x 0)
(test 6 'begin (begin (set! x (begin (begin 5)))
		      (begin ((begin +) (begin x) (begin (begin 1))))))
(SECTION 4 2 4)
(test '#(0 1 2 3 4) 'do (do ((vec (make-vector 5))
			    (i 0 (+ i 1)))
			   ((= i 5) vec)
			 (vector-set! vec i i)))
(test 25 'do (let ((x '(1 3 5 7 9)))
	       (do ((x x (cdr x))
		    (sum 0 (+ sum (car x))))
		   ((null? x) sum))))
(test 25 'do (let ((x '(1 3 5 7 9))
                   (sum 0))
	       (do ((x x (cdr x)))
		   ((null? x))
		 (set! sum (+ sum (car x))))
	       sum))
(test 1 'let (let foo () 1))
(test '((6 1 3) (-5 -2)) 'let
      (let loop ((numbers '(3 -2 1 6 -5))
		 (nonneg '())
		 (neg '()))
	(cond ((null? numbers) (list nonneg neg))
	      ((negative? (car numbers))
	       (loop (cdr numbers)
		     nonneg
		     (cons (car numbers) neg)))
	      (else
	       (loop (cdr numbers)
		     (cons (car numbers) nonneg)
		     neg)))))
;;From: Allegro Petrofsky <Allegro@Petrofsky.Berkeley.CA.US>
(test -1 'let (let ((f -)) (let f ((n (f 1))) n)))

(SECTION 4 2 6)
(test '(list 3 4) 'quasiquote `(list ,(+ 1 2) 4))
(test '(list a (quote a)) 'quasiquote (let ((name 'a)) `(list ,name ',name)))
(test '(a 3 4 5 6 b) 'quasiquote `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(test '((foo 7) . cons)
	'quasiquote
	`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))

;;; sqt is a local helper for the vector quasiquote test below.
(define (sqt x)
	(do ((i 0 (+ i 1)))
	    ((> (* i i) x) (- i 1))))

(test '#(10 5 2 4 3 8) 'quasiquote `#(10 5 ,(sqt 4) ,@(map sqt '(16 9)) 8))
(test 5 'quasiquote `,(+ 2 3))
(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
      'quasiquote `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
(test '(a `(b ,x ,'y d) e) 'quasiquote
	(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e)))
(test '(list 3 4) 'quasiquote (quasiquote (list (unquote (+ 1 2)) 4)))
(test '`(list ,(+ 1 2) 4) 'quasiquote '(quasiquote (list (unquote (+ 1 2)) 4)))
(SECTION 5 2 1)
(define (tprint x) #t)
(test #t 'tprint (tprint 56))
(define add3 (lambda (x) (+ x 3)))
(test 6 'define (add3 3))
(define first car)
(test 1 'define (first '(1 2)))
(define foo (lambda () 9))
(test 9 'define (foo))
;;; R7RS-small 5.3.1: at the outermost level of a program a definition has
;;; essentially the same effect as (set! ... ) when the variable is already
;;; bound, so a top-level `define' may redefine an existing binding. The
;;; "define an identifier at most once in a body" rule is 5.3.2 and governs
;;; INTERNAL definitions only, so it does not restrict these.
;;;
;;; Upstream uses plain `define' here. The port keeps `define' too (so the
;;; 5.3.1 assignment semantics are exercised rather than assumed), and wraps
;;; the CALLS in `guard' purely so that a host which mishandles the
;;; redefinition records a test failure instead of aborting the whole run.
;;; `guard' cannot make a non-conforming host pass: a host that wrongly clears
;;; the binding still fails the assertion below.
(define foo foo)
(test 9 'define-self-reference
      (guard (c (else (list 'raised c))) (foo)))
(define foo (let ((foo foo)) (lambda () (+ 1 (foo)))))
(test 10 'define-let-shadow
      (guard (c (else (list 'raised c))) (foo)))
;;; Restore a known value so the 5.2.2 tests below start from a defined `foo'.
;;; (set! ...) is deliberate here: this is harness bookkeeping, not an
;;; assertion about define semantics.
(set! foo (lambda () 7))
;;; R7RS-small 5.4: imported bindings are immutable, so the upstream
;;; `+'-shadowing block is excluded. Its underlying coverage (internal
;;; define shadowing in nested bodies) is retained by the 5.2.2
;;; internal-define tests below.
;;; (define old-+ +)
;;; (begin ... (define + (lambda (x y) (list y x))) ...
;;;        (test '(3 6) add3 6) ...)
;;; (set! + old-+)
(test 9 add3 6)
(begin)
(begin (begin))
(begin (begin (begin (begin))))
(SECTION 5 2 2)
(test 45 'define
      (let ((x 5))
	(begin (begin (begin)
		      (begin (begin (begin) (define foo (lambda (y) (bar x y)))
				    (begin)))
		      (begin))
	       (begin)
	       (begin)
	       (begin (define bar (lambda (a b) (+ (* a b) a))))
	       (begin))
	(begin)
	(begin (foo (+ x 3)))))
(set! x 34)
;;; R7RS-small 5.3.2: it is an error to define the same identifier more than
;;; once in the same <body>. That rule governs INTERNAL definitions only; it
;;; says nothing about a top-level definition sequence, which 5.3.1 makes
;;; assignment-like. So upstream's repeated top-level `define' of `foo' is
;;; permitted and is kept here, together with the function-definition
;;; shorthand (5.3: `(define (<variable> <formals>) <body>)' is equivalent to
;;; `(define <variable> (lambda (<formals>) <body>))') so that an internal
;;; define inside a shorthand-defined procedure stays covered. The internal
;;; define in each body is what 5.3.2's letrec* equivalence governs.
(define (foo) (define x 5) x)
(test 5 foo)
(test 34 'define x)
(define foo (lambda () (define x 5) x))
(test 5 foo)
(test 34 'define x)
(define (foo x) ((lambda () (define x 5) x)) x)
(test 88 foo 88)
(test 4 foo 4)
(test 34 'define x)
(test 99 'internal-define (letrec ((foo (lambda (arg)
					  (or arg (and (procedure? foo)
						       (foo 99))))))
			    (define bar (foo #f))
			    (foo #f)))
(test 77 'internal-define (letrec ((foo 77)
				   (bar #f)
				   (retfoo (lambda () foo)))
			    (define baz (retfoo))
			    (retfoo)))
(SECTION 6 1)
(test #f not #t)
(test #f not 3)
(test #f not (list 3))
(test #t not #f)
(test #f not '())
(test #f not (list))
(test #f not 'nil)

;;; R7RS-small: boolean? is (scheme base); re-enabled.
(test #t boolean? #f)
(test #f boolean? 0)
(test #f boolean? '())
(SECTION 6 2)
(test #t eqv? 'a 'a)
(test #f eqv? 'a 'b)
(test #t eqv? 2 2)
(test #t eqv? '() '())
(test #t eqv? '10000 '10000)
(test #f eqv? (cons 1 2)(cons 1 2))
(test #f eqv? (lambda () 1) (lambda () 2))
(test #f eqv? #f 'nil)
(let ((p (lambda (x) x)))
  (test #t eqv? p p))
(define gen-counter
 (lambda ()
   (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(let ((g (gen-counter))) (test #t eqv? g g))
(test #f eqv? (gen-counter) (gen-counter))
(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	 (g (lambda () (if (eqv? f g) 'g 'both))))
  (test #f eqv? f g))

(test #t eq? 'a 'a)
(test #f eq? (list 'a) (list 'a))
(test #t eq? '() '())
(test #t eq? car car)
(let ((x '(a))) (test #t eq? x x))
(let ((x '#())) (test #t eq? x x))
(let ((x (lambda (x) x))) (test #t eq? x x))

;;; R7RS-small 6.1: eq? may return #f where eqv? returns #t, but not the
;;; reverse, and on the pairs below 6.1 additionally guarantees eq? and
;;; eqv? behave the SAME. Every case tested here is drawn from that region:
;;; two boolean pairs; four pair pairs; two non-empty vector pairs; two
;;; non-empty string pairs; and two procedure pairs -- `car'/`car', which
;;; must agree because the location tags are equal, and `car'/`cdr', which
;;; must both be #f. Upstream hand-rolls this with a bare record-error,
;;; which silently
;;; skips the harness counters. Routing it through `test' keeps r7rs-total,
;;; the failure count, and the required/optional split automatic, so the
;;; invariant "one test wrapper per assertion" can no longer drift.
(define test-eq?-eqv?-agreement
  (lambda (obj1 obj2)
    (test #t 'test-eq?-eqv?-agreement
	  (eq? (eq? obj1 obj2) (eqv? obj1 obj2)))))

(test-eq?-eqv?-agreement '#f '#f)
(test-eq?-eqv?-agreement '#t '#t)
(test-eq?-eqv?-agreement '#t '#f)
(test-eq?-eqv?-agreement '(a) '(a))
(test-eq?-eqv?-agreement '(a) '(b))
(test-eq?-eqv?-agreement car car)
(test-eq?-eqv?-agreement car cdr)
(test-eq?-eqv?-agreement (list 'a) (list 'a))
(test-eq?-eqv?-agreement (list 'a) (list 'b))
(test-eq?-eqv?-agreement '#(a) '#(a))
(test-eq?-eqv?-agreement '#(a) '#(b))
(test-eq?-eqv?-agreement "abc" "abc")
(test-eq?-eqv?-agreement "abc" "abz")

(test #t equal? 'a 'a)
(test #t equal? '(a) '(a))
(test #t equal? '(a (b) c) '(a (b) c))
(test #t equal? "abc" "abc")
(test #t equal? 2 2)
(test #t equal? (make-vector 5 'a) (make-vector 5 'a))
(SECTION 6 3)
(test '(a b c d e) 'dot '(a . (b . (c . (d . (e . ()))))))
(set! x (list 'a 'b 'c))
(define y x)
(and list? (test #t list? y))
(set-cdr! x 4)
(test '(a . 4) 'set-cdr! x)
(test #t eqv? x y)
(test '(a b c . d) 'dot '(a . (b . (c . d))))
;;; R7RS-small 3.2/6.4: after (set-cdr! x 4), y is a dotted pair, so
;;; list? must return #f.
(and list? (test #f list? y))
;;; R7RS-small 3.2/6.4: a circular list is not a proper list.
(and list? (let ((x (list 'a))) (set-cdr! x x) (test #f 'list? (list? x))))

;;; R7RS-small: pair? is (scheme base); re-enabled.
(test #t pair? '(a . b))
(test #t pair? '(a . 1))
(test #t pair? '(a b c))
(test #f pair? '())
(test #f pair? '#(a b))

(test '(a) cons 'a '())
(test '((a) b c d) cons '(a) '(b c d))
(test '("a" b c) cons "a" '(b c))
(test '(a . 3) cons 'a 3)
(test '((a b) . c) cons '(a b) 'c)

(test 'a car '(a b c))
(test '(a) car '((a) b c d))
(test 1 car '(1 . 2))

(test '(b c d) cdr '((a) b c d))
(test 2 cdr '(1 . 2))

(test '(a 7 c) list 'a (+ 3 4) 'c)
(test '() list)

(test 3 length '(a b c))
(test 3 length '(a (b) (c d e)))
(test 0 length '())

(test '(x y) append '(x) '(y))
(test '(a b c d) append '(a) '(b c d))
(test '(a (b) (c)) append '(a (b)) '((c)))
(test '() append)
(test '(a b c . d) append '(a b) '(c . d))
(test 'a append '() 'a)

(test '(c b a) reverse '(a b c))
(test '((e (f)) d (b c) a) reverse '(a (b c) d (e (f))))

(test 'c list-ref '(a b c d) 2)

(test '(a b c) memq 'a '(a b c))
(test '(b c) memq 'b '(a b c))
(test '#f memq 'a '(b c d))
(test '#f memq (list 'a) '(b (a) c))
(test '((a) c) member (list 'a) '(b (a) c))
(test '(101 102) memv 101 '(100 101 102))

(define e '((a 1) (b 2) (c 3)))
(test '(a 1) assq 'a e)
(test '(b 2) assq 'b e)
(test #f assq 'd e)
(test #f assq (list 'a) '(((a)) ((b)) ((c))))
(test '((a)) assoc (list 'a) '(((a)) ((b)) ((c))))
(test '(5 7) assv 5 '((2 3) (5 7) (11 13)))
(SECTION 6 4)
;;; R7RS-small 2.1, 6.5: symbols are case-sensitive by default
;;; (fold-case only via include-ci). Upstream probed the ambient case
;;; and expected (eq? 'a 'A); here 'a and 'A are distinct.
;;; R7RS-small: symbol? is (scheme base); re-enabled.
(test #t symbol? 'foo)
(test #t symbol? (car '(a b)))
(test #f symbol? "bar")
(test #t symbol? 'nil)
(test #f symbol? '())
(test #f symbol? #f)
(test "a" symbol->string 'a)
(test "A" symbol->string 'A)
(test "flying-fish" symbol->string 'flying-fish)
(test "Martin" symbol->string 'Martin)
(test "Malvina" symbol->string (string->symbol "Malvina"))
(test #f eq? 'a 'A)

;;; R7RS-small 6.5: symbols are identical iff names are spelled the
;;; same; string->symbol interns, so it must COPY its argument's characters.
;;; Copy first: MIT/GNU Scheme 12.1 returns an immutable string from
;;; (string ...) and from string literals, so string-set! on them is an error.
;;; string-copy is specified to return a newly allocated (hence mutable)
;;; string, and every host here agrees.
(set! x (string-copy (string #\a #\b)))
(set! y (string->symbol x))
(string-set! x 0 #\c)
(test "cb" 'string-set! x)
(test "ab" symbol->string y)
(test y string->symbol "ab")

(test #f eq? 'mISSISSIppi 'mississippi)
;;; Upstream expected #f here by way of the R4RS case-insensitive reader,
;;; which folded 'bitBlt to the symbol bitblt while string->symbol kept the
;;; case, so the two symbols differed. R7RS-small 2.1 is case-SENSITIVE by
;;; default (folding requires include-ci), and 6.5 makes string->symbol
;;; intern, so the correct expectation is #t.
(test #t 'string->symbol (eq? 'bitBlt (string->symbol "bitBlt")))
(test 'JollyWog string->symbol (symbol->string 'JollyWog))

(SECTION 6 5 5)
(test #t number? 3)
;;; R7RS-small 6.2: exact 3 is complex, real, rational and integer.
(test #t complex? 3)
(test #t real? 3)
(test #t rational? 3)
(test #t integer? 3)

(test #t exact? 3)
(test #f inexact? 3)

(test 1 expt 0 0)
(test 0 expt 0 1)
(test 0 expt 0 256)
;;(test 0 expt 0 -255)
(test 1 expt -1 256)
(test -1 expt -1 255)
(test 1 expt -1 -256)
(test -1 expt -1 -255)
(test 1 expt 256 0)
(test 1 expt -256 0)
(test 256 expt 256 1)
(test -256 expt -256 1)
(test 8 expt 2 3)
(test -8 expt -2 3)
(test 9 expt 3 2)
(test 9 expt -3 2)

(test #t = 22 22 22)
(test #t = 22 22)
(test #f = 34 34 35)
(test #f = 34 35)
(test #t > 3 -6246)
(test #f > 9 9 -2424)
(test #t >= 3 -4 -6246)
(test #t >= 9 9)
(test #f >= 8 9)
(test #t < -1 2 3 4 5 6 7 8)
(test #f < -1 2 3 4 4 5 6 7)
(test #t <= -1 2 3 4 5 6 7 8)
(test #t <= -1 2 3 4 4 5 6 7)
(test #f < 1 3 2)
(test #f >= 1 3 2)

(test #t zero? 0)
(test #f zero? 1)
(test #f zero? -1)
(test #f zero? -100)
(test #t positive? 4)
(test #f positive? -4)
(test #f positive? 0)
(test #f negative? 4)
(test #t negative? -4)
(test #f negative? 0)
(test #t odd? 3)
(test #f odd? 2)
(test #f odd? -4)
(test #t odd? -1)
(test #f even? 3)
(test #t even? 2)
(test #t even? -4)
(test #f even? -1)

(test 38 max 34 5 7 38 6)
(test -24 min 3  5 5 330 4 -24)

(test 7 + 3 4)
(test '3 + 3)
(test 0 +)
(test 4 * 4)
(test 1 *)
(test 1 / 1)
(test -1 / -1)
(test 2 / 6 3)
(test -3 / 6 -2)
(test -3 / -6 2)
(test 3 / -6 -2)
(test -1 - 3 4)
(test -3 - 3)
(test 7 abs -7)
(test 7 abs 7)
(test 0 abs 0)

(test 5 quotient 35 7)
(test -5 quotient -35 7)
(test -5 quotient 35 -7)
(test 5 quotient -35 -7)
(test 1 modulo 13 4)
(test 1 remainder 13 4)
(test 3 modulo -13 4)
(test -1 remainder -13 4)
(test -3 modulo 13 -4)
(test 1 remainder 13 -4)
(test -1 modulo -13 -4)
(test -1 remainder -13 -4)
(test 0 modulo 0 86400)
(test 0 modulo 0 -86400)
(define (divtest n1 n2)
	(= n1 (+ (* n2 (quotient n1 n2))
		 (remainder n1 n2))))
(test #t divtest 238 9)
(test #t divtest -238 9)
(test #t divtest 238 -9)
(test #t divtest -238 -9)

(test 4 gcd 0 4)
(test 4 gcd -4 0)
(test 4 gcd 32 -36)
(test 0 gcd)
(test 288 lcm 32 -36)
(test 1 lcm)

(SECTION 6 5 5)
;;; R7RS-small 6.2.7: if string is not valid number notation, string->number
;;; returns #f, and "An error is never signaled due to the content of string."
;;; The division-by-zero spellings below are invalid notation, so #f is the
;;; only conforming answer.
;;;
;;; MIT/GNU Scheme 12.1 instead signals "Division by zero", which aborts the
;;; whole suite under --batch-mode rather than yielding #f. Wrapping the CALL
;;; in `guard` lets the run continue and records the violation as a failure;
;;; it does not let a non-conforming host pass, because the guarded result
;;; `(raised ...)` is neither #f nor a number.
(define (test-string->number str)
  (define ans (guard (c (else (list 'raised c))) (string->number str)))
  (cond ((not ans) #t) ((number? ans) #t) (else ans)))
(for-each (lambda (str) (test #t test-string->number str))
	  '("+#.#" "-#.#" "#.#" "1/0" "-1/0" "0/0"
	    "+1/0i" "-1/0i" "0/0i" "0/0-0/0i" "1/0-1/0i" "-1/0+1/0i"
	    "#i" "#e" "#" "#i0/0"))
(cond ((number? (guard (c (else #f)) (string->number "1+1i")))
       (test #t number? (string->number "#i-i"))
       (test #t number? (string->number "#i+i"))
       (test #t number? (string->number "#i2+i"))))

;;; Inexact-number tests.
(define (test-inexact)
  (define f3.9 (string->number "3.9"))
  (define f4.0 (string->number "4.0"))
  (define f-3.25 (string->number "-3.25"))
  (define f.25 (string->number ".25"))
  (define f4.5 (string->number "4.5"))
  (define f3.5 (string->number "3.5"))
  (define f0.0 (string->number "0.0"))
  (define f0.8 (string->number "0.8"))
  (define f1.0 (string->number "1.0"))
  (define f1e300 (and (string->number "1+3i") (string->number "1e300")))
  (define f1e-300 (and (string->number "1+3i") (string->number "1e-300")))
  (newline)
  (display ";testing inexact numbers; ")
  (newline)
  (SECTION 6 2)
  (test #f eqv? 1 f1.0)
  (test #f eqv? 0 f0.0)
  (test #t eqv? f0.0 f0.0)
  ;;; R7RS-small 6.2.4: 0.0 and -0.0 are numerically equal, and are
  ;;; "distinct (in the sense of eqv?)" -- but "an implementation is not
  ;;; required to distinguish negative zero". Upstream's bare
  ;;; (test #t eqv? f0.0 (- f0.0)) is therefore NOT portable: every modern
  ;;; host returns #f there (IEEE 754 does distinguish the zeroes), so
  ;;; upstream's assertion fails everywhere.
  ;;;
  ;;; Rather than replace it with an unfailable assertion, assert what the
  ;;; report actually requires and what CAN fail: the numeric equality
  ;;; (6.2.6 -- the two must compare =), and that the pair is handled
  ;;; consistently whichever way the host resolves eqv?. `(memv (eqv? a b)
  ;;; (list #t #f))' would be a tautology, so instead pin the observable
  ;;; consequence: if the host DOES distinguish them, -0.0 must still behave
  ;;; as a zero arithmetically and must not become eq? to any non-zero.
  (test #t 'signed-zero-num-eq (= f0.0 (- f0.0)))
  (test #t 'signed-zero-prod-num-eq (= f0.0 (* -5 f0.0)))
  ;;; -0.0 and 0.0 must not be eq? to a DIFFERENT value; if a host folds
  ;;; -0.0 to 0.0 entirely this still holds, and if it distinguishes them
  ;;; it also holds, so it is a real (if weak) consistency requirement
  ;;; rather than a restatement of 6.1.
  (test #f 'signed-zero-not-eqv-to-one (eqv? (- f0.0) 1.0))
  ;;; eqv? must be reflexive on numbers, which -0.0 is: a host that
  ;;; distinguishes the zeroes must still agree with itself. NOTE: do not
  ;;; assert anything about (round -0.0) here. Verified on the working
  ;;; hosts: chibi and sagittarius return -0.0, gauche returns 0.0. Since
  ;;; 6.2.4 lets an implementation decline to distinguish negative zero,
  ;;; that latitude makes the sign of (round -0.0) unspecified, so such an
  ;;; assertion would fail on a conforming host.
  (test #t 'signed-zero-eqv-reflexive (eqv? (- f0.0) (- f0.0)))
  (SECTION 6 5 5)
  ;;; R7RS-small: three `if' guards, enforcing two policies. The magnitude
  ;;; guard requires a usable finite, strictly positive real for both `mag'
  ;;; and `den' ((real? ..) (finite? ..) (> .. 0.0)); the (> mag 0.0) clause is
  ;;; load-bearing -- (magnitude (make-rectangular 1e-300 1e-300))
  ;;; underflows to 0.0, which IS finite, so a finite?-only guard would let
  ;;; a conforming restriction be scored as a defect.
  ;;; The two division guards require a real, non-NaN result, so a host that
  ;;; cannot divide at this scale is skipped rather than failed.
  ;;; When a guard is false no `test' runs, so that assertion is neither
  ;;; passed nor failed on that host. This is why the reported total varies
  ;;; by host: the assertions are conditional, not missing.
  ;;;
  ;;; The magnitude ratio keeps upstream's denominator (* f (sqrt 2)), which
  ;;; is mathematically right: |f+fi| = f*(sqrt 2). Verified on all three
  ;;; working hosts: (* 4 z) does NOT overflow at 1e300 (4e300 is well inside
  ;;; the binary64 range) and does not underflow at 1e-300 either. The NaN that
  ;;; makes upstream's `(/ z (* 4 z))' unusable comes from the DIVISION
  ;;; ALGORITHM, which forms c^2+d^2 = (4e300)^2 and overflows that
  ;;; intermediate to +inf.0, giving inf/inf. So upstream's form asserts
  ;;; nothing on any of these hosts.
  ;;;
  ;;; Complex-by-complex division is therefore exercised with a divisor small
  ;;; enough that the algorithm's intermediates stay finite, which keeps the
  ;;; complex-divisor code path covered instead of dropping it. Dividing by an
  ;;; exact real is also checked; dividing a complex by a real is
  ;;; componentwise, so (/ z 4) is (make-rectangular (/ f 4) (/ f 4)).
  ;;;
  ;;; Each block uses sequential `if's rather than a `cond' with an `else', so
  ;;; there is exactly one nesting level of assertion per guard and no
  ;;; clause-body/test ambiguity. When a guard is false no `test' runs, so the
  ;;; assertion is neither passed nor failed: 6.2.3 permits the restriction,
  ;;; and recording a failure for a sanctioned range limit would be wrong.
  (and f1e300
       (let* ((f1e300+1e300i (make-rectangular f1e300 f1e300))
	      (mag (guard (c (else #f)) (magnitude f1e300+1e300i)))
	      (den (guard (c (else #f)) (* f1e300 (sqrt 2))))
	      (q (guard (c (else #f)) (/ f1e300+1e300i 4)))
	      (q2 (guard (c (else #f))
		    (/ f1e300+1e300i (make-rectangular 1.0 1.0)))))
	 (if (and (real? mag) (finite? mag) (> mag 0.0)
		  (real? den) (finite? den) (> den 0.0))
	     (test f1.0 'magnitude (/ mag den)))
	 (if (and (number? q) (real? (real-part q)) (real? (imag-part q))
		  (not (nan? (real-part q))) (not (nan? (imag-part q))))
	     (test (make-rectangular (/ f1e300 4) (/ f1e300 4))
		   / f1e300+1e300i 4))
	 ;; Complex / complex. The divisor is kept SMALL so the division
	 ;; algorithm's intermediate c^2+d^2 stays finite at both magnitudes --
	 ;; a divisor scaled like f itself would square to +inf.0 at 1e300 and
	 ;; yield NaN, which is exactly the defect this probe exists to catch.
	 ;; (f+fi)/(1+i) = (f+f + (f-f)i)/2 = f + 0i.
	 (if (and (number? q2) (real? (real-part q2)) (real? (imag-part q2))
		  (not (nan? (real-part q2))) (not (nan? (imag-part q2))))
	     (test (make-rectangular f1e300 0.0)
		   / f1e300+1e300i (make-rectangular 1.0 1.0)))))
  (and f1e-300
       (let* ((f1e-300+1e-300i (make-rectangular f1e-300 f1e-300))
	      (mag (guard (c (else #f)) (magnitude f1e-300+1e-300i)))
	      (den (guard (c (else #f)) (* f1e-300 (sqrt 2))))
	      (q (guard (c (else #f)) (/ f1e-300+1e-300i 4)))
	      (q2 (guard (c (else #f))
		    (/ f1e-300+1e-300i (make-rectangular 1.0 1.0)))))
	 (if (and (real? mag) (finite? mag) (> mag 0.0)
		  (real? den) (finite? den) (> den 0.0))
	     (test f1.0 'magnitude (round (/ mag den))))
	 (if (and (number? q) (real? (real-part q)) (real? (imag-part q))
		  (not (nan? (real-part q))) (not (nan? (imag-part q))))
	     (test (make-rectangular (/ f1e-300 4) (/ f1e-300 4))
		   / f1e-300+1e-300i 4))
	 (if (and (number? q2) (real? (real-part q2)) (real? (imag-part q2))
		  (not (nan? (real-part q2))) (not (nan? (imag-part q2))))
	     (test (make-rectangular f1e-300 0.0)
		   / f1e-300+1e-300i (make-rectangular 1.0 1.0)))))
  (test #t = f0.0 f0.0)
  ;;; (= f0.0 (- f0.0)) and (= f0.0 (* -5 f0.0)) are already asserted above as
  ;;; signed-zero-num-eq / signed-zero-prod-num-eq; upstream repeats them here.
  (test #t inexact? f3.9)
  ;;; R7RS-small 6.2.6: if any argument is inexact, max returns an
  ;;; inexact result. Check exactness and value.
  (test #t 'max-inexact (inexact? (max f3.9 4)))
  (test #t = 4.0 (max f3.9 4))
  (test #t 'inexact-is-inexact (inexact? (inexact 4)))
  (test #t = f4.0 (inexact 4))
  (test #t = f4.0 (inexact f4.0))
  (test #t 'exact-is-exact (exact? (exact 4)))
  (test #t = 4 (exact 4))
  (test #t 'exact-of-inexact-is-exact (exact? (exact f4.0)))
  (test #t = 4 (exact f4.0))
  (test (- f4.0) round (- f4.5))
  (test (- f4.0) round (- f3.5))
  (test (- f4.0) round (- f3.9))
  (test f0.0 round f0.0)
  (test f0.0 round f.25)
  (test f1.0 round f0.8)
  (test f4.0 round f3.5)
  (test f4.0 round f4.5)

  ;;(test f1.0 expt f0.0 f0.0)
  ;;(test f1.0 expt f0.0 0)
  ;;(test f1.0 expt 0    f0.0)
  (test f0.0 expt f0.0 f1.0)
  (test f0.0 expt f0.0 1)
  ;;; Same R7RS-small 6.2.2 latitude as the zero-exponent case below: an exact
  ;;; base with exponent 1 may yield an exact result. MIT/GNU Scheme 12.1 returns
  ;;; exact 0 here, and exact 1 for the zero-exponent case; both are permitted.
  (test #t 'expt-one-exponent (= (expt 0 f1.0) f0.0))
  (test f1.0 expt -25  f0.0)
  (test f1.0 expt f-3.25 f0.0)
  ;;; R7RS-small 6.2.2: an operation may return an exact result if it can prove
  ;;; the result is unaffected by its arguments' inexactness. An exact zero
  ;;; exponent is the textbook case, so (expt -3.25 0) may return exact 1 or
  ;;; 1.0. Guile, Gambit and Chez return exact 1 and are conforming; chibi,
  ;;; gauche and chicken return 1.0. `test` compares with equal?, which is
  ;;; #t for 1 vs 1.0 numerically but the harness reports the raw result, so
  ;;; compare the two calls for equality instead.
  (test #t 'expt-zero-exponent
        (= (expt f-3.25 0) (expt f-3.25 0.0)))
  ;;(test f0.0 expt f0.0 f-3.25)

  (test (atan 1) atan 1 1)
  ;;; R7RS-small: file round-trip needs (scheme file); use string
  ;;; ports from (scheme base) instead.
  (test #t 'number-string-roundtrip
	(let ((s (call-with-port (open-output-string)
		   (lambda (p) (write (list f.25 f-3.25) p)
			      (get-output-string p)))))
	  (equal? (list f.25 f-3.25)
		  (call-with-port (open-input-string s) read))))
  (let ((x (string->number "4195835.0"))
	(y (string->number "3145727.0")))
    (test #t 'fdiv-accuracy (> f1.0 (- x (* (/ x y) y)))))
  (r7rs-report-errs))

(define (test-inexact-printing)
  (let ((f0.0 (string->number "0.0"))
	(f0.5 (string->number "0.5"))
	(f1.0 (string->number "1.0"))
	(f2.0 (string->number "2.0")))
    (define log2
      (let ((l2 (log 2)))
	(lambda (x) (/ (log x) l2))))

    (define (slow-frexp x)
      (if (zero? x)
	  (list f0.0 0)
	  (let* ((l2 (log2 x))
		 (e (floor (log2 x)))
		 (e (if (= l2 e)
			(exact e)
			(+ (exact e) 1)))
		 (f (/ x (expt 2 e))))
	    (list f e))))

    (define float-precision
      (let ((mantissa-bits
	     (do ((i 0 (+ i 1))
		  (eps f1.0 (* f0.5 eps))
		  ;; R7RS-small: guard against hosts whose inexact
		  ;; arithmetic never rounds to 1.0.
		  (guard-n 0 (+ guard-n 1)))
		 ((or (= f1.0 (+ f1.0 eps)) (> guard-n 2000))
		  i)))
	    (minval
	     (do ((x f1.0 (* f0.5 x))
		  (guard-n 0 (+ guard-n 1)))
		 ((or (zero? (* f0.5 x)) (> guard-n 10000)) x))))
	(lambda (x)
	  (apply (lambda (f e)
		   (let ((eps
			  (cond ((= f1.0 f) (expt f2.0 (+ 1 (- e mantissa-bits))))
				((zero? f) minval)
				(else (expt f2.0 (- e mantissa-bits))))))
		     (if (zero? eps)	;Happens if gradual underflow.
			 minval
			 eps)))
		 (slow-frexp x)))))

    (define (float-print-test x)
      (define (testit number)
	;;; R7RS-small 6.2.7: number->string output must read back to an
	;;; eqv? number. Do NOT mask an unparseable result -- that would
	;;; hide a genuine round-trip defect. NaN is the single exception
	;;; no host can satisfy, since (eqv? +nan.0 +nan.0) is #f, so it
	;;; is excluded explicitly rather than by accepting #f.
	(if (nan? number)
	    #t
	    (eqv? number (string->number (number->string number)))))
      (let ((eps (float-precision x))
	    (all-ok? #t))
	(do ((j -100 (+ j 1)))
	    ((or (not all-ok?) (> j 100)) all-ok?)
	  (let* ((xx (+ x (* j eps)))
		 (ok? (testit xx)))
	    (cond ((not ok?)
		   (display "Number readback failure for ")
		   (display `(+ ,x (* ,j ,eps))) (newline)
		   (display xx) (newline)
		   (display (string->number (number->string xx))) (newline)
		   (set! all-ok? #f))
		  ;;   (else (display xx) (newline))
		  )))))

    (define (mult-float-print-test x)
      (let ((res #t))
	(for-each
	 (lambda (mult)
	   (or (float-print-test (* mult x)) (set! res #f)))
	 (map string->number
	      '("1.0" "10.0" "100.0" "1.0e20" "1.0e50" "1.0e100"
		"0.1" "0.01" "0.001" "1.0e-20" "1.0e-50" "1.0e-100")))
	res))

    (define (float-rw-range-test)
      (define success #t)
      (do ((cnt -323 (+ 1 cnt)))
	  ((> cnt 308) success)
	(let* ((estr (string-append "1.e" (number->string cnt)))
	       (num (string->number estr))
	       (str (and num (number->string num)))
	       (back (and str (string->number str))))
	  ;;; R7RS-small 6.2.7 requires only that number->string output read
	  ;;; back to an equal number; it does NOT mandate a minimal digit
	  ;;; count. Upstream's extra (>= (string-length str) 10) check is
	  ;;; therefore dropped, at the cost of no longer flagging a host
	  ;;; that prints many redundant digits (e.g. 17+ per float).
	  ;;; Subnormal and overflow edges are implementation quality, not
	  ;;; conformance, so they are skipped rather than failed.
	  (cond ((and num back (not (equal? back num))
			(finite? num) (finite? back))
		 (set! success #f)
		 (for-each write (list estr num str back)))))))

    (define (float-powers-of-2-test)
      (define all-ok? #t)
      (do ((xx (expt f2.0 1000) (/ xx 2))
	   (p 1000 (+ p -1)))
	  ((zero? xx) all-ok?)
	(let ((cnv (guard (c (else #f))
		     (string->number (number->string xx)))))
	  ;; R7RS-small 6.2: subnormal readback is implementation
	  ;; quality; only compare finite values that parse.
	  (cond ((and cnv (finite? xx) (finite? cnv) (not (= xx cnv)))
		 (display "Number readback failure for ")
		 (display `(expt ,f2.0 ,p)) (newline)
		 (display xx) (newline)
		 (set! all-ok? #f))))))

    (SECTION 6 5 6)
    ;;; R7RS-small 6.2.7: number->string must round-trip via
    ;;; string->number (eqv?), using minimal digits at radix 10.
    (test #t 'float-print-test (float-print-test f0.0))
    (test #t 'mult-float-print-test (mult-float-print-test f1.0))
    (test #t 'mult-float-print-test (mult-float-print-test
			     (string->number "3.0")))
    (test #t 'mult-float-print-test (mult-float-print-test
			     (string->number "7.0")))
    (test #t 'mult-float-print-test (mult-float-print-test
			     (string->number "3.1415926535897931")))
    (test #t 'mult-float-print-test (mult-float-print-test
			     (string->number "2.7182818284590451")))
    (test #t 'float-rw-range-test (float-rw-range-test))
    (test #t 'float-powers-of-2-test (float-powers-of-2-test))))
(define (test-bignum)
  (define tb
    (lambda (n1 n2)
      (= n1 (+ (* n2 (quotient n1 n2))
	       (remainder n1 n2)))))
  (define sqr
    (lambda (n1)
      (= (* n1 n1)
         (+ (* (+ n1 1) (- n1 1)) 1))))
  ;;; R7RS-small: `ash' is not standard; no test-ash helper.
  (define b3-3 (string->number "33333333333333333333"))
  (define b3-2 (string->number "33333333333333333332"))
  (define b3-0 (string->number "33333333333333333330"))
  (define b1-1 (string->number "11111111111111111111"))
  (define b2-0 (string->number "2177452800"))
  (define b17 (string->number "#x10000"))
  (define b21 (string->number "#x100000"))
  (define b33 (string->number "#x100000000"))
  (define b37 (string->number "#x1000000000"))
  (define b40 (string->number "#x8000000000"))
  (define b65 (string->number "#x10000000000000000"))
  (define b69 (string->number "#x100000000000000000"))
  (newline)
  (display ";testing bignums; ")
  (newline)
  (test b33 * b17 b17)
  (test #t 'arithmetic (sqr b17))
  (test b37 * b17 b21)
  (test b65 * b33 b33)
  (test #t 'arithmetic (sqr b33))
  (test b69 * b33 b37)
  ;;; R7RS-small: upstream test-ash needs non-standard `ash'; excluded.
  (test b17 expt 2 16)
  (test b21 expt 2 20)
  (test b37 expt 2 36)
  (test b37 expt -2 36)
  (test b40 expt 2 39)
  (test b40 (lambda (b p) (- (expt b p))) -2 39)
  (test b65 expt 2 64)
  (test b65 expt -2 64)
  (test b69 expt 2 68)
  (test b69 expt -2 68)

  (SECTION 6 5 7)
  (test 0 modulo b3-3 3)
  (test 0 modulo b3-3 -3)
  (test 0 remainder b3-3 3)
  (test 0 remainder b3-3 -3)
  (test 2 modulo b3-2 3)
  (test -1 modulo b3-2 -3)
  (test 2 remainder b3-2 3)
  (test 2 remainder b3-2 -3)
  (test 1 modulo (- b3-2) 3)
  (test -2 modulo (- b3-2) -3)
  (test -2 remainder (- b3-2) 3)
  (test -2 remainder (- b3-2) -3)

  (test 3 modulo 3 b3-3)
  (test b3-0 modulo -3 b3-3)
  (test 3 remainder 3 b3-3)
  (test -3 remainder -3 b3-3)
  (test (- b3-0) modulo 3 (- b3-3))
  (test -3 modulo -3 (- b3-3))
  (test 3 remainder 3 (- b3-3))
  (test -3 remainder -3 (- b3-3))

  (test 0 modulo (- b2-0) 86400)
  (test 0 modulo b2-0 -86400)
  (test 0 modulo b2-0 86400)
  (test 0 modulo (- b2-0) -86400)
  (test 0 modulo  0 (- b2-0))
  (test #t 'remainder (tb (string->number "281474976710655325431") 65535))
  (test #t 'remainder (tb (string->number "281474976710655325430") 65535))

  (test b1-1 gcd b3-3 b1-1)
  (test 1 gcd b3-2 b1-1)
  (test 1 gcd b3-0 b1-1)
  (test 3 gcd b3-3 b3-0)

  (test b3-3 lcm b3-3 b1-1)
  (test b3-3 lcm -3 b1-1)

  (let ((n (string->number
	    "30414093201713378043612608166064768844377641568960512")))
    (and n (exact? n)
	 (do ((pow3 1 (* 3 pow3))
	      (cnt 21 (+ -1 cnt)))
	     ((negative? cnt)
	      (zero? (modulo n pow3))))))

  (SECTION 6 5 8)
  (test "281474976710655325431" number->string
	(string->number "281474976710655325431"))
  (r7rs-report-errs))

(define (test-numeric-predicates)
  (let* ((big-ex (expt 2 150))
	 (big-inex (inexact big-ex)))
    (newline)
    (display ";testing bignum-inexact comparisons;")
    (newline)
    (SECTION 6 5 5)
    (test #f = (+ big-ex 1) big-inex (- big-ex 1))
    (test #f = big-inex (+ big-ex 1) (- big-ex 1))
    (test #t < (- (exact big-inex) 1)
	  big-inex
	  (+ (exact big-inex) 1))))


(SECTION 6 5 9)
(test "0" number->string 0)
(test "100" number->string 100)
(test "100" number->string 256 16)
(test 100 string->number "100")
(test 256 string->number "100" 16)
(test #f string->number "")
(test #f string->number ".")
(test #f string->number "d")
(test #f string->number "D")
(test #f string->number "i")
(test #f string->number "I")
;;; R7RS-small 7.1.1: <complex R> has no bare-imaginary production, so
;;; unsigned "3i" is not valid notation and string->number must return
;;; #f. Chibi accepts it as an extension, which this flags as a
;;; conformance deviation rather than tolerating silently.
(test #f string->number "3i")
(test #f string->number "3I")
(test #f string->number "33i")
(test #f string->number "33I")
(test #f string->number "3.3i")
(test #f string->number "3.3I")
(test #f string->number "-")
(test #f string->number "+")
(test #t 'string->number (or (not (string->number "80000000" 16))
			     (positive? (string->number "80000000" 16))))
(test #t 'string->number (or (not (string->number "-80000000" 16))
			     (negative? (string->number "-80000000" 16))))

(SECTION 6 6)
;;; R7RS-small 6.6 requires only `#\space' (plus alarm backspace delete
;;; escape newline null return tab); `#\Space' is an extension, so only
;;; the portable spelling is asserted.
(test #t eqv? '#\  #\space)
(test #t char? #\a)
(test #t char? #\()
(test #t char? #\space)
(test #t char? '#\newline)

(test #f char=? #\A #\B)
(test #f char=? #\a #\b)
(test #f char=? #\9 #\0)
(test #t char=? #\A #\A)

(test #t char<? #\A #\B)
(test #t char<? #\a #\b)
(test #f char<? #\9 #\0)
(test #f char<? #\A #\A)

(test #f char>? #\A #\B)
(test #f char>? #\a #\b)
(test #t char>? #\9 #\0)
(test #f char>? #\A #\A)

(test #t char<=? #\A #\B)
(test #t char<=? #\a #\b)
(test #f char<=? #\9 #\0)
(test #t char<=? #\A #\A)

(test #f char>=? #\A #\B)
(test #f char>=? #\a #\b)
(test #t char>=? #\9 #\0)
(test #t char>=? #\A #\A)

;;; R7RS-small: char-ci tests need (scheme char); run as optional.
(r7rs-optional-thunk
 (lambda ()
(test #f char-ci=? #\A #\B)
(test #f char-ci=? #\a #\B)
(test #f char-ci=? #\A #\b)
(test #f char-ci=? #\a #\b)
(test #f char-ci=? #\9 #\0)
(test #t char-ci=? #\A #\A)
(test #t char-ci=? #\A #\a)

(test #t char-ci<? #\A #\B)
(test #t char-ci<? #\a #\B)
(test #t char-ci<? #\A #\b)
(test #t char-ci<? #\a #\b)
(test #f char-ci<? #\9 #\0)
(test #f char-ci<? #\A #\A)
(test #f char-ci<? #\A #\a)

(test #f char-ci>? #\A #\B)
(test #f char-ci>? #\a #\B)
(test #f char-ci>? #\A #\b)
(test #f char-ci>? #\a #\b)
(test #t char-ci>? #\9 #\0)
(test #f char-ci>? #\A #\A)
(test #f char-ci>? #\A #\a)

(test #t char-ci<=? #\A #\B)
(test #t char-ci<=? #\a #\B)
(test #t char-ci<=? #\A #\b)
(test #t char-ci<=? #\a #\b)
(test #f char-ci<=? #\9 #\0)
(test #t char-ci<=? #\A #\A)
(test #t char-ci<=? #\A #\a)

(test #f char-ci>=? #\A #\B)
(test #f char-ci>=? #\a #\B)
(test #f char-ci>=? #\A #\b)
(test #f char-ci>=? #\a #\b)
(test #t char-ci>=? #\9 #\0)
(test #t char-ci>=? #\A #\A)
(test #t char-ci>=? #\A #\a)

(test #t char-alphabetic? #\a)
(test #t char-alphabetic? #\A)
(test #t char-alphabetic? #\z)
(test #t char-alphabetic? #\Z)
(test #f char-alphabetic? #\0)
(test #f char-alphabetic? #\9)
(test #f char-alphabetic? #\space)
(test #f char-alphabetic? #\;)

(test #f char-numeric? #\a)
(test #f char-numeric? #\A)
(test #f char-numeric? #\z)
(test #f char-numeric? #\Z)
(test #t char-numeric? #\0)
(test #t char-numeric? #\9)
(test #f char-numeric? #\space)
(test #f char-numeric? #\;)

(test #f char-whitespace? #\a)
(test #f char-whitespace? #\A)
(test #f char-whitespace? #\z)
(test #f char-whitespace? #\Z)
(test #f char-whitespace? #\0)
(test #f char-whitespace? #\9)
(test #t char-whitespace? #\space)
(test #f char-whitespace? #\;)

(test #f char-upper-case? #\0)
(test #f char-upper-case? #\9)
(test #f char-upper-case? #\space)
(test #f char-upper-case? #\;)

(test #f char-lower-case? #\0)
(test #f char-lower-case? #\9)
(test #f char-lower-case? #\space)
(test #f char-lower-case? #\;)
))

(test #\. integer->char (char->integer #\.))
(test #\A integer->char (char->integer #\A))
(test #\a integer->char (char->integer #\a))
;;; R7RS-small: char-upcase/char-downcase need (scheme char); optional.
(r7rs-optional-thunk
 (lambda ()
(test #\A char-upcase #\A)
(test #\A char-upcase #\a)
(test #\a char-downcase #\A)
(test #\a char-downcase #\a)
))
(SECTION 6 7)
(test #t string? "The word \"recursion\\\" has many meanings.")
(test #t string? "")
(define f (make-string 3 #\*))
(test "?**" 'string-set! (begin (string-set! f 0 #\?) f))
(test "abc" string #\a #\b #\c)
(test "" string)
(test 3 string-length "abc")
(test #\a string-ref "abc" 0)
(test #\c string-ref "abc" 2)
(test 0 string-length "")
(test "" substring "ab" 0 0)
(test "" substring "ab" 1 1)
(test "" substring "ab" 2 2)
(test "a" substring "ab" 0 1)
(test "b" substring "ab" 1 2)
(test "ab" substring "ab" 0 2)
(test "foobar" string-append "foo" "bar")
(test "foo" string-append "foo")
(test "foo" string-append "foo" "")
(test "foo" string-append "" "foo")
(test "" string-append)
(test "" make-string 0)
(test #t string=? "" "")
(test #f string<? "" "")
(test #f string>? "" "")
(test #t string<=? "" "")
(test #t string>=? "" "")
;;; R7RS-small: string-ci comparisons need (scheme char); optional.
(r7rs-optional-thunk
 (lambda ()
(test #t string-ci=? "" "")
(test #f string-ci<? "" "")
(test #f string-ci>? "" "")
(test #t string-ci<=? "" "")
(test #t string-ci>=? "" "")
))

(test #f string=? "A" "B")
(test #f string=? "a" "b")
(test #f string=? "9" "0")
(test #t string=? "A" "A")

(test #t string<? "A" "B")
(test #t string<? "a" "b")
(test #f string<? "9" "0")
(test #f string<? "A" "A")

(test #f string>? "A" "B")
(test #f string>? "a" "b")
(test #t string>? "9" "0")
(test #f string>? "A" "A")

(test #t string<=? "A" "B")
(test #t string<=? "a" "b")
(test #f string<=? "9" "0")
(test #t string<=? "A" "A")

(test #f string>=? "A" "B")
(test #f string>=? "a" "b")
(test #t string>=? "9" "0")
(test #t string>=? "A" "A")

;;; R7RS-small: remaining string-ci tests need (scheme char); optional.
(r7rs-optional-thunk
 (lambda ()
(test #f string-ci=? "A" "B")
(test #f string-ci=? "a" "B")
(test #f string-ci=? "A" "b")
(test #f string-ci=? "a" "b")
(test #f string-ci=? "9" "0")
(test #t string-ci=? "A" "A")
(test #t string-ci=? "A" "a")

(test #t string-ci<? "A" "B")
(test #t string-ci<? "a" "B")
(test #t string-ci<? "A" "b")
(test #t string-ci<? "a" "b")
(test #f string-ci<? "9" "0")
(test #f string-ci<? "A" "A")
(test #f string-ci<? "A" "a")

(test #f string-ci>? "A" "B")
(test #f string-ci>? "a" "B")
(test #f string-ci>? "A" "b")
(test #f string-ci>? "a" "b")
(test #t string-ci>? "9" "0")
(test #f string-ci>? "A" "A")
(test #f string-ci>? "A" "a")

(test #t string-ci<=? "A" "B")
(test #t string-ci<=? "a" "B")
(test #t string-ci<=? "A" "b")
(test #t string-ci<=? "a" "b")
(test #f string-ci<=? "9" "0")
(test #t string-ci<=? "A" "A")
(test #t string-ci<=? "A" "a")

(test #f string-ci>=? "A" "B")
(test #f string-ci>=? "a" "B")
(test #f string-ci>=? "A" "b")
(test #f string-ci>=? "a" "b")
(test #t string-ci>=? "9" "0")
(test #t string-ci>=? "A" "A")
(test #t string-ci>=? "A" "a")
))
(SECTION 6 8)
(test #t vector? '#(0 (2 2 2 2) "Anna"))
(test #t vector? '#())
(test '#(a b c) vector 'a 'b 'c)
(test '#() vector)
(test 3 vector-length '#(0 (2 2 2 2) "Anna"))
(test 0 vector-length '#())
(test 8 vector-ref '#(1 1 2 3 5 8 13 21) 5)
(test '#(0 ("Sue" "Sue") "Anna") 'vector-set
	(let ((vec (vector 0 '(2 2 2 2) "Anna")))
	  (vector-set! vec 1 '("Sue" "Sue"))
	  vec))
(test '#(hi hi) make-vector 2 'hi)
(test '#() make-vector 0)
(test '#() make-vector 0 'a)
(SECTION 6 9)
(test #t procedure? car)
(test #f procedure? 'car)
(test #t procedure? (lambda (x) (* x x)))
(test #f procedure? '(lambda (x) (* x x)))
(test #t call-with-current-continuation procedure?)
(test #t procedure? /)
(test 7 apply + (list 3 4))
(test 7 apply (lambda (a b) (+ a b)) (list 3 4))
(test 17 apply + 10 (list 3 4))
(test '() apply list '())
(define compose (lambda (f g) (lambda args (f (apply g args)))))
(test 30 (compose sqt *) 12 75)

(test '(b e h) map cadr '((a b) (d e) (g h)))
(test '(5 7 9) map + '(1 2 3) '(4 5 6))
(test '(1 2 3) map + '(1 2 3))
(test '(1 2 3) map * '(1 2 3))
(test '(-1 -2 -3) map - '(1 2 3))
(test '#(0 1 4 9 16) 'for-each
      (let ((v (make-vector 5)))
	(for-each (lambda (i) (vector-set! v i (* i i)))
		  '(0 1 2 3 4))
	v))
(test -3 call-with-current-continuation
      (lambda (exit)
	(for-each (lambda (x) (if (negative? x) (exit x)))
		  '(54 0 37 -3 245 19))
	#t))
(define list-length
 (lambda (obj)
  (call-with-current-continuation
   (lambda (return)
    (letrec ((r (lambda (obj) (cond ((null? obj) 0)
				((pair? obj) (+ (r (cdr obj)) 1))
				(else (return #f))))))
	(r obj))))))
(test 4 list-length '(1 2 3 4))
(test #f list-length '(a b . c))
(test '() map cadr '())

;;; This tests full conformance of call-with-current-continuation.
;;; The function leaf-eq? compares the leaves of 2 arbitrary trees
;;; constructed of conses.
(define (next-leaf-generator obj eot)
  (letrec ((return #f)
	   (cont (lambda (x)
		   (recur obj)
		   (set! cont (lambda (x) (return eot)))
		   (cont #f)))
	   (recur (lambda (obj)
		      (if (pair? obj)
			  (for-each recur obj)
			  (call-with-current-continuation
			   (lambda (c)
			     (set! cont c)
			     (return obj)))))))
    (lambda () (call-with-current-continuation
		(lambda (ret) (set! return ret) (cont #f))))))
(define (leaf-eq? x y)
  (let* ((eot (list 'eot))
	 (xf (next-leaf-generator x eot))
	 (yf (next-leaf-generator y eot)))
    (letrec ((loop (lambda (x y)
		     (cond ((not (eq? x y)) #f)
			   ((eq? eot x) #t)
			   (else (loop (xf) (yf)))))))
      (loop (xf) (yf)))))
(define (test-cont)
  (newline)
  (display ";testing continuations; ")
  (newline)
  (SECTION 6 9)
  (test #t leaf-eq? '(a (b (c))) '((a) b c))
  (test #f leaf-eq? '(a (b (c))) '((a) b c d))
  (r7rs-report-errs))

;;; R7RS-small: delay/force are (scheme lazy), optional.
(define (test-delay)
  (newline)
  (display ";testing DELAY and FORCE; ")
  (newline)
  (r7rs-optional-thunk
   (lambda ()
  (SECTION 6 9)
  (test 3 'delay (force (delay (+ 1 2))))
  (test '(3 3) 'delay (let ((p (delay (+ 1 2))))
			(list (force p) (force p))))
  (test 2 'delay (letrec ((a-stream
			   (letrec ((next (lambda (n)
					    (cons n (delay (next (+ n 1)))))))
			     (next 0)))
			  (head car)
			  (tail (lambda (stream) (force (cdr stream)))))
		   (head (tail (tail a-stream)))))
  (letrec ((count 0)
	   (p (delay (begin (set! count (+ count 1))
			    (if (> count x)
				count
				(force p)))))
	   (x 5))
    (test 6 force p)
    (set! x 10)
    (test 6 force p))
  (test 3 'force
	(letrec ((p (delay (if c 3 (begin (set! c #t) (+ (force p) 1)))))
		 (c #f))
	  (force p)))
  ))
  (r7rs-report-errs))

;;; R7RS-small: this block needs (scheme file) for file ports (plus
;;; (scheme read)/(scheme write) for reader/writer coverage), so the
;;; whole block counts as optional.
;;; Tmp files live under TMPDIR/TEMP/TMP (falling back to /tmp when
;;; none is set or any is empty) and are removed after use. Absolute
;;; paths are used because `load' need not resolve relative names
;;; against the current directory.
(define r7rs-tmpdir
  (let ((d (or (get-environment-variable "TMPDIR")
               (get-environment-variable "TEMP")
               (get-environment-variable "TMP"))))
    (if (and d (> (string-length d) 0)) d "/tmp")))
(define r7rs-tmp-suffix ".scm")
(define (r7rs-tmp-path name)
  (string-append r7rs-tmpdir "/" name r7rs-tmp-suffix))
(define r7rs-tmp1 (r7rs-tmp-path "r7rstest-tmp1"))
(define r7rs-tmp2 (r7rs-tmp-path "r7rstest-tmp2"))
(define (r7rs-remove! f)
  (guard (c (else #f)) (delete-file f) #t))
;;; Remove any stale files from a previous run BEFORE writing them: 6.13.1
;;; leaves the effect of open-output-file on an existing file unspecified.
;;; All four tmp files are cleared, including the two written by the load
;;; test, so the guarantee holds uniformly.
(define (r7rs-clear-tmp!)
  (r7rs-remove! r7rs-tmp1)
  (r7rs-remove! r7rs-tmp2)
  (r7rs-remove! (r7rs-tmp-path "r7rstest-load"))
  (r7rs-remove! (r7rs-tmp-path "r7rstest-loaded")))
(define this-file #f)
(define test-file #f)
(define (check-test-file name)
  (define test-file (open-input-file name))
  (test #t 'input-port?
	(call-with-input-file
	    name
	  (lambda (test-file)
	    (test load-test-obj read test-file)
	    (test #t eof-object? (peek-char test-file))
	    (test #t eof-object? (read-char test-file))
	    (input-port? test-file))))
  (test #\; read-char test-file)
  (test #\; read-char test-file)
  (test #\; read-char test-file)
  (test write-test-obj read test-file)
  (test load-test-obj read test-file)
  (close-input-port test-file))
(SECTION 6 10 3)
(define write-test-obj
  '(#t #f a () 9739 -3 3.25 . #((test) "te \" \" st" "" test #() b c)))
;;; R7RS-small: `load-test-obj' is file content written to the tmp
;;; files and verified back with `read'; the `load' effect itself is
;;; verified separately by (test-sc4-load) via a file side effect.
(define load-test-obj
  (list 'define 'r7rs-loaded-foo (list 'quote write-test-obj)))
(define (r7rs-file-tests)
(SECTION 6 10 1)
(test #t input-port? (current-input-port))
(test #t output-port? (current-output-port))
(test #t call-with-input-file "r7rstest.scm" input-port?)
(set! this-file (open-input-file "r7rstest.scm"))
(test #t input-port? this-file)
(SECTION 6 10 2)
;;; R7RS-small: this file opens with comment lines, then the import
;;; form; assert the exact leading datums as upstream did.
(test #\; peek-char this-file)
(test #\; read-char this-file)
(test '(import (scheme base) (scheme read) (scheme write) (scheme file)
		(scheme char) (scheme lazy) (scheme inexact) (scheme complex)
		(scheme load) (scheme process-context))
      read this-file)
(test #\newline peek-char this-file)
(test '(define cur-section (quote ())) read this-file)
(test #\( peek-char this-file)
(close-input-port this-file)
;;; R7RS-small 6.13.1: closing an already-closed port has no effect.
(close-input-port this-file)
(test #t call-with-output-file
      r7rs-tmp1
      (lambda (test-file)
	(write-char #\; test-file)
	(display #\; test-file)
	(display ";" test-file)
	(write write-test-obj test-file)
	(newline test-file)
	(write load-test-obj test-file)
	(output-port? test-file)))
(check-test-file r7rs-tmp1)

(set! test-file (open-output-file r7rs-tmp2))
(write-char #\; test-file)
(display #\; test-file)
(display ";" test-file)
(write write-test-obj test-file)
(newline test-file)
(write load-test-obj test-file)
(test #t output-port? test-file)
(close-output-port test-file)
(check-test-file r7rs-tmp2)
;;; These files are read back by check-test-file above and removed by
;;; (test-sc4-load) below.
)
(r7rs-clear-tmp!)
(r7rs-optional-thunk r7rs-file-tests)
(define (test-sc4)
  (newline)
  (display ";testing scheme 4 functions; ")
  (newline)
  (SECTION 6 7)
  (test '(#\P #\space #\l) string->list "P l")
  (test '() string->list "")
  (test "1\\\"" list->string '(#\1 #\\ #\"))
  (test "" list->string '())
  (SECTION 6 8)
  (test '(dah dah didah) vector->list '#(dah dah didah))
  (test '() vector->list '#())
  (test '#(dididit dah) list->vector '(dididit dah))
  (test '#() list->vector '())
  (r7rs-report-errs))
(define (test-sc4-load)
  (newline)
  (display ";testing load; ")
  (newline)
  (SECTION 6 10 4)
  ;;; R7RS-small 6.14: verify `load' evaluates by loading a file
  ;;; that writes a sentinel datum to a known file, then reading
  ;;; the sentinel back. Portable across module systems because it
  ;;; observes a file side effect, not a script-local binding.
  (r7rs-optional-thunk
   (lambda ()
     (let ((src (r7rs-tmp-path "r7rstest-load"))
           (out (r7rs-tmp-path "r7rstest-loaded")))
       (call-with-output-file src
         (lambda (p)
           (write (list 'call-with-output-file out
                        (list 'lambda '(q)
                              (list 'write (list 'quote 'loaded-ok) 'q)))
                  p)
           (newline p)))
       (test 'loaded-ok 'load
             (guard (c (else (list 'load-failed c)))
               (begin (load src)
                      (call-with-input-file out read))))
       (r7rs-remove! src)
       (r7rs-remove! out))))
  (r7rs-remove! r7rs-tmp1)
  (r7rs-remove! r7rs-tmp2)
  (r7rs-report-errs))

;;; R7RS-small: (test-inexact), (test-inexact-printing) and
;;; (test-numeric-predicates) need (scheme inexact)/(scheme complex),
;;; so they run as optional. (test-bignum) exercises exact integer
;;; arithmetic from (scheme base), so it runs as required.
;;; File/load/lazy/char suites above already ran under
;;; r7rs-optional-thunk. `load' itself is covered by (test-sc4-load), which
;;; writes its own source file and observes the side effect.
;;; (test-sc4-load) below.
(let ((have-inexacts?
       (and (string->number "0.0") (inexact? (string->number "0.0"))))
      (have-bignums?
       (let ((n (string->number
		 "1427247692705959881058285969449495136382746625")))
	 (and n (exact? n)))))
  (r7rs-optional-thunk
   (lambda ()
     (cond (have-inexacts?
	    (test-inexact)
	    (test-inexact-printing)))
     (if (and have-inexacts? have-bignums?)
	 (test-numeric-predicates))))
  (if have-bignums? (test-bignum)))

(newline)
(display "Required suites: (test-cont) is R7RS-small 6.10;")
(newline)
(display "(test-sc4) string/vector parts are (scheme base);")
(newline)
(display "(test-sc4-load) is optional (scheme load);")
(newline)
(display "(test-delay) is optional (scheme lazy).")
(newline)
(test-cont)
(test-sc4)
(test-sc4-load)
(test-delay)
(r7rs-final-report)
"last item in file"
