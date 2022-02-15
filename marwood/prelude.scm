(define-syntax let
    (syntax-rules ()
    [(let ((name val) ...) body1 body2 ...)
        ((lambda (name ...) body1 body2 ...) val ...)]))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
     (let ((var1 <undefined>) ...)
       (set! var1 init1)
       ...
       (let () body1 body2 ...)))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
     (let ((var1 #f) ...)
       (set! var1 init1)
       ...
       (let () body1 body2 ...)))))

(define-syntax let
    (syntax-rules ()
      ((let ((name val) ...) body1 body2 ...)
       ((lambda (name ...) body1 body2 ...)
        val ...))
    ((let tag ((name val) ...) body1 body2 ...)
       ((letrec ((tag (lambda (name ...)
               body1 body2 ...)))
         tag)
       val ...))))

(define-syntax or
  (syntax-rules ()
    [(or) #f]
    [(or test) test]
    [(or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...)))]))

(define-syntax and
 (syntax-rules ()
   [(and) #t]
   [(and test) test]
   [(and test1 test2 ...)
    (if test1 (and test2 ...) #f)]))

(define-syntax when
  (syntax-rules ()
    [(when test result1 result2 ...)
     (if test
         (begin result1 result2 ...))]))

 (define-syntax begin
       (syntax-rules ()
         [(begin exp ...)
          ((lambda () exp ...))]))


(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))

(define-syntax let*
 (syntax-rules ()
 ((let* () body1 body2 ...)
          (let () body1 body2 ...))
         ((let* ((name1 val1) (name2 val2) ...)
            body1 body2 ...)
          (let ((name1 val1))
            (let* ((name2 val2) ...)
              body1 body2 ...)))))

(define-syntax cond
      (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
         (begin result1 result2 ...))
        ((cond (test => result))
         (let ((temp test))
           (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               (result temp)
               (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
         (let ((temp test))
        (if temp temp
               (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
         (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...)
               clause1 clause2 ...)
         (if test
             (begin result1 result2 ...)
             (cond clause1 clause2 ...)))))

(define (atom? obj)
    (and (not (pair? obj)) (not (vector? obj))))

(define (caar obj) (car (car obj)))
(define (cadr obj) (car (cdr obj)))
(define (cdar obj) (cdr (car obj)))
(define (cddr obj) (cdr (cdr obj)))

(define (list . l) l)

(define (length list)
    (cond
      ((null? list) 0)
      (else (+ (length (cdr list)) 1))))

(define (memq obj list)
    (cond
      ((null? list) #f)
      ((eq? (car list) obj) list)
      (else (memq obj (cdr list)))))

(define (memv obj list)
    (cond
      ((null? list) #f)
      ((eqv? (car list) obj) list)
      (else (memv obj (cdr list)))))

(define (member obj list)
    (cond
      ((null? list) #f)
      ((equal? (car list) obj) list)
      (else (member obj (cdr list)))))

(define (assq obj alist)
    (cond
    ((null? alist) #f)
    ((and (pair? (car alist))
          (eq? (caar alist) obj)) (car alist))
    (else (assq obj (cdr alist)))))

(define (assv obj alist)
    (cond
    ((null? alist) #f)
    ((and (pair? (car alist))
          (eqv? (caar alist) obj)) (car alist))
    (else (assv obj (cdr alist)))))

(define (assoc obj alist)
    (cond
    ((null? alist) #f)
    ((and (pair? (car alist))
          (equal? (caar alist) obj)) (car alist))
    (else (assoc obj (cdr alist)))))

(define (substring string start end)
    (string-copy string start end))