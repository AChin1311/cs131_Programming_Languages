(define (interp x y)
  (quasiquote (if % (unquote x) (unquote y))))

(define (diffkeyword x y)
  (or
    (xor (equal? x 'quote) (equal? y 'quote))
    (xor (equal? x 'let) (equal? y 'let))
    (xor (equal? x 'if) (equal? y 'if))
    (xor (equal? x 'lambda) (equal? y 'lambda))))

(define (check x y)
  (cond
    ((and (equal? x '()) (equal? y '())) #t)
    ((equal? (length x) (length y))
      (cond
        ((equal? (car (car x)) (car (car y)))
          (check (cdr x) (cdr y)))
        (else #f)))
    (else #f)))

(define (build-list x y)
  (cond
    ((or (equal? x '()) (equal? y '()))
      (values '() '()))
    ((equal? (length x) (length y))
      (cond
        ((equal? (car (car x)) (car (car y)))
          (build-list (cdr x) (cdr y)))
        (else
          (let-values (((xvar yvar) (build-list (cdr x) (cdr y))))
            (values 
              (cons 
                (car (car x))
                (cons  
                  (string->symbol (string-append (symbol->string (car (car x))) "!" (symbol->string (car (car y)))))
                  xvar))
              (cons 
                (car (car y))
                  (cons
                    (string->symbol (string-append (symbol->string (car (car x))) "!" (symbol->string (car (car y)))))
                    yvar)))))))
    (else
      (values '() '()))))

(define (member? x lst)
     (if (equal? lst '()) #f                                
         (if (equal? x (car lst)) lst                   
              (member? x (cdr lst)))))                 

(define (getvalue x xvar)
  (cond 
    ((equal? (member? x xvar) #f) x)
    (else (car (cdr (member? x xvar))))))

(define (append list1 list2)
  (foldr cons list2 list1))

(define (replace-arg-list x xvar)
  (cond
    ((equal? x '()) '())
    (else
      (let ((retx (getvalue (car (car x)) xvar)))
        (if (equal? retx (car (car x)))
          (append (list (car x)) (replace-arg-list (cdr x) xvar))
          (append (list (cons retx (cdr (car x)))) (replace-arg-list (cdr x) xvar)))))))

(define (replace-val-list x var)
  (cond
    ((equal? x '()) '())
    (else
      (let ((retx (getvalue (car (cdr (car x))) var)))
        (if (equal? retx (car (cdr (car x))))
          (append (list (car x)) (replace-val-list (cdr x) var))
          (append (list (list (car (car x)) retx)) (replace-val-list (cdr x) var)))))))

(define (replace-expr-list x var)
  (cond
    ((equal? x '()) '())
    ((list? x)
      (cond 
        ((equal? (car x) 'lambda)
          (cons
            'lambda
            (replace-expr-list (cdr x) var)))
        ((equal? (car x) 'let)
          (cons 'let
            (append
              (list (replace-val-list (car (cdr x)) var))
              (replace-expr-list (cdr (cdr x)) '()))))
        (else
          (cons 
            (replace-expr-list (car x) var)
            (replace-expr-list (cdr x) var)))))
    (else
      (getvalue x var))))

(define (let-cmp x y)
  (let-values (((xvar yvar) (build-list (car (cdr x)) (car (cdr y)))))
    (if (and (equal? xvar '()) (equal? yvar '()))
      (cons 'let (expr-compare (cdr x) (cdr y)))
      (let ((args_x (replace-arg-list (car (cdr x)) xvar)) (args_y (replace-arg-list (car (cdr y)) yvar)))
        (let ((expr_x (replace-expr-list (car (cdr (cdr x))) xvar)) 
                (expr_y (replace-expr-list (car (cdr (cdr y))) yvar)))
          (cons 'let
            (expr-compare 
              (list args_x expr_x) 
              (list args_y expr_y))))))))

(define (lambda-args x y)
  (cond
    ((or (equal? x '()) (equal? y '()))
      (values '() '()))
    ((equal? (length x) (length y))
      (cond 
        ((equal? (car x) (car y))
          (lambda-args (cdr x) (cdr y)))
        (else
          (let-values (((xvar yvar) (lambda-args (cdr x) (cdr y))))
            (values 
              (cons 
                (car x)
                (cons 
                  (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))
                  xvar))
              (cons 
                (car y)
                (cons
                  (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))
                  yvar)))))))
    (else
      (values '() '()))))

(define (lambda-cmp x y)
  (let-values (((varx vary) (lambda-args (car (cdr x)) (car (cdr y)))))
    (if (and (equal? varx '()) (equal? vary '()))
      (cons 'lambda (expr-compare (cdr x) (cdr y)))
      (let ((args_x (replace-expr-list (car (cdr x)) varx)) 
            (args_y (replace-expr-list (car (cdr y)) vary)))
        (let ((expr_x (replace-expr-list (car (cdr (cdr x))) varx)) 
              (expr_y (replace-expr-list (car (cdr (cdr y))) vary)))
            (cons 
              'lambda
              (expr-compare (list args_x expr_x) (list args_y expr_y))))))))

(define (expr-compare x y) 
  (cond
    ((equal? x y) x)
    ((and (equal? x #t) (equal? y #f)) (quote %))
    ((and (equal? x #f) (equal? y #t)) (quote (not %)))
    ((and (list? x) (list? y)) 
      (cond
        ((equal? (length x) (length y))
          (cond
            ((diffkeyword (car x) (car y))
              (interp x y))
            ((and (equal? 'quote (car x)) (equal? 'quote (car y)))
              (if (equal? (cdr x) (cdr y))
                (quote x)
                (interp x y)))
            ((and (equal? 'let (car x)) (equal? 'let (car y)))
              (let-cmp x y))
            ((and (equal? 'lambda (car x)) (equal? 'lambda (car y)))
              (lambda-cmp x y))
            (else 
              (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y))))))
        (else (interp x y))))
    (else (interp x y))))

(define (test-expr-compare x y)
  (let ((expr (expr-compare x y)))
    (if 
      (and
        (equal? 
          (eval x) (eval (cons 'let (cons '((% #t)) (list expr)))))
        (equal? 
          (eval y) (eval (cons 'let (cons '((% #f)) (list expr))))))
      #t #f)))

(define test-expr-x
  '(let ((a ((lambda (b c) (+ b c)) 1 2)) (d 3))
    ((lambda (x y) 
      (let ((z (+ x 1))(w (+ y 1))) (equal? z w))) 
      a d)))
(define test-expr-y
  '(let ((b ((lambda (a d) (+ a d)) 2 1)) (c 3))
    ((lambda (x y) 
      (let ((z (+ x 1))(w (+ y 1))) (equal? z w))) 
      b c)))
