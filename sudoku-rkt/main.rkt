#lang racket

(provide (all-defined-out))

(define (solution? board)
  (let [(perm? (lambda (xs ys)
                 (equal? (list->set xs)
                         (list->set ys))))]
    (define (solution-groups? grps)
      (cond [(null? grps) #t]
            [(not (perm? (range 1 10) (car grps))) #f]
            [else (solution-groups? (cdr grps))]))
    (solution-groups? (groups board))))

(define (rand-board)
  (define split (lambda (xs)
                  (if (null? xs)
                      '()
                      (cons (take xs 9) (split (drop xs 9))))))
  (split
   (shuffle
    (flatten
     (build-list 9 (lambda (_) (range 1 10)))))))

(define (columns board)
  (if (null? (car board))
      '()
      (cons (map car board)
            (columns (map cdr board)))))

(define (boxes board)
  (define (take-box x y n)
    (foldr append
           '()
           (map (lambda (row) (take (drop row x) n))
                (columns
                 (map (lambda (row) (take (drop row y) n)) board)))))
  (let [(xs '(0 3 6))]
    (foldr append
           '()
           (map (lambda (x)
                  (map (lambda (y)
                         (take-box x y 3)) xs))
                xs))))

(define (groups board)
  (append board
          (columns board)
          (boxes board)))

(define board-indexes
  (build-list 9 (lambda (i) (build-list 9 (lambda (j) (list i j))))))

(define (peers point)
  (let [(group-indexes (groups board-indexes))]
    (remove point
            (remove-duplicates
             (foldr append
                    '()
                    (filter (lambda (grp)
                              (member point grp))
                            group-indexes))))))

(define (board-ref board idx)
  (let [(x (car idx))
        (y (cadr idx))]
    (list-ref (list-ref board x) y)))

(define (board-replace board idx point)
  (let [(x (car idx))
        (y (cadr idx))]
    (append (take board x)
            (list
             (append (take (list-ref board x) y)
                     (list point)
                     (drop (list-ref board x)  (add1 y))))
            (drop board (add1 x)))))

(define (prev-index idx)
  (if (equal? idx '(0 0))
      #f
      (let [(x (car idx))
            (y (cadr idx))]
        (if (= y 0)
            (list (sub1 x) 8)
            (list x (sub1 y))))))

(define (solutions board)
  (define (helper board idx)
    (if (not idx)
        (list board)
        (if (= (board-ref board idx) 0)
            (let* [(pps (remove-duplicates (map (curry board-ref board) (peers idx))))
                   (vs (filter-not (lambda (x) (member x pps)) (range 1 10)))]
              (foldr append
                     '()
                     (map (lambda (v) (helper (board-replace board idx v) (prev-index idx)))
                          vs)))
            (let [(point (board-ref board idx))]
              (if (member point (peers point))
                  '()
                  (helper board (prev-index idx)))))))
  (helper board '(8 8)))

(define (empty-board)
  (build-list 9 (lambda (_) (build-list 9 (lambda (_) 0)))))

(define example
  '((4 0 0 0 0 0 8 0 5)
    (0 3 0 0 0 0 0 0 0)
    (0 0 0 7 0 0 0 0 0)
    (0 2 0 0 0 0 0 6 0)
    (0 0 0 0 8 0 4 0 0)
    (0 0 0 0 1 0 0 0 0)
    (0 0 0 6 0 3 0 7 0)
    (5 0 0 2 0 0 0 0 0)
    (1 0 4 0 0 0 0 0 0)))
