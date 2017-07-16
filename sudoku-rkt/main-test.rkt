#lang racket

(require rackunit
         rackunit/text-ui
         "main.rkt")

(define (test-rand-board)
  (test-case "test rand-board"
    (for-each
     (lambda (i)
       (check = 9 (count (curry = i) (flatten (rand-board)))))
     (range 1 10))))

(define (test-columns)
  (test-case "test columns"
    (check = 9 (length (columns (rand-board))))
    (for-each
     (lambda (row) (check = 9 (length row)))
     (columns (rand-board)))
    (for-each
     (lambda (i)
       (check = 9 (count (curry = i) (flatten (columns (rand-board))))))
     (range 1 10))))

(define (test-boxes)
  (test-case "test boxes"
    (check = 9 (length (boxes (rand-board))))))

(define (test-groups)
  (test-case "test groups"
    (check = 27 (length (groups (rand-board))))))

(define (test-peers)
  (test-case "test peers"
    (for-each
     (lambda (point) (check = 20 (length (peers point))))
     (foldr append '() board-indexes))))

(define main-tests
  (test-suite
   "tests for main.rkt"
   (test-rand-board)
   (test-columns)
   (test-boxes)
   (test-groups)
   (test-peers)))

(run-tests main-tests)

