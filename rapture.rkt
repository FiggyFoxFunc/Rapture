#lang racket

(struct turing-machine
  (Q G b S q0 F d)
  #:transparent)

(struct head-state (pos qn) #:transparent)

(define (apply-transition tape head turing-machine)
  (let* ([pos (head-state-pos head)]
         [cell (vector-ref tape pos)]
         [state (head-state-qn head)]
         [t-result (assoc
                    (list cell state)
                    (turing-machine-d turing-machine))]
         [transition 
          (if t-result (second t-result) #f)])
    (if transition
        (begin
          (vector-set! tape pos (first transition))
          (head-state
           (+ pos (cond [(eq? 'R (second transition)) 1]
                        [(eq? 'L (second transition)) -1]))
           (third transition)))
        head)))
  

(define (run-turing-machine tape head turing-machine)
  (cond
    [(member (head-state-qn head) (turing-machine-F turing-machine))
     (display (format "~a, ~v" tape head))]
    [else
     (displayln (format "~a, ~v" tape head))
     ;(print head)
     ;(displayln "")
     (run-turing-machine
      tape
      (apply-transition tape head turing-machine)
      turing-machine)]))
       

; Example 3-state, 2-symbol busy beaver
(define tape (make-vector 6 0))
(define head (head-state 3 'A))
  
(define busy-beaver-3
  (turing-machine
   '(A B C HALT)
   '(0 1)
   0
   '(1)
   'A
   '(HALT)
   '(((0 A) (1 R B))
     ((1 A) (1 L C))
     ((0 B) (1 L A))
     ((1 B) (1 R B))
     ((0 C) (1 L B))
     ((1 C) (1 R HALT)))))

(run-turing-machine tape head busy-beaver-3)