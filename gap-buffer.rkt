#lang racket
;; Exploring approaches to managing a list of annotations parallel to
;; a gap buffer.

(struct annotations (front back total gap) #:prefab)

(define (annotations-seek anns pos0)
  (match-define (annotations old-front old-back total old-gap) anns)
  (define pos (max 0 (min total pos0)))
  (cond
    [(< pos old-gap) ;; move from front to back
     (let loop ((F old-front) (B old-back))
       (match F
         ['() (annotations F B total pos)]
         [(cons (cons k v) more-F)
          (if (>= k pos)
              (loop more-F (cons (cons (- k total) v) B))
              (annotations F B total pos))]))]
    [(= pos old-gap) anns] ;; no-op
    [(> pos old-gap) ;; move from back to front
     (let loop ((F old-front) (B old-back))
       (match B
         ['() (annotations F B total pos)]
         [(cons (cons k v) more-B)
          (if (< (+ k total) pos)
              (loop (cons (cons (+ k total) v) F) more-B)
              (annotations F B total pos))]))]))

(let* ((max-pos (* 8 1024 1024))
       (A (annotations (reverse
                        (for/list [(i (in-range max-pos))]
                          (list i 'annotation)))
                       '()
                       max-pos
                       max-pos))
       (A (annotations-seek A 2))
       (A (annotations-seek A max-pos))
       (_ (collect-garbage))
       (A (time (annotations-seek A 0))))
  (void A))
