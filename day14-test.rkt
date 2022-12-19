#lang racket
(require rackunit "day14.rkt" "shared.rkt")

(define test-scans '(((498 . 4) (498 . 6) (496 . 6)) 
                     ((503 . 4) (502 . 4) (502 . 9) (494 . 9))))

(define (pr r) (format "~s" r))
(define (test-cave size)
  (if (eq? size 'small)
    (make-cave test-scans 
               #:vp (viewport '(0 . 0) 10 7)
               #:drop-sand-from '(0 . 0))
    (make-cave test-scans 
               #:vp (viewport '(494 . 0) 10 10) 
               #:drop-sand-from '(500 . 0))))


(test-case "draw-line!"
           (check-equal? (pr (draw-line! (test-cave 'small) rock '(0 . 0) '(0 . 5))) #<<eof

▩         
▩         
▩         
▩         
▩         
▩         
          

eof
)
           (check-equal? (pr (draw-line! (test-cave 'small) rock '(1 . 5) '(1 . 0))) #<<eof

 ▩        
 ▩        
 ▩        
 ▩        
 ▩        
 ▩        
          

eof
)
           (check-equal? (pr (draw-line! (test-cave 'small) rock '(1 . 1) '(5 . 1))) #<<eof

          
 ▩▩▩▩▩    
          
          
          
          
          

eof
))

(test-case "animate-sand"
           (check-equal? (pr (animate-sand (test-cave 'large))) #<<eof

          
          
      .   
     ...  
    ▩...▩▩
   .▩...▩ 
  ▩▩▩...▩ 
    ....▩ 
 . .....▩ 
▩▩▩▩▩▩▩▩▩ 

eof
))
