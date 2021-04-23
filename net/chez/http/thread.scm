(library
    (thread)
    (export fork yield thread-exit)
    (import (rnrs))

   (define *queue* '())
 
   (define (empty-queue?)
     (null? *queue*))
 
   (define (enqueue x)
     (set! *queue* (append *queue* (list x))))
 
   (define (dequeue)
     (let ((x (car *queue*)))
       (set! *queue* (cdr *queue*))
       x))
 
   ;;; This starts a new thread running (proc).
 
   (define (fork proc)
     (call/cc
      (lambda (k)
        (enqueue k)
        (if (procedure? proc) (proc) 'null))))
 
   ;;; This yields the processor to another thread, if there is one.
 
   (define (yield)
     (call/cc
      (lambda (k)
        (enqueue k)
        (let ([p (dequeue)])
            (if (procedure? p) (p) 'null)
        ))))
 
   ;;; This terminates the current thread, or the entire program
   ;;; if there are no other threads left.
 
   (define (thread-exit)
     (if (empty-queue?)
         (exit)
         (let ([p (dequeue)])
            (if (procedure? p) (p) 'null)
        )))
)