(library (back-propagation-neural-network)
	 (export 
	   make-data-node 
	   data-node-attributes 
	   data-node-attributes-set! 
	   data-node-type
	   data-node-type-set!

	   make-a-counts
	   make-a-neuron
	   make-a-neuron-network
	   train-nodes-set!
	   train
	   test

	   type-set! 
	   forward-input-value 
	   forward-input-value-set! 
	   forward-output-value 
	   backward-input-value 
	   backward-input-value-set! 
	   backward-output-value 
	   )
	 (import (rnrs) (only (scheme) random))

	 ;(define-record-type attributes (fields (mutable a) (mutable b) (mutable c) (mutable d)))
	 (define-record-type datanode 
	   (fields (mutable attributes) 
		   (mutable type)))

	 (define make-data-node make-datanode)
	 (define data-node-attributes datanode-attributes)
	 (define data-node-attributes-set! datanode-attributes-set!)
	 (define data-node-type datanode-type)
	 (define data-node-type-set! datanode-type-set!)

	 (define-record-type neuron 
	   (fields (mutable type)
		   (mutable forward-input-value)
		   (mutable forward-output-value)
		   (mutable backward-input-value)
		   (mutable backward-output-value)))

	 (define make-a-neuron make-neuron)
	 (define type-set! neuron-type-set!)
	 (define forward-input-value neuron-forward-input-value)
	 (define forward-input-value-set! neuron-forward-input-value-set!)
	 (define forward-output-value neuron-forward-output-value)

	 (define backward-input-value neuron-backward-input-value)
	 (define backward-input-value-set! neuron-backward-input-value-set!)
	 (define backward-output-value neuron-backward-output-value)

	 (define (forward-sigmoid a-neuron in)
	   (case (neuron-type a-neuron)
	     ('node-input in)
	     ('node-hidden (tanh in))
	     ('node-output (tanh in))
	     (else 0.0)))

	 (define tanh 
	   (lambda (in)
	     (/ (- (exp in) (exp (- in))) (+ (exp in) (exp (- in))))))

	 (define (pow x n)
	   (define (iter i product)
	     (if (< i n)
		 (iter (+ i 1) (* product x))
		 product))
	   (iter 0 1))

	 (define (tanh-derivative a-neuron in)
	   (* (- 1 (pow (neuron-forward-output-value a-neuron) 2)) in))

	 (define (back-propagative a-neuron in)
	   (case (neuron-type a-neuron)
	     ('node-input in)
	     ('node-hidden (tanh-derivative in))
	     ('node-output (tanh-derivative in))
	     (else 0.0)))

	 (define-record-type counts 
	   (fields (mutable input-count)
		   (mutable hidden-count)
		   (mutable output-count)))

	 (define make-a-counts make-counts)

	 (define-record-type neuron-network 
	   (fields (mutable input-nodes)		      
		   (mutable hidden-nodes)
		   (mutable output-nodes)
		   (mutable train-nodes)
		   (mutable input-hidden-weight)
		   (mutable hidden-output-weight)))

	 (define make-a-neuron-network
	   (lambda (a-counts)
	     (let ([in-nodes (make-list (counts-input-count a-counts) (make-a-neuron 'node-input 0 0 0 0))]
		   [hid-nodes (make-list (counts-hidden-count a-counts) (make-a-neuron 'node-hidden 0 0 0 0))]
		   [out-nodes (make-list (counts-output-count a-counts) (make-a-neuron  'node-output 0 0 0 0))])
	       (make-neuron-network in-nodes hid-nodes out-nodes '() '() '()))))

	 (define train-nodes-set! neuron-network-train-nodes-set!)

	 (define (trace title obj)
	   (display title)
	   (display " : ")
	   (display obj)
	   (newline))

	 (define-syntax maplist
	   (syntax-rules 
	     ()
	     [(_ fn lst)
	      (cond 
		((null? lst) '())
		(else
		  (append (list (fn (car lst))) (maplist fn (cdr lst)))))]))

	 (define train
	   (lambda (a-neuron-network a-counts eta n)
	     (reset a-neuron-network a-counts)
	     (do ([i 0 (+ i 1)])
	       ((= i n) 'finished)
	       (begin
		 (for-each 
		   (lambda (x)
		     (forward a-neuron-network (datanode-attributes x))
		     (backward a-neuron-network (datanode-type x))
		     (update-weights a-neuron-network eta))
		   (neuron-network-train-nodes a-neuron-network))
		 (display "n= ")
		 (display i)
		 (newline)))))

	 (define test
	   (lambda (a-neuron-network test-node)
	     (forward a-neuron-network (datanode-attributes test-node))
	     (let ([result 2]
		   [type 0]
		   [i 0])
	       (set! i 0)
	       (call/cc 
		 (lambda (cc)
		   (for-each 
		     (lambda (x)
		       (if (< (- 1 (neuron-forward-output-value x)) result)
			   (begin
			     (set! result (- 1 (neuron-forward-output-value x)))
			     (set! type i)
			     (cc type)))
		       (set! i (+ i 1)))
		     (neuron-network-output-nodes a-neuron-network))))
	       type)))
	 (define-syntax list-set!
	   (lambda (x)
	     (syntax-case x ()
			  [(_ lst i value)
			   #'(let ([vec (list->vector lst)])
			       (vector-set! vec i value)
			       (set! lst (vector->list vec)))])))

	 (define-syntax two-dimension-list-set!
	   (lambda (x)
	     (syntax-case x ()
			  [(_ lst i j value)
			   #'(let ([row (list-ref lst i)])
			       (list-set! row j value)
			       (list-set! lst i row))])))

	 (define two-dimension-list-get
	   (lambda (lst i j)
	     (list-ref (list-ref lst i) j)))

	 (define update-weights
	   (lambda (a-neuron-network eta)
	     (let ([input-hidden-weights (neuron-network-input-hidden-weight a-neuron-network)]
	           [hidden-output-weights (neuron-network-hidden-output-weight a-neuron-network)]
		   [input-nodes (neuron-network-input-nodes a-neuron-network)]
		   [output-nodes (neuron-network-output-nodes a-neuron-network)]
		   [hidden-nodes (neuron-network-hidden-nodes a-neuron-network)])
	       (let ([i 0]
		     [j 0]
		     [value 0])
		 (neuron-network-input-hidden-weight-set! 
		   a-neuron-network 
		   (map (lambda (x)
			  (set! j 0)
			  (map (lambda (y) 
				 (set! value (- (list-ref (list-ref input-hidden-weights i) j)
						(* eta 										   (neuron-forward-output-value (list-ref input-nodes i))
		(neuron-backward-output-value (list-ref hidden-nodes j)))))
				 (set! j (+ j 1))
				 value) hidden-nodes)
			  (set! i (+ i 1))
			  x) input-nodes))
		 (set! i 0)
		 (set! j 0)
		 (set! value 0)
		 (neuron-network-hidden-output-weight-set! 
		   a-neuron-network 
		   (map (lambda (x)
			  (set! j 0)
			  (map (lambda (y) 
				 (set! value (- (list-ref (list-ref hidden-output-weights i) j)
						(* eta 
						   (neuron-forward-output-value (list-ref hidden-nodes i))
						   (neuron-backward-output-value (list-ref output-nodes j)))))
				 (set! j (+ j 1))
				 value) output-nodes)
			  (set! i (+ i 1))
			  x) hidden-nodes)))
	       (trace 'update-weights a-neuron-network)
	       
	       )))

	 (define forward
	   (lambda (a-neuron-network a-list)
	     ;;;;;
	     (let ([i 0])
	       (trace 'input-nodes (neuron-network-input-nodes a-neuron-network))
	       (neuron-network-input-nodes-set!
		 a-neuron-network
		 (map
		   (lambda (x)
		     (neuron-forward-input-value-set! x (list-ref a-list i))
		     (set! i (+ i 1))
		     x)
		   (neuron-network-input-nodes a-neuron-network))))
	     ;;
	     (let ([j 0]
		   [temp 0])
	       (set! temp 0)
	       (neuron-network-hidden-nodes-set!
		 a-neuron-network
		 (map
		   (lambda (x) 
		     (set! temp (apply + (map * 
					      (list-ref (apply map list (neuron-network-input-hidden-weight a-neuron-network)) j)
					      (map neuron-forward-output-value (neuron-network-input-nodes a-neuron-network)))))
		     (neuron-forward-input-value-set! x temp)
		     (set! j (+ j 1))
		     x)
		   (neuron-network-hidden-nodes a-neuron-network))))
	     ;;
	     (let ([j 0]
		   [temp 0])
	       (set! temp 0)
	       (neuron-network-output-nodes-set! 
		 a-neuron-network 
		 (map 
		   (lambda (x) 
		     (set! temp (apply + (map *
					      (list-ref (apply map list (neuron-network-hidden-output-weight a-neuron-network)) j)
					      (map neuron-forward-output-value (neuron-network-hidden-nodes a-neuron-network)))))
		     (neuron-forward-input-value-set! x temp)
		     (set! j (+ j 1))
		     x)
		   (neuron-network-output-nodes a-neuron-network))))))

	 (define backward
	   (lambda (a-neuron-network type)
	     ;;
	     (let ([j 0]
		   [result -1])
	       (neuron-network-output-nodes-set!
		 a-neuron-network
		 (map 
		   (lambda (x) 
		     (if (= j type)
			 (set! result 1)
			 (set! result -1))
		     (neuron-backward-input-value-set! x (- (neuron-backward-output-value x) result))
		     (set! j (+ j 1))
		     x)
		   (neuron-network-output-nodes a-neuron-network))))
	     ;;
	     (let ([j 0]
		   [temp 0])
	       (set! temp 0)
	       (neuron-network-hidden-nodes-set!
		 a-neuron-network
		 (map
		   (lambda (x) 
		     (set! temp (apply + (map * 
					      (list-ref (neuron-network-hidden-output-weight a-neuron-network) j)
					      (map neuron-backward-output-value (neuron-network-output-nodes a-neuron-network)))))
		     (neuron-backward-input-value-set! x temp)
		     (set! j (+ j 1))
		     x)
		   (neuron-network-hidden-nodes a-neuron-network))))))

	 (define reset
	   (lambda (a-neuron-network a-counts)
	     (neuron-network-input-nodes-set! a-neuron-network (make-list (counts-input-count a-counts) (make-a-neuron 'node-input 0 0 0 0)))
	     (neuron-network-hidden-nodes-set! a-neuron-network (make-list (counts-hidden-count a-counts) (make-a-neuron 'node-hidden 0 0 0 0)))
	     (neuron-network-output-nodes-set! a-neuron-network (make-list (counts-output-count a-counts) (make-a-neuron 'node-output 0 0 0 0)))
	     (neuron-network-input-hidden-weight-set! a-neuron-network (random-map (counts-input-count a-counts) (counts-hidden-count a-counts)))
	     (neuron-network-hidden-output-weight-set! a-neuron-network (random-map (counts-hidden-count a-counts) (counts-output-count a-counts)))
	     ))

	 (define make-list
	   (case-lambda
	     [(n) (make-list n #f)]
	     [(n x)
	      (do ([n n (- n 1)]
		   [ls '() (cons x ls)])
		((zero? n) ls))]))

	 (define random-map
	   (lambda (count1 count2)
	     (let ([weight (make-list count1 (make-list count2 0))])
	       (map (lambda (x) (map (lambda (y) (random 1.0)) x)) weight))))


	 )
