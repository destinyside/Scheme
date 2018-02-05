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

	 (define-record-type neuron (fields (mutable type)
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

	 (define-record-type counts (fields (mutable input-count)
					    (mutable hidden-count)
					    (mutable output-count)))

	 (define make-a-counts make-counts)

	 (define-record-type neuron-network (fields (mutable input-nodes)		      
						    (mutable hidden-nodes)
						    (mutable output-nodes)
						    (mutable train-nodes)
						    (mutable input-hidden-weight)
						    (mutable hidden-output-weight)))

	 (define make-a-neuron-network
	   (lambda (a-counts)
	     (let ([in-nodes (make-list (counts-input-count a-counts) (make-a-neuron 0 0 0 0 0))]
		   [hid-nodes (make-list (counts-hidden-count a-counts) (make-a-neuron 0 0 0 0 0))]
		   [out-nodes (make-list (counts-output-count a-counts) (make-a-neuron  0 0 0 0 0))])
	       (make-neuron-network in-nodes hid-nodes out-nodes '() '() '()))))

	 (define train-nodes-set! neuron-network-train-nodes-set!)

	 (define train
	   (lambda (a-neuron-network a-counts eta n)
	     (reset a-neuron-network a-counts)
	     (do ([i 0 (+ i 1)])
	       ((= i n) 'finished)
	       (begin
		 (map (lambda (x)
			(forward a-neuron-network (datanode-attributes x))
			(backward a-neuron-network (datanode-type x))
			(update-weights a-neuron-network eta))
		      (neuron-network-train-nodes a-neuron-network))
		 (display "n= ")
		 (display i)
		 (newline)))))

	 (define test
	   (lambda (a-neuron-network test-nodes)
	     (forward a-neuron-network (datanode-attributes test-nodes))
	     (let ([result 2]
		   [type 0]
		   [i 0])
	       (set! i 0)
	       (map (lambda (x)
		      (if (< (- 1 (neuron-forward-output-value x)) result)
			(begin
			  (set! result (- 1 (neuron-forward-output-value x)))
			  (set! type i)))
		      (set! i (+ i 1)))
		    (neuron-network-output-nodes a-neuron-network))
	       type)))

	 (define update-weights
	   (lambda (a-neuron-network eta)
	     (neuron-network-input-hidden-weight-set! 
	       a-neuron-network
	       (map (lambda (i j)
		      (map (lambda (x)
			     (map (lambda (y) (- y (* eta 
						      (neuron-forward-output-value i) 
						      (neuron-backward-output-value j)))) x)) 
			   (neuron-network-input-hidden-weight a-neuron-network)))
		    (neuron-network-input-nodes a-neuron-network)
		    (neuron-network-hidden-nodes a-neuron-network)))

	     (neuron-network-hidden-output-weight-set! 
	       a-neuron-network
	       (map (lambda (i j)
		      (map (lambda (x)
			     (map (lambda (y) (- y (* eta 
						      (neuron-forward-output-value i) 
						      (neuron-backward-output-value j)))) x)) 
			   (neuron-network-input-hidden-weight a-neuron-network)))
		    (neuron-network-hidden-nodes a-neuron-network)
		    (neuron-network-output-nodes a-neuron-network)))))

	 (define forward
	   (lambda (a-neuron-network a-list)
	     ;;
	     (let ([i 0])
	       (map (lambda (x)
		      (neuron-forward-input-value-set! x (list-ref a-list i))
		      (set! i (+ i 1)))
		    (neuron-network-input-nodes a-neuron-network)))
	     ;;
	     (let ([j 0]
		   [temp 0])
	       (set! temp 0)
	       (map (lambda (x) 
		      (set! temp (apply + (map (lambda (x y) (* x y)) 
					       (list-ref (neuron-network-input-hidden-weight a-neuron-network) j))
					(map (lambda (x) (neuron-forward-output-value x)) (neuron-network-input-nodes a-neuron-network))))
		      (neuron-forward-input-value-set! x temp)
		      (set! j (+ j 1)))
		    (neuron-network-hidden-nodes a-neuron-network)))
	     ;;
	     (let ([j 0]
		   [temp 0])
	       (set! temp 0)
	       (map (lambda (x) 
		      (set! temp (apply + (map (lambda (x y) (* x y)) 
					       (list-ref (neuron-network-hidden-output-weight a-neuron-network) j))
					(map (lambda (x) (neuron-forward-output-value x)) (neuron-network-hidden-nodes a-neuron-network))))
		      (neuron-forward-input-value-set! x temp)
		      (set! j (+ j 1)))
		    (neuron-network-output-nodes a-neuron-network)))))

	 (define backward
	   (lambda (a-neuron-network type)
	     ;;
	     (let ([j 0]
		   [result -1])
	       (map (lambda (x) 
		      (if (= j type)
			(set! result 1)
			(set! result -1))
		      (neuron-backward-input-value-set! x (- (neuron-backward-output-value x) result))
		      (set! j (+ j 1)))
		    (neuron-network-output-nodes a-neuron-network)))
	     ;;
	     (let ([j 0]
		   [temp 0])
	       (set! temp 0)
	       (map (lambda (x) 
		      (set! temp (apply + (map (lambda (x y) (* x y)) 
					       (list-ref (neuron-network-hidden-output-weight a-neuron-network) j))
					(map (lambda (x) (neuron-backward-output-value x)) (neuron-network-output-nodes a-neuron-network))))
		      (neuron-backward-input-value-set! x temp)
		      (set! j (+ j 1)))
		    (neuron-network-hidden-nodes a-neuron-network)))))

	   (define reset
	     (lambda (a-neuron-network a-counts)
	       (neuron-network-input-nodes-set! a-neuron-network (make-list (counts-input-count a-counts) (make-a-neuron 0 0 0 0 0)))
	       (neuron-network-hidden-nodes-set! a-neuron-network (make-list (counts-hidden-count a-counts) (make-a-neuron 0 0 0 0 0)))
	       (neuron-network-output-nodes-set! a-neuron-network (make-list (counts-output-count a-counts) (make-a-neuron 0 0 0 0 0)))
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

#include<iostream>
#include<stdlib.h>
#include<vector>
#include<fstream>
#include<sstream>
#include "AnnClassifier.h"
#include "DataNode.h"
using namespace std;
enum Iris{
	Iris_versicolor,
	Iris_setosa,
	Iris_virginica
};
vector<DataNode> initTrain();
vector<DataNode> initTest();

int main() {
	//²Î¿¼²©¿Í£ºhttp://blog.csdn.net/zhongkejingwang/article/details/44514073
	
	int typeCount = 3;
	double eta = 0.02f;
	int nIter = 1000;
	vector<DataNode> trainList = initTrain();
	vector<DataNode> testList = initTest();
	AnnClassifier* annClassifier = new AnnClassifier(
		trainList.at(0).getAttrList().size(), 
		trainList.at(0).getAttrList().size() + 8, 
		typeCount
	);
	annClassifier->setTrainNodes(trainList);
	annClassifier->train(eta, nIter);
	double rightCount = 0;
	for (int i = 0; i < testList.size(); i++)
	{
		DataNode test = testList.at(i);
		int type = annClassifier->test(test);
		vector<double> attribs = test.getAttrList();
		for (int n = 0; n < attribs.size(); n++)
		{
			cout << attribs.at(n) << ",";
		}
		if (type == testList.at(i).getType()) {
			rightCount++;
		}
		switch (type) {
		case 0: {
			cout << "Iris_versicolor" << endl;
			break;
		}

		case 1: {
			cout << "Iris_setosa" << endl;
			break;
		}
		case 2:
		{
			cout << "Iris_virginica" << endl;
			break;
		}
		default: {
			break;
		}
		}
	}
	cout << "[" << rightCount / testList.size() << "]" << endl;
	delete annClassifier;
	
	system("pause");
}

vector<DataNode> initTrain() {
	vector<DataNode> tmp;
	
	tmp.push_back(DataNode(6.1, 2.8, 4.7, 1.2, Iris_versicolor));
	tmp.push_back(DataNode(5.5, 2.4, 3.8, 1.1, Iris_versicolor));
	tmp.push_back(DataNode(5.1, 3.5, 1.4, 0.3, Iris_setosa));
	tmp.push_back(DataNode(6.7, 3.1, 5.6, 2.4, Iris_virginica));
	tmp.push_back(DataNode(5.4, 3.4, 1.7, 0.2, Iris_setosa));
	tmp.push_back(DataNode(6.9, 3.1, 5.1, 2.3, Iris_virginica));
	tmp.push_back(DataNode(6.7, 3.0, 5.2, 2.3, Iris_virginica));
	tmp.push_back(DataNode(5.7, 3.0, 4.2, 1.2, Iris_versicolor));
	tmp.push_back(DataNode(6.3, 2.5, 4.9, 1.5, Iris_versicolor));
	tmp.push_back(DataNode(6.3, 2.3, 4.4, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(5.6, 2.5, 3.9, 1.1, Iris_versicolor));
	tmp.push_back(DataNode(6.9, 3.2, 5.7, 2.3, Iris_virginica));
	tmp.push_back(DataNode(6.1, 2.8, 4.0, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(7.2, 3.6, 6.1, 2.5, Iris_virginica));
	tmp.push_back(DataNode(6.4, 2.8, 5.6, 2.1, Iris_virginica));
	tmp.push_back(DataNode(6.1, 2.9, 4.7, 1.4, Iris_versicolor));
	tmp.push_back(DataNode(6.1, 3.0, 4.9, 1.8, Iris_virginica));
	tmp.push_back(DataNode(6.7, 3.3, 5.7, 2.5, Iris_virginica));
	tmp.push_back(DataNode(5.4, 3.7, 1.5, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.7, 4.4, 1.5, 0.4, Iris_setosa));
	tmp.push_back(DataNode(6.3, 3.3, 6.0, 2.5, Iris_virginica));
	tmp.push_back(DataNode(5.0, 3.6, 1.4, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.1, 3.4, 1.5, 0.2, Iris_setosa));
	tmp.push_back(DataNode(4.6, 3.6, 1.0, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.5, 2.6, 4.4, 1.2, Iris_versicolor));
	tmp.push_back(DataNode(4.9, 3.0, 1.4, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.9, 3.0, 4.2, 1.5, Iris_versicolor));
	tmp.push_back(DataNode(6.7, 3.0, 5.0, 1.7, Iris_versicolor));
	tmp.push_back(DataNode(5.4, 3.9, 1.7, 0.4, Iris_setosa));
	tmp.push_back(DataNode(6.4, 3.2, 4.5, 1.5, Iris_versicolor));
	tmp.push_back(DataNode(7.0, 3.2, 4.7, 1.4, Iris_versicolor));
	tmp.push_back(DataNode(5.7, 2.8, 4.1, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(6.2, 2.2, 4.5, 1.5, Iris_versicolor));
	tmp.push_back(DataNode(6.2, 2.9, 4.3, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(4.8, 3.0, 1.4, 0.3, Iris_setosa));
	tmp.push_back(DataNode(5.1, 2.5, 3.0, 1.1, Iris_versicolor));
	tmp.push_back(DataNode(6.1, 2.6, 5.6, 1.4, Iris_virginica));
	tmp.push_back(DataNode(5.5, 4.2, 1.4, 0.2, Iris_setosa));
	tmp.push_back(DataNode(4.9, 2.5, 4.5, 1.7, Iris_virginica));
	tmp.push_back(DataNode(5.5, 2.3, 4.0, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(5.0, 2.3, 3.3, 1.0, Iris_versicolor));
	tmp.push_back(DataNode(7.3, 2.9, 6.3, 1.8, Iris_virginica));
	tmp.push_back(DataNode(7.2, 3.0, 5.8, 1.6, Iris_virginica));
	tmp.push_back(DataNode(5.4, 3.0, 4.5, 1.5, Iris_versicolor));
	tmp.push_back(DataNode(6.4, 3.1, 5.5, 1.8, Iris_virginica));
	tmp.push_back(DataNode(5.7, 2.5, 5.0, 2.0, Iris_virginica));
	tmp.push_back(DataNode(5.2, 3.5, 1.5, 0.2, Iris_setosa));
	tmp.push_back(DataNode(6.3, 2.9, 5.6, 1.8, Iris_virginica));
	tmp.push_back(DataNode(6.3, 3.3, 4.7, 1.6, Iris_versicolor));
	tmp.push_back(DataNode(4.4, 3.2, 1.3, 0.2, Iris_setosa));
	tmp.push_back(DataNode(6.3, 2.8, 5.1, 1.5, Iris_virginica));
	tmp.push_back(DataNode(7.2, 3.2, 6.0, 1.8, Iris_virginica));
	tmp.push_back(DataNode(6.5, 2.8, 4.6, 1.5, Iris_versicolor));
	tmp.push_back(DataNode(4.6, 3.2, 1.4, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.0, 2.0, 3.5, 1.0, Iris_versicolor));
	tmp.push_back(DataNode(7.7, 3.8, 6.7, 2.2, Iris_virginica));
	tmp.push_back(DataNode(5.7, 2.6, 3.5, 1.0, Iris_versicolor));
	tmp.push_back(DataNode(6.7, 3.1, 4.4, 1.4, Iris_versicolor));
	tmp.push_back(DataNode(5.6, 3.0, 4.1, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(7.6, 3.0, 6.6, 2.1, Iris_virginica));
	tmp.push_back(DataNode(5.1, 3.8, 1.6, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.5, 3.5, 1.3, 0.2, Iris_setosa));
	tmp.push_back(DataNode(4.4, 2.9, 1.4, 0.2, Iris_setosa));
	tmp.push_back(DataNode(6.2, 2.8, 4.8, 1.8, Iris_virginica));
	tmp.push_back(DataNode(5.9, 3.0, 5.1, 1.8, Iris_virginica));
	tmp.push_back(DataNode(6.5, 3.2, 5.1, 2.0, Iris_virginica));
	tmp.push_back(DataNode(6.3, 2.7, 4.9, 1.8, Iris_virginica));
	tmp.push_back(DataNode(4.9, 3.1, 1.5, 0.1, Iris_setosa));
	tmp.push_back(DataNode(6.0, 3.4, 4.5, 1.6, Iris_versicolor));
	tmp.push_back(DataNode(4.9, 2.4, 3.3, 1.0, Iris_versicolor));
	tmp.push_back(DataNode(5.4, 3.4, 1.5, 0.4, Iris_setosa));
	tmp.push_back(DataNode(6.5, 3.0, 5.8, 2.2, Iris_virginica));
	tmp.push_back(DataNode(5.7, 2.8, 4.5, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(7.1, 3.0, 5.9, 2.1, Iris_virginica));
	tmp.push_back(DataNode(5.8, 2.7, 5.1, 1.9, Iris_virginica));
	tmp.push_back(DataNode(5.1, 3.8, 1.5, 0.3, Iris_setosa));
	tmp.push_back(DataNode(6.5, 3.0, 5.2, 2.0, Iris_virginica));
	tmp.push_back(DataNode(5.8, 2.7, 4.1, 1.0, Iris_versicolor));
	tmp.push_back(DataNode(5.6, 2.8, 4.9, 2.0, Iris_virginica));
	tmp.push_back(DataNode(6.9, 3.1, 5.4, 2.1, Iris_virginica));
	tmp.push_back(DataNode(5.0, 3.0, 1.6, 0.2, Iris_setosa));
	tmp.push_back(DataNode(6.0, 2.9, 4.5, 1.5, Iris_versicolor));
	tmp.push_back(DataNode(5.0, 3.4, 1.5, 0.2, Iris_setosa));
	tmp.push_back(DataNode(6.7, 3.1, 4.7, 1.5, Iris_versicolor));
	tmp.push_back(DataNode(5.8, 2.8, 5.1, 2.4, Iris_virginica));
	tmp.push_back(DataNode(4.8, 3.4, 1.9, 0.2, Iris_setosa));
	tmp.push_back(DataNode(7.7, 3.0, 6.1, 2.3, Iris_virginica));
	tmp.push_back(DataNode(6.8, 3.0, 5.5, 2.1, Iris_virginica));
	tmp.push_back(DataNode(6.0, 3.0, 4.8, 1.8, Iris_virginica));
	tmp.push_back(DataNode(7.4, 2.8, 6.1, 1.9, Iris_virginica));
	tmp.push_back(DataNode(4.9, 3.1, 1.5, 0.1, Iris_setosa));
	tmp.push_back(DataNode(5.2, 3.4, 1.4, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.9, 3.2, 4.8, 1.8, Iris_versicolor));
	tmp.push_back(DataNode(6.7, 2.5, 5.8, 1.8, Iris_virginica));
	tmp.push_back(DataNode(7.9, 3.8, 6.4, 2.0, Iris_virginica));
	tmp.push_back(DataNode(4.7, 3.2, 1.3, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.8, 4.0, 1.2, 0.2, Iris_setosa));
	tmp.push_back(DataNode(6.8, 3.2, 5.9, 2.3, Iris_virginica));
	tmp.push_back(DataNode(6.0, 2.2, 5.0, 1.5, Iris_virginica));
	tmp.push_back(DataNode(5.8, 2.6, 4.0, 1.2, Iris_versicolor));
	tmp.push_back(DataNode(5.1, 3.3, 1.7, 0.5, Iris_setosa));
	tmp.push_back(DataNode(6.9, 3.1, 4.9, 1.5, Iris_versicolor));
	tmp.push_back(DataNode(5.8, 2.7, 5.1, 1.9, Iris_virginica));
	tmp.push_back(DataNode(5.6, 2.9, 3.6, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(4.9, 3.1, 1.5, 0.1, Iris_setosa));
	tmp.push_back(DataNode(6.4, 2.7, 5.3, 1.9, Iris_virginica));
	tmp.push_back(DataNode(5.2, 4.1, 1.5, 0.1, Iris_setosa));
	tmp.push_back(DataNode(5.0, 3.5, 1.6, 0.6, Iris_setosa));
	tmp.push_back(DataNode(6.3, 3.4, 5.6, 2.4, Iris_virginica));
	tmp.push_back(DataNode(6.6, 3.0, 4.4, 1.4, Iris_versicolor));
	tmp.push_back(DataNode(6.4, 2.9, 4.3, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(5.1, 3.5, 1.4, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.1, 3.8, 1.9, 0.4, Iris_setosa));
	tmp.push_back(DataNode(6.3, 2.5, 5.0, 1.9, Iris_virginica));
	tmp.push_back(DataNode(6.8, 2.8, 4.8, 1.4, Iris_versicolor));
	tmp.push_back(DataNode(6.2, 3.4, 5.4, 2.3, Iris_virginica));
	tmp.push_back(DataNode(4.7, 3.2, 1.6, 0.2, Iris_setosa));
	tmp.push_back(DataNode(6.4, 2.8, 5.6, 2.2, Iris_virginica));
	tmp.push_back(DataNode(4.6, 3.1, 1.5, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.7, 3.8, 1.7, 0.3, Iris_setosa));
	tmp.push_back(DataNode(6.5, 3.0, 5.5, 1.8, Iris_virginica));
	tmp.push_back(DataNode(5.3, 3.7, 1.5, 0.2, Iris_setosa));
	tmp.push_back(DataNode(4.6, 3.4, 1.4, 0.3, Iris_setosa));
	tmp.push_back(DataNode(4.8, 3.0, 1.4, 0.1, Iris_setosa));
	return tmp;
}

vector<DataNode> initTest()
{
	vector<DataNode> tmp;
	tmp.push_back(DataNode(6.7, 3.3, 5.7, 2.1, Iris_virginica));
	tmp.push_back(DataNode(6.4, 3.2, 5.3, 2.3, Iris_virginica));
	tmp.push_back(DataNode(4.4, 3.0, 1.3, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.0, 3.5, 1.3, 0.3, Iris_setosa));
	tmp.push_back(DataNode(5.0, 3.4, 1.6, 0.4, Iris_setosa));
	tmp.push_back(DataNode(5.4, 3.9, 1.3, 0.4, Iris_setosa));
	tmp.push_back(DataNode(4.8, 3.1, 1.6, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.8, 2.7, 3.9, 1.2, Iris_versicolor));
	tmp.push_back(DataNode(7.7, 2.8, 6.7, 2.0, Iris_virginica));
	tmp.push_back(DataNode(4.3, 3.0, 1.1, 0.1, Iris_setosa));
	tmp.push_back(DataNode(5.0, 3.3, 1.4, 0.2, Iris_setosa));
	tmp.push_back(DataNode(6.6, 2.9, 4.6, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(7.7, 2.6, 6.9, 2.3, Iris_virginica));
	tmp.push_back(DataNode(5.2, 2.7, 3.9, 1.4, Iris_versicolor));
	tmp.push_back(DataNode(4.5, 2.3, 1.3, 0.3, Iris_setosa));
	tmp.push_back(DataNode(5.7, 2.9, 4.2, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(5.5, 2.5, 4.0, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(5.1, 3.7, 1.5, 0.4, Iris_setosa));
	tmp.push_back(DataNode(6.1, 3.0, 4.6, 1.4, Iris_versicolor));
	tmp.push_back(DataNode(5.6, 3.0, 4.5, 1.5, Iris_versicolor));
	tmp.push_back(DataNode(6.0, 2.7, 5.1, 1.6, Iris_versicolor));
	tmp.push_back(DataNode(5.6, 2.7, 4.2, 1.3, Iris_versicolor));
	tmp.push_back(DataNode(5.0, 3.2, 1.2, 0.2, Iris_setosa));
	tmp.push_back(DataNode(5.5, 2.4, 3.7, 1.0, Iris_versicolor));
	tmp.push_back(DataNode(4.8, 3.4, 1.6, 0.2, Iris_setosa));
	tmp.push_back(DataNode(6.0, 2.2, 4.0, 1.0, Iris_versicolor));
	return tmp;
}
