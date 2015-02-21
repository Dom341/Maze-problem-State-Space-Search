;;;a* algorithm
(defun a* (problem)
  (let* ((explored '())
	 (frontier '())
	 (initstate (get problem 'start-state))
	 (node (list initstate '() 0.0))
	 (snode '())
	 (psuccessors '())
	 (child '())
	 (fnode '()))

    (if(eq (goal-test problem (car node)) t) (return (cdr node)))
    (setf fnode (list node (f node (get problem 'goal-state))));                                               

    (push fnode frontier)

    (loop
     do
        (if (eq frontier  nil)(return "PATH NOT FOUND"))

        (setf frontier (stable-sort frontier #'< :key #'second))
	
	
        (setf fnode (pop frontier))


        (if (eq (goal-test problem (car(car fnode))) t) (return-from a* (cdr (car fnode))))
                                                                 
        (push (car(car fnode)) explored)                        
        (setf psuccessors (successors problem (car (car fnode))))
	
	(dolist (snode psuccessors)
          (setf child (list (make-child (car fnode) snode) (f (make-child (car fnode) snode) (get problem 'goal-state))))
        ;only true if child is NOT in both explored and frontier - 
	  (if (not(checke (car (car child))explored)) nil
            (if (and (checke (car (car child)) explored)
                     (checkf (car child) frontier))

		(if (eq (checkmultiples child frontier)t)(push child frontier))
	      
              (if(not(eq frontier nil))
                  (if(> (nth 1 (car frontier)) (nth 1 child))
		      (if (eq (checkmultiples child frontier)t)
                      (progn
                        (pop frontier)
                        (push child frontier)
                        ))
                    );if                                                                                       
                );if                                                                                           

            );if                                                                                 
               
          );if 
                                                                                                 

          );dolist                                                                                           
   
    );loop                                                                                             
    );let        
                                                                                              
);defun          

;;;bfs algorithm
(defun bfs(problem)
  (let* ((explored '())
	(frontier '())
	(initstate (get problem 'start-state))
	(node (list initstate '() 0.0))
	(snode '())
	(psuccessors '())
	(child '()))

    (if(eq (goal-test problem (car node)) t) (return (cdr node)))
    (push node frontier)
    
    (loop
     do
	(if (eq frontier  nil)(return "PATH NOT FOUND"))         
	    (setf node (pop frontier)) 
	     
	    (if(checke (car node) explored)(push (car node) explored));
	   
	    (setf psuccessors (successors problem (car node)))

	     (dolist (snode psuccessors)
	       (setf child (make-child node snode))

	       ;only true if child is NOT in both explored and frontier - 
	       (if (and (checke (car child) explored)
			(checkf child frontier))	
		      (progn
		        (if (eq (goal-test problem (car child)) t) (return-from bfs (cdr child)))
			(push child frontier)
		      );progn
		 
		 );if
	       );dolist
    );loop
    );let
);defun

;;;ucs algorithm
(defun ucs(problem)
  (let* ((explored '())
        (frontier '())
        (initstate (get problem 'start-state))
        (node (list initstate '() 0.0))
        (snode '())
        (psuccessors '())
        (child '()))

    (if(eq (goal-test problem (car node)) t) (return (cdr node)))
    (push node frontier)

    (loop
     do
        (if (eq frontier  nil)(return "PATH NOT FOUND"))

	(setf frontier (stable-sort frontier #'< :key #'third))      
	(setf node (pop frontier))

	(if (eq (goal-test problem (car node)) t) (return-from ucs (cdr node)))

           ;(if(checke (car node) explored)(push (car node) explored));                                         
	(push (car node) explored);
	
            (setf psuccessors (successors problem (car node)))

             (dolist (snode psuccessors)
               (setf child (make-child node snode))

               ;only true if child is NOT in both explored and frontier -                                       
               (if (not (checke (car child)explored)) nil
		 (if (and (checke (car child) explored)
                        (checkf child frontier))

		     (if (eq (checkmultiples child frontier)t)(push child frontier))
		 
		 (if(not(eq frontier nil))
		     (if(> (nth 2 (car frontier)) (nth 2 child))
		         (if (eq (checkmultiples child frontier)t)
			 (progn
			   
			   (pop frontier)
			   (push child frontier)
			 ))
		     );if
		 );if 
		       
	       );if
	      );if
		 
              );dolist
    );loop
    );let
);defun

;;;gbfs algorithm
(defun gbfs(problem)
(let* ((explored '())
        (frontier '())
        (initstate (get problem 'start-state))
        (node (list initstate '() 0.0))
        (snode '())
        (psuccessors '())
        (child '())
	(fnode '()))

    (if(eq (goal-test problem (car node)) t) (return (cdr node)))
    (setf fnode (list node (h node (get problem 'goal-state))));
    (push fnode frontier)

    (loop
     do
        (if (eq frontier  nil)(return "PATH NOT FOUND"))
	
        (setf frontier (stable-sort frontier #'< :key #'second))
        (setf fnode (pop frontier))
	

        (if (eq (goal-test problem (car(car fnode))) t) (return-from gbfs (cdr (car fnode))))
                                                                                                                
        (push (car(car fnode)) explored);                                                                             

        (setf psuccessors (successors problem (car (car fnode))))

	(dolist (snode psuccessors)
	  (setf child (list (make-child (car fnode) snode) (h (make-child (car fnode) snode) (get problem 'goal-state))))

	;only true if child is NOT in both explored and frontier -                                      \
          (if (not (checke (car (car child))explored)) nil                                                                                                      
	    (if (and (checke (car (car child)) explored)
		     (checkf (car child) frontier))
		(push child frontier)

	      (if(not(eq frontier nil))
		  (if(> (nth 1 (car frontier)) (nth 1 child))
		      (progn
			(pop frontier)
			(push child frontier)
			)
		    );if                                                                                       
		);if                                                                                           

	    );if
	  );if


	  );dolist                                                                                          
    );loop                                                                                                      
    );let                                                                                                       
);defun   


;;;checks the explored list to see if a state is in it
(defun checke (check explored)
  (let ((proceed t))
    (dolist (element explored)
      (if (and (equal (car check) (car element))
	       (equal (cadr check) (cadr element)))
	  (setf proceed nil)))
  proceed)
)

;;;checks the frontier list to see if a node is in it
(defun checkf (check frontier)
  (let ((proceed t))
    (dolist (element frontier)
      (if (and (equal (car (car check)) (car (car element)))
	       (equal (cadr (car check))(cadr (car element))))
	  (if (equal (cadr check) (cadr element))
	      (if(equal (nth 2 check) (nth 2 element))
		  (setf proceed nil)))))
    proceed)
 )

;;;checks the frontier list to see if multiple nodes with the same state exist
(defun checkmultiples (check frontier)
  (let ((proceed t))
    (dolist (element frontier)
      (if (equal (car (car check)) (car (car element))) (setf proceed nil))
)
proceed)
)

;;;stats functions of the previous algorithms

(defun a*-stats(problem)
(let* ((explored '())
         (frontier '())
         (initstate (get problem 'start-state))
         (node (list initstate '() 0.0))
         (snode '())
         (psuccessors '())
         (child '())
         (fnode '())
	 (entered 0)
	 (expanded 0)
	 (maintained 0)
	 (result nil))

    (if(eq (goal-test problem (car node)) t) (return (cdr node)))
    (setf fnode (list node (f node (get problem 'goal-state))));                                                                      

    (push fnode frontier)

    (setf result (loop
     do
        (if (eq frontier  nil)(return "PATH NOT FOUND"))

        (setf frontier (stable-sort frontier #'< :key #'second))


        (setf fnode (pop frontier))


        (if (eq (goal-test problem (car(car fnode))) t) nil)

        (push (car(car fnode)) explored)
	(incf entered)
        (setf psuccessors (successors problem (car (car fnode))))
	
	(dolist (snode psuccessors)
          (setf child (list (make-child (car fnode) snode) (f (make-child (car fnode) snode) (get problem 'goal-state))))
	  (incf expanded)
        ;only true if child is NOT in both explored and frontier -                                                                    
          (if (not(checke (car (car child))explored)) nil
            (if (and (checke (car (car child)) explored)
                     (checkf (car child) frontier))

                (if (eq (checkmultiples child frontier)t)
		    (progn
		      (push child frontier)
		      (incf maintained)
		    )
              (if(not(eq frontier nil))
                  (if(> (nth 1 (car frontier)) (nth 1 child))
                      (if (eq (checkmultiples child frontier)t)
                      (progn
                        (pop frontier)
                        (push child frontier)
                        ))
                    );if                                                                                                              
                );if                                                                                                                  

            );if                                                                                                                      

          );if                                                                                                                        


          );dolist                                                                                                                    

    );loop                                                                                                                            
    );let                                                                                                                             
)
(push maintained result)
(push expanded result)
(push entered result)
result);defun
)

(defun gbfs-stats(problem)
(let* ((explored '())
        (frontier '())
        (initstate (get problem 'start-state))
        (node (list initstate '() 0.0))
        (snode '())
        (psuccessors '())
        (child '())
        (fnode '())
	(entered 0)
	(expanded 0)
	(maintained 0)
	(result nil))

    (if(eq (goal-test problem (car node)) t) (return (cdr node)))
    (setf fnode (list node (h node (get problem 'goal-state))));                                                
    (push fnode frontier)

    (setf result (loop
		  do
        (if (eq frontier  nil)(return "PATH NOT FOUND"))

        (setf frontier (stable-sort frontier #'< :key #'second))
        (setf fnode (pop frontier))


        (if (eq (goal-test problem (car(car fnode))) t) nil)

        (push (car(car fnode)) explored);                                                                   
         (incf entered)                                                                                       
                

        (setf psuccessors (successors problem (car (car fnode))))
	(dolist (snode psuccessors)
          (setf child (list (make-child (car fnode) snode) (h (make-child (car fnode) snode) (get problem 'goal-state))))
	  (incf expanded)

        ;only true if child is NOT in both explored and frontier -                                      \       
          (if (not (checke (car (car child))explored)) nil (if (and (checke (car (car child)) explored)
								    (checkf (car child) frontier))
							       (progn
								 (push child frontier)
					                        (incf maintained)
								 )
							     (if(not(eq frontier nil))nil
							       (if(> (nth 1 (car frontier)) (nth 1 child))
								   (progn
								     (pop frontier)
								     (push child frontier)
								     )
								 );if                                                                                      
                );if                                                                                           

            );if                                                                                                
          );if                                                                                                  


          );dolist                                                                                            
         
    );loop                                                                                                       
    );let                                                                                                     
                                                                                                                
(push maintained result)
(push expanded result)
(push entered result)
result);defun                    
)


(defun ucs-stats(problem)
  (let* ((explored '())
        (frontier '())
        (initstate (get problem 'start-state))
        (node (list initstate '() 0.0))
        (snode '())
        (psuccessors '())
        (child '())
	(entered 0)
	(expanded 0)
	(maintained 0)
	(result nil))

    (if(eq (goal-test problem (car node)) t) (return (cdr node)))
    (push node frontier)

    (setf result (loop
     do
        (if (eq frontier  nil)(return "PATH NOT FOUND"))

        (setf frontier (stable-sort frontier #'< :key #'third))
        (setf node (pop frontier))

        (if (eq (goal-test problem (car node)) t) nil)

           ;(if(checke (car node) explored)(push (car node) explored));                                        \
                                                                                                                
        (push (car node) explored);                                                                             
	(incf entered)

            (setf psuccessors (successors problem (car node)))

	    (dolist (snode psuccessors)
               (setf child (make-child node snode))
	       (incf expanded)
               ;only true if child is NOT in both explored and frontier -                                      \
                                                                                                                
               (if (not (checke (car child)explored)) nil
                 (if (and (checke (car child) explored)
                        (checkf child frontier))

                     (if (eq (checkmultiples child frontier)t)
			 (progn
			   (push child frontier)
			   (incf maintained)
			   ))
                 (if(not(eq frontier nil))
                     (if(> (nth 2 (car frontier)) (nth 2 child))
                         (if (eq (checkmultiples child frontier)t)
                         (progn

                           (pop frontier)
                           (push child frontier)
                         ))
                     );if                                                                                       
                 );if                                                                                           

		        );if                                                                                             
              );if                                                                                              

              );dolist                                                                                          
    );loop                                                                                                      
    );let                                                                                                       
(push maintained result)
(push expanded result)
(push entered result)
result);defun                                                                                                        
)	    

(defun bfs-stats(problem)
  (let* ((explored '())
	 (frontier '())
	 (initstate (get problem 'start-state))
	 (node (list initstate '() 0.0))
	 (snode '())
	 (psuccessors '())
	 (child '())
	 (result nil)
	 (entered 0)
	 (expanded 0)
	 (maintained 0))

    (if(eq (goal-test problem (car node)) t) (return (cdr node)))
    (push node frontier)

    (setf result (loop
     do
        (if (eq frontier  nil)(return "PATH NOT FOUND"))
            (setf node (pop frontier))
	    
            (if(checke (car node) explored)
		(progn
		(push (car node) explored)
		(incf entered)
		))

            (setf psuccessors (successors problem (car node)))

             (dolist (snode psuccessors)
	       (incf expanded)
               (setf child (make-child node snode))

               ;only true if child is NOT in both explored and frontier -                                       
               (if (and (checke (car child) explored)
                        (checkf child frontier))
                      (progn
                        (if (eq (goal-test problem (car child)) t) nil)
                        (push child frontier)
			(incf maintained)
                      );progn                                                                                   

                 );if                                                                                           
               );dolist                                                                                         
    );loop                                                                                                      
    );let                                                                                                       
(push maintained result)
(push expanded result)
(push entered result)
result);defun        
)