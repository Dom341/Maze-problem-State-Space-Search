;;;;Author: Josh Kang

;;;loads the maze from text file
(defun load-maze-problem (fname)
       (let ((istream (open fname :direction :input :if-does-not-exist nil))
	     (x 0)
	     (y 0)
	     (startx 0)
	     (starty 0)
	     (goalx 0)
	     (goaly 0)
	     (currentx 0)
	     (currenty 0))
         
	 (when istream
           (setf x (read istream))
           (setf y (read istream))
           (let ((store(make-array (list y x))))	     
 
	     (loop for char = (read-char istream nil)
		   while char
		   do
		
		   (cond
		    ((CHAR= char #\Newline) ())
		    (t (setf (aref store currenty currentx) char)
		       (incf currentx)
		       (if (CHAR= char #\S)(setf startx (- currentx 1) starty currenty))
		       (if (CHAR= char #\G)(setf goalx (- currentx 1) goaly currenty))
		       
		       (when (eq currentx x)
			 (setf currentx 0)
			 (if (not(eq currenty (- y 1))) (incf currenty))))))	
		   

	   (setf (get 'return 'start-state) (list startx starty))
	   (setf (get 'return 'goal-state) (list goalx goaly))
	   (setf (get 'return 'maze) store))

	   (close istream))
	 'return))

;;;prints maze from array
(defun print-maze (maze)
 (loop for i below (car (array-dimensions maze))
       do
	(loop for j below (cadr (array-dimensions maze))
	      do
	      (let ((element (aref maze i j)))
		(format t "~a" element)))
	(format t "~%")))

;;;finds the successors of a given state and problem
(defun successors (problem state)
  (let ((newnode '())
	(next '())
      	(newstate '())
	(posx (car state))
	(posy (cadr state))
	(movechar 0)
	(limitx (cadr (array-dimensions (get problem 'maze))))
	(limity (car (array-dimensions (get problem 'maze)))))


    ;have to check to make sure the program doesn't try to move out of the maze boundaries
    ;can go west
     (if(> posx 0)
	 (progn
	  (setf movechar (aref (get problem 'maze) posy (- posx 1)))
	  (if (CHAR/= movechar #\%)
	      (progn
		(setf newnode '((west) 1.0))
		(setf newstate (list (- posx 1) posy))
		(push newstate newnode)
		(push newnode next)))))
    ;can go east
     (if(< posx (- limitx 1))
         (progn
          (setf movechar (aref (get problem 'maze) posy (+ posx 1)))
          (if (CHAR/= movechar #\%)
              (progn
                (setf newnode '((east) 1.0))
                (setf newstate (list (+ posx 1) posy))
                (push newstate newnode)
                (push newnode next)))))
    ;can go north
     (if(> posy 0)
         (progn
          (setf movechar (aref (get problem 'maze) (- posy 1) posx))
          (if (CHAR/= movechar #\%)
              (progn
                (setf newnode '((north) 1.0))
                (setf newstate (list posx (- posy 1)))
                (push newstate newnode)
                (push newnode next)))))
    ;can go south
     (if(< posy (- limity 1))
         (progn
          (setf movechar (aref (get problem 'maze) (+ posy 1) posx))
          (if (CHAR/= movechar #\%)
              (progn
                (setf newnode '((south) 1.0))
                (setf newstate (list posx (+ posy 1)))
                (push newstate newnode)
                (push newnode next)))))
     next)
)

;;;creates a child node from a node and its successor
(defun make-child (node successor)
  (let ((child '())
	(currentpath '()))

    (setf child (list (+ (nth 2 node) (nth 2 successor))))
    ;if statement?
    (setf currentpath (append (nth 1 successor) (nth 1 node)))
    (push currentpath child)
    (push (car successor) child)
   child)
)
;;;tests a state to see if it is the goal state
(defun goal-test (problem state)
  (let ((posx (car state))
	(posy (nth 1 state))
	(goalx (car (get problem 'goal-state)))
	(goaly (nth 1 (get problem 'goal-state)))
	(check nil))
    
    (if (and (eq posx goalx)
	     (eq posy goaly))
	(setf check t))
check)
)

;;;calculates the distance between two states
(defun h (node goal)
  (let* ((state (car node))
	(x1 (car state))
	(y1 (cadr state))
	(x2 (car goal))
	(y2 (cadr goal))
	(d 0.0))

    (setf d (sqrt(+ (expt(- x2 x1) 2) (expt(- y2 y1) 2))))
d)       
)

;;;calculates distance plus path cost 
(defun f (node goal)
  (let* ((state (car node))
        (x1 (car state))
        (y1 (cadr state))
        (x2 (car goal))
        (y2 (cadr goal))
        (fd 0.0))

    (setf fd (sqrt(+ (expt(- x2 x1) 2) (expt(- y2 y1) 2))))
    (setf fd (+ fd (nth 2 node)))
fd)
)
