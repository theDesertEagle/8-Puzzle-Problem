;;; UTILITY FUNCTIONS FOR STATE MANAGEMENT
(defun copyState(stateOne stateTwo)
  (setf (gethash 'stateRepresentation stateOne) (copy-tree (gethash 'stateRepresentation stateTwo)))
  (setf (gethash 'heuristicValue stateOne) (gethash 'heuristicValue stateTwo))
  (setf (gethash 'costToCurrentNode stateOne) (gethash 'costToCurrentNode stateTwo)))

(defun totalCostPath(state)
  (return-from totalCostPath (+ (gethash 'heuristicValue state) (gethash 'costToCurrentNode state))))

(defun setStateValues(state sRValue hValue cTCNValue)
  (cond ((and (not (null sRValue)) (not (null hValue)) (not (null cTCNValue)))
	 	(setf (gethash 'stateRepresentation state) sRValue)
	 	(setf (gethash 'heuristicValue state) hValue)
	 	(setf (gethash 'costToCurrentNode state) cTCNValue))
	(t)))

;; Functions to access Characteristics of State Space more "Neatly"
(defun getStateRepresentation(state)
  (return-from getStateRepresentation (gethash 'stateRepresentation state)))

(defun getHeuristicValue(state)
  (return-from getheuristicValue (gethash 'heuristicvalue state)))

(defun getCostToCurrentNode(state)
  (return-from getCostToCurrentNode (gethash 'costToCurrentNode state)))

(defun setStateRepresentation(state sRValue)
  (setf (gethash 'stateRepresentation state) (copy-tree sRValue)))

(defun setHeuristicValue(state hValue)
  (setf (gethash 'heuristicValue state) hValue))

(defun setCostToCurrentNode(state cTCNValue)
  (setf (gethash 'costToCurrentNode state) cTCNValue))

;;; INITIALIZING GOAL-STATE, START-STATE AND PUSHING THE NODE INTO THE TREE
(defvar startState (make-hash-table))
;;;
;(setf (gethash 'stateRepresentation startState) (list (list 'E 1 3) (list 4 2 5) (list 7 8 6)))
;(setf (gethash 'heuristicValue startState) 5)
;(setf (gethash 'costTocurrentNode startState) 0)
;;;
(setStateValues startState (list (list 'E 1 3) (list 4 2 5) (list 7 8 6)) 5 0)
(defvar a*Tree (list startState))
(defvar goalState (make-hash-table))
(setStateRepresentation goalState (list (list 1 2 3) (list 4 5 6) (list 7 8 'E)))
;(write (car (car (getStateRepresentation goalState)))) ;Show leftmost block of goal state
;;;
;(write (gethash 'stateRepresentation (car a*Tree)))
;(write (type-of (caddr (car (gethash 'stateRepresentation (car a*Tree))))))
;(write (+ (cadr (car (gethash 'stateRepresentation (car a*Tree)))) (car (cadr (gethash 'stateRepresentation (car a*Tree)))))) ; (2,1) in the stateRep + (1,0) in the stateRep, where lefttopmost corner = (2,0)
;;;
;;;
;(write (gethash 'stateRepresentation (car a*Tree)))
;(terpri)
;(write (gethash 'stateRepresentation goalState))
;;;

;;; INITIALIZING TEMPORARY-STATE TO THE START-STATE
(defparameter examinationState (make-hash-table))
(copyState examinationState startState)
;(defvar startStateSR (getStateRepresentation startState))
;(defvar startStateHV (getHeuristicValue startState))
;(defvar startStateCTCN (getCostToCurrentNode startState))
;(setStateValues examinationState startStateSR startStateHV startStateCTCN)
;(setStateRepresentation examinationState (getStateRepresentation startState))
;(setHeuristicValue examinationState (getHeuristicValue startState))
;(setCostToCurrentNode examinationState (getCostToCurrentNode startState))
;(write examinationState)


;;;PRIORITY-QUEUE OPERATIONS


(defvar optimalSolutionSequence (list startState))
(defvar priorityQueue (list startState))

(defvar stateChildNumber 1)

;;;FINDING POSITION OF EMPT SQUARE IN SEQUENCE
(defun getPositionOfEmptySquare(stateSR)
  (defvar rowNumber 0)
  (defvar rowInConsideration (car stateSR))
  (defvar indexOfEmptySquareInRow nil)
  (setf indexOfEmptySquareInRow (position 'E rowInConsideration :test #'equal)) ;Start for Empty Square Search Search in First Row
  (if (not (null indexOfEmptySquareInRow))
    (return-from getPositionOfEmptySquare (list rowNumber indexOfEmptySquareInRow)))
  (setf rowNumber (+ rowNumber 1))
  (setf rowInConsideration (cadr stateSR))
  (setf indexOfEmptySquareInRow (position 'E rowInConsideration :test #'equal)) ;Start for Empty Square Search Search in Second Row
  (if (not (null indexOfEmptySquareInRow))
    (return-from getPositionOfEmptySquare (list rowNumber indexOfEmptySquareInRow)))
  (setf rowNumber (+ rowNumber 1))
  (setf rowInConsideration (caddr stateSR))
  (setf indexOfEmptySquareInRow (position 'E rowInConsideration :test #'equal)) ;Start for Empty Square Search Search in Third Row
  (if (not (null indexOfEmptySquareInRow))
    (return-from getPositionOfEmptySquare (list rowNumber indexOfEmptySquareInRow))))

;;;RETURNS A ROW OF THE STATE REPRESENTATION
(defun returnRowOfStateSR(stateSR rowIndex)
  (cond ((eq rowIndex 0)
	 (return-from returnRowOfStateSR (car stateSR)))
	((eq rowIndex 1)
	 (return-from returnRowOfStateSR (cadr stateSR)))
	((eq rowIndex 2)
	 (return-from returnRowOfStateSR (caddr stateSR)))))



;;;GENERATING CHILDREN
(defun generateChildrenOfState(givenState)
  ;(defvar nameOfChildOne (intern (concatenate 'string "s" (write-to-string stateChildNumber))))
  ;(defvar (intern (concatenate 'string "s" (write-to-string stateChildNumber))) (make-hash-table))
  ;(copyState nameOfChildOne state)
  ;(setf stateChildNumber (+ stateChildNumber 1))
  ;(write nameOfChildOne))
  (defvar givenStateSR (getStateRepresentation givenState))
  (defvar givenStateCTCN (getCostToCurrentNode givenState))
  ;(defvar leftMoveStateName (intern (concatenate 'string "s" (write-to-string stateChildNumber))))
  ;(set (intern (concatenate 'string "s" (write-to-string stateChildNumber))) (make-hash-table))
  ;(copyState (symbol-value leftMoveStateName) examinationState)
  ;(write (gethash 'stateRepresentation (symbol-value childOneName))))
  (defvar positionOfEmptySquare (getPositionOfEmptySquare givenStateSR))
  ;(write (and (not (equal positionOfEmptySquare (list 0 0))) (not (equal positionOfEmptySquare (list 1 0))) (not (equal positionOfEmptySquare (list 2 0)))))

  ;;SHOULD TRY TO OPTIMIZE THIS BIT OF CODE!!!!! (REMOVE REDUNDACY)
  (if (and (not (equal positionOfEmptySquare (list 0 0))) (not (equal positionOfEmptySquare (list 1 0))) (not (equal positionOfEmptySquare (list 2 0)))) ;Needs Attention
     (progn
       (defvar leftMoveStateName (intern (concatenate 'string "s" (write-to-string stateChildNumber))))
       (set (intern (concatenate 'string "s" (write-to-string stateChildNumber))) (make-hash-table))
       (setf stateChildNumber (+ stateChildNumber 1))
       (setStateRepresentation (symbol-value leftMoveStateName) givenStateSR)
       (setf (nth (cadr positionOfEmptySquare) (nth (car positionOfEmptySquare) (getStateRepresentation (symbol-value leftMoveStateName))))  (nth (- (cadr positionOfEmptySquare) 1) (nth (car positionOfEmptySquare) (getStateRepresentation (symbol-value leftMoveStateName))))) ; (i,j)th entry containing E = (i, j-1)th entry
       (setf (nth (- (cadr positionOfEmptySquare) 1) (nth (car positionOfEmptySquare) (getStateRepresentation (symbol-value leftMoveStateName)))) 'E) ; (i,j-1)th = E
       (format t "~a : ~a ~%" (symbol-name leftMoveStateName) (getStateRepresentation (symbol-value leftMoveStateName)))))

  (if (and (not (equal positionOfEmptySquare (list 0 2))) (not (equal positionOfEmptySquare (list 1 2))) (not (equal positionOfEmptySquare (list 2 2)))) ;Needs Attention
     (progn
       (defvar rightMoveStateName (intern (concatenate 'string "s" (write-to-string stateChildNumber))))
       (set (intern (concatenate 'string "s" (write-to-string stateChildNumber))) (make-hash-table))
       (setf stateChildNumber (+ stateChildNumber 1))
       (setStateRepresentation (symbol-value rightMoveStateName) givenStateSR)
       (setf (nth (cadr positionOfEmptySquare) (nth (car positionOfEmptySquare) (getStateRepresentation (symbol-value rightMoveStateName))))  (nth (+ (cadr positionOfEmptySquare) 1) (nth (car positionOfEmptySquare) (getStateRepresentation (symbol-value rightMoveStateName))))) ; (i,j)th entry containing E = (i, j-1)th entry
       (setf (nth (+ (cadr positionOfEmptySquare) 1) (nth (car positionOfEmptySquare) (getStateRepresentation (symbol-value rightMoveStateName)))) 'E) ; (i,j-1)th = E
       ;(setStateRepresentation startState (list (list 'E 1 3) (list 4 2 5) (list 7 8 6)))
       ;(setf givenStateSR (list (list 'E 1 3) (list 4 2 5) (list 7 8 6)))
       (format t "~a : ~a ~%" (symbol-name rightMoveStateName) (getStateRepresentation (symbol-value rightMoveStateName)))))

   (if (and (not (equal positionOfEmptySquare (list 0 0))) (not (equal positionOfEmptySquare (list 0 1))) (not (equal positionOfEmptySquare (list 0 2)))) ;Needs Attention
     (progn
       (defvar upMoveStateName (intern (concatenate 'string "s" (write-to-string stateChildNumber))))
       (set (intern (concatenate 'string "s" (write-to-string stateChildNumber))) (make-hash-table))
       (setf stateChildNumber (+ stateChildNumber 1))
       (setStateRepresentation (symbol-value upMoveStateName) givenStateSR)
       (setf (nth (cadr positionOfEmptySquare) (nth (car positionOfEmptySquare) (getStateRepresentation (symbol-value upMoveStateName))))  (nth (cadr positionOfEmptySquare) (nth (- (car positionOfEmptySquare) 1) (getStateRepresentation (symbol-value upMoveStateName))))) ; (i,j)th entry containing E = (i, j-1)th entry
       (setf (nth (cadr positionOfEmptySquare) (nth (- (car positionOfEmptySquare) 1) (getStateRepresentation (symbol-value upMoveStateName)))) 'E) ; (i,j-1)th = E
       (format t "~a ~%" (getStateRepresentation (symbol-value upMoveStateName)))))

  (if (and (not (equal positionOfEmptySquare (list 2 0))) (not (equal positionOfEmptySquare (list 2 1))) (not (equal positionOfEmptySquare (list 2 2)))) ;Needs Attention
     (progn
       (defvar downMoveStateName (intern (concatenate 'string "s" (write-to-string stateChildNumber))))
       (set (intern (concatenate 'string "s" (write-to-string stateChildNumber))) (make-hash-table))
       (setf stateChildNumber (+ stateChildNumber 1))
       (setStateRepresentation (symbol-value downMoveStateName) givenStateSR)
       (setf (nth (cadr positionOfEmptySquare) (nth (car positionOfEmptySquare) (getStateRepresentation (symbol-value downMoveStateName))))  (nth (cadr positionOfEmptySquare) (nth (+ (car positionOfEmptySquare) 1) (getStateRepresentation (symbol-value downMoveStateName))))) ; (i,j)th entry containing E = (i, j-1)th entry
       (setf (nth (cadr positionOfEmptySquare) (nth (+ (car positionOfEmptySquare) 1) (getStateRepresentation (symbol-value downMoveStateName)))) 'E) ; (i,j-1)th = E
       (format t "~a : ~a ~%" (symbol-name downMoveStateName) (getStateRepresentation (symbol-value downMoveStateName)))))
  )

(loop
  (cond ((equal (getStateRepresentation examinationState) (getStateRepresentation goalState))
	    (return "Goal State Reached")))
  (format t "Examination State Before Children Generation : ~a ~%" (getStateRepresentation examinationState))
  (generateChildrenOfState examinationState)
  (format t "Examination State After Children Generation: ~a ~%" (getStateRepresentation examinationState))
  (format t "Start State After Children Generation: ~a ~%" (getStateRepresentation startState))
  (format t "Goal State After Chldren Generation: ~a ~%" (getStateRepresentation goalState))
  (format t "~a ~%" (symbol-value rightMoveStateName))
  (format t "~a ~%" (eq (getStateRepresentation examinationState) (getStateRepresentation startState)))
  ;(format t "<<<<<>>>>>>>>> ~a ~%" examinationState)
  ;(format t "<<<<<>>>>>>>>> ~a ~%" examinationState)
  (return 1)
  ;expand tree by generating children (state spaces) of the examination node
  ;calculate the totalCost of all children nodes and push in queue
  ;select the node with smallest totalCost and dequeue it
  ;add to optimalSolutionSequence
  ;repeat
  )
