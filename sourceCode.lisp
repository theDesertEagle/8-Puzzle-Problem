;;; UTILITY FUNCTIONS FOR STATE MANAGEMENT
(defun copyState(stateOne stateTwo)
  (setf (gethash 'stateRepresentation stateOne) (gethash 'stateRepresentation stateTwo))
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
  (setf (gethash 'stateRepresentation state) sRValue))

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
(defvar examinationState (make-hash-table))
(copyState examinationState startState)
;(write examinationState)


;;;PRIORITY-QUEUE OPERATIONS


(defvar optimalSolutionSequence (list startState))
(defvar priorityQueue (list startState))

(defvar stateChildNumber 1)

;;;GENERATING CHILDREN
(defun generateChildrenOfState(state)
  ;(defvar nameOfChildOne (intern (concatenate 'string "s" (write-to-string stateChildNumber))))
  ;(defvar (intern (concatenate 'string "s" (write-to-string stateChildNumber))) (make-hash-table))
  ;(copyState nameOfChildOne state)
  ;(setf stateChildNumber (+ stateChildNumber 1))
  ;(write nameOfChildOne))
  (defvar childOneName (intern (concatenate 'string "s" (write-to-string stateChildNumber))))
  (set (intern (concatenate 'string "s" (write-to-string stateChildNumber))) (make-hash-table))
  (copyState (symbol-value childOneName) startState)
  (write (type-of (gethash 'stateRepresentation (symbol-value childOneName)))))



(loop
  (cond ((equal (getStateRepresentation examinationState) (getStateRepresentation goalState))
            (return "Goal State Reached")))
  (generateChildrenOfState examinationState)
  (return 1)
  ;expand tree by generating children (state spaces) of the examination node
  ;calculate the totalCost of all children nodes and push in queue
  ;select the node with smallest totalCost and dequeue that bitch
  ;add to optimalSolutionSequence
  ;repeat
  )
                                                                                       53,1          Bot

