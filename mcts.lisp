;; ========================================
;;  CMPU-365, Spring 2019
;;  Monte Carlo Tree Search -- TEMPLATE!
;; ========================================

;;  Contracts for the following functions used by MCTS algorithm
;; ----------------------------------------------------------
;;     GET-ROOT-NODE
;;     NEW-MC-TREE
;;     INSERT-NEW-NODE
;;     SIM-TREE
;;     SIM-DEFAULT (defined for you)
;;     BACKUP
;;     UCT-SEARCH
;;     SELECT-MOVE

;;  In addition, for testing, the COMPETE function is defined for you.


;;  Your MCTS functions may call the following DOMAIN-DEPENDENT
;;  functions that are defined in "othello-starter.lisp":
;; ------------------------------------------------------------------
;;     COPY-GAME               -- creates a copy of the given othello game board
;;     MAKE-HASH-KEY-FROM-GAME -- returns list of the form (WHITE-PCS BLACK-PCS WHOSE-TURN)
;;     WHOSE-TURN              -- returns *BLACK* or *WHITE*

;;  Your MCTS functions may call the following DOMAIN-DEPENDENT
;;  functions that are defined in "othello-the-rest.lisp":
;; ------------------------------------------------------------------
;;     DO-MOVE!        --  does a move (destructively modifies game struct)
;;     LEGAL-MOVES     --  returns VECTOR of legal moves
;;     GAME-OVER?      --  returns T or NIL
;;     DEFAULT-POLICY  --  returns random legal move

;;  Your MCTS functions should not need to call any of the MACROs defined
;;  in "othello-macros.lisp".


;;  Note:  If a player has no legal moves, but the game isn't over, then that
;;         player *must* pass...


;;  MC-NODE struct -- a node in the MCTS tree
;; ----------------------------------------------------------------------------
;;  KEY:          a hash-table key (compact rep'n of current state of game)
;;  WHOSE-TURN:   *BLACK* or *WHITE*
;;  NUM-VISITS:   the number of times this state has been visited
;;  VECK-MOVES:   a VECTOR of the legal moves from this state
;;  VECK-VISITS:  a VECTOR recording the number of times each legal move
;;                   has been visited during MCTS
;;  VECK-SCORES:  a VECTOR recording the average scores for the legal
;;                   moves visited during MCTS

(defstruct mc-node
  key
  whose-turn
  (num-visits 0)
  veck-moves
  veck-visits
  veck-scores
  )

;;  MC-TREE struct -- the MCTS tree
;; -------------------------------------------------------------
;;  HASHY:     a hash-table whose entries are (key,value), where
;;               key = compact repn of state, value = mc-node
;;  ROOT-KEY:  the hash-table key for the root node of the mcts tree

(defstruct mc-tree
  (hashy (make-hash-table :test #'equal))
  root-key)

;;  GET-ROOT-NODE
;; ------------------------------------------------------
;;  INPUT:   TREE, a MCTS struct
;;  OUTPUT:  The MC-NODE corresponding to the root of the TREE

(defun get-root-node
    (tree)
  (gethash (mc-tree-root-key tree) (mc-tree-hashy tree)))

;; -------------------------------------------------
;;  Easiest to define the following functions
;;  in the following order (to facilitate testing)
;; -------------------------------------------------

;;  NEW-MC-TREE
;; ---------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived
;;           from GAME.

(defun new-mc-tree
    (game)
    ;; make a new mc-tree struct
    (let*
      ((key (make-hash-key-from-game game))
       (tree (make-mc-tree
              ;; get ROOT-KEY from GAME
              ;; SUGGESTION: insert root node here by key?
              :root-key key)))
      (progn
       (insert-new-node game tree key)
       tree)))

;;  INSERT-NEW-NODE
;; -----------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           KEY, a hash-key representing the state of the game
;;  OUTPUT:  The newly created and inserted node
;;  SIDE EFFECT:  Inserts a new node into TREE using KEY.

;;  VECK-MOVES:   a VECTOR of the legal moves from this state
;;  VECK-VISITS:  a VECTOR recording the number of times each legal move
;;                   has been visited during MCTS
;;  VECK-SCORES:  a VECTOR recording the average scores for the legal
;;                   moves visited during MCTS

(defun insert-new-node
    (game tree key)
    (let* ((moves (legal-moves game))
           (num-moves (length moves))
           (new-node
    (make-mc-node
      :key key
      :whose-turn (whose-turn game)
      :veck-moves moves
      :veck-visits (make-array num-moves :initial-element 0)
      :veck-scores (make-array num-moves :initial-element 0)
      )))
      (setf (gethash key (mc-tree-hashy tree)) new-node)
      new-node))

;;  SELECT-MOVE
;; ------------------------------------------
;;  INPUTS:  NODEY, an MC-NODE struct
;;           C, exploitation-exploration constant
;;  OUTPUT:  The INDEX of the selected move into the moves vector

(defun select-move
    (nodey c)
    (if
      (eq (mc-node-whose-turn nodey) *BLACK*)
      (max-action-index
        (mc-node-veck-scores nodey)
        (mc-node-veck-visits nodey)
        (mc-node-veck-moves nodey)
        c
        (mc-node-num-visits nodey))
      (min-action-index
        (mc-node-veck-scores nodey)
        (mc-node-veck-visits nodey)
        (mc-node-veck-moves nodey)
        c
        (mc-node-num-visits nodey))))

(defun max-action-index
  (scores visits moves c n)
  (let* ((lengthy (length scores))
        (max-value -100)
        (max-value-index nil))
  (dotimes (i lengthy)
  (let ((action-n (aref visits i)))
  (if (eq action-n 0)
    (return-from max-action-index i)
    (let* ((action (aref moves i))
          (action-q (aref scores i))
          (action-value (+ action-q (* c (sqrt (/ (log n) action-n))))))
    (if (> action-value max-value)
        (progn
          (setf max-value action-value)
          (setf max-value-index i)))))))
max-value-index))

(defun min-action-index
  (scores visits moves c n)
  (let* ((lengthy (length scores))
        (min-value 100)
        (min-value-index nil))
  (dotimes (i lengthy)
  (let ((action-n (aref visits i)))
  (if (eq action-n 0)
    (return-from min-action-index i)
    (let* ((action (aref moves i))
          (action-q (aref scores i))
          (action-value (- action-q (* c (sqrt (/ (log n) action-n))))))
    (if (< action-value min-value)
        (progn
          (setf min-value action-value)
          (setf min-value-index i)))))))
min-value-index))

;;  SIM-TREE
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           C, the exploration/exploitation constant
;;  OUTPUT:  A list of the form (state0 move0 state1 move1 ... statek movek)
;;    where each state_i is a key into the hashtable, and each move_i
;;    is an index into the MOVES vector of the node assoc with state_i.

; (defun sim-tree
;     (game tree c)
;   (let ((result nil)
;         (key (make-hash-key-from-game game))
;         (hashy (mc-tree-hashy tree)))
;     (multiple-value-bind
;      (nodey intree-p)
;      (gethash key hashy)
;      (cond
;        ((game-over? game)
;         result)
;        (intree-p
;         ()
;         (sim-tree (select-move nodey c)))))))

(defun sim-tree
  (game tree c)
  (labels
   ((sim-tree-acc
     (game acc)
     (cond
       ((game-over? game)
        acc)
       (t
        (let ((key (make-hash-key-from-game game))
              (hashy (mc-tree-hashy tree)))
          (myltiple-value-bind
           (nodey intree-p)
           (gethash key hashy)
           (let ((mv-index (select-move nodey c)))
             (if intree-p
               (sim-tree-acc
                (apply #'do-move! game nil (svref (mc-node-veck-moves nodey) mv-index))
                (append c (list key mv-index)))
               (progn
                (insert-new-node game tree key)
                (append c (list key mv-index)))))))))))
   (sim-tree-acc game nil)))

;;  SIM-DEFAULT -- defined for you!
;; ----------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  The result of following the game's default policy
;;             (domain-dependent method)

(defun sim-default
    (game)
  (default-policy game))

;;  BACKUP
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY

(defun backup
    (hashy key-move-acc result)
  (dotimes
   (index (/ (length key-move-acc) 2))
   (let*
     ((key (nth (* index 2) key-move-acc))
      (mv-index (nth (+ (* index 2) 1) key-move-acc))
      (nodey (gethash key hashy)))
     (progn
      (incf (mc-node-num-visits nodey))
      (incf (svref (mc-node-veck-visits nodey) mv-index))
      (incf (svref (mc-node-veck-scores nodey) mv-index)
            (/
             (-
              result
              (svref (mc-node-veck-scores nodey) mv-index))
             (svref (mc-node-veck-visits nodey) mv-index)))))))

;;  UCT-SEARCH
;; ---------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  Best move from that state determined by
;;             doing *NUM-SIMS* simulations of MCTS.

;;  The following global parameter can be used to decide whether
;;  UCT-SEARCH should print out stats about the current round
;;  of MCTS.  The COMPETE function sets *verbose* to T; the
;;  COMPETE-NO-PRINTING function sets it to NIL.

(defparameter *verbose* t)


(defun uct-search
    (orig-game num-sims c)
  (let
    ((tree (new-mc-tree orig-game)))
    (progn
     (dotimes
      (sim num-sims)
      (let*
        ((game (copy-game orig-game))
         (key-move-acc (sim-tree game tree c))
         (result (sim-default game)))
        (backup (mc-tree-hashy tree) key-move-acc result)))
     (let*
       ((root-node (get-root-node tree))
        (best-move-index (selselect-move root-node c))
        (best-move (svref (mc-node-veck-moves root-node) best-move-index))
        (veck-scores (mc-node-veck-scores root-node)))
       (progn
        (when *verbose*
          (format t "Best score: ~A score veck: ~A,~%Visits veck ~A"
                  (svref veck-scores best-move-index)
                  (veck-scores)
                  (mc-node-veck-visits root-node)))
        best-move)))))


;;  COMPETE -- defined for you!
;; ------------------------------------------------------------------------------
;;  INPUTS:  BLACK-NUM-SIMS, the number of simulations for each of black's moves
;;           BLACK-C, the exploration/exploitation constant used by black
;;           WHITE-NUM-SIMS, the number of simulations for each of white's moves
;;           WHITE-C, the exploration/exploitation constant used by white
;;  OUTPUT:  Don't care
;;  SIDE EFFECT:  Displays the entire game using UCT-SEARCH to compute best moves
;;    for both players according to the specified parameters.

(defun compete
    (black-num-sims black-c white-num-sims white-c)
  (let ((g (new-othello)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "BLACK'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g nil (uct-search g black-num-sims black-c))))
       (t
	(format t "WHITE'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g nil (uct-search g white-num-sims white-c))))))))


;;  COMPETE-NO-PRINTING
;; --------------------------------------------------
;;  Same as COMPETE, but only shows the end result

(defun compete-no-printing
    (black-num-sims black-c white-num-sims white-c)
  (let ((g (new-othello)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "B ")
	(apply #'do-move! g nil (uct-search g black-num-sims black-c)))
       (t
	(format t "W ")
	(apply #'do-move! g nil (uct-search g white-num-sims white-c)))))
    (format t "~%~A~%" g)))
