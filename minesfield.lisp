(in-package :minesfield-control)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(defparameter *show-only-visible* T)

(defclass flex-board ()
  ((rows :initarg :rows :reader rows)
   (cols :initarg :cols :reader cols)
   (mines :initarg :mines :reader mines)
   (bc :initarg :board-controller :accessor bc)
   (board :accessor board))
  
  (:documentation "flex board once initilized it randomly selects bombs and keep tracks of fields that 
   were already shown to user. Once bomb is selected it tries to recompute the board 
   matching already revealed fields"))

(defmacro do-neighbors (n p &rest body)
  (let ((d-name (gensym)) (p-name (gensym)))
    `(let ((,p-name ,p))
       (loop for ,d-name in '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))
	     for ,n = (mapcar #'+ ,p-name ,d-name)
	   ,@body))))

(defmethod get-neighbors ((board flex-board) x y)
  (loop for (a b) in '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))
     when (in-board-p (list (+ x a) (+ y b)) board) collect (list (+ x a) (+ y b))))

(defmethod in-board-p (p (board flex-board))
  (with-slots ((r rows) (c cols)) board
    (and (<= 0 (car p)) (<= 0 (cadr p)) (> r (car p)) (> c (cadr p)))))

(defmethod print-object ((f flex-board) out)
  (print-unreadable-object (f out :type t)
    (with-slots (rows cols mines board) f
      (format out ":rows ~d :cols ~d :mines ~d" rows cols mines)
      (loop for i from 0 to (- rows 1) do
	(format out "~%")
	(loop for j from 0 to (- cols 1) do
	  (format out "~a" (pretty-print (aref board i j))))))))

(defmethod click ((fb flex-board) x y)
  (let ((f (aref (slot-value fb 'board) x y)))
    (with-slots (is-visible is-mine mines-count) f
      (if is-mine (recompute fb f) (travers fb (list x y))))))

(defmethod recompute((fb flex-board) f)
  (game-over (slot-value fb 'bc) f))

(defmethod travers ((fb flex-board) p)
  (with-slots (board bc) fb
    (format t "travers ~a~%" p)
    (let ((f (aref board (car p) (cadr p))))
      (with-slots (is-visible is-mine mines-count) f
	(cond
	  ((or is-mine) nil)
	  (is-visible nil)
	  ((< 0 mines-count)
	   (progn
	     (setf is-visible t)
	     (reveal bc f)))
	  (t (progn
	       (setf is-visible t)
	       (reveal bc f)
	       (do-neighbors n p when (in-board-p n fb) do (travers fb n)))))))))
	   
(defmethod initialize-instance :after ((fb flex-board) &key)
  (with-slots (rows cols mines) fb
    (with-accessors ((board board) (rows rows) (cols cols) (mines mines)) fb
      (setf board (make-array (list rows cols)))
      (loop for i from 0 to (- rows 1) do
	(loop for j from 0 to (- cols 1) do
	  (setf (aref board i j) (make-instance 'field :x i :y j))))
      (loop with used = 0 until (= used mines) do
	(let* ((c (random cols)) (r (random rows)) (p (list r c)) (f (aref board r c)))
	  (format t "~a~%" f)
	  (when (not (slot-value f 'is-mine))
	     (incf used) 
	     (make-mine f)
	     (do-neighbors n p do
	       (let ((nr (car n)) (nc (cadr n)))
		 (if (and (in-board-p n fb) (not (slot-value (aref board nr nc) 'is-mine)))
		     (incf (slot-value (aref board nr nc) 'mines-count)))))))))))

(defclass field ()
  ((mines-count :initarg cnt :initform 0)
   (is-mine :initarg :mine :initform nil)
   (is-visible :initarg :visible :initform nil)
   (x :initarg :x)
   (y :initarg :y))
  (:documentation "used to represent a field in a flex array as well as transfer between flex and ui"))

(defmethod make-mine ((f field))
  (setf (slot-value f 'is-mine) t))

(defmethod pretty-print ((f field))
  (with-slots (is-mine mines-count is-visible) f
    (cond ((and *show-only-visible* (not is-visible)) #\#)
	  (is-mine #\*)
	  (t mines-count))))

(defmethod print-object ((f field) out)
  (print-unreadable-object (f out :type t)
    (with-slots (x y) f
      (format out ":x ~d :y ~d ~a" x y (pretty-print f)))))

(defclass board-controller ()
  ((board)
   (ui)))

(defmethod initialize-instance :after ((bc board-controller)
				       &key rows cols mines content)
  (setf (slot-value bc 'board) (make-instance 'flex-board
					      :rows rows
					      :cols cols
					      :mines mines
					      :board-controller bc))
  (setf (slot-value bc 'ui) (make-instance 'ltk-ui
					      :rows rows
					      :cols cols
					      :controller bc))
  (setf (slot-value (slot-value bc 'ui) 'content) content))

(defmethod click ((bc board-controller) x y)
  (with-slots (board) bc
    (click board x y)))

(defmethod game-over ((bc board-controller) f)
  (with-slots (ui) bc
    (game-over ui f)))

(defun test-rev (bc f)
  (reveal bc f))

(defmethod reveal ((bc board-controller) f)
  (format t "reveal: ~a~%" f)
  (display (slot-value bc 'ui) f))
      

(defmethod start ((bc board-controller))
  (start (slot-value bc 'ui)))

(defclass board-ui ()
  ((rows)
   (cols)
   (board)
   (controller)))

(defgeneric display (ui f))

(defgeneric click (ui i j))

(defmethod initialize-instance :after ((bu board-ui)
				       &key rows cols controller)
  (setf (slot-value bu 'rows) rows)
  (setf (slot-value bu 'cols) cols)
  (setf (slot-value bu 'board) (make-array (list rows cols)))
  (setf (slot-value bu 'controller) controller))

(defclass ltk-ui (board-ui)
  ((content)))

(defclass simple-ui (board-ui)
  ())

;(defgeneric game-over (ui))
(defgeneric start (ui))

(defmethod start ((ui ltk-ui))
  (setf *debug-tk* t)
  (format t "~a~%" (slot-value ui 'content))
  (with-slots (rows cols board controller content) ui
    (setup-style)
    (loop for i from 0 to (- rows 1) do
      (loop for j from 0 to (- cols 1) do
	(let ((r i) (c j))
	  (setf (aref board i j) (make-instance 'button :master content
							:width 1
							:command (lambda () (click ui r c))))
	  (grid (aref board i j) i j))))))

(defmethod start ((ui simple-ui))
  (with-slots (rows cols board) ui
    (loop for i from 0 to (- rows 1) do
      (loop for j from 0 to (- cols 1) do
	(setf (aref board i j) #\.)))))

(defmethod click ((ui board-ui) i j)
  (with-slots (board controller) ui
    (click controller i j)))

(defmethod click ((ui simple-ui) i j)
  (with-slots (board controller) ui
    (click controller i j)
    (loop for row in (2d-array-to-list board) do
      (format t "~a~%" (concatenate 'string row)))))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
	collect (loop for j below (array-dimension array 1)
		      collect (aref array i j))))

(defun setup-style ()
  (send-wish "ttk::style configure Bomb.TButton -relief sunken -background red -disabledbackground red")
  (send-wish "ttk::style configure Pressed0.TButton -relief sunken")
  (send-wish "ttk::style configure Pressed1.TButton -relief sunken -foreground blue -disabledforeground blue")
  (send-wish "ttk::style configure Pressed2.TButton -relief sunken -foreground green -disabledforeground green")
  (send-wish "ttk::style configure Pressed3.TButton -relief sunken -foreground red -disabledforeground red")
  (send-wish "ttk::style configure Pressed4.TButton -relief sunken -foreground brown -disabledforeground brown")
  (send-wish "ttk::style configure Pressed5.TButton -relief sunken -foreground orange -disabledforeground orange")
  (send-wish "ttk::style configure Pressed6.TButton -relief sunken -foreground violet -disabedforeground violet")
  (send-wish "ttk::style configure Pressed7.TButton -relief sunken -foreground black -disabledforeground black"))

(defmethod game-over ((ui ltk-ui) f)
  (with-slots (board rows cols content) ui
    (loop for i from 0 to (- rows 1) do
      (loop for j from 0 to (- cols 1) do
	(configure (aref board i j) :state :disabled)))
    (with-slots (x y) f
      (let ((b (aref board x y)))
	(setf (text b) #\*)
	(configure b :style "Bomb.TButton" :state :disabled)))
    (if (ask-yesno "Start New Game?" :title "Game Over") (get-params content #'go-board))))
	
(defmethod display ((ui ltk-ui) f)
  (with-slots (board) ui
    (with-slots (is-mine x y mines-count) f
       (let ((b (aref board x y)))
	(cond ((and is-mine) (progn
			      (setf (text b) #\*)
			      (configure b :style "Bomb.TButton")))
	      ((= mines-count 0) (configure b :style "Pressed0.TButton"))
	      (t (progn
		   (setf (text b) mines-count)
		   (configure b :style (format nil "Pressed~a.TButton" mines-count)))))))))

(defmethod display ((ui simple-ui) f)
  (with-slots (board) ui
    (with-slots (mines-count x y) f
      (setf (aref board x y) (char (write-to-string (slot-value f 'mines-count)) 0)))))


(defun get-params (frame cont)
    (let* ((tl (make-instance 'toplevel))
	  (content (make-instance 'frame :master tl)))
      (configure content :padding "3 3 12 12")
      (grid content 0 0 :sticky "nsew")
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid-rowconfigure *tk* 0 :weight 1)
      (let* (
	     (r (make-instance 'entry  :master content :text "10"))
	     (rl (make-instance 'label :master content :text "rows:"))
	     (c (make-instance 'entry :master content :text "10"))
	     (cl (make-instance 'label :master content :text "columns:"))	     
	     (m (make-instance 'entry :master content :text "10"))
	     (ml (make-instance 'label :master content :text "mines:"))
	     (b (make-instance 'button :master content
				       :text "OK"
				       :command (lambda ()
				(grab-release tl)
				(funcall cont
					 (read-from-string (text r))
					 (read-from-string (text c))
					 (read-from-string (text m))
					 frame)
				(destroy tl)))))
	(grid rl 1 1)
	(grid r 1 2)
	(grid cl 2 1)
	(grid c 2 2)
	(grid ml 3 1)
	(grid m 3 2)
	(grid b 4 2)
	(grab tl))))

(defun go-board (rows cols mines content)
  (format t "~a~a~a~a" rows cols mines content)
  
  (start (make-instance 'board-controller :rows rows :cols cols :mines mines :content content)))
  

(defun main ()
  (with-ltk ()
    ;(setf *debug-tk t)
    (wm-title *tk* "Minesweeper")
    (let ((header (make-instance 'frame))
	  (content (make-instance 'frame)))
      (configure header :padding "3 3 12 12")
      (configure content :padding "3 3 12 12")
      (grid header 0 0 :sticky "nsew")
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid-rowconfigure *tk* 0 :weight 1)
      (grid-columnconfigure content 0 :weight 1)
      (grid-rowconfigure content 0 :weight 1)
      (grid header 0 0)
      (grid content 1 0)
      (grid (make-instance 'button :master header
				   :text "New Game"
				   :command (lambda () (get-params content #'go-board)))
	    0 0))))
