;; -*- lexical-binding: t; -*- ;
(require 'dash)
(require 's)
(require 'cl)

;;; Define mode
(defvar turing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'turing-execute)
    map))

(defvar turing-highlights '((";.*" . font-lock-comment-face)
                            ("^[^ ]+ [^ ]+" . font-lock-keyword-face)))

(define-derived-mode turing-mode prog-mode "turing"
  "Major mode for editing turing machine code."
  (setq font-lock-defaults '(turing-highlights))
  (setq comment-start ";"))

(add-hook 'turing-mode-hook (lambda () (highlight-numbers-mode -1)))


;;; Define turing machine.
(defface turing-current-face
  `((t (:foreground ,(face-attribute 'default :background) :background ,(face-attribute 'default :foreground) :height 200)))
  "WAT")

(defface turing-tape-face
  `((t (:height 200)))
  "WOT")

;; Set up an empty hash table of commands
(defvar turing--commands (make-hash-table :test #'equal))

(defun turing--buffer-to-hash ()
  "Parse the current buffer into a hash table of cammands."
  (interactive)
  ;; Clear the table first.
  (clrhash turing--commands)
  (let* ((file-string (buffer-substring-no-properties (point-min) (point-max)))
         (file-lines (-remove #'turing--invalid-line (split-string file-string "\n")))
         (command-list (mapcar #'turing--line-to-command file-lines)))
    (dolist (command command-list)
      (puthash (car command) (cadr command) turing--commands))
    turing--commands))

(defun turing--invalid-line (line)
  "Strip all comment lines."
  (or (string-empty-p line) (s-prefix? ";" (string-trim line))))

(defun turing--line-to-command (line)
  "Turn each line into a grouped list like: `((a b) (c d e))'."
  (let ((elems (split-string (string-trim (car (split-string line ";"))) " ")))
    (list (reverse (nthcdr 3 (reverse elems))) (nthcdr 2 elems))))

;; (defun turing--find-val)

(defun turing-execute ()
  "Run the turing machine."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((commands (turing--buffer-to-hash))
           (tape (-remove #'string-empty-p
                          (split-string
                           (concat
                            "_"
                            (string-trim
                             (or (progn (search-forward-regexp ";; INITIAL:\\(.*\\)" nil t)
                                        (match-string-no-properties 1))
                                 "0"))
                            "_")
                           "")))
           (rate (string-to-number
                  (string-trim
                   (or (progn (search-forward-regexp ";; RATE:\\(.*\\)" nil t)
                              (match-string-no-properties 1))
                       "0"))))
           (place 1)
           (state "0")
           (key (list "0" (cadr tape)))
           (wild-key (list state "*")))
      ;; If we still have a command associated with key
      (while (and (or (gethash key commands)
                      (gethash wild-key commands))
                  (not (s-prefix? "halt" (car key))))
        ;; Update rate
        (redisplay t)
        (sleep-for rate)
        ;; Get things to do from hash table
        (cl-multiple-value-bind (new-char action new-state) (if (gethash key commands)
                                                                (gethash key commands)
                                                              (gethash wild-key commands))
          ;; Update the tape accordingly
          (setf (nth place tape)
                (if (not (string= new-char "*"))
                    new-char
                  (nth place tape)))
          (cond ((string= action "l")
                 (setq place (1- place))
                 (when (= place -1) ; Handle moving past beginning.
                   (push "_" tape)
                   (setq place 0)))
                ((string= action "r")
                 (setq place (1+ place))
                 (when (= place (length tape)) ; Handle moving past end.
                   (setq tape (append tape '("_"))))))

          ;; Update the current state/key/wild
          (setq state new-state)
          (setq key (list state (nth place tape)))
          (setq wild-key (list state "*"))
          ;; Make visualization
          (let ((tape-viz (copy-tree tape)))
            (with-current-buffer-window
             (get-buffer-create "*turing-machine*")
             nil
             nil
             ;; (setf (nth place tape-viz)
             ;;       (propertize (nth place tape-viz) 'face 'turing-current-face))
             (erase-buffer)
             (insert
              (concat
               (propertize
                (string-join (-slice tape-viz 0 place))
                'face
                'turing-tape-face)
               (propertize
                (nth place tape-viz)
                'face
                'turing-current-face)
               (propertize
                (string-join (-slice tape-viz (1+ place) (length tape-viz)))
                'face
                'turing-tape-face)))))))
      (if(not (s-prefix? "halt" (car key)))
          (message "No rule for state '%s' and char '%s'" state (nth place tape))
        (message "Done!")))))

;; INITIAL: 1111
