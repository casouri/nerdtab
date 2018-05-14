;;; nerdtab.el --- A sidebar of tabs

;;; Commentary:
;;


;;; Code:

;;
;; Customizations
;;

(defgroup nerdtab nil
  "Customizations of nerdtab."
  :prefix "nerdtab-"
  :group 'files)

(defcustom nerdtab-window-position 'left
  "The position of nerdtab window."
  :group 'nerdtab
  :type '(choice (const left)
                 (const right)
                 (const top)
                 (const bottom)))

(defcustom nerdtab-mode-line-format nil
  "Mode-line format of nerdtab buffer."
  :group 'nerdtab
  :type 'sexp)

(defcustom nerdtab-tab-width 15
  "Width of nerdtab tab."
  :group 'nerdtab
  :type 'number)

(defcustom nerdtab-max-tab-vertical 50
  "Maximum number of tabs when displayed vertically."
  :group 'nerdtab
  :type 'number)

(defcustom nerdtab-max-tab-horizontal 20
  "Maximum number of tabs when displayed horizontally."
  :group 'nerdtab
  :type 'number)

(defcustom nerdtab-regex-blacklist '("\\*.*\\*")
  "The regex blacklist of buffer names.
Nerdtab does not list buffers that match any regex in this blacklist."
  :group 'nerdtab
  :type 'sexp)


;;
;; Variables
;;

(defface nerdtab-tab-face
  '((t (:inherit default :underline nil)))
  "Face of tabs in nerdtab buffer."
  :group 'nerdtab)

(defface nerdtab-tab-mouse-face
  '((t (:inherit highlight :underline nil)))
  "Face of tabs under mouse in nerdtab buffer."
  :group 'nerdtab)

(defvar nerdtab--tab-list ()
  "A list of tabs.
Each tab is of form: (buffer-display-name buffer).
buffer-display-name is turncated if too long, so don't depend on it.
buffer is a buffer object,
for simplicity it can only be a buffer object,
not a string of buffer name.

Also note that the order of buffers in `nerdtab--tab-list'
does not nessesarily match that in (buffer-list).

Because that order in (buffer-list) changes all the time,
and I want my tab list to be more stable.
So user can expect the index of a tab to not change very often.

`nerdtab-refresh' syncs both list.")

(defvar nerdtab--window nil
  "Nerdtab window.")

(defvar nerdtab--buffer nil
  "Nerdtab buffer.")

(defvar nerdtab-buffer-name "*nerdtab*"
  "Name of nerdtab buffer.")

;;
;; Modes
;;

(defvar nerdtab-major-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") #'nerdtab-quit)
    keymap)
  "Keymap of nerdtab major mode.")

(define-derived-mode nerdtab-major-mode special-mode
  "NerdTab")

(define-minor-mode nerdtab-mode
  "A global minor mode that controls nerdtab hooks."
  :global t
  (if nerdtab-mode
      (progn
        (nerdtab--show-ui)
        (nerdtab-refresh)
        (nerdtab--install-advice))
    (nerdtab--remove-advice)
    (kill-buffer nerdtab--buffer)
    (setq nerdtab--buffer nil)
    (delete-window nerdtab--window)
    (setq nerdtab--window nil)))

;;
;; Functions -- sort of inverse hiearchy, the final function that calls everyone else is in the bottom.
;;

(defun nerdtab--make-tab (buffer)
  "Make a tab from BUFFER."
  `(,(nerdtab-turncate-buffer-name (buffer-name buffer))
    ,buffer))

(defmacro nerdtab--h-this-v-that| (this that)
  "Do THIS is nerdtad is horizontal, THAT if vertical.
The macro checks `nerdtab-window-position',
and only check it against 'top and 'bottom,
it assumes other case means vertical.

THIS and THAT have to ba lists of sexps to be evaluate."
  `(if (member nerdtab-window-position '(top bottom)) ; horizontal
      ,@this
    ,@that))

(defun nerdtab--get-buffer-or-create ()
  "Get nerdtab buffer, create one if not exist."
  (get-buffer-create nerdtab-buffer-name))

(defun nerttab--get-window-or-create ()
  "Select nerdtab window, create one if not exist.
Return original window for convience."
  (let ((original-window (selected-window)))
    (if (window-live-p nerdtab--window)
        original-window
      (pcase nerdtab-window-position
        ('top (let
                ((original-window (split-window-below 2)))
                (setq nerdtab--window (selected-window))
                original-window))
        ('bottom (progn
                   (setq nerdtab--window
                         (select-window (split-window-below 2)))
                   original-window))
        ('left (let ((original-window (split-window-right nerdtab-tab-width)))
                 (setq nerdtab--window (selected-window))
                 original-window))
        ('right (progn
                  (setq nerdtab--window
                        (select-window (split-window-right nerdtab-tab-width)))
                  original-window))))))

;; (defun nerdtab-turncate-buffer-name (buffer-name)
;;   "Make sure BUFFER-NAME is short enough."
;;   (let ((max-width (- nerdtab-tab-width 3))) ; numbering take 2-3 char
;;     (if (< (length buffer-name) max-width)
;;         buffer-name
;;       ;; half-length is the length of each half of the name
;;       ;; someloooooo...ooooongname
;;       ;; ^   this  ^   ^ & this  ^
;;       ;;              pos3    pos4
;;       ;; these poses are indexes of substring
;;       (let* ((half-length (- (/ max-width 2) 2))
;;              (pos3 (- (length buffer-name) half-length))
;;              (pos4 (length buffer-name)))
;;         (format "%s...%s"
;;                 (substring buffer-name 0 (1- half-length))
;;                 (substring buffer-name pos3 pos4))))))

(defun nerdtab-turncate-buffer-name (buffer-name)
  "Make sure BUFFER-NAME is short enough."
  (let ((max-width (- nerdtab-tab-width 3))
        (name-length (length buffer-name)))
    (if (< name-length max-width)
        buffer-name
      (format "%s.." (substring buffer-name 0 (- max-width 3))))))

(defun nerdtab--if-valid-buffer (buffer)
  "Check if BUFFER is suitable for a tab.
If yes, return t, otherwise return nil."
  (let ((black-regex (string-join nerdtab-regex-blacklist "\\|")))
    (if (and (not (equal black-regex "")) (string-match black-regex (buffer-name buffer)))
        nil
      t)))

(defun nerdtab--show-ui ()
  "Get nerdtab window and buffer displayed.
This function makes sure both buffer and window are present."
  (let ((original-window (nerttab--get-window-or-create)))
    (select-window nerdtab--window)
    (setq nerdtab--buffer (switch-to-buffer (nerdtab--get-buffer-or-create)))
    (set-window-buffer nerdtab--window nerdtab--buffer)
    (nerdtab-major-mode)
    (setq mode-line-format nil)
    (when (featurep 'linum) (linum-mode -1))
    (when (featurep 'nlinum) (nlinum-mode -1))
    (when (featurep 'display-line-numbers) (display-line-numbers-mode -1))
    (select-window original-window)
    ))

(defun nerdtab--draw-tab (tab index)
  "Draw a single TAB, marked with INDEX, as a button in current buffer.
This function doesn't insert newline.
The button lookes like: 1 *Help*.
You can see index is at the beginning."
  (let ((tab-name (car tab))
        (buffer (nth 1 tab)))
    (insert-text-button (format "%d %s" index tab-name)
                   'action
                   `(lambda (_)
                     (display-buffer ,buffer))
                   'help-echo
                   (buffer-name buffer)
                   'follow-link
                   t
                   'face
                   'nerdtab-tab-face
                   'mouse-face
                   'nerdtab-tab-mouse-face
                   ;; TODO not working
                   'line-height
                   1.5)))

(defun nerdtab--redraw-all-tab ()
  "Redraw every tab in `nerdtab-buffer'."
  (interactive)
  (nerdtab--show-ui)
  (let ((original-window (selected-window)))
    (select-window nerdtab--window)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((index 0))
      (dolist (tab nerdtab--tab-list)
        (nerdtab--draw-tab tab index)
        (setq index (1+ index))
        (insert (nerdtab--h-this-v-that| ("  ") ("\n"))))
      (goto-char 0))
    (setq buffer-read-only t)
    (select-window original-window)))

(defun nerdtab-full-update-tab-list ()
  "Make a complete refresh of `nerdtab--tab-list'."
  (let ((tab-list ())
        (max-tab-num (nerdtab--h-this-v-that|
                  (nerdtab-max-tab-horizontal)
                  (nerdtab-max-tab-vertical)))
        (count 0))
    (catch 'max-num
      (dolist (buffer (buffer-list))
        (when (nerdtab--if-valid-buffer buffer)
          (when (>= count max-tab-num)
            (throw 'max-num count))
          (setq count (1+ count))
          (push (nerdtab--make-tab buffer)
                tab-list))))
    (setq nerdtab--tab-list tab-list)))

(defun nerdtab-refresh ()
  "Refresh nerdtab buffer.
This function syncs tab list and (buffer-list),
which most likely will change the order of your tabs.
So don't use it too often."
  (interactive)
  (nerdtab-full-update-tab-list)
  (nerdtab--redraw-all-tab))

;; (defun nerdtab-refresh-on-hook (&rest _)
;;   "Refresh in 0.5 seconds."
;;   ;; to avoid recurisve calling
;;   (advice-remove #'get-buffer-create #'nerdtab-refresh-on-hook)
;;   (run-with-idle-timer
;;    0.5 1
;;    (lambda ()
;;      (nerdtab-refresh)
;;      (when nerdtab-mode
;;        (advice-remove #'get-buffer-create #'nerdtab-refresh-on-hook))
;;      )))

;;
;; Advices (add, rename, kill)
;;

(defun nerdtab--add-buffer (buffer)
  "Add BUFFER to `nerdtab--tab-list'."
  (when (nerdtab--if-valid-buffer buffer)
    (add-to-list
     'nerdtab--tab-list
     (nerdtab--make-tab buffer)
     t))
  buffer)

(defun nerdtab--add-buffer-advice (oldfunc buffer-or-name)
  "Advice around `get-buffer-create' (OLDFUNC). BUFFER-OR-NAME.
Add new buffer to `nerdtab--tab-list'."
  (let ((buffer (nerdtab--add-buffer (apply oldfunc (list buffer-or-name)))))
    (nerdtab--redraw-all-tab)
    buffer))

(defun nerdtab--remove-buffer (buffer)
  "Remove BUFFER from `nerdtab--tab-list'."
  (let ((tab (nerdtab--make-tab buffer)))
    (when (member tab nerdtab--tab-list)
      (delete tab nerdtab--tab-list))))

(defun nerdtab--remove-buffer-advice (oldfunc &optional buffer-or-name)
  "Advice around `kill-buffer' (OLDFUNC). BUFFER-OR-NAME.
Add new buffer to `nerdtab--tab-list'."
  (let ((buffer-to-kill (if buffer-or-name
                            (get-buffer-create buffer-or-name)
                          (current-buffer))))
    (nerdtab--remove-buffer buffer-to-kill)
    (nerdtab--redraw-all-tab)
    (apply oldfunc (list buffer-or-name))))

(defun nerdtab--rename-buffer (new-name)
  "Rename a tab to NEW-NAME."
  (let ((old-tab (nerdtab--make-tab (current-buffer)))
        (index 0))
    (dolist (tab nerdtab--tab-list)
      (when (equal tab old-tab)
        (setf (nth index nerdtab--tab-list) `(,(nerdtab-turncate-buffer-name
                                                new-name)
                                              ,(current-buffer))))
      (setq index (1+ index))))
  new-name)

(defun nerdtab--rename-buffer-advice (oldfunc new-name &optional unique)
  "Advice around `rename-buffer' (OLDFUNC). NEW-NAME. UNIQUE.
Add new buffer to `nerdtab--tab-list'."
  (let ((new-name (nerdtab--rename-buffer (apply oldfunc (list new-name unique)))))
    (nerdtab--redraw-all-tab)
    new-name))

(defun nerdtab--install-advice ()
  "Install advices."
  (advice-add 'get-buffer-create :around #'nerdtab--add-buffer-advice)
  (advice-add 'kill-buffer       :around #'nerdtab--remove-buffer-advice)
  (advice-add 'rename-buffer     :around #'nerdtab--rename-buffer-advice))

(defun nerdtab--remove-advice ()
  "Remove advices."
  (advice-remove 'get-buffer-create #'nerdtab--add-buffer-advice)
  (advice-remove 'kill-buffer       #'nerdtab--remove-buffer-advice)
  (advice-remove 'rename-buffer     #'nerdtab--rename-buffer-advice))

(provide 'nerdtab)

;;; nerdtab.el ends here
