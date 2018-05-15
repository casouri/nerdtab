;;; nerdtab.el --- A sidebar of tabs

;;; Commentary:
;; This package gives you tabs.
;; But instead of the normal GUI tabs you might think of,
;; it provides a more keyboard-oriented (and visually inadequate) tabs.
;; You can jump to a specific buffer by `nerdtab-jump-<number>'
;; or by clicking the tab.

;; `nerdtab-mode' is a global minor mode.
;; Turn it on and it will open a side window and display buffers as tabs for you.

;; You may notice a slight delay between change in buffer list
;; and update in nerdtab window.
;; That is because I use a timer to invoke tab list updates
;; because 1) you probably don't need the buffer to show up in list
;; immediatly 2) it protects Emacs from crashing when it opens 10000 buffers at once.
;;
;; If there are people that actually use this package
;; and someone actually cares about that lag,
;; I can add a mode or an option.

;; Conventions: nerdtab-- means private, nerdtab- means public / customizable (for variables)


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

(defcustom nerdtab-mode-line-format '(" ")
  "Mode-line format of nerdtab buffer."
  :group 'nerdtab
  :type 'plist)

(defcustom nerdtab-tab-width 15
  "Width of nerdtab tab."
  :group 'nerdtab
  :type 'number)

(defcustom nerdtab-tab-height 1
  "Height of tabs if `nerdtab-window-position' is 'top or 'bottom."
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

(defcustom nerdtab-regex-blacklist '("\\*.*\\*" "^magit.*")
  "The regex blacklist of buffer names.
Nerdtab does not list buffers that match any regex in this blacklist."
  :group 'nerdtab
  :type 'sexp)

(defcustom nerdtab--update-interval 2
  "Nerdtab checkes if it needs to update tab list in every this seconds."
  :group 'nerdtab
  :type 'number)


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

`nerdtab-full-refresh' syncs both list.")

(defvar nerdtab--window nil
  "Nerdtab window.")

(defvar nerdtab--buffer nil
  "Nerdtab buffer.")

(defvar nerdtab-buffer-name "*nerdtab*"
  "Name of nerdtab buffer.")

(defvar nerdtab--do-update nil
  "If non-nil, nerdtab will update tab list in next cycle.
Time interval between to cycle is defined by `nerdtab--update-interval'.")

(defvar nerdtab--timer nil
  "The object that is used to disable timer.")

(defvar nerdtab-open-func #'switch-to-buffer
  "The function to open buffer.
Used in tab button and nerdtab-jump functions.

The function should take a singgle buffer as argument.")

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
        (nerdtab-full-refresh)
        (add-hook 'buffer-list-update-hook #'nerdtab--update-next-cycle)
        (setq nerdtab--timer (run-with-timer 1 nerdtab--update-interval #'nerdtab--timer-update)))
    (remove-hook 'buffer-list-update-hook #'nerdtab--update-next-cycle)
    (kill-buffer nerdtab--buffer)
    (setq nerdtab--buffer nil)
    (delete-window nerdtab--window)
    (setq nerdtab--window nil)
    (cancel-timer nerdtab--timer)))

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
  (let ((original-window (selected-window)))
    (setq nerdtab--buffer (nerdtab--get-buffer-or-create))
    (if (window-live-p nerdtab--window)
        (progn
          (select-window nerdtab--window)
          (switch-to-buffer nerdtab--buffer))
      (select-window
       (setq nerdtab--window
             (display-buffer-in-side-window
              nerdtab--buffer
              `((side . ,nerdtab-window-position)
                ,(nerdtab--h-this-v-that|
                  (`(window-height . ,nerdtab-tab-height))
                  (`(window-width . ,nerdtab-tab-width)))))))
      (switch-to-buffer nerdtab--buffer))
    (nerdtab-major-mode)
    (setq mode-line-format nerdtab-mode-line-format)
    (nerdtab--h-this-v-that|
     ((window-preserve-size nil t)
      (setq-local line-spacing 5))
     ((window-preserve-size)
      (setq-local line-spacing 3)))
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
                     (funcall nerdtab-open-func ,buffer))
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

(defun nerdtab--make-tab-list ()
  "Make a tab list from `nerdtab--tab-list'."
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
    tab-list))

(defun nerdtab-full-refresh ()
  "Refresh nerdtab buffer.
This function syncs tab list and (buffer-list),
which most likely will change the order of your tabs.
So don't use it too often."
  (interactive)
  (setq nerdtab--tab-list (nerdtab--make-tab-list))
  (nerdtab--show-ui)
  (nerdtab--redraw-all-tab))

(defun nerdtab--update-tab-list ()
  "Update nerdtab list upon buffer creation, rename, delete."
  (let ((new-list (nerdtab--make-tab-list))
        (old-list nerdtab--tab-list)
        (return-list ()))
    (dolist (old-tab old-list)
      (when (member old-tab new-list)
        (add-to-list 'return-list old-tab t)
        (delete old-tab new-list)))
    (dolist (remaining-new-tab new-list)
      (add-to-list 'return-list remaining-new-tab t))
    (setq nerdtab--tab-list return-list)))

;; (defun nerdtab--update-on-hook (&rest _)
;;   "Update in 0.5 seconds."
;;   ;; to avoid recurisve calling
;;   (remove-hook 'buffer-list-update-hook #'nerdtab--update-on-hook)
;;   (run-with-idle-timer
;;    0.5 1
;;    (lambda ()
;;      (nerdtab--show-ui)
;;      (nerdtab--update-tab-list)
;;      (nerdtab--redraw-all-tab)
;;      (when nerdtab-mode
;;        (add-hook 'buffer-list-update-hook #'nerdtab--update-on-hook))
;;      )))

(defun nerdtab--update-next-cycle (&optional do)
  "Make nerdtab update tab list on next cycle.
If DO is non-nil, make it not to."
  (setq nerdtab--do-update (not do)))

(defun nerdtab-update ()
  "Update nerdtab tab list."
  (nerdtab--show-ui)
  (nerdtab--update-tab-list)
  (nerdtab--redraw-all-tab)
  (nerdtab--update-next-cycle -1))

(defun nerdtab--timer-update ()
  "Update when needs to."
  (when nerdtab--do-update
    (nerdtab-update)))

(defun nerdtab-jump (index)
  "Jump to INDEX tab."
  (interactive "nIndex of tab: ")
  (funcall nerdtab-open-func (nth 1 (nth index nerdtab--tab-list))))

(defun nerdtab-make-jump-func (max)
  "Make `nerdtab-jump-n' functions from 1 to MAX."
  (dolist (index (number-sequence 0 max))
    (fset (intern (format "nerdtab-jump-%d" index))
          `(lambda () ,(format "Jump to %sth tab." index)
             (interactive)
             (nerdtab-jump ,index)))))


(nerdtab-make-jump-func 50)

(provide 'nerdtab)

;;; nerdtab.el ends here
