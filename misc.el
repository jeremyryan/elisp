;;; misc.el --- Miscellaneous utility functions -*- lexical-binding: t; -*-


(defun org-show-elapsed-since-timestamp ()
  "Show how much time has passed since the Org timestamp at point."
  (interactive)
  (let* ((ts (org-element-context))
         (type (org-element-type ts)))
    (unless (memq type '(timestamp))
      (user-error "No Org timestamp at point"))
    (let* ((start (org-element-property :raw-value ts))
           (time (org-time-string-to-time start))
           (now (current-time))
           (secs (float-time (time-subtract now time)))
           (hours (/ secs 3600))
           (mins (/ (mod secs 3600) 60)))
      (message "Elapsed time: %.0f hours, %.0f minutes" hours mins))))


(defun org-show-elapsed-since-timestamp ()
  "Show how much time has passed since the Org timestamp at point."
  (interactive)
  (let* ((ts (org-element-context))
         (type (org-element-type ts)))
    (unless (memq type '(timestamp))
      (user-error "No Org timestamp at point"))
    (let* ((start (org-element-property :raw-value ts))
           (time (org-time-string-to-time start))
           (now (current-time))
           (secs (float-time (time-subtract now time)))
           (hours (/ secs 3600))
           (mins (/ (mod secs 3600) 60)))
      (message "Elapsed time: %.0f hours, %.0f minutes" hours mins))))


(defun insert-filenames-from-directory (dir)
  "Insert the names of all non-hidden files in DIR into the current buffer."
  (interactive "DDirectory: ")
  (dolist (f (directory-files dir nil "^[^.].*" t))
    (insert f "\n")))


(defun run-shell-command-to-new-buffer (command)
  "Prompt for a shell COMMAND, run it, and put the output in a new buffer."
  (interactive "sShell command: ")
  (let* ((buf-name "*Shell Command Output*")
         (buf (generate-new-buffer buf-name)))
    (with-current-buffer buf
      (insert (shell-command-to-string command)))
    (pop-to-buffer buf)))


(defun insert-color-from-list ()
  "Display colors and insert the selected color name into the current buffer.
Calls `list-colors-display', and when RET is pressed on a color,
inserts that color name into the buffer from which this function was called."
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (list-colors-display nil "*Select Color*" (lambda (color-name)
						(kill-buffer "*Select Color*")
						(pop-to-buffer orig-buffer)
						(insert color-name)))))
    ;; (pop-to-buffer "*Colors*")
    ;; (local-set-key (kbd "RET")
    ;;                (lambda ()
    ;;                  (interactive)
    ;;                  (let ((btn (button-at (point))))
    ;;                    (if btn
    ;;                        (let ((color (button-label btn)))
    ;;                          (kill-buffer "*Colors*")
    ;;                          (pop-to-buffer orig-buffer)
    ;;                          (insert color))
    ;;                      (user-error "No color at point")))))))
