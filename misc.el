;;; misc.el --- Miscellaneous utility functions -*- lexical-binding: t; -*-

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

