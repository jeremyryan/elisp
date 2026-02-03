;;; daily.el --- Daily file management utilities -*- lexical-binding: t; -*-

(defvar daily-org-dir "~/org"
  "Path to the directory where org files are stored")

;; Global variable for the template file path
(defvar daily-template-file (concat daily-org-dir "/Templates/daily-template.org")
  "Path to the template file used when creating new daily files.")

;; Global variable for the archive directory
(defvar daily-archive-directory (concat daily-org-dir "/Archive")
  "Base directory for archiving daily files.
Files will be organized as <archive>/YYYY/MM.")

(defun daily-create-file (target-directory)
  "Create a new daily file in TARGET-DIRECTORY from the template.
The file is named 'today_MM_DD_YYYY.org' with today's date."
  (interactive "DTarget directory: ")
  (unless daily-template-file
    (user-error "daily-template-file is not set"))
  (unless (file-exists-p daily-template-file)
    (user-error "Template file does not exist: %s" daily-template-file))
  (unless (file-directory-p target-directory)
    (user-error "Target directory does not exist: %s" target-directory))

  ;; Get today's date
  (let* ((date-list (decode-time))
         (year (nth 5 date-list))
         (month (nth 4 date-list))
         (day (nth 3 date-list))
         (filename (format "today_%02d_%02d_%04d.org" month day year))
         (new-file-path (expand-file-name filename target-directory))
         (symlink-path (expand-file-name "today.org" target-directory))
         (template-content (with-temp-buffer
                             (insert-file-contents daily-template-file)
                             (buffer-string))))

    (if (file-exists-p new-file-path)
        (user-error "File already exists: %s" new-file-path)
      (write-region template-content nil new-file-path)
      (message "Created daily file: %s" new-file-path)

      ;; Create or update the "today.org" symlink
      (when (file-exists-p symlink-path)
        (delete-file symlink-path))
      (make-symbolic-link filename symlink-path)
      (message "Created symlink: %s -> %s" symlink-path filename)

      (find-file new-file-path))))

(defun daily-open-today-file ()
  "Open the file pointed to by the today.org symlink in daily-org-dir.
If today.org does not exist, create a new daily file."
  (interactive)
  (let* ((today-org-path (expand-file-name "today.org" daily-org-dir))
         (target-file (if (file-exists-p today-org-path)
                          (expand-file-name (file-symlink-p today-org-path) daily-org-dir)
                        nil)))
    (if (and target-file (file-exists-p target-file))
        (find-file target-file)
      (daily-create-file daily-org-dir))))

(defun daily-archive-current-file ()
  "Move the current buffer's file to the archive directory based on its creation date.
The file will be moved to <archive>/YYYY/MM/ directory."
  (interactive)
  (unless daily-archive-directory
    (user-error "daily-archive-directory is not set"))
  (unless (buffer-file-name)
    (user-error "Current buffer is not associated with a file"))

  (let* ((current-file (buffer-file-name))
         (file-attrs (file-attributes current-file))
         (mod-time (nth 5 file-attrs))
         (time-list (decode-time mod-time))
         (year (nth 5 time-list))
         (month (nth 4 time-list))
         (filename (file-name-nondirectory current-file))
         (archive-subdir (expand-file-name
                          (format "%04d/%02d" year month)
                          daily-archive-directory))
         (new-file-path (expand-file-name filename archive-subdir)))

    ;; Create the archive subdirectory if it doesn't exist
    (unless (file-directory-p archive-subdir)
      (make-directory archive-subdir t)
      (message "Created archive directory: %s" archive-subdir))

    ;; Check if file already exists in archive
    (if (file-exists-p new-file-path)
        (user-error "File already exists in archive: %s" new-file-path)
      ;; Move the file
      (rename-file current-file new-file-path)
      (message "Archived file to: %s" new-file-path)
      ;; Close the buffer if the file was moved
      (kill-buffer (current-buffer)))))

(defun daily-clear-today-file ()
  "Clear clocked work time and archive completed TODO items in the today file.
Removes all CLOCK entries and empty LOGBOOK drawers, then archives
any entries that are in a done state."
  (interactive)
  (require 'org)
  (let* ((today-org-path (expand-file-name "today.org" daily-org-dir))
         (target-file (if (file-exists-p today-org-path)
                          (expand-file-name (file-symlink-p today-org-path) daily-org-dir)
                        nil)))
    (unless (and target-file (file-exists-p target-file))
      (user-error "Today file does not exist"))

    (find-file target-file)

    ;; Clear clocked time - remove CLOCK lines
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*CLOCK:.*\n" nil t)
        (replace-match "")))

    ;; Remove empty LOGBOOK drawers
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ":LOGBOOK:[ \t]*\n[ \t]*:END:[ \t]*\n?" nil t)
        (replace-match "")))

    ;; Archive done items - collect positions first, then archive from bottom up
    (let (done-positions)
      (org-map-entries
       (lambda ()
         (when (org-entry-is-done-p)
           (push (point-marker) done-positions)))
       nil
       'file)
      ;; Archive from bottom to top to preserve positions
      (dolist (pos done-positions)
        (goto-char pos)
        (org-archive-subtree)
        (set-marker pos nil))
      (message "Cleared clocks and archived %d done items" (length done-positions)))))

(provide 'daily)
;;; daily.el ends here
