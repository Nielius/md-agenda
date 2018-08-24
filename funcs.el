(defun md-agenda--get-file-name (desc)
  "Return the file name for my agenda file for a specific date.
Can be either a symbol ('tomorrow, 'today, 'week), or the number
of days from today (positive, negative or zero)."
  (if (integerp desc)
      (format-time-string "%Y-W%V-%u.md" (time-add (current-time) (* desc 24 60 60)))
    (case desc
      ('today (md-agenda--get-file-name 0))
      ('tomorrow (md-agenda--get-file-name 1))
      ('yesterday (md-agenda--get-file-name -1))
      ('next-day
       (format-time-string "%Y-W%V-%u.md"
                           (apply #'md-agenda--year-week-day-to-time
                            (mapcar* #'+ '(0 0 1) (md-agenda--year-week-day-of-current-file)))))
      ('previous-day
       (format-time-string "%Y-W%V-%u.md"
                           (apply #'md-agenda--year-week-day-to-time
                            (mapcar* #'+ '(0 0 -1) (md-agenda--year-week-day-of-current-file)))))
      ('previous-week
       (format-time-string "%Y-W%V.md"
                           (apply #'md-agenda--year-week-day-to-time
                            (mapcar* #'+ '(0 -1 0) (md-agenda--year-week-day-of-current-file)))))
      ('next-week
       (format-time-string "%Y-W%V.md"
                           (apply #'md-agenda--year-week-day-to-time
                                  (mapcar* #'+ '(0 1 0) (md-agenda--year-week-day-of-current-file)))))
      ('week (format-time-string "%Y-W%V.md"))
      ('week-links (format-time-string "%Y-W%V-links.md"))
      ('week-review (format-time-string "%Y-W%V-review.md")))))

;; A lot of this is going back and forth between different ways to represent the
;; date... There should probably be a nicer pattern for this.

(defun md-agenda--year-week-day-to-time (year weekn dayn)
  "Convert a date given by year, week number and day number to an
emacs time format. The week number and days can be out of range
(i.e., day can be 8 or -12 or...) and it should still work. "
  (encode-time 0 0 0 (+ dayn (* 7 (- weekn 1))) 1 year))

(defun md-agenda--year-week-day-of-current-file ()
  "Get the year, week and day of the current file,
assuming the file name is of the form '%Y-W%V-%u-.....md'.
Returns '(YEAR WEEK DAY)."
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (list (string-to-number (substring filename 0 4))
          (string-to-number (substring filename 6 8))
          (let ((n (string-to-number (substring filename 9 10))))
            (if (= n 0) 1 n)))))

(defun md-agenda-rename-this-file (new-suffix)
  "Rename the \"timestamp\" of this file:
a file called 2018-W31-1-123412.md can be renamed to
2018-W31-1-somethingelse.md."
  ;; adapted from spacemacs/rename-current-buffer-file ()
  (interactive "sNew suffix: ")
  (let*
      ((filename (buffer-file-name))
       (day-string
        (if filename ; could be nil if buffer not visiting file
            (md--get-day-from-filename filename) nil))
       (ext
        (if filename (file-name-extension filename) nil))
       (new-filename (concat day-string "-" new-suffix "." ext)))
    (cond
     ((not day-string) (error "Not visiting a correct file"))
     ((get-buffer new-filename)
      (error "A buffer named %s already exists" new-filename))
     (t
      (progn
        (rename-file filename new-filename 1)
        (rename-buffer new-filename)
        (set-visited-file-name new-filename)
        (set-buffer-modified-p nil)
        (when (fboundp 'recentf-add-file)
          (recentf-add-file new-filename)
          (recentf-remove-if-non-kept filename))
        (when (and (configuration-layer/package-usedp 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully renamed to '%s'" filename new-filename))
      ))))


(defun md--get-day-from-filename (filename)
  (if (string-match "\\([0-9]\\{4\\}-W[0-9]\\{2\\}-[1-7]\\)" filename)
      (match-string-no-properties 0 filename)
    nil))

;; Test:
;; (md--get-day-from-filename "2018-W34-4-123123.md") ; should return "2018-W34-4"
;; (md--get-day-from-filename "shouldntwork") ; should return nil
;; (md--get-day-from-filename nil) ; should return nil?
