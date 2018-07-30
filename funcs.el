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
