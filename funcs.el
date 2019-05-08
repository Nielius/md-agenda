;; Requirements:
;; - cal-iso.el and cal.el
;; - common lisp functions? (at least destructuring-bind)


(defcustom md-agenda-hakyll-site-root (expand-file-name "~/proj/hakyll-site/")
  "Directory of the Hakyll site where the notes from the markdown-agenda will be compiled..
Should contain a file 'build-script.sh'."
  :group 'md-agenda
  :type 'string)

(defcustom md-agenda-folder "~/doc/notes/agenda"
  "Folder that contains all markdown agenda files."
  :type 'string
  :group 'md-agenda)


;; Functions to get filenames corresponding to certain dates
;;

(defun md-agenda--get-file-name (desc)
  "Return the file name for my agenda file for a specific date.
Can be either a symbol ('tomorrow, 'today, 'week), or the number
of days from today (positive, negative or zero)."
  (if (integerp desc)
      (apply #'md-agenda--get-file-name-for-year-week-day
             (md-agenda--add-iso-week-dates (md-agenda--current-year-week-day) (list 0 0 desc)))
    (case desc
      ('today (md-agenda--get-file-name 0))
      ('tomorrow (md-agenda--get-file-name 1))
      ('yesterday (md-agenda--get-file-name -1))
      ('next-day
       (apply #'md-agenda--get-file-name-for-year-week-day
              (md-agenda--add-iso-week-dates (md-agenda--year-week-day-of-current-file) (list 0 0 1))))
      ('previous-day
       (apply #'md-agenda--get-file-name-for-year-week-day
              (md-agenda--add-iso-week-dates (md-agenda--year-week-day-of-current-file) (list 0 0 -1))))
      ('next-week
       (apply #'md-agenda--get-file-name-for-year-week
              (md-agenda--add-iso-week-dates (md-agenda--year-week-day-of-current-file) (list 0 1 0))))
      ('previous-week
       (apply #'md-agenda--get-file-name-for-year-week
              (md-agenda--add-iso-week-dates (md-agenda--year-week-day-of-current-file) (list 0 -1 0))))
      ('this-week
       (apply #'md-agenda--get-file-name-for-year-week (md-agenda--current-year-week-day))))))


(defun md-agenda--get-file-name-for-year-week-day (year week day)
  (format "%d-W%02d-%d.md" year week day))

(defun md-agenda--get-file-name-for-year-week (year week &optional rest)
  ;; ignore if we also give a day
  (format "%d-W%02d-planning.md" year week))


;; General functions to manipulate ISO week dates
;;
;; (defun md-agenda--current-year-week-day ())
;; (defun md-agenda--add-iso-week-dates (date1 date2))
;;
;; iso-cal is pretty shitty...
;; Probably better to rewrite


(defun md-agenda--current-year-week-day ()
  (md-agenda--emacs-time-to-year-week-day (current-time)))


(defun md-agenda--add-iso-week-dates (date1 date2)
  "Adds two iso week dates.
This just adds the two lists and tries to make sense of the result.

Each date should be of the form (year week day), but the week and
day do not need to fulfill the normal restrictions, i.e., can be
arbitrarily large and negative."
  (destructuring-bind (year week day)
      (mapcar* #'+ date1 date2)
    (md-agenda--renormalize-iso-week-date year week day)))


(defun md-agenda--renormalize-iso-week-date (year week day)
  "Renormalizes the iso week date, i.e., if the week number or
  day number is larger than it should be, or even if it is negative."
  (md-agenda--emacs-time-to-year-week-day
   (encode-time 0 0 0 (+ (- day 1) (* 7 (- week 1))) 1 year)))


(defun md-agenda--emacs-time-to-year-week-day (emacstime)
  (mapcar
   (lambda (x)
     (string-to-number (format-time-string x emacstime)))
   (list "%G" "%V" "%u")))

;; Test-functions
;; (md-agenda--renormalize-iso-week-date 2019 1 8) -> 2019 2 1
;; (md-agenda--renormalize-iso-week-date 2019 2 0) -> 2019 1 7
;; (md-agenda--renormalize-iso-week-date 2019 2 -1) -> 2019 1 6


;; A lot of this is going back and forth between different ways to represent the
;; date... There should probably be a nicer pattern for this.

;; This function is not necessary anymore, since we started using
;; md-agenda--renormalize-iso-week-date.
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


;; Renaming files
;;

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

(defun md-agenda-compile-hakyll-site (&optional git-pull-q)
  "Compile the Hakyll site.
With prefix argument, also git pull before compiling."
  (interactive "P")
  (let ((buildscript (concat
                      (file-name-as-directory md-agenda-hakyll-site-root)
                      "build-script.sh"
                      (if git-pull-q
                          " -g"
                        ""))))
    (async-shell-command buildscript)))

(defun md-agenda-open-file-in-hakyll-site (&optional new-window-q)
  "Open the current markdown agenda file in the hakyll site.
With prefix argument, open it in new firefox window."
  (interactive "P")
  (let*
      ((project-root (projectile-project-root))
       (rel-filename-sans-ext (file-name-sans-extension (file-relative-name (buffer-file-name) project-root)))
       (target-html-file
        (concat
         (file-name-as-directory md-agenda-hakyll-site-root)
         "_site/doc/"
         rel-filename-sans-ext
         ".html")))
    (browse-url-firefox target-html-file new-window-q)))


;; Test:
;; (md--get-day-from-filename "2018-W34-4-123123.md") ; should return "2018-W34-4"
;; (md--get-day-from-filename "shouldntwork") ; should return nil
;; (md--get-day-from-filename nil) ; should return nil?


(defun md-agenda--get-first-line ()
  "Helper function: get the first line of the buffer."
  (save-excursion
    (goto-char (point-min))
    (thing-at-point 'line t)))

(defun md-agenda--extract-default-extension ()
  "Create a suitable filename based on the first line: take the
first string from the file, remove strange characters, and put
hyphens in between the words."
  (string-join
   (split-string
    (replace-regexp-in-string "[^[:alnum:] _-]" "" (downcase (md-agenda--get-first-line))))
   "-"))

(defun md-agenda-rename-file-with-default-extension ()
  "Suggest to rename the file to the title given by the function
md-agenda--extract-default-extension."
  (interactive)
  (let ((new-suffix (md-agenda--extract-default-extension) ))
    (if (y-or-n-p (format "Rename file with suffix %s?" new-suffix))
        (md-agenda-rename-this-file new-suffix)
      (progn
        (message "Aborted.")
        nil))))


;; Functions for starting a file renaming session.
;; ---
;; This cycles through all files without a descriptive name
;; (these are files that automatically get a timestamp instead of an appropriate name)
;; and lets you rename them.

(defun md-agenda-start-renaming-session ()
  (interactive)
  ;; (md-agenda-start-renaming-session--loop (md-agenda--get-list-of-files-with-undescriptive-names))
  (let ((file-list (md-agenda--get-list-of-files-with-undescriptive-names)))
    (if file-list
        (mapcar 'md-agenda--open-file-and-rename file-list)
      (message (format "No files with undescriptive suffixes in %s." md-agenda-folder)))))

(defun md-agenda--open-file-and-rename (filename)
  (progn
    (find-file filename)
    (if (not (md-agenda-rename-file-with-default-extension)) ; if the user doesn't accept the suggestion, ask for new
        (let ((new-suffix (read-string "Other suffix? (Empty to skip.)\t")))
          (if (string= new-suffix "")
              (message "File skipped.")
            (md-agenda-rename-this-file new-suffix)
            )))))

(defun md-agenda--get-list-of-files-with-undescriptive-names ()
  (directory-files md-agenda-folder t "^20[0-9]\\{2\\}-W[0-9]\\{2\\}-[1-7]-[0-9]*.md"))


;; This may be a useful function?
(defun save-this-file-name ()
  "Append the filename of the current buffer to the buffer named
 '*saved-files-names*'."
  (interactive)
  (let
      ((filename (buffer-file-name))
       (targetbuffer
        (get-buffer-create "*saved-file-names*"))
       )
    (with-current-buffer targetbuffer
      (insert (concat filename "\n")))))


;; Misc. functions
;;

(defun md-agenda-go-to-agenda-dir ()
  (interactive)
  (progn
    (find-file md-agenda-folder)
    (dired-sort-other "-alt")
    (beginning-of-buffer)
    (forward-line 5)))

(defun md-agenda-go-to-current-week-file ()
  (interactive)
  ;; You could also go to the file "current-week.md", but I use
  ;; that one mostly as a trick to get easy links on my phone.
  (find-file (concat (file-name-as-directory md-agenda-folder)
                     (apply #'md-agenda--get-file-name-for-year-week
                            (md-agenda--current-year-week-day)))))

(defun md-agenda-go-to-todays-file ()
  (interactive)
  ;; You could also go to the file "current-week.md", but I use
  ;; that one mostly as a trick to get easy links on my phone.
  (find-file (concat (file-name-as-directory md-agenda-folder)
                     (apply #'md-agenda--get-file-name-for-year-week-day
                            (md-agenda--current-year-week-day)))))

(defun md-agenda-go-to-working-memory ()
  (interactive)
  (find-file "~/doc/notes/working-memory.md"))
