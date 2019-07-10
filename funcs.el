;; Functions to get filenames corresponding to certain dates
;;
;; A lot of this is going back and forth between different ways to represent the
;; date... There should probably be a nicer pattern for this.

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
  (mapca
   (lambda (x)
     (string-to-number (format-time-string x emacstime)))
   (list "%G" "%V" "%u")))

(defun md--get-day-from-filename (filename)
  (if (string-match "\\([0-9]\\{4\\}-W[0-9]\\{2\\}-[1-7]\\)" filename)
      (match-string-no-properties 0 filename)
    nil))

(defun md-agenda--year-week-day-of-current-file ()
  "Get the year, week and day of the current file,
assuming the file name is of the form '%Y-W%V-%u-.....md'.
Returns '(YEAR WEEK DAY)."
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (list (string-to-number (substring filename 0 4))
          (string-to-number (substring filename 6 8))
          (let ((n (string-to-number (substring filename 9 10))))
            (if (= n 0) 1 n)))))

;; Test-functions (far from a complete test suite)
(eval-when-compile
  (assert (equal (md-agenda--renormalize-iso-week-date 2019 1 8) '(2019 2 1)))
  (assert (equal (md-agenda--renormalize-iso-week-date 2019 2 0) '(2019 1 7)))
  (assert (equal (md-agenda--renormalize-iso-week-date 2019 2 -1) '(2019 1 6)))

  (assert (equal (md--get-day-from-filename "2018-W34-4-123123.md") "2018-W34-4"))
  (assert (equal (md--get-day-from-filename "2018-W34-9-123123.md") nil))
  (assert (equal (md--get-day-from-filename "shouldntwork") nil))
  )


;; 
;; Functions for renaming timestamped files
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

;; 
;;; Functions for starting a file renaming session
;;
;; Main function: `md-agenda-start-renaming-session'.

(defun md-agenda-start-renaming-session ()
  "Walk through all timestamped files and ask the user to give
them to a more descriptive name."
  (interactive)
  (let ((file-list (md-agenda--get-list-of-files-with-undescriptive-names)))
    (if file-list
        (mapcar 'md-agenda--open-file-and-rename file-list)
      (message (format "No files with undescriptive suffixes in %s." md-agenda-dir)))))

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
  (directory-files md-agenda-dir t "^20[0-9]\\{2\\}-W[0-9]\\{2\\}-[1-7]-[0-9]*.md"))


;; 
;; Functions for inserting old date files
;;
;; Main funtion: md-agenda--insert-all-older-files, which insert into the
;; current file all date files that are older than the current file.

(defun md-agenda--insert-all-older-files ()
  "When executed in a date file (i.e., a file of the form \"2019-W33-4.md\"),
  insert all date files that are older. In addition, it inserts a small shell
  script that can be executed to remove all those older date files."
  (interactive)
  (let* ((file-date (file-name-base (buffer-file-name)))
         (file-list (md-agenda--get-list-of-date-files file-date)))
    (mapc (lambda (x)
            (insert (format "rm %s\n" x))) file-list)
    (when file-list ; only when we are in fact adding anything
      (insert "\n\n"))
    (mapc (lambda (x)
            (progn
              (insert (format "\# From %s \n\n" (file-name-base x)))
              (forward-line 2)
              (insert-file-contents x)
              (insert "\n\n")
              )) file-list)))

(defun md-agenda--get-list-of-date-files (&optional latest-date)
  "Get all files of the form
20[0-9]\\{2\\}-W[0-9]\\{2\\}-[1-7].md (i.e., date files). If the
optional argument is given, get all files that correspond to a
day that is before `latest-date' (which should be the filename of
a date file represented as a string)."
  (let ((allfiles (directory-files md-agenda-dir t "^20[0-9]\\{2\\}-W[0-9]\\{2\\}-[1-7].md"))
        (latest-date-chopped (file-name-base latest-date)))
    (if latest-date
        (-filter (lambda (x) (string<  (file-name-base x) latest-date-chopped)) allfiles)
      allfiles)))


;; 
;;;; Agenda layout functions
;;
;; These are functions that provide a layout of a week that looks like an
;; actual, physical agenda.
;;
;; Overview of the functions:
;;
;; md-agenda--open-agenda-layout ()
;; --- open new frame with windows split as I want them for the agenda;
;;     does not open any agenda files.
;;
;; md-agenda-open-agenda (&optional weeknum)
;; --- open agenda on <weeknum> (if given) or on the current week (otherwise).
;;
;; md-agenda-agenda-to-week (year week)
;; --- assumes agenda is already open (see note below) and sets the
;;     agenda to the given week.
;;
;; md-agenda-agenda-next-week ()
;; --- assumes user is in the agenda and goes to the next week.
;;
;;
;; Note on the implementation:
;; The windows that are opened for the agenda layout are saved in the variable
;; `md-agenda--agenda-layout-windows', so that `md-agenda-agenda-to-week' and
;; `md-agenda-agenda-next-week' can use this variable to change the files that
;; the windows are visiting.


(defun md-agenda--open-agenda-layout ()
  "Make a new frame and split the windows for an agenda layout.
Save the new windows in the global variable md-agenda--agenda-layout-windows."
  (select-frame (make-frame)) ; make a new frame and select it
  (toggle-frame-maximized)
  (setq md-agenda--agenda-layout-windows
        ;; This splits all the windows correctly and stores them in a list (split-windows),
        ;; so that we can loop over them.
        (list
         (selected-window)
         (split-window-below)
         (progn
           (windmove-down)
           (split-window-below))
         (progn
           (windmove-up)
           (split-window-right))
         (progn
           (windmove-down)
           (split-window-right))
         (progn
           (windmove-down)
           (split-window-right))
         (let ((window-combination-resize nil)) ; otherwise, automatic rebalance => bottom three have same size
           (windmove-right)
           (split-window-right)))))

(defun md-agenda-agenda-to-week (year week)
  "Assuming that the agenda layout is already given
(i.e., there are windows, saved in md-agenda--agenda-layout-windows,
that are split in the right way for an agenda overview),
open the given week.

If year is nil, use current year."
  ;; TODO: check whether the agenda windows are open; and maybe come up with good terminology?
  (let ((year (or year (first (md-agenda--current-year-week-day))))
        (i 1))
    (mapc (lambda (window)
            (progn
              (select-window window)
              (find-file
               (concat (file-name-as-directory md-agenda-dir)
                       (md-agenda--get-file-name-for-year-week-day year week i)))
              (setf i (+ i 1))))
          md-agenda--agenda-layout-windows)))

(defun md-agenda-open-agenda (&optional weeknum)
  "Makes a new frame with split windows, in which the current week is opened as an agenda.
Use this to start working with the agenda.

Accepts as prefix argument an integer that specifies the week to open."
  (interactive "p")
  ;; TODO: only open if not already open? or maybe use this to always open?
  (md-agenda--open-agenda-layout) ; splits all the windows in a new frame
  ;; Note that weeknum defaults to one (via (interactive "p"))
  (let ((weeknum (if (= weeknum 1)
                     (second (md-agenda--current-year-week-day))
                   weeknum))
        (year (first (md-agenda--current-year-week-day))))
    (md-agenda-agenda-to-week year weeknum)))

(defun md-agenda-agenda-next-week (&optional week-offset)
  "Assuming the user is in the agenda view, go to the next week.
Accepts an integer prefix argument to skip several weeks."
  (interactive "p")
  (destructuring-bind
      (year week day) (md-agenda--year-week-day-of-current-file)
    (md-agenda-agenda-to-week year (+ (or week-offset 1) week))))

(defun md-agenda-agenda-previous-week (&optional week-offset)
  "Like md-agenda-agenda-next-week, but with negative argument."
  (interactive "p")
  (md-agenda-agenda-next-week (* -1 (or week-offset 1))))


;; 
;;;; Convenience functions
;;


(defun md-agenda-paste-as-relative-link (&optional filename)
  "Pastes the filename in the kill ring as a relative link.
Useful in combination with `spacemacs/copy-fily-path', which
copies the path to the file that is being visited in the current
buffer. This makes it easier to use simple markdown files as a
kind of personal wiki.

If the optional argument FILENAME is given, then make a link to
that file, instead of to the filename in the kill ring."
  (interactive)
  (let
      ((filename
        (if filename ; if the optional argument is given,
            filename ; use that
          (substring-no-properties ; otherwise, kill ring
           (car kill-ring)))))
    (unless (eolp) (forward-char)) ; necessary to get evil's normal paste-after effect
    (insert-for-yank
     (concat
      "["
      (file-name-base filename)
      "]("
      (s-trim
       (file-relative-name filename
                           (file-name-directory (buffer-file-name))))
      ")"))))


(defun md-agenda-go-to-agenda-dir ()
  (interactive)
  (progn
    (find-file md-agenda-dir)
    (dired-sort-other "-alt")
    (beginning-of-buffer)
    (forward-line 5)))

(defun md-agenda-go-to-current-week-file ()
  (interactive)
  ;; You could also go to the file "current-week.md", but I use
  ;; that one mostly as a trick to get easy links on my phone.
  (find-file (concat (file-name-as-directory md-agenda-dir)
                     (apply #'md-agenda--get-file-name-for-year-week
                            (md-agenda--current-year-week-day)))))

(defun md-agenda-go-to-todays-file ()
  (interactive)
  ;; You could also go to the file "current-week.md", but I use
  ;; that one mostly as a trick to get easy links on my phone.
  (find-file (concat (file-name-as-directory md-agenda-dir)
                     (apply #'md-agenda--get-file-name-for-year-week-day
                            (md-agenda--current-year-week-day)))))

(defun md-agenda-go-to-todays-file-and-merge-old-files ()
  "Go to today's agenda file and insert all older files, together
with a bash script that can be used to remove all those older
files."
  (interactive)
  (md-agenda-go-to-todays-file)
  (save-excursion
    (goto-char (point-max))
    (md-agenda--insert-all-older-files)))
