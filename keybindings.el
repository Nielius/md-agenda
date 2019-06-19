(spacemacs/set-leader-keys-for-major-mode 'markdown-mode
  "at" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'today))))
  "aw" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'this-week))))
  "ap" 'md-agenda-agenda-previous-week
  "an" 'md-agenda-agenda-next-week
  ;; "ap" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'previous-day))))
  ;; "an" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'next-day))))
  "ab" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'previous-week))))
  "af" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'next-week))))
  "ao" 'spacemacs/custom-perspective-@Agenda-opruim-layout
  "ar" 'md-agenda-rename-this-file
  "aR" 'md-agenda-start-renaming-session
  "ad" 'md-agenda-rename-file-with-default-extension
  "ab" 'md-agenda-compile-hakyll-site
  "av" 'md-agenda-open-file-in-hakyll-site
  "aa" 'spacemacs/md-agenda-transient-state-transient-state/body)


(spacemacs/set-leader-keys
  ;; If I get too many, I may have to move some of them to longer strings.
  "oa" 'md-agenda-go-to-agenda-dir
  "oc" 'md-agenda-go-to-current-week-file
  "ot" 'md-agenda-open-agenda
  "oh" 'niels-go-home-and-open
  ;; was: "ot" 'md-agenda-go-to-todays-file-and-merge-old-files
  "ow" 'md-agenda-go-to-working-memory)


(spacemacs|define-transient-state md-agenda-transient-state
  :title "Transient state for navigating markdown files."
  :doc "Where is this doc-string?"
  :bindings
  ("t" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'today)))) "today")
  ("w" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'week)))) "this week")
  ("p" md-agenda-agenda-previous-week "previous week")
  ("n" md-agenda-agenda-next-week "next week")
  ;; ("p" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'previous-day)))) "previous day")
  ;; ("n" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'next-day)))) "next day")
  ("b" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'previous-week)))) "previous week")
  ("f" (lambda () (interactive) (find-file (concat md-agenda-dir (md-agenda--get-file-name 'next-week)))) "next week")
  ("q" nil :exit t))


