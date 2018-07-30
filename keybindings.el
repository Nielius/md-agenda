(spacemacs/set-leader-keys-for-major-mode 'markdown-mode
  "at" (lambda () (interactive) (find-file (md-agenda--get-file-name 'today)))
  "aw" (lambda () (interactive) (find-file (md-agenda--get-file-name 'week)))
  "ap" (lambda () (interactive) (find-file (md-agenda--get-file-name 'previous-day)))
  "an" (lambda () (interactive) (find-file (md-agenda--get-file-name 'next-day)))
  "ab" (lambda () (interactive) (find-file (md-agenda--get-file-name 'previous-week)))
  "af" (lambda () (interactive) (find-file (md-agenda--get-file-name 'next-week)))
  "aa" 'spacemacs/md-agenda-transient-state-transient-state/body)

(spacemacs|define-transient-state md-agenda-transient-state
  :title "Transient state for navigating markdown files."
  :doc "Where is this doc-string?"
  :bindings
  ("t" (lambda () (interactive) (find-file (md-agenda--get-file-name 'today))) "today")
  ("w" (lambda () (interactive) (find-file (md-agenda--get-file-name 'week))) "this week")
  ("p" (lambda () (interactive) (find-file (md-agenda--get-file-name 'previous-day))) "previous day")
  ("n" (lambda () (interactive) (find-file (md-agenda--get-file-name 'next-day))) "next day")
  ("b" (lambda () (interactive) (find-file (md-agenda--get-file-name 'previous-week))) "previous week")
  ("f" (lambda () (interactive) (find-file (md-agenda--get-file-name 'next-week))) "next week")
  ("q" nil :exit t))
