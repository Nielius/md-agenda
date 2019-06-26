(defcustom md-agenda-hakyll-site-root (expand-file-name "~/proj/hakyll-site/")
  "Directory of the Hakyll site where the notes from the markdown-agenda will be compiled..
Should contain a file 'build-script.sh'."
  :group 'md-agenda
  :type 'string)

(defcustom md-agenda-dir "~/doc/notes/agenda"
  "Directory that contains all markdown agenda files."
  :type 'string
  :group 'md-agenda)


;;(with-eval-after-load 'helm
;;  (setf
;;   (alist-get "Insert as markdown link" helm-type-file-actions nil nil #'equal)
;;   'nielius-helm--insert-markdown-links-action)
;;
;;  (setf
;;   (alist-get "Append to this file and delete current file" helm-type-file-actions nil nil #'equal)
;;   'md-agenda-append-this-file-to-other))

;; (spacemacs|define-custom-layout "@Agenda-opruim-layout"
;; :binding "p"
;; :body
;; (find-file "~/doc/notes/agenda/")
;; (dired-hide-details-mode)
;; (dired-sort-toggle-or-edit)
;; (spacemacs/toggle-current-window-dedication)
;; (split-window-right-and-focus) ;; Create the right side and move focus
;; (find-file (concat md-agenda-dir (md-agenda--get-file-name (quote week))))
;; ;; the following increases the window width to take up 70% of the frame
;; (enlarge-window-horizontally
;;  (- (floor (* 0.70 (frame-width)))
;;     (window-size nil t) ; gives window width
;;     )))

;; (spacemacs//update-custom-layouts)
