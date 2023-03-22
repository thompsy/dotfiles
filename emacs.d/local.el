;; These are things that are local to the specific installation which I don't want to commit to Git.


(provide 'local-setup)

(setq user-full-name "Andrew Thompson"
      user-mail-address "github@downthewire.co.uk")

(defconst my/org-dir
  "~/notebook/")

(defconst my/org-agenda-files
  (list
   my/org-dir
   (concat my/org-dir "journal")))

