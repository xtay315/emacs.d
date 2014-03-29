(setq magit-save-some-buffers nil
      magit-process-popup-time 10
      magit-completing-read-function 'magit-ido-completing-read)

(defun magit-status-somedir ()
  (interactive)
  (let ((current-prefix-arg t))
    (magit-status default-directory)))

;; Sometimes I want check other developer's commit
;; show file of specific version
(autoload 'magit-show "magit" "" t nil)
;; show the commit
(autoload 'magit-show-commit "magit" "" t nil)

(global-set-key [(meta f12)] 'magit-status)
(global-set-key [(shift meta f12)] 'magit-status-somedir)

(eval-after-load 'magit
  '(progn
     ;; Don't let magit-status mess up window configurations
     ;; http://whattheemacsd.com/setup-magit.el-01.html
     (defadvice magit-status (around magit-fullscreen activate)
       (window-configuration-to-register :magit-fullscreen)
       ad-do-it
       (delete-other-windows))

     (defun magit-quit-session ()
       "Restores the previous window configuration and kills the magit buffer"
       (interactive)
       (kill-buffer)
       (jump-to-register :magit-fullscreen))

     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

(when *is-a-mac*
  (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))

(eval-after-load 'magit
  '(progn
     (require 'magit-key-mode)
     (require 'magit-svn)
     ))

;;----------------------------------------------------------------------------
;; git-svn conveniences
;;----------------------------------------------------------------------------
(eval-after-load 'compile
  '(progn
     (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                         '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
       (add-to-list 'compilation-error-regexp-alist-alist defn))
     (dolist (defn '(git-svn-updated git-svn-needs-update))
       (add-to-list 'compilation-error-regexp-alist defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands")

(defun git-svn (dir)
  "Run git svn"
  (interactive "DSelect directory: ")
  (unless git-svn--available-commands
    (setq git-svn--available-commands
          (string-all-matches "^  \\([a-z\\-]+\\) +" (shell-command-to-string "git svn help") 1)))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn "
                     (ido-completing-read "git-svn command: " git-svn--available-commands nil t)))))


;;----------------------------------------------------------------------------
;; gist fixes
;;----------------------------------------------------------------------------

;; If using a "password = !some command" in .gitconfig, we need to
;; run the specified command to find the actual value

(defadvice gh-config (after sanityinc/maybe-execute-bang (key) activate)
  (when (and (string= key "password")
             (string-prefix-p "!" ad-return-value))
    (setq ad-return-value (shell-command-to-string (substring ad-return-value 1)))))

;; {{ git-messenger
(require 'git-messenger)
;; show to details to play `git blame' game
(setq git-messenger:show-detail t)
(add-hook 'git-messenger:after-popup-hook (lambda (msg)
                                            ;; extract commit id and put into the kill ring
                                            (when (string-match "\\(commit *: *\\)\\([0-9a-z]+\\)" msg)
                                              (kill-new (match-string 2 msg)))
                                            (kill-new msg)
                                            (with-temp-buffer
                                              (insert msg)
                                              (shell-command-on-region (point-min) (point-max)
                                                                       (cond
                                                                        ((eq system-type 'cygwin) "putclip")
                                                                        ((eq system-type 'darwin) "pbcopy")
                                                                        (t "xsel -ib")
                                                                        )))
                                            (message "commit details > clipboard & kill-ring")))
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
;; }}

(provide 'init-git)

