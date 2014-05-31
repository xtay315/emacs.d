(add-hook 'find-file-hooks 'highlight-hooks)
(defun highlight-hooks()
  ;(highlight-parentheses-mode t)
  (setq highlight-symbol-idle-delay 0.5)
  (highlight-symbol-mode t))
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [(shift f8)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key [(control meta f3)] 'highlight-symbol-query-replace)
;; higlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil) ; initially, hide#
;; toggle changes visibility
(global-set-key (kbd "<f6>") 'highlight-changes-visible-mode) ;; changes
;; remove the change-highlight in region
(global-set-key (kbd "S-<f6>") 'highlight-changes-remove-highlight)
;; alt-pgup/pgdown jump to the previous/next change
(global-set-key (kbd "<M-prior>") 'highlight-changes-previous-change)
(global-set-key (kbd "<M-next>")  'highlight-changes-next-change)
(set-face-foreground 'highlight-changes nil)
(set-face-foreground 'highlight-changes-delete nil)
;(set-face-background 'highlight-changes "#382f2f")
;(set-face-background 'highlight-changes-delete "#916868")
;;(set-face-background 'highlight-symbol-face 'color-27)
(custom-set-faces '(highlight-symbol-face
                    ((((type tty)) :background "cyan" ))))
(provide 'init-highlight-symbol)

