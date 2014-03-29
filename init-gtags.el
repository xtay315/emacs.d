; @see http://emacs-fu.blogspot.com.au/2008/01/navigating-through-source-code-using.html
(defun gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (let ((olddir)
        (topdir)
        (oldenv (getenv "GTAGSROOT")))
    (setenv "GTAGSROOT" nil)
    (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
        (progn
          (setq olddir default-directory)
          (setq topdir (read-directory-name
                        "gtags: top of source tree:" default-directory))
          (cd topdir)
          (shell-command "gtags && echo 'created tagfile'")
          (cd olddir)) ; restore
      ;;  tagfile already exists; update it
      (shell-command "global -u && echo 'updated tagfile'")
    (setenv "GTAGSROOT" oldenv))))
;;=======================================================================================
;;support gtags library
(defvar danie-helm-gtags-lib-table nil "hold the path of the library which contain G*TAGS" )
(eval-after-load 'helm-gtags
  '(eval
    '(defun helm-gtags-base-directory () "redefine the helm-gtags-base-directory"
       default-directory)))
(eval-after-load 'helm-gtags
  '(eval
    '(defun helm-gtags-exec-global-command (cmd)
       "redefine the helm-gtags for suport library GTAGS."
       (let ((has-tag-flag)
             (gtagsStr "GTAGSROOT")
             (oldenv)
             (default-directory default-directory)
             (input (car (last (split-string cmd)))))
         (setq helm-gtags-tag-location default-directory)
         (setq oldenv (getenv gtagsStr))
         (setenv gtagsStr nil)
         (if (= 0 (call-process "global" nil nil nil " -p"))
             (setq has-tag-flag t))
         (helm-gtags-save-current-context)
         (with-current-buffer (helm-candidate-buffer 'global)
           (if has-tag-flag
               (progn
                 (setenv gtagsStr nil)
                 (call-process-shell-command cmd nil t)))
           (mapcar (lambda (path)
                     (if (file-exists-p (expand-file-name "GTAGS" path))
                         (progn
                           ;; (setq default-directory "/")
                           (setq helm-gtags-tag-location path)
                           (setenv gtagsStr path)
                           (call-process-shell-command (format "%s %s" cmd " -a") nil t))))
                   danie-helm-gtags-lib-table)
           (setenv gtagsStr oldenv)
           (when (helm-empty-buffer-p (current-buffer))
             (error (format "%s: not found" input))))))))

(defun danie-gtags-add-lib()
  "add library path to 'danie-helm-gtags-lib-table"
  (interactive)
  (let ((tags-file (read-file-name "Visit tags table(default GTAGS): "
                                    default-directory
                                    (expand-file-name "GTAGS" default-directory) t)))
  (add-to-list 'danie-helm-gtags-lib-table  (file-name-directory tags-file))))

(defun danie-gtags-del-lib ()
  "del library path from 'danie-helm-gtags-lib-table"
  (interactive)
  (let ((tags-file (read-file-name "Visit tags table(default GTAGS): "
                                    default-directory
                                    (expand-file-name "GTAGS" default-directory) t)))
    (setq danie-helm-gtags-lib-table
          (remov (file-name-directory tags-file) danie-helm-gtags-lib-table))))
;;TODO: create library GTAGS by filter ".c" ".h" ".cpp" files
;; find $f -iregex '.*\.\(h\|c\|cpp\)' > /tmp/tagsfile.tmp
;; gtags -v -f /tmp/gtagsfile.tmp
;; {{config the gtags library path
(add-to-list 'danie-helm-gtags-lib-table "/usr/include/")
;; }}

;;=====================================================================
;;use the find files feature of gtags to improve the ff-find-other-file
(defvar danie-helm-source-gtags-files
  '((name . "GNU GLOBAL")
    (init . danie-helm-gtags-files-init)
    (candidates-in-buffer)
    (real-to-display . danie-helm-gtags-files-candidate-transformer)
    (candidate-numbe-limit . 9999)
    (type . file)))
(defun danie-helm-gtags-files-init ()
  (let ((fname (ff-treat-as-special))
        (cmd "global -Poa")
        (has-file-name))
    (if fname
        (progn
          (setq cmd (format "%s %s" cmd  fname))
          (setq has-file-name t))
      (progn
        (setq fname
              (if (buffer-file-name)
                  (file-name-base (buffer-file-name))
                nil))
        (if fname
            (progn
              (setq cmd (format "%s %s" cmd fname))
              (setq has-file-name t))
          (setq has-file-name nil))))
    (if has-file-name
        (helm-gtags-exec-global-command cmd))))
(defun danie-helm-gtags-files-candidate-transformer (file)
  file
  )
(defun danie-helm-gtags-find-files ()
  "find file with gnu global"
  (interactive)
  (helm-gtags-common '(danie-helm-source-gtags-files)))
(defun danie-hooks-ff-not-found ()
  (setq ff-not-found-hook 'danie-helm-gtags-find-files))
(add-hook 'c++-mode-hook 'danie-hooks-ff-not-found)
(add-hook 'c-mode-hook 'danie-hooks-ff-not-found)

;;pacthing helm-gtags-find-files
(eval-after-load 'helm-gtags
  '(helm-attrset 'real-to-display 'danie-helm-gtags-files-candidate-transformer helm-source-gtags-files))
;;========================================================
(defun add-gtagslibpath ()
  "add external library directory to environment variable GTAGSLIBPATH.
gtags will can that directory if needed.
C-u M-x add-gtagslibpath will remove the directory from GTAGSLIBPATH."
  (interactive "DDirectory containing GTAGS:\nP")
  (let (sl)
  (if (not (file-exists-p (concat (file-name-as-directory libdir) "GTAGS")))
      ;; create tags
      (let ((olddir default-directory))
        (cd libdir)
        (shell-command "gtags && echo 'created tagfile'")
        (cd olddir)
        )
    )
  (setq libdir (directory-file-name libdir)) ;remove final slash
  (setq sl (split-string (if (getenv "GTAGSLIBPATH") (getenv "GTAGSLIBPATH") "")  ":" t))
  (if del (setq sl (delete libdir sl)) (add-to-list 'sl libdir t))
  (setenv "GTAGSLIBPATH" (mapconcat 'identity sl ":")))
  )

(defun print-gtagslibpath ()
  "print the GTAGSLIBPATH (for debug purpose)"
  (interactive)
  (message "GTAGSLIBPATH=%s" (getenv "GTAGSLIBPATH"))
  )

(provide 'init-gtags)
