;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ===========================
;; Functions
;; ===========================

;; Thank you Alex for another snippet of your config
;; It makes enum values indent in the declaration statement
(defun my/c-code-hook ()
  (setq c-offsets-alist '((brace-list-intro . +)))
)

(defun sp-wrap-quotes ()
  (interactive)
  (sp-wrap-with-pair "'")
)

(defun sp-wrap-double-quotes ()
  (interactive)
  (sp-wrap-with-pair "\"")
)

(defun get-file-as-string (file-path)
  "Return the contents of a file as a string"
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string))
)

(defun auto-insert-yas-expand ()
  (yas-expand-snippet (buffer-string) (point-min) (point-max))
)

;; Function for automatically putting yourself into insert mode after
;; a file snippet has been inserted (see auto-insert-alist further down)
(defun auto-insert-set-insert-mode ()
  (evil-insert (point))
)

(defun new-blog-post (name)
  "Creates a new blog post in website repo"
  (interactive "MEnter blog name: ")
  (let ((blog-path (expand-file-name (concat name ".org")
                                     "~/Dev/Web/barrolskii.github.io/org/blogs/")))

    (switch-to-buffer(with-current-buffer (or (get-file-buffer blog-path)
                             (find-file-noselect blog-path))))
    (message "blog: %s" blog-path))
)

(defun implement-header ()
    "Creates a C source file with empty definitions of headers from given header file"
    (interactive)
    (shell-command  (concat "funcdefine " (file-name-nondirectory (buffer-file-name))) nil)
)

;; There seems to be an issue with ws-butler not respecting the require-final-newline
;; variable. This function will ensure that when the require-final-newline variable
;; evaluates to t then we will get a final newline
(defun ensure-final-newline ()
  (interactive)
  (when (eq require-final-newline t)
    (let ((curr-point (point)))
      (goto-char (point-max))
      (insert "\n")
      (goto-char curr-point)))
)


;; ===========================
;; Setq / Variable Settings
;; ===========================

(setq user-full-name "Barrolskii")
(after! org
  (setq org-directory "~/Dev/Org"
        org-agenda-files '("~/Dev/Org/Agenda.org")
        org-agenda-span 14
        org-startup-folded t)
)

(setq doom-theme 'doom-one)
(setq fancy-splash-image "~/Dev/KeepItClean256.png")

(setq +doom-quit-messages '(
                            "You rebel scum!"
                            "Blast 'em!"
                            "For a brick he flew pretty good."
                            "Oh I know what the ladies like."
                            "Wow that was really cool."
                            "If it took more than one shot, you weren't using a Jakobs."
                            "This is where the fun begins."
                            "Now THIS is pod racing!"
                            "Todays Emacs session was sponsored by RAID: Shadow Legends"
                            "Become C R A B"
))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq
 company-idle-delay 0
 company-minimum-prefix-length 1
 )

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4) ; Set default indentation
(setq-default c-syntactic-indentation nil)

(setq-default electric-indent-mode -1) ;; Disable auto-indentation


(setq-default yas-snippet-dirs '("~/Dev/Templates/Snippets"))
(setq-default yas--default-user-snippets-dir "~/Dev/Templates/Snippets")
(setq yas-indent-line 'fixed)
(yas-global-mode t)

(setq +snippets-dir "~/Dev/Templates/Snippets")
(setq doom-snippets-dir "~/Dev/Templates/Snippets")
(setq +file-templates-dir "~/Dev/Templates/Snippets")

(setq auto-insert-directory "~/Dev/Templates/FileTemplates/")
(setq auto-insert-alist '(
                          (("\\.h$") . [ "template.h" auto-insert-yas-expand auto-insert-set-insert-mode])
                          (("main\\.c$") . [ "main.c" auto-insert-yas-expand ])
                          (("makefile$") . [ "makefile" auto-insert-yas-expand])
                          ))

(setq auto-insert-query nil)
(auto-insert-mode t)


(setq evil-want-Y-yank-to-eol t)



;; Org export html stuff
(setq org-html-content-class "content container"
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html--pre/postamble-class nil
      org-html-divs '((preamble "div" "")
                      (content "div")
                      (postamble "div" "postamble"))
      org-html-postamble nil
      org-html-preamble (get-file-as-string "~/Dev/Templates/FileTemplates/preamble.html")
      org-html-head-include-default-style nil
      )


(setq org-publish-project-alist
      '(("barrolskii.github.io"
         :base-directory "~/Dev/Web/barrolskii.github.io/org"
         :publishing-directory "~/Dev/Web/barrolskii.github.io"
         :base-extension "org"
         :recursive t
         :with-title nil
         :publishing-function org-html-publish-to-html
         )
))


;; ===========================
;; Hooks
;; ===========================

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c-mode-common-hook 'my/c-code-hook)

;; Before save hook is buffer local so add it to specified buffers
;; I don't want every mode to end with a blank newline so they'll
;; just be added here
;;(add-hook 'c-mode-hook
  ;; 100 here ensures that the function is added to the end of the hook list
  ;;(lambda () (add-hook 'before-save-hook 'ensure-final-newline 100 t)))


;; ===========================
;; Key mappings
;; ===========================

(map!
 :leader
 :desc "Eval last sexp"
 :n "e" #'eval-last-sexp
 )

(map!
 :leader
 :desc "Open dashboard" "d d" #'+doom-dashboard/open
)

(map!
 :desc "Wrap word/s in ("
 :leader
 :n "(" #'sp-wrap-round
)

(map!
 :desc "Wrap word/s in ["
 :leader
 :n "[" #'sp-wrap-square
 )

(map!
 :desc "Wrap word/s in {"
 :leader
 :n "{" #'sp-wrap-curly
 )

(map!
 :desc "Wrap word/s in '"
 :leader
 :n "'" #'sp-wrap-quotes
 )

(map!
 :desc "Wrap word/s in \""
 :leader
 :n "\"" #'sp-wrap-double-quotes
 )

(map!
 :desc "Implement header file"
 :leader
 :n "i h" #'implement-header
 )

;; ===========================
;; The bad corner
;; ===========================

;; This is called "The bad corner" because this is the Windows only settings
;; I know my config is sectioned out but I want Windows settings in their own
;; area. Windows bad

(if (eq system-type 'windows-nt)

    (progn
        ;; Set the window to maximised because it is not by default on Windows
        (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
        (setq-default compile-command "nmake")
    )
)
