;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))


;; ===========================
;; Functions
;; ===========================


;; Change to the home directory in dired.
(defun dired-home ()
  (interactive)
  (dired "~/")
)

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


(defun insert-tab-char ()
  (interactive)
  (insert "    ")
)

;; ===========================
;; Setq
;; ===========================

(setq user-full-name "Barrolskii")
(after! org
  (setq org-directory "~/Dev/Org")
  (setq org-agenda-files '("~/Dev/Org/Agenda.org"))
  (setq org-agenda-span 14)
)
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


(setq org-startup-folded t) ;; Set each list item in org mode to be folded by default

(setq
 company-idle-delay 0
 company-minimum-prefix-length 1
 )

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4) ; Set default indentation

(setq-default electric-indent-mode -1)

(setq +doom-quit-messages '(
                            "You rebel scum!"
                            "For a brick he flew pretty good."
                            "Oh I know what the ladies like."
                            "Wow that was really cool."
                            "If it took more than one shot, you weren't using a Jakobs."
))

(setq yas-snippet-dirs '("~/Dev/Templates/Snippets"))

;; ===========================
;; Hooks
;; ===========================

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c-mode-common-hook 'my/c-code-hook)


;; ===========================
;; Variable settings
;; ===========================


(setq yas-indent-line 'fixed)
(yas-global-mode t)

;; ===========================
;; Key mappings
;; ===========================

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
 :after dired
 :map dired-mode-map
 :leader
 :n "c" 'find-file
 )

(global-set-key (kbd "TAB") 'insert-tab-char)

;; ===========================
;; The bad corner
;; ===========================

;; This is called "The bad corner" because this is the Windows only settings
;; I know my config is sectioned out but I want Windows settings in their own
;; area. Windows bad

(if (eq system-type 'windows-nt)

    (progn
        ;; Set the window to maximised because it is not by default on Windows
        ;; We don't have this problem on Linux because tiling window
        ;; managers set the scale of the window automatically
        (add-hook 'emacs-startup-hook 'toggle-frame-maximized)

        ;; If we're on Windows set the default compile command to run a batch file
        ;; I refuse to suffer with visual studio so I'll do everything myself
        ;; Yes I am aware that batch is gross
        (setq compile-command "nmake")

        ;; Most of this doesn't seem to do anything??
        ;; Bash in Windows seems to work so I might not need to set these
        ;; The behaviour seems to work the same regardless if I set them or not
        (setq explicit-shell-file-name "E:/Program Files/Git/bin/bash.exe")
        (setq shell-file-name "bash")
        (setq explicit-bash.exe-args '("--noediting" "--login" "-i" "-m"))
        (setq shell-file-name "SHELL")
    )
)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
