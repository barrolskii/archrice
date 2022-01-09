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

(defun native-comp-available-p () nil)

(defun create-blog-post ()
    "Create a new blog post"
    (interactive)
    (let ((name (read-string "Filename: ")))
    (expand-file-name (format "%s.org" name) "~/Dev/Web/barrolskii.github.io/org/blogs/")))

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

(setq org-capture-templates
      '(("p" "Post" plain
         (file create-blog-post)
         (file "~/Dev/Templates/blogtemplate.org"))))

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
                            "This is where the fun begins."
                            "Now THIS is pod racing!"
                            "Todays Emacs sessions was sponsored by RAID: Shadow Legends"
))

(setq yas-snippet-dirs '("~/Dev/Templates/Snippets"))

;; Org export html stuff

(setq org-html-content-class "content container"
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html--pre/postamble-class nil
      org-html-divs '((preamble "div" "")
                      (content "div")
                      (postamble "div" "postamble"))
      org-html-postamble nil
      ; This is ugly but it's midnight, this works, and I'll probably make it neater at some point
      ; At some point...
      org-html-preamble-format '(("en" "<div class=\"sidebar\">
<div class=\"sidebar-about\">
  <span class=\"site__title\">
        <a href=\"https://barrolskii.github.io/\">Ashley Barrell</a>
  </span>
  <div class=\"author-image\">
        <!--<img src=\"https://htr3n.github.io//img/avatar.png\" alt=\"Author Image\" class=\"img--circle img--headshot element--center\">-->
  </div>
        <p class=\"site__description\"></p>
</div>

    <div class=\"collapsible-menu\">

        <input type=\"checkbox\" id=\"menuToggle\">
        <label for=\"menuToggle\">Ashley Barrell</label>
        <div class=\"menu-content\">

            <ul class=\"sidebar-nav\">
            <li>
                <a href=\"https://barrolskii.github.io/blogindex.html\">
                <span>Blog</span>
                </a>
            </li>
            <li>
                <a href=\"https://barrolskii.github.io/projects.html\">
                <span>Projects</span>
                </a>
            </li>
            <li>
                <a href=\"https://barrolskii.github.io/about.html\">
                <span>About</span>
                </a>
            </li>
            </ul>

            <section class=\"social\">
            <a href=\"https://github.com/barrolskii\">
                <svg class=\"svg-inline--fa fa-github fa-w-16 fa-lg\" aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fab\" data-icon=\"github\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 496 512\" data-fa-i2svg=\"\">

                <path fill=\"currentColor\" d=\"M165.9 397.4c0 2-2.3 3.6-5.2 3.6-3.3.3-5.6-1.3-5.6-3.6 0-2 2.3-3.6 5.2-3.6 3-.3 5.6 1.3 5.6 3.6zm-31.1-4.5c-.7 2 1.3 4.3 4.3 4.9 2.6 1 5.6 0 6.2-2s-1.3-4.3-4.3-5.2c-2.6-.7-5.5.3-6.2 2.3zm44.2-1.7c-2.9.7-4.9 2.6-4.6 4.9.3 2 2.9 3.3 5.9 2.6 2.9-.7 4.9-2.6 4.6-4.6-.3-1.9-3-3.2-5.9-2.9zM244.8 8C106.1 8 0 113.3 0 252c0 110.9 69.8 205.8 169.5 239.2 12.8 2.3 17.3-5.6 17.3-12.1 0-6.2-.3-40.4-.3-61.4 0 0-70 15-84.7-29.8 0 0-11.4-29.1-27.8-36.6 0 0-22.9-15.7 1.6-15.4 0 0 24.9 2 38.6 25.8 21.9 38.6 58.6 27.5 72.9 20.9 2.3-16 8.8-27.1 16-33.7-55.9-6.2-112.3-14.3-112.3-110.5 0-27.5 7.6-41.3 23.6-58.9-2.6-6.5-11.1-33.3 2.6-67.9 20.9-6.5 69 27 69 27 20-5.6 41.5-8.5 62.8-8.5s42.8 2.9 62.8 8.5c0 0 48.1-33.6 69-27 13.7 34.7 5.2 61.4 2.6 67.9 16 17.7 25.8 31.5 25.8 58.9 0 96.5-58.9 104.2-114.8 110.5 9.2 7.9 17 22.9 17 46.4 0 33.7-.3 75.4-.3 83.6 0 6.5 4.6 14.4 17.3 12.1C428.2 457.8 496 362.9 496 252 496 113.3 383.5 8 244.8 8zM97.2 352.9c-1.3 1-1 3.3.7 5.2 1.6 1.6 3.9 2.3 5.2 1 1.3-1 1-3.3-.7-5.2-1.6-1.6-3.9-2.3-5.2-1zm-10.8-8.1c-.7 1.3.3 2.9 2.3 3.9 1.6 1 3.6.7 4.3-.7.7-1.3-.3-2.9-2.3-3.9-2-.6-3.6-.3-4.3.7zm32.4 35.6c-1.6 1.3-1 4.3 1.3 6.2 2.3 2.3 5.2 2.6 6.5 1 1.3-1.3.7-4.3-1.3-6.2-2.2-2.3-5.2-2.6-6.5-1zm-11.4-14.7c-1.6 1-1.6 3.6 0 5.9 1.6 2.3 4.3 3.3 5.6 2.3 1.6-1.3 1.6-3.9 0-6.2-1.4-2.3-4-3.3-5.6-2z\">
                </path>
                </svg>
            </a>

            <a href=\"https://linkedin.com/in/ashley-barrell-0a8804176\">

                <svg class=\"svg-inline--fa fa-linkedin fa-w-14 fa-lg\" aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fab\" data-icon=\"linkedin\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 448 512\" data-fa-i2svg=\"\">

                <path fill=\"currentColor\" d=\"M416 32H31.9C14.3 32 0 46.5 0 64.3v383.4C0 465.5 14.3 480 31.9 480H416c17.6 0 32-14.5 32-32.3V64.3c0-17.8-14.4-32.3-32-32.3zM135.4 416H69V202.2h66.5V416zm-33.2-243c-21.3 0-38.5-17.3-38.5-38.5S80.9 96 102.2 96c21.2 0 38.5 17.3 38.5 38.5 0 21.3-17.2 38.5-38.5 38.5zm282.1 243h-66.4V312c0-24.8-.5-56.7-34.5-56.7-34.6 0-39.9 27-39.9 54.9V416h-66.4V202.2h63.7v29.2h.9c8.9-16.8 30.6-34.5 62.9-34.5 67.2 0 79.7 44.3 79.7 101.9V416z\">
                </path>
                </svg>
            </a>
            </social>

        </div>
    </div>

</div>"))
      )

;(setq org-html-htmlize-output-type 'inline-css
;      org-html-head-include-default-style t
;      )

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

;;(if (eq system-type 'windows-nt)
;;
;;    (progn
;;        ;; Set the window to maximised because it is not by default on Windows
;;        ;; We don't have this problem on Linux because tiling window
;;        ;; managers set the scale of the window automatically
;;        (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
;;
;;        ;; If we're on Windows set the default compile command to run a batch file
;;        ;; I refuse to suffer with visual studio so I'll do everything myself
;;        ;; Yes I am aware that batch is gross
;;        (setq compile-command "nmake")
;;
;;        ;; Most of this doesn't seem to do anything??
;;        ;; Bash in Windows seems to work so I might not need to set these
;;        ;; The behaviour seems to work the same regardless if I set them or not
;;        (setq explicit-shell-file-name "E:/Program Files/Git/bin/bash.exe")
;;        (setq shell-file-name "bash")
;;        (setq explicit-bash.exe-args '("--noediting" "--login" "-i" "-m"))
;;        (setq shell-file-name "SHELL")
;;    )
;;)


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
