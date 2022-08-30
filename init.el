;;; Personal configuration -*- lexical-binding: t -*-

;; Based on a preset generated by https://emacs.amodernist.com

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;; Set default font face
(set-face-attribute 'default nil :font "Iosevka" :height 200)
(set-fontset-font t 'han "Sarasa Mono SC")
(set-fontset-font t 'hangul "Sarasa Mono K")
(set-fontset-font t 'kana "Sarasa Mono J")
(set-fontset-font t 'cjk-misc "Sarasa Mono SC")
(set-fontset-font t nil "Sarasa Mono CL" nil 'append)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile")

;; Load a custom theme
(unless (package-installed-p 'ef-themes)
  (package-install 'ef-themes))

(require 'ef-themes)
(setq
 ef-themes-headings '((0 . (variable-pitch 2.2))
		      (1 . (variable-pitch 2.0))
		      (2 . (variable-pitch 1.8))
		      (3 . (variable-pitch 1.6))
		      (4 . (variable-pitch 1.4))
		      (5 . (variable-pitch 1.2))
		      (t . (variable-pitch 1.1)))
 ef-themes-mixed-fonts t
 ef-themes-variable-pitch-ui t)
(load-theme 'ef-autumn :no-confirm)

;; Force fancy splash screen.
(defun ShadowRZ/use-fancy-splash-screen-p ()
  "Return t if fancy splash screens should be used."
  (and (display-graphic-p)
       (or (and (display-color-p)
		(image-type-available-p 'xpm))
           (image-type-available-p 'pbm))))
(advice-add 'use-fancy-splash-screens-p :override #'ShadowRZ/use-fancy-splash-screen-p)

;; Fullscreen on startup
(set-frame-parameter nil 'fullscreen 'fullboth)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;;; Completion framework
(unless (package-installed-p 'ivy)
  (package-install 'ivy))

(unless (package-installed-p 'counsel)
  (package-install 'counsel))

(ivy-mode)
(setq
 ivy-use-virtual-buffers t
 enable-recursive-minibuffers t)
(counsel-mode 1)

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Automatically pair parentheses
(electric-pair-mode t)

;;; Pop-up auto-completion
(unless (package-installed-p 'company)
  (package-install 'company))

;; Enable Company by default in programming buffers
(add-hook 'prog-mode-hook #'company-mode)

;;; Git client
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;; Show word-granularity differences within diff hunks
(setq magit-diff-refine-hunk t)

;;; Indication of local VCS changes
(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))

;; Enable `diff-hl' support by default in programming buffers
(add-hook 'prog-mode-hook #'diff-hl-mode)

;; Update the highlighting without saving
(diff-hl-flydiff-mode t)

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;;; Outline-based notes management and organizer
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)

;;; Additional Org-mode related functionality
(unless (package-installed-p 'org-contrib)
  (package-install 'org-contrib))

;;; Jump to arbitrary positions
(unless (package-installed-p 'avy)
  (package-install 'avy))
(global-set-key (kbd "C-c z") #'avy-goto-word-1)

;; Jump to any open window or frame
(setq avy-all-windows 'all-frames)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'y-or-n-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

;; Replace all yes/no with y/n.
(fset 'yes-or-no-p #'y-or-n-p)

;; Emacs Pinentry
(unless (package-installed-p 'pinentry)
  (package-install 'pinentry))

(require 'pinentry)
(pinentry-start)

;; Swiper
(unless (package-installed-p 'swiper)
  (package-install 'swiper))

(advice-add 'isearch-forward :override #'swiper)

;; Setup Notmuch
(autoload 'notmuch "notmuch"
  "Run notmuch and display saved searches, known tags, etc." t)
(setq
 ;; Tell Emacs what to do in sending my mail.
 message-send-mail-function 'sendmail-send-it ;; Via msmtp's sendmail.

 ;; Who am I ?
 user-mail-address "shadowrz@disroot.org"
 user-full-name "夜坂雅"

 ;; Misc.
 message-kill-buffer-on-exit t
 notmuch-search-oldest-first nil

 ;; Don't display logo.
 notmuch-show-logo nil

 ;; Set Notmuch as mail reader
 read-mail-command 'notmuch)
;; Discourage HTML part.
(with-eval-after-load 'mm-decode
  (add-to-list 'mm-discouraged-alternatives "text/html"))

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
