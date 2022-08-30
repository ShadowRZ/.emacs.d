;;; Personal configuration -*- lexical-binding: t -*-

;; Load dumped autoloads
(setq package-quickstart t)

;; FIXME: Append MELPA for our purposes described below
;; Remove when next version of nix-mode is avaliable on NonGNU ELPA
;;
;; This `with-eval-after-load' is used to corporate with `package-quickstart'
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq
   ;; Make GNU ELPA and NonGNU ELPA has higher priority (1 > 0)
   package-archive-priorities '(("gnu" . 1)
				("nongnu" . 1))
   ;; Pin nix-mode to MELPA
   ;; MELPA -git version of nix-mode has a fix for Emacs 28
   package-pinned-packages '((nix-mode . "melpa"))))

;; Don't bother to do a bell ring
(setq ring-bell-function 'ignore)

;; Use short answers
(setq use-short-answers t)

;; Set default font face
(set-face-attribute 'default nil :font "Iosevka" :height 200)
(set-fontset-font t 'han "Sarasa Mono SC")
(set-fontset-font t 'hangul "Sarasa Mono K")
(set-fontset-font t 'kana "Sarasa Mono J")
(set-fontset-font t 'cjk-misc "Sarasa Mono SC")
(set-fontset-font t nil "Sarasa Mono CL" nil 'append)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile")

;; Fullscreen on startup
(set-frame-parameter nil 'fullscreen 'fullboth)
;; Disable the menu bar
(menu-bar-mode -1)
;; Disable the tool bar
(tool-bar-mode -1)
;; Disable the scroll bars
(scroll-bar-mode -1)

;; Load a custom theme
(ShadowRZ/with-eval-after-install 'ef-themes
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
  (load-theme 'ef-autumn :no-confirm))

;; Force fancy splash screen.
(defun ShadowRZ/use-fancy-splash-screen-p ()
  "Return t if fancy splash screens should be used."
  (and (display-graphic-p)
       (or (and (display-color-p)
		(image-type-available-p 'xpm))
           (image-type-available-p 'pbm))))
(advice-add 'use-fancy-splash-screens-p :override #'ShadowRZ/use-fancy-splash-screen-p)

;;; Completion framework
(ShadowRZ/with-eval-after-install 'vertico
  ;; Enable completion by narrowing
  (vertico-mode t)

  ;; Improve directory navigation
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
    (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char)
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))

(ShadowRZ/with-eval-after-install 'consult
  (advice-add 'isearch-forward :override #'consult-line))

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Automatically pair parentheses
(electric-pair-mode t)

;;; Pop-up auto-completion
(ShadowRZ/with-eval-after-install 'company
  ;; Enable Company by default in programming buffers
  (add-hook 'prog-mode-hook #'company-mode))

;;; Git client
(ShadowRZ/with-eval-after-install 'magit
  ;; Bind the `magit-status' command to a convenient key.
  (global-set-key (kbd "C-c g") #'magit-status)
  ;; Show word-granularity differences within diff hunks
  (setq magit-diff-refine-hunk t))

;;; Indication of local VCS changes
(ShadowRZ/with-eval-after-install 'diff-hl
  ;; Enable `diff-hl' support by default in programming buffers
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  ;; Update the highlighting without saving
  (diff-hl-flydiff-mode t))

;;; Markdown support
(ShadowRZ/with-eval-after-install 'markdown-mode)

;;; Outline-based notes management and organizer
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)

;;; Additional Org-mode related functionality
(ShadowRZ/with-eval-after-install 'org-contrib)

;;; Avy
(ShadowRZ/with-eval-after-install 'avy
  ;; Jump to arbitrary positions
  (global-set-key (kbd "C-c z") #'avy-goto-word-1)
  ;; Jump to any open window or frame
  (setq avy-all-windows 'all-frames))

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

;; Emacs Pinentry
(ShadowRZ/with-eval-after-install 'pinentry
  (require 'pinentry)
  (pinentry-start))

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

;; Use Diminish to omit mode line items.
(ShadowRZ/with-eval-after-install 'diminish
  (require 'diminish)
  (diminish 'ivy-mode)
  (diminish 'counsel-mode))

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
