;;; Personal configuration -*- lexical-binding: t -*-

;; Load dumped autoloads
(setq package-quickstart t)

;; Don't bother to do a bell ring
(setq ring-bell-function 'ignore)

;; Use short answers
(setq use-short-answers t)

;; Set default font face
(set-face-attribute 'default nil :font "Iosevka Minoko" :width 'expanded :height 200)
(set-fontset-font t 'han "Sarasa Mono SC")
(set-fontset-font t 'hangul "Sarasa Mono K")
(set-fontset-font t 'kana "Sarasa Mono J")
(set-fontset-font t 'cjk-misc "Sarasa Mono SC")
(set-fontset-font t nil "Sarasa Mono CL" nil 'append)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile Minoko")

;; Fullscreen on startup
(set-frame-parameter nil 'fullscreen 'fullboth)

;; Enable recursive minibuffer
(setq enable-recursive-minibuffers t)

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; Automatically pair parentheses
(electric-pair-mode t)

;; Force fancy splash screen.
(defun ShadowRZ/use-fancy-splash-screen-p ()
  "Return t if fancy splash screens should be used."
  (and (display-graphic-p)
       (or (and (display-color-p)
		(image-type-available-p 'xpm))
           (image-type-available-p 'pbm))))
(advice-add 'use-fancy-splash-screens-p :override #'ShadowRZ/use-fancy-splash-screen-p)

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
   ef-themes-mixed-fonts t)
  (load-theme 'ef-winter :no-confirm))

;;; Completion framework
(ShadowRZ/with-eval-after-install 'vertico
  ;; Enable completion by narrowing
  (vertico-mode t)
  (setq
   vertico-resize t
   vertico-count 6)
  ;; Improve directory navigation
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
    (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char)
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))

;; Consult
(ShadowRZ/with-eval-after-install 'consult
  ;; C-x bindings (ctl-x-map)
  (global-set-key (kbd "C-x M-:") 'consult-complex-command)     ;; orig. repeat-complex-command
  (global-set-key (kbd "C-x b") 'consult-buffer)                ;; orig. switch-to-buffer
  (global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  (global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  (global-set-key (kbd "C-x r b") 'consult-bookmark)            ;; orig. bookmark-jump
  (global-set-key (kbd "C-x p b") 'consult-project-buffer)      ;; orig. project-switch-to-buffer
  ;; M-g bindings (goto-map)
  (global-set-key (kbd "M-g e") 'consult-compile-error)
  (global-set-key (kbd "M-g f") 'consult-flymake)               ;; Alternative: consult-flycheck
  (global-set-key (kbd "M-g g") 'consult-goto-line)             ;; orig. goto-line
  (global-set-key (kbd "M-g M-g") 'consult-goto-line)           ;; orig. goto-line
  (global-set-key (kbd "M-g o") 'consult-outline)               ;; Alternative: consult-org-heading
  (global-set-key (kbd "M-g m") 'consult-mark)
  (global-set-key (kbd "M-g k") 'consult-global-mark)
  (global-set-key (kbd "M-g i") 'consult-imenu)
  (global-set-key (kbd "M-g I") 'consult-imenu-multi)
  ;; M-s bindings (search-map)
  (global-set-key (kbd "M-s d") 'consult-find)
  (global-set-key (kbd "M-s D") 'consult-locate)
  (global-set-key (kbd "M-s g") 'consult-grep)
  (global-set-key (kbd "M-s G") 'consult-git-grep)
  (global-set-key (kbd "M-s r") 'consult-ripgrep)
  (global-set-key (kbd "M-s l") 'consult-line)
  (global-set-key (kbd "M-s L") 'consult-line-multi)
  (global-set-key (kbd "M-s m") 'consult-multi-occur)
  ;; Isearch integration
  (global-set-key (kbd "M-s e") 'consult-isearch-history)
  (define-key isearch-mode-map (kbd "M-s l") 'consult-line)                  ;; needed by consult-line to detect isearch
  (define-key isearch-mode-map (kbd "M-s L") 'consult-line-multi)            ;; needed by consult-line to detect isearch
  (add-hook 'completion-list-mode 'consult-preview-at-point-mode)
  (setq
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref
   consult-narrow-key "<"))

;; Orderless
(ShadowRZ/with-eval-after-install 'orderless
  (require 'orderless)
  (setq
   completion-styles '(orderless basic)
   completion-category-overrides '((file (styles basic partial-completion)))))

;; Marginalia
(ShadowRZ/with-eval-after-install 'marginalia
  (marginalia-mode))

;; Embark
(ShadowRZ/with-eval-after-install 'embark
  (global-set-key (kbd "C-.") 'embark-act)
  (global-set-key (kbd "M-.") 'embark-dwim)
  (global-set-key (kbd "C-h B") 'embark-bindings)
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))
  ;; Embark + Consult
  (ShadowRZ/with-eval-after-install 'embark-consult
    (require 'embark-consult)))

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

;;; Which-key
(ShadowRZ/with-eval-after-install 'which-key
  (which-key-mode))

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

;; Emacs Pinentry
(ShadowRZ/with-eval-after-install 'pinentry
  (require 'pinentry)
  (pinentry-start))

;; Use Diminish to omit mode line items.
(ShadowRZ/with-eval-after-install 'diminish
  (require 'diminish)
  (add-hook 'company-mode-hook (lambda ()
				 (diminish 'company-mode)
				 (diminish 'which-key-mode))))

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
