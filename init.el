;;; Personal configuration -*- lexical-binding: t -*-

;; Load dumped autoloads
(setq package-quickstart t)

;; Set default font face
(defun ShadowRZ/set-font-faces ()
  (set-face-attribute 'default nil :font "Iosevka Minoko" :width 'expanded :height 230 :weight 'extra-light)
  (set-fontset-font t 'han "Sarasa Mono SC")
  (set-fontset-font t 'hangul "Sarasa Mono K")
  (set-fontset-font t 'kana "Sarasa Mono J")
  (set-fontset-font t 'cjk-misc "Sarasa Mono SC")
  (set-fontset-font t nil "Sarasa Mono CL" nil 'append)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile Minoko" :weight 'extra-light))
(ShadowRZ/set-font-faces)

;; Force fancy splash screen.
(defun ShadowRZ/use-fancy-splash-screen-p ()
  "Return t if fancy splash screens should be used."
  (and (display-graphic-p)
       (or (and (display-color-p)
		(image-type-available-p 'xpm))
           (image-type-available-p 'pbm))))
(advice-add 'use-fancy-splash-screens-p :override #'ShadowRZ/use-fancy-splash-screen-p)

;; Use Diminish to omit mode line items.
(use-package diminish :ensure t)

;; Load a custom theme
(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-headings '((0 . (variable-pitch 2.2))
			     (1 . (variable-pitch 2.0))
			     (2 . (variable-pitch 1.8))
			     (3 . (variable-pitch 1.6))
			     (4 . (variable-pitch 1.4))
			     (5 . (variable-pitch 1.2))
			     (t . (variable-pitch 1.1)))
	ef-themes-mixed-fonts t)
  (load-theme 'ef-winter :no-confirm))

;;; Completion framework
(use-package vertico
  :ensure t
  ;; Enable completion by narrowing
  :init
  (vertico-mode t)
  (setq
   vertico-resize t
   vertico-count 6)
  ;; Improve directory navigation
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-word)
	      ("M-d" . vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
)

;; Orderless
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Pop-up auto-completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `global-corfu-modes'.
  :init
  (global-corfu-mode))

;;; Git client
(use-package magit
  :ensure t
  ;; Bind the `magit-status' command to a convenient key.
  :bind (("C-c g" . magit-status))
  :config
  ;; Show word-granularity differences within diff hunks
  (setq magit-diff-refine-hunk t))

;;; Indication of local VCS changes
(use-package diff-hl
  ;; Enable `diff-hl' support by default in programming buffers
  :hook ((prog-mode . diff-hl-mode))
  :config
  ;; Update the highlighting without saving
  (diff-hl-flydiff-mode t))

;;; Markdown support
(use-package markdown-mode
  :ensure t)

;;; Outline-based notes management and organizer
(use-package org
  :init
  (setq org-directory "~/Documents/Org")
  (setq org-hidden-keywords '(title author email date subtitle))
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)))

;;; Additional Org-mode related functionality
(use-package org-contrib
  :ensure t
  :after (org))

;;; Which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

;; Emacs Pinentry
(use-package pinentry
  :ensure t
  :init
  (pinentry-start))

(use-package emacs
  :hook (
	 ;; Enable line numbering in `prog-mode'
	 (prog-mode . display-line-numbers-mode)
	 ;; Set font faces for Server frames
	 (server-after-make-frame . ShadowRZ/set-font-faces))
  :diminish eldoc-mode
  :init
  ;; Don't bother to do a bell ring
  (setq ring-bell-function 'ignore)
  ;; Use short answers
  (setq use-short-answers t)
  ;; Enable recursive minibuffer
  (setq enable-recursive-minibuffers t)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  ;; Automatically pair parentheses
  (electric-pair-mode t)
  ;; Store automatic customisation options elsewhere
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))
