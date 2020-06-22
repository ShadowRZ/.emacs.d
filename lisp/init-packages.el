;;; init-package.el --- Packages init.
;;; Commentary:

;;; Code:

;; Requires
(use-package delight
  :config
  (delight 'eldoc-mode nil "eldoc"))

(use-package recentf
  :config
  (recentf-mode 1))

(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  :bind (("C-c a" . org-agenda)))

;; - EMMS.
(use-package emms
  :config
  (require 'emms-setup)
  (require 'emms-lyrics)
  (require 'emms-source-playlist)
  (emms-all)
  (emms-default-players)
  (emms-lyrics 1)
  (setq emms-info-functions '(emms-info-libtag)))

(use-package popwin
  :config
  (popwin-mode 1))

(use-package wl
  :ensure nil
  :commands (wl wl-other-frame))

(use-package wl-draft
  :ensure nil
  :after wl
  :commands (wl-draft wl-user-agent-compose)
  :config
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook)))

;; - Ivy / Counsel / Swiper
(use-package ivy
  :delight ivy-mode
  :config
  (ivy-mode 1))
(use-package counsel
  :delight counsel-mode
  :config
  (counsel-mode 1))
(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package flycheck
  :config
  (global-flycheck-mode 1))

(use-package company
  :delight company-mode
  :config
  (global-company-mode 1))

(use-package projectile
  :config
  (projectile-mode 1))

(use-package which-key
  :delight which-key-mode
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))

(use-package semantic
  :config
  (global-semanticdb-minor-mode)
  (global-semantic-idle-scheduler-mode)
  (global-semantic-highlight-func-mode)
  (global-semantic-stickyfunc-mode -1)
  (semantic-mode))

(use-package pinentry
  :config
  (pinentry-start))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package pyim
  :ensure nil
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))

  ;; 五笔用户使用 wbdict 词库
  ;; (use-package pyim-wbdict
  ;;   :ensure nil
  ;;   :config (pyim-wbdict-gbk-enable))

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 pupup-el 来绘制选词框
  (setq pyim-page-tooltip 'popup)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  :bind
  (("M-j" . pyim-convert-code-at-point) ;; 与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("<f8>"      . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

;; - JS2 / Web mode.
(use-package js2-mode
  :mode "\\.js\\'")
(use-package web-mode
  :mode "\\.html'")

(delete-selection-mode 1)
(global-hl-line-mode 1)
(abbrev-mode 1)
(global-auto-revert-mode 1)

(require 'dired-x)

(defun ShadowRZ/dired-modes ()
  "Dired modes setup."
  (dired-hide-details-mode 1)
  (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'ShadowRZ/dired-modes)

(provide 'init-packages)
;;; init-packages.el ends here
