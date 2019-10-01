;;; init-package.el --- Packages init.
;;; Commentary:

;;; Code:

;; Requires
(use-package recentf
  :ensure t
  :config
  (recentf-mode 1))

(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode))

;; - EMMS.
(use-package emms)
(use-package emms-setup
  :config
  (emms-all)
  :after (emms))
(use-package emms-lyrics
  :config
  (emms-lyrics 1)
  :after (emms))
(use-package emms-source-playlist
  :config
  (setq emms-player-list '(emms-player-mpv))
  :after (emms))

(use-package dired-x)

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package elfeed)

(use-package wl
  :commands (wl wl-other-frame))

(use-package wl-draft
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

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))
(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1))
(use-package company
  :ensure t
  :config
  (global-company-mode 1))
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))
(use-package semantic
  :ensure t
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

(delete-selection-mode 1)
(global-hl-line-mode 1)
(abbrev-mode 1)
(global-auto-revert-mode 1)
(rcirc-track-minor-mode 1)

(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       '(("\\.html\\'" . web-mode))
       auto-mode-alist))

(defun ShadowRZ/dired-modes ()
  "Dired modes setup."
  (dired-hide-details-mode 1)
  (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'ShadowRZ/dired-modes)

(provide 'init-packages)
;;; init-packages.el ends here
