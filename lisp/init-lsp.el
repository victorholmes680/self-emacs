;; init-lsp.el
;; Simplified LSP configuration for Java development

;; LSP Mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :config
  ;; Basic LSP settings
  (setq lsp-auto-guess-root t
        lsp-prefer-capf t
        lsp-enable-file-watchers t
        lsp-enable-snippet t
        lsp-enable-symbol-highlighting t
        lsp-signature-auto-activate t
        lsp-signature-auto-activate-delay 0.2
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable t
        lsp-completion-enable t)

  ;; Performance
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 100000000)

  ;; Key bindings
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l g") 'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-c l f") 'lsp-format-buffer))

;; LSP UI
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-delay 0.2
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-peek-enable t))

;; Company mode for completion
(use-package company
  :commands company-mode
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-show-numbers t)

  ;; Key bindings
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)

  (global-company-mode))

;; Yasnippet for code templates
(use-package yasnippet
  :commands yas-global-mode
  :config
  (yas-global-mode 1))

;; Flycheck for syntax checking
(use-package flycheck
  :commands flycheck-mode
  :config
  (global-flycheck-mode))

;; Auto-enable LSP for Java
(add-hook 'java-mode-hook
          (lambda ()
            (when (fboundp 'lsp-deferred)
              (lsp-deferred))))

(provide 'init-lsp)