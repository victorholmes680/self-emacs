;; init-mouse.el
;; Mouse support configuration

;; Enable mouse support in terminal (always available)
(unless (terminal-parameter nil 'xterm-mouse-mode)
  (xterm-mouse-mode 1))

;; Enable mouse wheel support (works in both GUI and terminal)
(mouse-wheel-mode 1)

;; GUI-specific mouse features
(when (display-graphic-p)
  ;; Enable context menu with right mouse button (if available)
  (when (fboundp 'context-menu-mode)
    (context-menu-mode 1)))

;; Enable mouse selection support (works in both environments)
(setq mouse-drag-copy-region t)
(setq mouse-yank-at-point t)

;; Mouse wheel scrolling configuration
;; Terminal-friendly scrolling
(when (not (display-graphic-p))
  ;; Terminal-specific mouse wheel settings
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . 5)))
  ;; Make terminal scrolling more responsive
  (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1))))

;; GUI mouse wheel settings
(when (display-graphic-p)
  ;; Make mouse wheel scroll faster in GUI
  (setq mouse-wheel-scroll-amount '(5 ((shift) . hscroll)))
  (setq mouse-wheel-progressive-speed nil))

;; Enable mouse support for mode line and header line
(setq mouse-1-click-follows-link nil)

;; Make mouse work in minibuffer
(setq minibuffer-follows-selected-frame t)

;; Enable mouse focus (if available)
(when (fboundp 'mouse-autoselect-window-mode)
  (mouse-autoselect-window-mode 1))

;; Smooth scrolling with mouse wheel
(setq scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t)

;; Provide fallback for x-hide-tip function if not available
(unless (fboundp 'x-hide-tip)
  (defun x-hide-tip ()
    "Fallback function for x-hide-tip."
    (when (fboundp 'tooltip-hide)
      (tooltip-hide))))

;; Better mouse behavior for different modes
(dolist (hook '(text-mode-hook
                prog-mode-hook
                org-mode-hook
                dired-mode-hook))
  (add-hook hook #'(lambda ()
                     (local-set-key [mouse-1] 'mouse-set-point)
                     (local-set-key [mouse-2] 'mouse-yank-primary)
                     (local-set-key [mouse-3] 'mouse-save-then-kill))))

(provide 'init-mouse)