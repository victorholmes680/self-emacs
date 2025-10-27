;; init-treemacs.el
(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)
        ("C-x t t" . treemacs)
        ("C-x t B" . treemacs-bookmark))
  :config
  (setq treemacs-width 30
        treemacs-is-never-other-window t))

(use-package treemacs-evil
  :after treemacs
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)
