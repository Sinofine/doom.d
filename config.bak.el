;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Sinofine Lotusie"
      user-mail-address "i@sinofine.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Luxi Mono" :size 18));(font-spec :family "Noto Sans Mono CJK SC" :size 14)

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-opera)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq confirm-kill-emacs nil)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq default-frame-alist '((width . 90)
                            (height . 50)
                            (alpha-background . 80)))
(pixel-scroll-precision-mode)
(cnfonts-mode 1)

(setq fancy-splash-image (concat doom-user-dir "logo/hfut.svg"))
(setq +doom-dashboard-banner-padding '(2 . 3))
(setq +doom-dashboard-menu-sections '(
                                      ("Recently opened files" :icon
                                       (nerd-icons-faicon "nf-fa-file_text" :face 'doom-dashboard-menu-title)
                                       :action recentf-open-files)
                                      ("Open org-agenda" :icon
                                       (nerd-icons-octicon "nf-oct-calendar" :face 'doom-dashboard-menu-title)
                                       :when
                                       (fboundp 'org-agenda)
                                       :action org-agenda)
                                      ("Open telega" :icon
                                       (nerd-icons-faicon "nf-fae-telegram" :face 'doom-dashboard-menu-title)
                                       :action telega)
                                      ("Open project" :icon
                                       (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
                                       :action projectile-switch-project)
                                      ("Open private configuration" :icon
                                       (nerd-icons-octicon "nf-oct-tools" :face 'doom-dashboard-menu-title)
                                       :when
                                       (file-directory-p doom-user-dir)
                                       :action doom/open-private-config)
                                      ))
(map! :leader :desc "Open nix folder" :n "nix" (lambda ()
                                                 (interactive)
                                                 (doom-project-find-file "~/nixconfig")))
(map! :leader :desc "Open telega" :n "ot" 'telega)
;; (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
(defun my-yas-try-expanding-auto-snippets ()
  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))
(add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)
;;(after! yasnippet (map! :map yas-minor-mode-map "SPC" yas-maybe-expand))
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
(setq org-latex-preview-default-process 'dvisvgm )
(setq org-latex-preview-options '(:foreground auto :background "Transparent" :scale 1 :matchers
                                  ("begin" "$1" "$" "$$" "\\(" "\\[")
                                  :zoom 1.0)
      )
(setq cnfonts-personal-fontnames '(("Luxi Mono") ("HYXinRenWenSongW") () ()))
(setq preview-image-type 'dvipng)
(set-default 'preview-default-document-pt 12)
(set-default 'preview-scale-function 0.8)
(setq xenops-math-image-scale-factor .5)
;; (add-hook 'latex-mode-hook #'xenops-mode)
;; (add-hook 'LaTeX-mode-hook #'xenops-mode)
;; (add-hook 'org-mode-hook #'xenops-mode)
;;(setq lsp-rust-analyzer-store-path (shell-command-to-string "readlink -f $(which rust-analyzer )"))
(load! "/home/sinofine/.local/share/whizzytex/emacs/whizzytex.el")
(setq telega-avatar-workaround-gaps-for '(return t)
      telega-emoji-use-images nil)
(setq cnfonts-use-face-font-rescale t)
(define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
(define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)
(add-hook 'telega-load-hook (lambda ()
                              (set-face-attribute 'telega-msg-heading nil
                                                  :background nil
                                                  :underline 't
                                                  :height 1.2
                                                  )
                              (set-face-attribute 'telega-msg-inline-forward nil
                                                  ;; :background "light gray"
                                                  :underline nil
                                                  :height 0.84)
                              (set-face-attribute 'telega-msg-inline-reply nil
                                                  ;; :background "light gray"
                                                  :underline nil
                                                  :height 0.84)
                              ))
(setq telega-brackets
      `(((chat (type private bot))
         ;; ,(concat (nerd-icons-mdicon "nf-md-robot_outline") " ")
         ,(concat " "
                  (nerd-icons-mdicon "nf-md-account"
                                     :face '(:foreground "#86dffd" :height 0.7))
                  " ") " ")
        ((chat (type basicgroup))
         ,(concat " "
                  (nerd-icons-mdicon "nf-md-account_multiple"
                                     :face '(:foreground "#70bcff" :height 0.7))
                  " ") " ")
        ((chat (type supergroup))
         ,(concat " "
                  (nerd-icons-mdicon "nf-md-account_multiple"
                                     :face '(:foreground "#70bcff" :height 0.7))
                  " ") " ")
        ((chat (type channel))
         ,(concat " "
                  (nerd-icons-faicon "nf-fa-feed"
                                     :face '(:foreground "#ffa95f" :height 0.7))
                  " ") " ")
        ((user (return t))
         ,(concat " "
                  (nerd-icons-mdicon "nf-md-account"
                                     :face '(:foreground "#86dffd" :height 0.7))
                  " ") " ")
        ((return t)
         ,(concat " "
                  (nerd-icons-faicon "nf-fa-question_circle"
                                     :face '(:foreground "#ff0000" :height 0.7))
                  " ") " "))
      )
(defun init-cjk-fonts (_fsl)
  ;; (dolist (charset '(kana han cjk-misc bopomofo))
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;     charset (font-spec :family "HYXinRenWenSongW" :size 20)))
  (set-fontset-font (frame-parameter nil 'font) 'emoji (font-spec :family "Segoe UI Emoji"))
  )
(add-hook 'cnfonts-set-font-finish-hook 'init-cjk-fonts)
(require 'telega-bridge-bot)
(setq telega-bridge-bot-bridge-info-plist
      '(-1001773572820 ; id of the @emacs_china
        (420415423 ; id of the @matrix_t2bot
         ;; will fetch member info with this matrix room id
         (:type :matrix :chat-id "!EGzPXoyqkJdTByDCjD:mozilla.org"))
        -1001882351848
        (5846938060
         (:type :matrix :chat-id "!uoEcEMNaQYWmDWUQYY:matrix.org")))
      telega-bridge-bot-matrix-user "@sinofine:envs.net")

;; init sage
(custom-set-variables
 '(sage-shell:use-prompt-toolkit nil)
 '(sage-shell:use-simple-prompt t)
 '(sage-shell:set-ipython-version-on-startup nil)
 '(sage-shell:check-ipython-version-on-startup nil))
;; Run SageMath by M-x run-sage instead of M-x sage-shell:run-sage
(sage-shell:define-alias)

;; Turn on eldoc-mode in Sage terminal and in Sage source files
(add-hook 'sage-shell-mode-hook #'eldoc-mode)
(add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)

;; org mode settings
;; Ob-sagemath supports only evaluating with a session.
(setq org-babel-default-header-args:sage '((:session . t)
                                           (:results . "output")))

;; C-c c for asynchronous evaluating (only for SageMath code blocks).
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))

;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; Do not evaluate code blocks when exporting.
(setq org-export-use-babel nil)

;; Show images when opening a file.
(setq org-startup-with-inline-images t)

;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(setq cdlatex-math-modify-alist '((102 "\\mathfrak" "\\textsf" t nil nil)))
