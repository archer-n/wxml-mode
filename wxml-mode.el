;;; wxml-mode.el --- WXML eidting mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'sgml-mode)
(require 'company)

(easy-menu-define sgml-mode-menu sgml-mode-map
  "Menu for WXML mode."
  '("WXML"
    ["Format" wxml-format-buffer]))

(define-derived-mode wxml-mode sgml-mode "WXML"
   "Major mode for editing wxml."
   (setq-local electric-pair-inhibit-predicate #'wxml-electric-pair-conservative-inhibit)
   (setq-local sgml-quick-keys 'close)
   (setq-local completion-at-point-functions '(wxml-completion-at-point-functions))
   (setq-local company-backends '(company-capf))
   (setq-local company-minimum-prefix-length 0)
   (setq-local company-idle-delay 0))

(add-to-list 'auto-mode-alist '("\\.wxml\\'" . wxml-mode))

;; keymap
(defvar wxml-mode-map
  (let ((map (make-keymap)))	;`sparse' doesn't allow binding to charsets.
    map)
  "Keymap for WXML mode. ")

;; electric
(defun wxml-electric-pair-conservative-inhibit (char)
  (or
   ;; I find it more often preferable not to pair when the
   ;; same char is next.
   (eq char (char-after))
   ;; Don't pair up when we insert the <.
   (eq char ?<)
   ;; I also find it often preferable not to pair next to a word.
   (eq (char-syntax (following-char)) ?w)))


;; completion
(defun wxml-completion-tag-name ()
  (let ((json-object-type 'alist)
        (wxml-data (json-read-file "/home/archer/.emacs.d/site-lisp/wxml-mode/wxml-data.json")))
    (wxml-completion-tags (cdr (assq 'tags wxml-data)))))


(defun wxml-completion-tags (wxml-tags)
  (if (vectorp wxml-tags)
      (let ((list (append wxml-tags nil)))
        (mapcar (lambda (wxml-tag)
                  (cdr (assq 'name wxml-tag)))
                list))))

(defun wxml-completion-at-point-functions ()
  (let ((symbol (company-grab-line "<[[:space:]]*\\([[:alnum:]_-]\\)*")))
    (if symbol
        (let ((beg (- (point) (length symbol) -1)) (end (point)))
            (list beg
                  end
                  (completion-table-dynamic
                   (lambda (_)
                     (wxml-completion-tag-name))))))))

;; helper
(defun wxml-format-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun wxml-find-tag-name ()
  (save-excursion
    (let ((sgml-tag (car(sgml-get-context))))
      (if (and (sgml-tag-p sgml-tag)
               (equal (sgml-tag-type sgml-tag) 'open))
          (message (sgml-tag-name sgml-tag))))))

(provide 'wxml-mode)
;;; wxml-mode.el ends here


