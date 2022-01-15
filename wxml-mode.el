;;; wxml-mode.el --- WXML eidting mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'sgml-mode)
(require 'company)


(define-derived-mode wxml-mode sgml-mode "WXML"
   "Major mode for editing wxml."
   (setq-local electric-pair-inhibit-predicate #'wxml-electric-pair-conservative-inhibit)
   (setq-local sgml-quick-keys 'close)
   (setq-local completion-at-point-functions '(wxml-completion-at-point))
   (setq-local company-backends '(company-capf))
   (setq-local company-minimum-prefix-length 0)
   (setq-local company-idle-delay 0))

(add-to-list 'auto-mode-alist '("\\.wxml\\'" . wxml-mode))

;; keymap
(defvar wxml-mode-map
  (let ((map (make-keymap)))	;`sparse' doesn't allow binding to charsets.
    map)
  "Keymap for WXML mode. ")

(easy-menu-define wxml-mode-menu sgml-mode-map
  "Menu for WXML mode."
  '("WXML"
    ["Format" wxml-format-buffer]))

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
                  (let ((tag (cdr (assq 'name wxml-tag)))
                        (signature (cdr (assq 'description wxml-tag))))
                    (wxml-put-property tag 'signature signature)))
                list))))

(defun wxml-completion-at-point ()
  (let ((symbol (company-grab-line "<[[:space:]]*\\([[:alnum:]_-]\\)*")))
    (if symbol
        (let ((beg (- (point) (length symbol) -1)) (end (point)))
            (list beg
                  end
                  (completion-table-dynamic
                   (lambda (_)
                     (wxml-completion-tag-name)))
                  :annotation-function #'wxml-capf--get-annotation
                  :company-docsig #'wxml-cap--get-docsig)))))


(defun wxml-capf--get-annotation (str)
   "Generate annotation for STR.
STR is a candidate in a capf session.  See the implementation of
`wxml-completion-at-point'."
   (wxml-get-property 'annotation str))

(defun wxml-cap--get-docsig (str)
     "Generate docsig for STR.
STR is a candidate in a capf session.  See the implementation of
`wxml-completion-at-point'."
  (wxml-get-property 'signature str))

;;;; Text property

(defun wxml-get-property (field str)
  "Get the text property corresponding to FIELD in STR.
STR should be propertized by `wxml-put-property'.

What it actually does is prefix FIELD by `wxml-', and get that
text property."
  (get-text-property 0 (intern (concat "wxml-" (symbol-name field))) str))

(defun wxml-put-property (str &rest properties)
  "Set the text property of STR.
STR is the string to be modified.  PROPERTIES form a sequence of
PROPERTY VALUE pairs for test properties to add.  Each PROPERTY
is prefixed by \"wxml-\".  Propertized STR is returned."
  (let ((i 0)
        (len (length properties)))
    (while (< (1+ (* 2 i)) len)
      (let ((prop (nth (* 2 i) properties))
            (val (nth (1+ (* 2 i)) properties)))
        (put-text-property 0 (length str)
                           (intern (concat "wxml-" (symbol-name prop)))
                           val str))
      (cl-incf i)))
  str)


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


