(require 'helm)
(require 'request)

(defcustom helm-wikidata-suggest-item-actions
  '(("Insert QID at point" . helm-wikidata-insert-id-at-point)
    ("Visit item in browser" . helm-wikidata-visit-entity-in-browser)
    ("Kill QID" . helm-wikidata-kill-id))
  "List of actions for `helm-wikidata-suggest-item-source'."
  :group 'helm-wikidata
  :type '(alist :key-type string :value-type function))

(defcustom helm-wikidata-suggest-property-actions
  '(("Insert PID at point" . helm-wikidata-insert-id-at-point)
    ("Visit property in browser" . helm-wikidata-visit-entity-in-browser)
    ("Kill PID" . helm-wikidata-kill-id))
  "List of actions for `helm-wikidata-suggest-property-source'."
  :group 'helm-wikidata
  :type '(alist :key-type string :value-type function))

(defcustom helm-wikidata-suggest-lexeme-actions
  '(("Insert LID at point" . helm-wikidata-insert-id-at-point)
    ("Visit lexeme in browser" . helm-wikidata-visit-entity-in-browser)
    ("Kill LID" . helm-wikidata-kill-id))
  "List of actions for `helm-wikidata-suggest-lexeme-source'."
  :group 'helm-wikidata
  :type '(alist :key-type string :value-type function))

(defcustom helm-wikidata-suggest-form-actions
  '(("Insert FID at point" . helm-wikidata-insert-id-at-point)
    ("Visit form in browser" . helm-wikidata-visit-entity-in-browser)
    ("Kill FID" . helm-wikidata-kill-id))
  "List of actions for `helm-wikidata-suggest-form-source'."
  :group 'helm-wikidata
  :type '(alist :key-type string :value-type function))

(defcustom helm-wikidata-buffer-name "*helm wikidata*"
  "Buffer name where completions of this package are shown")

(defvar helm-wikidata-suggest-search-url
  "https://www.wikidata.org/entity/%s")

(defun helm-wikidata-suggest-set-candidates (type)
  "Set candidates with result and number of wikidata results found."
  (let ((suggestions
         (unless (equal helm-pattern "")
           (helm-wikidata-suggest-fetch type helm-pattern))))
    (mapcar (lambda (suggestion)
              (cons
               ;; The strings are decoded so that Unicode coding character
               ;; (e.g. \345\247\223\346\260\217) are not shown
               (concat
                "QID: "
                (decode-coding-string (plist-get suggestion 'id) 'utf-8)
                ;; Check that suggestion has a "label" or
                ;; "description". If it doesn't the label is not
                ;; shown. It also avoids getting (wrong-type-argument
                ;; stringp nil) when calling decode-coding-string with
                ;; nil.
                (when (plist-get suggestion 'label)
                  (concat
                   "\nLabel: "
                   (decode-coding-string (plist-get suggestion 'label) 'utf-8)))
                (when (plist-get suggestion 'description)
                  (concat
                   "\nDescription: "
                   (decode-coding-string (plist-get suggestion 'description) 'utf-8))))
               (plist-get suggestion 'id)))
            suggestions)))

(defun helm-wikidata-suggest-fetch (type input)
  "Fetch suggestions for INPUT from XML buffer."
  (when (and input (not (equal input "")))
    (request-response-data
     (request "https://www.wikidata.org/w/api.php"
       :sync t
       :params `(("action" . "wbsearchentities")
                 ("search" . ,input)
                 ("format" . "xml")
                 ("errorformat" . "plaintext")
                 ("type" . ,type)
                 ,@helm-wikidata-wbsearchentities-custom-params)
       :parser (lambda ()
                 (cl-loop
                  with result-alist = (xml-get-children
                                       (assq 'search
                                             (cdar
                                              (xml-parse-region (point-min) (point-max))))
                                       'entity)
                  for i in result-alist collect
                  (list
                   'id
                   (alist-get 'id (cadr i))
                   'label
                   (alist-get 'label (cadr i))
                   'description
                   (alist-get 'description (cadr i)))))))))

(defvar helm-wikidata-suggest-item-source
  (helm-build-sync-source "Wikidata Suggest"
    :candidates (lambda ()
                  (helm-wikidata-suggest-set-candidates "item"))
    :action 'helm-wikidata-suggest-item-actions
    :match-dynamic t
    :keymap helm-map
    :multiline t))

(defvar helm-wikidata-suggest-property-source
  (helm-build-sync-source "Wikidata Suggest"
    :candidates (lambda ()
                  (helm-wikidata-suggest-set-candidates "property"))
    :action 'helm-wikidata-suggest-property-actions
    :match-dynamic t
    :keymap helm-map
    :multiline t))

(defvar helm-wikidata-suggest-lexeme-source
  (helm-build-sync-source "Wikidata Suggest"
    :candidates (lambda ()
                  (helm-wikidata-suggest-set-candidates "lexeme"))
    :action 'helm-wikidata-suggest-lexeme-actions
    :match-dynamic t
    :keymap helm-map
    :multiline t))

(defvar helm-wikidata-suggest-form-source
  (helm-build-sync-source "Wikidata Suggest"
    :candidates (lambda ()
                  (helm-wikidata-suggest-set-candidates "form"))
    :action 'helm-wikidata-suggest-form-actions
    :match-dynamic t
    :keymap helm-map
    :multiline t))

(defun helm-wikidata-visit-entity-in-browser (candidate)
  (let ((url (format helm-wikidata-suggest-search-url
                     (url-hexify-string candidate))))
    (browse-url url)))

(defun helm-wikidata-insert-id-at-point (candidate)
  (insert candidate))

(defun helm-wikidata-kill-id (candidate)
  (kill-new candidate))

(defun helm-wikidata-suggest-item ()
  (interactive)
  (helm-other-buffer 'helm-wikidata-suggest-item-source helm-wikidata-buffer-name))

(defun helm-wikidata-suggest-property ()
  (interactive)
  (helm-other-buffer 'helm-wikidata-suggest-property-source helm-wikidata-buffer-name))

(defun helm-wikidata-suggest-lexeme ()
  (interactive)
  (helm-other-buffer 'helm-wikidata-suggest-lexeme-source helm-wikidata-buffer-name))

(defun helm-wikidata-suggest-form ()
  (interactive)
  (helm-other-buffer 'helm-wikidata-suggest-form-source helm-wikidata-buffer-name))

(provide 'helm-wikidata)
