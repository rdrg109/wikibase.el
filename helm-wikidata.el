(require 'helm-net)

(defvar helm-wikidata-suggest-url
  "https://www.wikidata.org/w/api.php?action=wbsearchentities&search=%s&format=xml&errorformat=plaintext&language=es&uselang=es&type=item&limit=20&utf8=1")

(defvar helm-wikidata-suggest-search-url
  "https://www.wikidata.org/wiki/%s")

(defvar helm-wikidata-suggest-default-function
  'helm-wikidata-suggest-set-candidates
  "Default function to use in `helm-wikidata-suggest'.")

(defun helm-wikidata-suggest-set-candidates ()
  "Set candidates with result and number of wikidata results found."
  (let ((suggestions
         (unless (equal helm-pattern "")
           (helm-wikidata-suggest-fetch helm-pattern))))
    (mapcar (lambda (suggestion)
              (cons
               ;; The strings are decoded so that Unicode coding character
               ;; (e.g. \345\247\223\346\260\217) are not shown
               (concat
                "QID: "
                (decode-coding-string (plist-get suggestion 'id) 'utf-8)
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

(defun helm-wikidata-suggest-parser ()
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
    (alist-get 'description (cadr i)))))

(defun helm-wikidata-suggest-fetch (input)
  "Fetch suggestions for INPUT from XML buffer."
  (let ((request (format helm-wikidata-suggest-url
                         (url-hexify-string input))))
    (helm-net--url-retrieve-sync
     request #'helm-wikidata-suggest-parser)))

(defvar helm-source-wikidata-suggest
  (helm-build-sync-source "Wikidata Suggest"
    :candidates (lambda ()
                  (funcall helm-wikidata-suggest-default-function))
    :action 'helm-wikidata-suggest-actions
    :match-dynamic t
    :keymap helm-map
    :multiline t))

(defun helm-wikidata-visit-url (candidate)
  (let ((arg (format helm-wikidata-suggest-search-url
                     (url-hexify-string candidate))))
    (browse-url-xdg-open arg)))

(defun helm-wikidata-insert-id-at-point (candidate)
  (insert candidate))

(defun helm-wikidata-kill-id (candidate)
  (kill-new candidate))

(defcustom helm-wikidata-suggest-actions
  '(("Visit URL" . helm-wikidata-visit-url)
    ("Insert QID at point" . helm-wikidata-insert-id-at-point)
    ("Kill QID" . helm-wikidata-kill-id))
  "List of actions for `helm-source-wikidata-suggest'."
  :group 'helm-wikidata
  :type '(alist :key-type string :value-type function))

(defun helm-wikidata-suggest ()
  "Preconfigured `helm' for Google search with Google suggest."
  (interactive)
  (helm-other-buffer 'helm-source-wikidata-suggest "*helm wikidata*"))
