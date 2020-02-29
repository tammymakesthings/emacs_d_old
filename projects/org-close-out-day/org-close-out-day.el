(require 'org-element)

(defcustom *tlc/org-close-out-migrate-headings*
  '(
    ("Work Tasks" . '())
    ("Personal Tasks" . '())
    ("EOS - PSA Track" . '())
    ("Draft SOWs" . '())
    ("Upload SOWs to SpringCM" . '())
    ("Send SOWs for Signature" . '()))
  "The collection of migration data.

Each item is a cons cell. The car of the cell is the text of a heading
containing todos to be migrated. The cdr of the cell is a list in which
the items to be mgirated will be collected."
  :type 'setting :group 'tlc-org-close-out)

(defcustom *tlc/org-close-out-entry-template*
    '(
        (date-header :type datestamp :level 8)
        (capacity-mgr :type heading :text "Capacity Manager" :level 10)
        (draft-sows :type todo-list :text "Draft SOWs" :level 12 :todo-state "TODO" :cookie t :has-children t)
        (upload-sows :type todo-list :text "Upload SOWs to SpringCM" :level 12 :todo-state "TODO" :cookie t :has-children t)
        (send-sows :type todo-list :text "Send SOWs for Signature" :level 12 :todo-state "TODO" :cookie t :has-children t)
        (eos-tasks :type todo-list :text "EOS - PSA Track" :level 10 :todo-state "TODO" :tag ":EOS" :has-children t)
        (work-tasks :type todo-list :text "Work Tasks" :level 10 :todo-state "TODO" :has-children t)
        (personal-tasks :type todo-list :text "Personal Tasks" :level 10 :todo-state "TODO" :has-children t)
    )
    "The template for the new todo entry. Each element in the list is a plist which
can contain the following elements:

    :type            The entry type.
    :level           The outline level for the entry.
    :text            The text for the entry heading.
    :todo-state      A TODO state tag. Will be prepended to the entry if specified.
    :cookie          If non-nil, a progress cookie will be appended to the entry. If
                     this has the value 'percent, a percentage cookie will be used.
                     Otherwise an item coount ([1/3]) style cookie will be used.
    :tag             If specified, the tag will be applied to the entry.
    :has-children    If t, look for children in *tlc/org-close-out-migrate-headings*
                     and add them. They will be added at entry level (:level + 2)

The :type attribute of each element can be one of the following symbols:

    datestamp    A date stamp (in %Y-%m-%d %A) format will be used as the text.
                 The child entries list will not be consulted.
    heading      The value of the text attribute will be used as the heading.
                 The child entry list will not be consulted.
    todo-list    A todo entry will be created. Child tasks will be added from the
                 child entry list if desired."
  :type 'setting :group 'tlc-org-close-out)

(defcustom org-close-out-day-before-generate-hook nil
  "Hook function called before the day entry is generated. The collected todos to
be migrated will be present in *tlc/org-close-out-migrate-headings so you can modify
them if desired."
  :type 'hook :group 'tlc-org-close-out)

(defcustom org-close-out-day-after-generate-hook nil
  "Hook function called atfer the day entry is generated."
  :type 'hook :group 'tlc-org-close-out)

(defvar tlc/org-close-out-day-generated-entry nil
  "The new day's entry generatted by tlc/org-close-out-day--generate-new-entry.
If you want to manipulate this entry before it's added to the outline, you can
define an org-close-out-day-after-generate-hook hook to modify this value.")

(defun tlc/org-close-out-day--generate-new-entry-elt (elt-template)
  "Generate a single entry from the template."
  (let (
        (the-elt '())
        )

    (setq the-elt (append (make-string (plist-get elt-template :level) "*")))
    (if (plist-get elt-template 'todo-state))
      (setq the-elt (append (plist-get elt-template 'todo-state') the-elt))

    (cond
      ((eq (plist-get elt-template 'type) 'datestamp)
       (setq the-elt (append (format-time-string "%Y-%m-%d %A") the-elt )))
      (t (setq the-elt (append (plist-get elt-template 'text)))))

    (if (plist-get elt-template 'cookie'))
      (cond
        ((eq (plist-get elt-template 'cookie) 'percent)
         (setq the-elt (append "[%]" the-elt)))
        (t (setq the-elt (append "[/]" the-elt))))

    (if (plist-get elt-template 'tag)
      (setq the-elt (append (concat (":" (plist-get elt-template 'tag) ":") the-elt))))

    the-elt))

(defun tlc/org-close-out-day--generate-new-entry nil
  "Generate the list of entry lines for the new entry.

Calls the org-close-out-day-before-generate and org-close-out-day-after-generate
hooks if they're defined, so the generated entry can be modified."

; org-close-out-day-before-generate-hook
; org-close-out-day-after-generate-function
    (run-hooks 'org-close-out-day-before-generate-hook)

    ;; TODO: Build the entry list

    (run-hooks 'org-close-out-day-after-generate-hook)
    tlc/org-close-out-day-generated-entries)

(defun tlc/org-close-out-day--extract-todos-from-elt (doc element)
    ""
)

(defun tlc/org-close-out-day ()
  "Close out a daily entry.

If the element at point is a headline and its :CLOSED_OUT: property is not set,
todos will be extracted and migrated. The list of subheadings to be processed
is defined in *tlc/org-close-out-migrate-headings*.

The migration process proceeds as follows:

1. Empty all the collecteed item lists in *tlc/org-close-out-migrate-headings*.
2. Narrow the buffer to the current subtree
3. For each heading in *tlc/org-close-out-migrate-headings*, look for subheadings
   with a todo state of TODO, DOING, or WAITING. For each of these that is found,
   add it to the appropriate list in *tlc/org-close-out-migrate-headings* and then
   mark it as MIGRATED.
4. Set the :CLOSED_OUT: property of the migrated entry.
5. Widen the buffer.
6. Create a new daily entry for today.
7. Insert all of the migrated tasks (and daily tasks) into the new entry. The ebtry
   format is defined by *tlc/org-close-out-entry-template*.
"
  (interactive)

  (let ((analyze-element (org-element-at-point))
        (analyze-tree)
        (new-day-element)
        (dolist-elt)
        (tree-node)
        )
    (save-excursion
      (if (and (eq (car analyze-element) 'headline)
               (not (member :CLOSED_OUT (assoc (cdr analyze-element) :tags))))
          (progn
            ; Do the things
            (org-narrow-to-subtree)
            (setq analyze-tree (org-parse-buffer))
            (dolist (dolist-elt analyze-tree tree-node)
              (print (format "Got element \"%s\" of type \"%s\ (parent=\"%s\")"
                             (assoc :raw-value (cdr dolist-elt))
                             (car dolist-elt)
                             (assoc :parent (cdr dolist-elt)))))
            (message "Not on a daily entry that can be closed out")))))
  ;; If we're looking at a day entry AND
  ;; If the entry is for yesterday's date AND
  ;; If the entry does not have the close-out property set
  ;;     1. Find the Capacity Manager todo entries
  ;;     2. For each of those entries, collect its incomplete children
  ;;     3. Collect incomplete children from Work Todos
  ;;     4. Collect incomplete children from Personal Todos
  ;;     5. Build today's entry and append it
  ;;     6. Set the close-out property.
  ;;     7. Set the start-up folded property.
  ;; Otherwise display a message.

  )))

;; (org-data nil
;;     (headline (:raw-value "2020-02-27 Thursday" :begin 6688 :end 7386 :pre-blank 0 :contents-begin 6716 :contents-end 7386 :level 4 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 6688 :STARTUP "folded" :title (#("2020-02-27 Thursday" 0 19 (:parent #1))) :parent #0)
;;     (section (:begin 6716 :end 6753 :contents-begin 6716 :contents-end 6753 :post-blank 0 :post-affiliated 6716 :parent #1) (property-drawer (:begin 6716 :end 6753 :contents-begin 6729 :contents-end 6747 :post-blank 0 :post-affiliated 6716 :parent #2) (node-property (:key "STARTUP" :value "folded" :begin 6729 :end 6747 :post-blank 0 :post-affiliated 6729 :parent #3))))
;;      (headline (:raw-value "Capacity Manager" :begin 6753 :end 7091 :pre-blank 0 :contents-begin 6780 :contents-end 7091 :level 5 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 6753 :title (#("Capacity Manager" 0 16 (:parent #2))) :parent #1)     (headline (:raw-value "Draft SOWs [1/2]" :begin 6780 :end 6935 :pre-blank 0 :contents-begin 6814 :contents-end 6935 :level 6 :priority nil :tags nil :todo-keyword #("DONE" 0 4 (fontified t line-prefix #("**********" 0 10 (face org-indent)) wrap-prefix #("********************* " 0 10 (face org-indent) 10 22 (face org-indent))
;; org-todo-head #("TODO" 0 4 (face (:foreground "red" :weight bold))) face (:foreground "medium orchid" :weight bold))) :todo-type done :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 6780 :closed (timestamp (:type inactive :raw-value "[2020-02-28 Fri 07:05]" :year-start 2020 :month-start 2 :day-start 28 :hour-start 7 :minute-start 5 :year-end 2020 :month-end 2 :day-end 28 :hour-end 7 :minute-end 5 :begin 6822 :end 6844 :post-blank 0)) :title (#("Draft SOWs " 0 11 (:parent #3))
;; (statistics-cookie (:begin 6808 :end 6813 :value "[1/2]" :post-blank 0 :parent #3))) :parent #2)
;; (section (:begin 6814 :end 6845 :contents-begin 6814 :contents-end 6845 :post-blank 0 :post-affiliated 6814 :parent #3) (planning (:closed (timestamp (:type inactive :raw-value "[2020-02-28 Fri 07:05]" :year-start 2020 :month-start 2 :day-start 28 :hour-start 7 :minute-start 5 :year-end 2020 :month-end 2 :day-end 28 :hour-end 7 :minute-end 5 :begin 6822 :end 6844 :post-blank 0)) :deadline nil :scheduled nil :begin 6814 :end 6845 :post-blank 0 :post-affiliated 6814 :parent #4)))
;;      (headline (:raw-value "Revise AM pilot SOWs per Denise's email" :begin 6845 :end 6935 :pre-blank 0 :contents-begin 6904 :contents-end 6935 :level 7 :priority nil :tags nil :todo-keyword #("DONE" 0 4 (fontified t line-prefix #("************" 0 12 (face org-indent)) wrap-prefix #("************************* " 0 12 (face org-indent) 12 26 (face org-indent))
;; org-todo-head #("TODO" 0 4 (face (:foreground "red" :weight bold))) face (:foreground "medium orchid" :weight bold) org-category "master" org-stats 50)) :todo-type done :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 6845 :closed (timestamp (:type inactive :raw-value "[2020-02-27 Thu 13:18]" :year-start 2020 :month-start 2 :day-start 27 :hour-start 13 :minute-start 18 :year-end 2020 :month-end 2 :day-end 27 :hour-end 13 :minute-end 18 :begin 6912 :end 6934 :post-blank 0)) :title (#("Revise AM pilot SOWs per Denise's email" 0 39 (:parent #4))) :parent #3) (section (:begin 6904 :end 6935 :contents-begin 6904 :contents-end 6935 :post-blank 0 :post-affiliated 6904 :parent #4) (planning (:closed (timestamp (:type inactive :raw-value "[2020-02-27 Thu 13:18]" :year-start 2020 :month-start 2 :day-start 27 :hour-start 13 :minute-start 18 :year-end 2020 :month-end 2 :day-end 27 :hour-end 13 :minute-end 18 :begin 6912 :end 6934 :post-blank 0)) :deadline nil :scheduled nil :begin 6904 :end 6935 :post-blank 0 :post-affiliated 6904 :parent #5)))))
;; (headline (:raw-value "Upload SOWs to SpringCM [0/0]" :begin 6935 :end 7013 :pre-blank 0 :contents-begin 6982 :contents-end 7013 :level 6 :priority nil :tags nil :todo-keyword #("DONE" 0 4 (fontified t line-prefix #("**********" 0 10 (face org-indent)) wrap-prefix #("********************* " 0 10 (face org-indent) 10 22 (face org-indent))
;; org-todo-head #("TODO" 0 4 (face (:foreground "red" :weight bold))) face (:foreground "medium orchid" :weight bold))) :todo-type done :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 6935 :closed (timestamp (:type inactive :raw-value "[2020-02-28 Fri 07:05]" :year-start 2020 :month-start 2 :day-start 28 :hour-start 7 :minute-start 5 :year-end 2020 :month-end 2 :day-end 28 :hour-end 7 :minute-end 5 :begin 6990 :end 7012 :post-blank 0)) :title (#("Upload SOWs to SpringCM " 0 24 (:parent #3)) (statistics-cookie (:begin 6976 :end 6981 :value "[0/0]" :post-blank 0 :parent #3))) :parent #2) (section (:begin 6982 :end 7013 :contents-begin 6982 :contents-end 7013 :post-blank 0 :post-affiliated 6982 :parent #3) (planning (:closed (timestamp (:type inactive :raw-value "[2020-02-28 Fri 07:05]" :year-start 2020 :month-start 2 :day-start 28 :hour-start 7 :minute-start 5 :year-end 2020 :month-end 2 :day-end 28 :hour-end 7 :minute-end 5 :begin 6990 :end 7012 :post-blank 0)) :deadline nil :scheduled nil :begin 6982 :end 7013 :post-blank 0 :post-affiliated 6982 :parent #4))))
;;  (headline (:raw-value "Send SOWs for signature [0/0]" :begin 7013 :end 7091 :pre-blank 0 :contents-begin 7060 :contents-end 7091 :level 6 :priority nil :tags nil :todo-keyword #("DONE" 0 4 (fontified t line-prefix #("**********" 0 10 (face org-indent)) wrap-prefix #("********************* " 0 10 (face org-indent) 10 22 (face org-indent)) org-todo-head #("TODO" 0 4 (face (:foreground "red" :weight bold))) face (:foreground "medium orchid" :weight bold))) :todo-type done :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 7013 :closed (timestamp (:type inactive :raw-value "[2020-02-28 Fri 07:05]" :year-start 2020 :month-start 2 :day-start 28 :hour-start 7 :minute-start 5 :year-end 2020 :month-end 2 :day-end 28 :hour-end 7 :minute-end 5 :begin 7068 :end 7090 :post-blank 0)) :title (#("Send SOWs for signature " 0 24 (:parent #3)) (statistics-cookie (:begin 7054 :end 7059 :value "[0/0]" :post-blank 0 :parent #3))) :parent #2) (section (:begin 7060 :end 7091 :contents-begin 7060 :contents-end 7091 :post-blank 0 :post-affiliated 7060 :parent #3) (planning (:closed (timestamp (:type inactive :raw-value "[2020-02-28 Fri 07:05]" :year-start 2020 :month-start 2 :day-start 28 :hour-start 7 :minute-start 5 :year-end 2020 :month-end 2 :day-end 28 :hour-end 7 :minute-end 5 :begin 7068 :end 7090 :post-blank 0)) :deadline nil :scheduled nil :begin 7060 :end 7091 :post-blank 0 :post-affiliated 7060 :parent #4))))
;; )
;;     (headline (:raw-value "EOS - PSA Track" :begin 7091 :end 7240 :pre-blank 0 :contents-begin 7161 :contents-end 7240 :level 5 :priority nil :tags (#("EOS" 0 3 (org-category "master" keymap (keymap (C-down-mouse-1 . org-mouse-move-tree-start) (C-drag-mouse-1 . org-mouse-move-tree) (follow-link . mouse-face) (mouse-3) (mouse-2 . org-open-at-mouse)) mouse-face highlight face (:foreground "gold" :weight bold) fontified t wrap-prefix #("***************** " 0 8 (face org-indent) 8 18 (face org-indent)) line-prefix #("********" 0 8 (face org-indent)) font-lock-fontified t))) :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 7091 :title (#("EOS - PSA Track" 0 15 (:parent #2))) :parent #1) (headline (:raw-value "Update EOS Track Status Report" :begin 7161 :end 7240 :pre-blank 0 :contents-begin 7209 :contents-end 7240 :level 6 :priority nil :tags nil :todo-keyword #("DONE" 0 4 (fontified t line-prefix #("**********" 0 10 (face org-indent)) wrap-prefix #("********************* " 0 10 (face org-indent) 10 22 (face org-indent)) org-todo-head #("TODO" 0 4 (face (:foreground "red" :weight bold))) face (:foreground "medium orchid" :weight bold))) :todo-type done :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 7161 :closed (timestamp (:type inactive :raw-value "[2020-02-27 Thu 15:30]" :year-start 2020 :month-start 2 :day-start 27 :hour-start 15 :minute-start 30 :year-end 2020 :month-end 2 :day-end 27 :hour-end 15 :minute-end 30 :begin 7217 :end 7239 :post-blank 0)) :title (#("Update EOS Track Status Report" 0 30 (:parent #3))) :parent #2) (section (:begin 7209 :end 7240 :contents-begin 7209 :contents-end 7240 :post-blank 0 :post-affiliated 7209 :parent #3) (planning (:closed (timestamp (:type inactive :raw-value "[2020-02-27 Thu 15:30]" :year-start 2020 :month-start 2 :day-start 27 :hour-start 15 :minute-start 30 :year-end 2020 :month-end 2 :day-end 27 :hour-end 15 :minute-end 30 :begin 7217 :end 7239 :post-blank 0)) :deadline nil :scheduled nil :begin 7209 :end 7240 :post-blank 0 :post-affiliated 7209 :parent #4)))))
;;      (headline (:raw-value "Tasks - Work" :begin 7240 :end 7263 :pre-blank 0 :contents-begin nil :contents-end nil :level 5 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 7240 :title (#("Tasks - Work" 0 12 (:parent #2))) :parent #1))
;;      (headline (:raw-value "Tasks - Personal" :begin 7263 :end 7371 :pre-blank 0 :contents-begin 7290 :contents-end 7370 :level 5 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 1 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 7263 :title (#("Tasks - Personal" 0 16 (:parent #2))) :parent #1) (headline (:raw-value "Call shop to check in about car" :begin 7290 :end 7370 :pre-blank 0 :contents-begin 7339 :contents-end 7370 :level 6 :priority nil :tags nil :todo-keyword #("DONE" 0 4 (org-todo-head #("TODO" 0 4 (face (:foreground "red" :weight bold))) line-prefix #("**********" 0 10 (face org-indent)) wrap-prefix #("********************* " 0 10 (face org-indent) 10 22 (face org-indent)) fontified t face (:foreground "medium orchid" :weight bold))) :todo-type done :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 7290 :closed (timestamp (:type inactive :raw-value "[2020-02-27 Thu 15:30]" :year-start 2020 :month-start 2 :day-start 27 :hour-start 15 :minute-start 30 :year-end 2020 :month-end 2 :day-end 27 :hour-end 15 :minute-end 30 :begin 7347 :end 7369 :post-blank 0)) :title (#("Call shop to check in about car" 0 31 (:parent #3))) :parent #2) (section (:begin 7339 :end 7371 :contents-begin 7339 :contents-end 7370 :post-blank 1 :post-affiliated 7339 :parent #3) (planning (:closed (timestamp (:type inactive :raw-value "[2020-02-27 Thu 15:30]" :year-start 2020 :month-start 2 :day-start 27 :hour-start 15 :minute-start 30 :year-end 2020 :month-end 2 :day-end 27 :hour-end 15 :minute-end 30 :begin 7347 :end 7369 :post-blank 0)) :deadline nil :scheduled nil :begin 7339 :end 7370 :post-blank 0 :post-affiliated 7339 :parent #4)))))
;;      (headline (:raw-value "Notes" :begin 7371 :end 7386 :pre-blank 0 :contents-begin nil :contents-end nil :level 5 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 7371 :title (#("Notes" 0 5 (:parent #2))) :parent #1))))
;; ;; (org-data nil (headline (:raw-value "2020-02-27 Thursday" :begin 6688 :end 7386 :pre-blank 0 :contents-begin 6716 :contents-end 7386 ...) (section (:begin 6716 :end 6753 :contents-begin 6716 :contents-end 6753 :post-blank 0 :post-affiliated 6716 ...) (property-drawer ... ...)) (headline (:raw-value "Capacity Manager" :begin 6753 :end 7091 :pre-blank 0 :contents-begin 6780 :contents-end 7091 ...) (headline ... ... ...) (headline ... ...) (headline ... ...)) (headline (:raw-value "EOS - PSA Track" :begin 7091 :end 7240 :pre-blank 0 :contents-begin 7161 :contents-end 7240 ...) (headline ... ...)) (headline (:raw-value "Tasks - Work" :begin 7240 :end 7263 :pre-blank 0 :contents-begin nil :contents-end nil ...)) (headline (:raw-value "Tasks - Personal" :begin 7263 :end 7371 :pre-blank 0 :contents-begin 7290 :contents-end 7370 ...) (headline ... ...)) (headline (:raw-value "Notes" :begin 7371 :end 7386 :pre-blank 0 :contents-begin nil :contents-end nil ...))))
