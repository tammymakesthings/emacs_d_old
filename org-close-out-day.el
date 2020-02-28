(require 'org-element)

(defun tlc/org-subtree-incomplete-children (parent-element)
  )

(defun tlc/org-close-out-day (nil)
  (let ((analyze-element (org-element-at-point))
        (analyze-tree)
        (new-day-element)
        (dolist-elt)
        (tree-node)
        (cm-draft-sow-tasks '())
        (cm-upload-sow-tasks '())
        (cm-send-sow-tasks '())
        (work-todos '())
        (personal-todos '())
        )
    (save-excursion
      (if (and (eq (car analyze-element) 'headline) 
               (eq (assoc (cdr analyze-element) :headline) (format-time-string "%Y-%m-%d %A" (cl-incf (nth 3 (decode-time)) -1)))
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
