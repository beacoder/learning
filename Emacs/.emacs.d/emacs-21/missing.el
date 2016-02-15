;;; missing.el --- pick up those missing features in recent emacs.

(defcustom use-empty-active-region nil
  "Whether \"region-aware\" commands should act on empty regions.
If nil, region-aware commands treat empty regions as inactive.
If non-nil, region-aware commands treat the region as active as
long as the mark is active, even if the region is empty.

Region-aware commands are those that act on the region if it is
active and Transient Mark mode is enabled, and on the text near
point otherwise."
  :type 'boolean
  :version "23.1"
  :group 'editing-basics)

(defun use-region-p ()
  "Return t if the region is active and it is appropriate to act on it.
This is used by commands that act specially on the region under
Transient Mark mode.

The return value is t if Transient Mark mode is enabled and the
mark is active; furthermore, if `use-empty-active-region' is nil,
the region must not be empty.  Otherwise, the return value is nil.

For some commands, it may be appropriate to ignore the value of
`use-empty-active-region'; in that case, use `region-active-p'."
  (and (region-active-p)
       (or use-empty-active-region (> (region-end) (region-beginning)))))

(defun region-active-p ()
  "Return t if Transient Mark mode is enabled and the mark is active.

Some commands act specially on the region when Transient Mark
mode is enabled.  Usually, such commands should use
`use-region-p' instead of this function, because `use-region-p'
also checks the value of `use-empty-active-region'."
  (and transient-mark-mode mark-active))

(provide 'missing)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
