(put 'ctl-meta 'rcsid
 "$Id$")

(require 'nums)

(defun char-ctrl (char)
	"set control on char"
	(logior (lsh 1 26) char))


(defconst *ctl* (dec "0x4000000"))
(defun ctl (c) (+ *ctl* c))

; maybe the flag key
(defconst *meta* (dec "0x800000"))
(defun meta (c) (+ *meta* c))

(provide 'ctl-meta)
