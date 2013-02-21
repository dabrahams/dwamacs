(require 'grep-ed nil 'noerror)

(cl-flet ((set-face-foreground (&rest args))
       (set-face-background (&rest args)))
  (let (grep-hit-face)
    (require 'grep+ nil 'noerror)))
