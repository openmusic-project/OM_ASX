;****************************
;    OM-functions by Hans Tutschku april 2005

(in-package ASX)

;************

(om::defmethod! ASX::clip-par  ((vals list) (filename string)) 
  :indoc '("list")
  :icon 999
  :doc""
    ;(ASX::save-or-not vals filename)
vals
)
