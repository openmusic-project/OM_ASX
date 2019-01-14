;--------------------------------------------------
;Package to communicate with AudioSculpt

;OM functions by Hans Tutschku (Ircam) 10/10/98
;version 1.01 16/3/99
;version 1.1  29/10/99
;version 1.2  28/02/02
;version 1.3  10/12/03 for AS OSX (commande supervp)
;version 2.1  april 2005 for AS 2.4.1

;--------------------------------------------------
(defvar ASX)

(defpackage ASX
  (:nicknames "as"))

(in-package ASX)

 
;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *sources-dir* "ASX source files")
(setf *sources-dir* (append (pathname-directory *load-pathname*) (list "sources")))

(defvar *OM_ASX-lib-files* nil)
(setf *OM_ASX-lib-files* '(
                           "OM-ASX-Sounds"
                           "OM-ASX-util" 
                           "OM-ASX-timing"
                           "OM-ASX-fbande"
                           "OM-ASX-fbreakpt"
                           "OM-ASX-cross"
                           "OM-ASX-fof"
                           "OM-ASX-transpose"
                           "OM-ASX-clipping"
                           ))


;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'(lambda (file) (om::compile&load (make-pathname :directory *sources-dir* :name file))) *OM_ASX-lib-files*)

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '(
        ("01-timestretch" nil nil (time-stretch stretch-dyn-random stretch-dyn-exact) nil)
        ("02-transpose" nil nil (transposition trans-melody vibrato fund-trans) nil)
        ("03-fbande" nil nil (fbande freq-fbande fund-fbande chord-to-fbande) nil)
        ("04-fbreakpt" nil nil (fbreakpt partials-fbreakpt2) nil)
        ("05-formantfilter" nil nil (fundamental-fof seq-to-fifof fof fifof) nil)
        ("06-clipping" nil nil (clipping clip-par) nil)
        ("07-util" nil nil (write-lists2 write-lists3 vel-db vel-db-list db-vel db-vel-list list-sounds snd-duration fund-zero-filter) nil)
        ("08-set-path" nil nil (set-audiosculpt-folder print-audiosculpt-folder set-om-as-parameterfolder print-om-as-parameterfolder 
                                                       set-om-as-source-soundfolder print-om-as-source-soundfolder set-om-as-result-soundfolder 
                                                       print-om-as-result-soundfolder) nil)))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)

(om::set-lib-release 2.2)

(om::add-lib-alias "OM_AS" "OM_ASX")
