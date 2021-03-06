;****************************
;   OM-functions by Hans Tutschku 15/10/98 IRCAM
; Heavy modifications to adapt to AS 2.4.1 in Aril 2005

(in-package ASX)


(defvar ASX::*svp_unixpath* nil)
(setf ASX::*svp_unixpath* nil)

(defvar ASX::*om-as-source-sound-folder* nil)
(setf ASX::*om-as-source-sound-folder* nil)
(defvar ASX::*om-as-result-sound-folder* nil)
(setf ASX::*om-as-result-sound-folder* nil)
(defvar ASX::*om-as-parameter-folder* nil)
(setf ASX::*om-as-parameter-folder* nil)

(defvar ASX::*om-as-appli-prefsfile* nil)
(setf ASX::*om-as-appli-prefsfile* (namestring (make-pathname :directory asx::*sources-dir* :name "prefappli")))

(defvar ASX::*om-as-soundout-prefsfile* nil)
(setf ASX::*om-as-soundout-prefsfile* (namestring (make-pathname :directory asx::*sources-dir* :name "prefsoundout")))

(defvar ASX::*om-as-soundin-prefsfile* nil)
(setf ASX::*om-as-soundin-prefsfile* (namestring (make-pathname :directory asx::*sources-dir* :name "prefsoundin")))

(defvar ASX::*om-as-param-prefsfile* nil)
(setf ASX::*om-as-param-prefsfile* (namestring (make-pathname :directory asx::*sources-dir* :name "prefparam")))


(if (probe-file ASX::*om-as-appli-prefsfile*)
  (load ASX::*om-as-appli-prefsfile* :verbose nil)
  (print "FOLDER for the application AUDIOSCULPT must be specified"))

(if (probe-file ASX::*om-as-param-prefsfile*)
  (load ASX::*om-as-param-prefsfile* :verbose nil)
  (print "FOLDERS parameterfiles must be specified"))

(if (probe-file ASX::*om-as-soundin-prefsfile*)
  (load ASX::*om-as-soundin-prefsfile* :verbose nil)
  (print "FOLDERS soundsources must be specified"))

(if (probe-file ASX::*om-as-soundout-prefsfile*)
  (load ASX::*om-as-soundout-prefsfile* :verbose nil)
  (print "FOLDERS resulting sounds must be specified"))



(om::defmethod! ASX::set-AudioSculpt-folder ()
  :icon 999
  :doc "specify the folder where the AudioSculpt application is on your harddrive"
  
  (let ((newpath (namestring (oa::om-choose-directory-dialog))))
    (when newpath
      (setf ASX::*svp_unixpath* (namestring (om::string+ newpath "Kernels")))
      (WITH-OPEN-FILE (out ASX::*om-as-appli-prefsfile* :direction :output  
                           :if-does-not-exist :create :if-exists :supersede) 
        (prin1 '(in-package :ASX) out)
        (let ((*package* (find-package :ASX)))
          (prin1 `(setf ASX::*svp_unixpath* ,(namestring (om::string+ newpath "Kernels"))) out)))
      )
    ))

(om::defmethod! ASX::print-AudioSculpt-folder ()
  :icon 999
  :doc "prints the folder where the AudioSculpt application is on your harddrive"
  (print ASX::*svp_unixpath*)
  )


(om::defmethod! ASX::set-om-as-source-soundfolder ()
  :icon 999
  :doc "specify the folder where the source soundfiles are stored"

  (let ((newpath (namestring (oa::om-choose-directory-dialog))))
    (when newpath
      (setf ASX::*om-as-source-sound-folder* (namestring  newpath ))
      (WITH-OPEN-FILE (out ASX::*om-as-soundin-prefsfile* :direction :output  
                           :if-does-not-exist :create :if-exists :supersede) 
        (prin1 '(in-package :ASX) out)
        (let ((*package* (find-package :ASX)))
          (prin1 `(setf ASX::*om-as-source-sound-folder* ,(namestring  newpath)) out)))
      )
    ))

(om::defmethod! ASX::print-om-as-source-soundfolder ()
  :icon 999
  :doc "prints the folder where the source soundfiles are stored"
  (print ASX::*om-as-source-sound-folder*)
  )


(om::defmethod! ASX::set-om-as-result-soundfolder ()
  :icon 999
  :doc "specify the folder where the resulting soundfiles will be stored"
  
  (let ((newpath (namestring (oa::om-choose-directory-dialog))))
    (when newpath
      (setf ASX::*om-as-result-sound-folder* (namestring  newpath ))
      (WITH-OPEN-FILE (out ASX::*om-as-soundout-prefsfile* :direction :output  
                           :if-does-not-exist :create :if-exists :supersede) 
        (prin1 '(in-package :ASX) out)
        (let ((*package* (find-package :ASX)))
          (prin1 `(setf ASX::*om-as-result-sound-folder* ,(namestring  newpath)) out)))
      )
    ))

(om::defmethod! ASX::print-om-as-result-soundfolder ()
  :icon 999
  :doc "prints the folder where the resulting soundfiles are stored"
  (print ASX::*om-as-result-sound-folder*)
  )

(om::defmethod! ASX::set-om-as-transientfolder ()
  :icon 999
  :doc "specify the folder where the transient SDIF files will be stored"
  
  (let ((newpath (namestring (oa::om-choose-directory-dialog))))
    (when newpath
      (setf ASX::*om-as-transient-folder* (namestring  newpath))
      (WITH-OPEN-FILE (out ASX::*om-as-soundout-prefsfile* :direction :output  
                           :if-does-not-exist :create :if-exists :supersede) 
        (prin1 '(in-package :ASX) out)
        (let ((*package* (find-package :ASX)))
          (prin1 `(setf ASX::*om-as-transient-folder* ,(namestring  newpath)) out)))
      )
    ))

(om::defmethod! ASX::print-om-as-transientfolder ()
  :icon 999
  :doc "prints the folder where the transient SDIF files will be stored"
  (print ASX::*om-as-transient-folder*)
  )



(om::defmethod! ASX::set-om-as-parameterfolder ()
  :icon 999
  :doc "specify the folder where the parameter files will be stored"
  
  (let ((newpath (namestring (oa::om-choose-directory-dialog))))
    (when newpath
      (setf ASX::*om-as-parameter-folder* (namestring  newpath ))
      (WITH-OPEN-FILE (out ASX::*om-as-param-prefsfile* :direction :output  
                           :if-does-not-exist :create :if-exists :supersede) 
        (prin1 '(in-package :ASX) out)
        (let ((*package* (find-package :ASX)))
          (prin1 `(setf ASX::*om-as-parameter-folder* ,(namestring  newpath)) out)))
      )
    ))

(om::defmethod! ASX::print-om-as-parameterfolder ()
  :icon 999
  :doc "prints the folder where the parameter files will be stored"
  (print ASX::*om-as-parameter-folder*)
  )


;;***********************************************************************
;; fonction write-list de Mikhail Malt 

(om::defmethod! ASX::write-lists  ((myfilename string) &rest donnees  ) 
  :initvals '("toto.par" '(1 2 3) )
  :indoc '("myfilename" "list" )
  :icon 999 
  :doc  "Cette boite rassemble les tables et notes pour les imprimer dans un fichier <file.sco>"
  (let (fichier) 
    (setf donnees (om::flat  donnees 1))
    ;;; modif jean : si un fichier est donne on met pas le dialogue
    (setq fichier 
          (print (if myfilename (pathname myfilename)
              (oa::om-choose-new-file-dialog :directory myfilename :prompt "Save txt"))
          ))
    (delete-file fichier)
    (when fichier
      (with-open-file  (fd fichier
                           :direction :output :if-exists :supersede 
                           :if-does-not-exist :create)
        (dotimes (n (length donnees))
          (dotimes (j (length (nth n donnees)))
            (format fd "~D "(nth j (nth n donnees))))
          (format fd "~A" #\Linefeed))
        ))
    ;;; modif jean : retourne le nom du fichier
    fichier
))

;;***********************************************************************
;; fonction write-list de Mikhail Malt 

(om::defmethod! ASX::write-lists2  ( (donnees list ) (myfilename string))
  :initvals '( '(1 2 3) "toto.par")
  :indoc '("list" "filename")
  :icon 999 
  :doc  "Cette boite rassemble les tables et notes pour les imprimer dans un fichier <file.sco>"
  (let (fichier) 
    ;(setf donnees (om::flat  donnees 1))
    (setq fichier (oa::om-choose-new-file-dialog :directory myfilename :prompt "Save txt"))
   (delete-file fichier)
    (when fichier
      (with-open-file  (fd fichier
                           :direction :output :if;(setf donnees (om::flat  donnees 1))
    ;(setq fichier (ccl:choose-new-file-dialog  :directory myfilename :prompt "Save txt"))-exists :supersede 
                           :if-does-not-exist :create)
        (dotimes (n (length donnees))
          (dotimes (j (length (nth n donnees)))
            (format fd "~D "(nth j (nth n donnees))))
          (format fd "~A" #\Linefeed))
        ))))

;(ASX::write-lists "dilat.par" '((1 2 3)(1 2)(1 2 3 4)(1 2 3 4 5)))

;;***********************************************************************


(om::defmethod! ASX::write-lists3  ( (donnees list ) (myfilename string))
  :initvals '( '((1 2) (2323 3)) "G3-Daten02:toto.par")
  :indoc '("list" "filename")
  :icon 999 
  :doc  ""
  (let (fichier) 
    ;(setf donnees (om::flat  donnees 1))
    ;(setq fichier (oa::om-choose-new-file-dialog  :directory myfilename :prompt "Save txt"))
    (setq fichier myfilename)
   (delete-file fichier)
    (when fichier
      (with-open-file  (fd fichier
                           :direction :output :if-exists :supersede 
                           :if-does-not-exist :create)
        (dotimes (n (length donnees))
          (dotimes (j (length (nth n donnees)))
            (format fd "~D "(nth j (nth n donnees))))
          (format fd "~A" #\Linefeed))
        )))
(om::string+ myfilename "     saved")
)


;********************************************************************************************
;functions by Mikhail Malt for BPF

(om::defmethod! ASX::rang? ((liste list) (elem t))
  :initvals '( nil nil)
  :indoc '("liste" "elements")
  :icon 999 
  :doc  "les rangs de elem dans liste"
  (let ((aux nil) (index 0))
    (mapcar #'(lambda (z) 
                (progn (when (funcall 'equalp  z elem) (push index aux))
                                 (incf index))) liste)
    (reverse aux)))

;(ASX::rang? '(1 (3 4) 6 7 8 9) '(3 4))

(om::defmethod! ASX::linear-interpol (x1 x2 y1 y2 x0)
  :initvals '( 1 2 2 5 1.2)
  :indoc '("x1" "x2" "y1" "y2" "x0")
  :icon 999 
  :doc  "interpolation lineaire entre deux points dans le plan

          ^
          |
          |
        y2|..................*
          |                  .
        y0|............*     .
          |            .     .
        y1|......*     .     .
          |      .     .     .
          |      .     .     .
          |      .     .     .
          |______._____._____.______>
                 x1    x0    x2

"
  (if (= x1 x2) y1
      
      (+ y1
         (* (- y2 y1)
            (/ (- x0 x1)
               (- x2 x1))))))

;(ASX::linear-interpol 1 2 2 5 1.2)


(om::defmethod! ASX::x-around (x paires)
  :initvals '( nil nil)
  :indoc '("x" "paires")
  :icon 999 
  :doc  "trouve les paires en dessous et au dessus de x"
  (let* ((plus-grand  (find  x paires :test #'(lambda (x r) (<= x (first r)))))
         (rang (if (< (1- (first (rang?  paires  plus-grand ))) 0) 0
                   (1- (first (rang?  paires  plus-grand )))))
         (plus-petit (nth rang paires)))

    

    (list (if (< rang 0) plus-grand plus-petit) plus-grand)
))


;(find  2 '((-1 0) (1   4) (5 6)) :test #'(lambda (x r) (<= x (first r))))

;(find  x paires :test #'(lambda (x r) (<= x (first r))))

;(nth (1- (first (rang?  paires  plus-grand 'equalp))) paires)

;(ASX::x-around 51 '((0 0) (41 3) (50 6) (69 5) (100 8)))

(om::defmethod! ASX::f-transfer ((bpf om::bpf) (x0 number))
  :initvals '( '(1 2 3) 3.4)
  :indoc '("liste" "x0")
  :icon 999 
  :doc  "transfer avec interpolation lineaire"
  (let* ((paires (om::mat-trans
                  (om::list
                   (om::x-points bpf)
                   (om::y-points bpf))))
         (bornes  (x-around x0 paires)))
    (linear-interpol (caar bornes)            ; x1
                     (first (second bornes)) ; x2
                     (second (first bornes)) ; y1
                     (second (second bornes)) ; y2
                     x0)))
    

(om::defmethod! ASX::f-transfer-l ((bpf om::bpf) (liste list))
  :initvals '( '(1 2 3) 3.4)
  :indoc '("liste" "x0")
  :icon 999 
  :doc  "transfer avec interpolation lineaire"
  (let ((liste (om::list! liste)))
    (mapcar #'(lambda (k) (ASX::f-transfer bpf k)) liste)))


;********************************************************************************************

(om::defmethod! ASX::repeat ((self t) (num integer)) 
  :numouts 1 
  :initvals '(nil 0) 
  :indoc '("patch" "times")
  :doc "a version of om::repeat-n which does not evaluate <self>" 
  :icon 999
  
  (let (rep)
    (loop for i from 1 to num do
          (push self rep))
    (reverse rep)))

;(ASX::repeat  '(0 3 0 0) 2)

;********************************************************************************************
#|
(in-package AS)
(om::defmethod! ASX::list-explode ((list list) 
                                (nlists integer)) 
  :initvals '('(1 2 3 4 5) 2) 
  :indoc '("list" "number") 
  :icon 999
  :doc "list-explode divides a list into <nlist> sublists of consecutives elements.  
For example, if list is (1 2 3 4 5 6 7 8 9), and ncol is 2, the result is ((1 2 3 4 5) 
(6 7 8 9)),
if list is (1 2 3 4 5 6 7 8 9), and ncol is 5, the result is: ((1 2) (3 4) (5 6) (7 8) (9)). 
If the number of divisions exceeds the number of elements in the list, the 
remaining divisions are returned as nil."
  
  (if (> nlists (length list)) 
    (setq list (append list (make-list (- nlists (length list)) :initial-element (first (last list))))))
  (if (<= nlists 1) list
      (let* ((length (length list))
             (low (floor length nlists))
             (high (ceiling length nlists))
             (step (if (< (abs (- length (* (1- nlists) high))) (abs (- length (* nlists low))))
                     high  low))
             (rest (mod length nlists))
             (end (- length 1 rest)) 
             (ser (om::arithm-ser 0  (1- step) 1))
             res)
        (om::for (i 0 step end)
          (push (remove () (om::posn-match list (om::om+ i ser))) res))
        (setq low (length (om::flat-once res)))
        (if (< low length) (setq res (cons (append (first res) (nthcdr low list)) (rest res))))
        (cond ((> (length res) nlists) 
               (nreverse (cons (nconc (second res) (first res)) (nthcdr 2 res))))
              ((< (length res) nlists)
               (when (= (length (first res)) 1)
                 (setq res (cons (nconc (second res) (first res)) (nthcdr 2 res))))
               (nreverse (nconc (nreverse (ASX::list-explode (first res) (1+ (- nlists (length res)))))
                                (rest res))))
              (t (nreverse res))))))

;(ASX::list-explode '(1 2 3 4 5 6) 3)

|#
;********************************************************************************************

(om::defmethod! ASX::vel-db ((vel number)) ;formula from jimmys
  :initvals '('120) 
  :indoc '("MIDI-velodity") 
  :icon 999
  :doc "converts MIDI-velocity (0-127) to dB" 
  
  (om::om-round
   (om::om- 0 (om::om* 0.599694 (om::om- 126 (om::om- vel 1))))
   1)
  )

;(ASX::vel-db 120) 

;********************************************************************************************
(om::defmethod! ASX::vel-db-list ((vel list)) ;formula from jimmys
  :initvals '('(120 127 0 3 60)) 
  :indoc '("list") 
  :icon 999
  :doc "converts MIDI-velocity (0-127) to dB" 
  
  (om::om-round
   (om::om- 0 (om::om* 0.599694 (om::om- 126 (om::om- vel 1))))
   1)
  )

;(ASX::vel-db-list '(127  0 3 60)) 

;********************************************************************************************
(om::defmethod! ASX::db-vel ((dB number)) ;formula from jimmys
  :initvals '('-12) 
  :indoc '("list") 
  :icon 999
  :doc "converts dB to MIDI-velocity (0-127)" 
  
  (om::om-round
   (om::om+ 127 (om::om/ dB 0.599694 )))
  )
;********************************************************************************************
(om::defmethod! ASX::db-vel-list ((dB list)
                                 (lowscale number)
                                 (highscale number)) ;formula from jimmys
  :initvals '('(-50 -60 -12 0) 0 127) 
  :indoc '("list" "lowscale" "highscale") 
  :icon 999
  :doc "converts dB to MIDI-velocity (0-127)" 
  
  (om::om-round
   (om::om-scale 
   (om::om+ 127 (om::om/ dB 0.599694 )) lowscale highscale))
  )

;********************************************************************************************
;x-append, weil die Version von OM noch nicht funktioniert

(om::defmethod! ASX::x-append  (&rest liste)
  
  
  (apply 'append (mapcar #'(lambda (x)
                             (if (atom x)
                               (list x) x) )
                         liste))
  
  
  )

;(ASX::x-append '(1 (2 3) 3) '(4 5) 4 '(8 9 ))
;********************************************************************************************

(om::defmethod! ASX::save-or-not  ((final list)
                                  (filename string))
  (let ((aux nil))
    #|
 (if (< 800 (om::list-max (dolist (n final (nreverse aux))
                               (push (length (format nil "~D" n)) aux))))
      "parameterlines becomes to long for Audiosculpt - reduce parameters"

      (ASX::write-lists filename final)
      )
|#  
    
    (ASX::write-lists (om::string+ ASX::*om-as-parameter-folder* filename) final)
    
    )
  )

;(ASX::save-or-not '((1 2 3) (4 5) (6 7 8 9)) "toto" )


;********************************************************************************************


(om::defmethod! ASX::repeat  ((self t) 
                             (num integer)) 
  :numouts 1 
  :initvals '(nil 0)
  :indoc '("patch" "times")
  :icon 999
  :doc"this patch is creating a parameterfile for Audiosculpt formant-filter from a fundamental analysis"
  
  :doc "repeats n times the evaluation of <self> and collects the n results into a list. " 
  (let* (rep)
    (loop for i from 1 to num do
          (push self rep))
    (reverse rep)))
;(ASX::repeat '(90 6) 5)

;********************************************************************************************
(om::defmethod! ASX::groupe-beats ((liste list))    ;to translate leons midifile
  :initvals '(nil)
  :indoc '("liste des paires temps freqs")
  :icon 999 
  :doc""
  ; aux0 liste d'attente
  
  (let ((aux0 nil) (aux1 nil) (index 2))
    
    (dolist (n liste aux1)
      (if (not (=  (first n) 1))
        (progn ()
               (push (remove nil aux0) aux1)
               (setf aux0 nil)
               (push (list index (second n)) aux0)
               (setf index (+ index 1)))
        (progn ()
               (setf index 2)
               (push n aux0))))
    
    ;(om::mat-trans (list (om::remove-dup  (first (om::mat-trans liste)) 'equalp 1)
     ;                    (remove nil (reverse (push aux0 aux1)))))
    
    )
  )

;********************************************************************************************
(om::defmethod! ASX::rec-read-from-string (string)
  "utilissimo!"
  (labels ((fun (x)
             (multiple-value-list (read-from-string x nil))))
    (if (null (read-from-string string nil))
      nil
      (cons (car (fun string))
            (rec-read-from-string (coerce
                                   (nthcdr
                                    (cadr (fun string))
                                    (coerce string 'list))
                                   'string))))))

(om::defmethod! ASX::string-to-list (string)    ; by Mauro Lanza
  "utilissimo!"
  (labels ((fun (x)
             (multiple-value-list ( x nil))))
    (if (null (read-from-string string nil))
      nil
      (cons (car (fun string))
            (rec-read-from-string (coerce
                                   (nthcdr
                                    (cadr (fun string))
                                    (coerce string 'list))
                                   'string))))))

;********************************************************************************************
;********************************************************************************************
;;***********************************************************************

;; diese Funktionen messen durch neuere ersetzt werden

#|

(om::defmethod! ASX::soundlength-sd2 ((soundname string))
  
  (print "function soundlength-sd2 must be replaced by snd-duration" )
     
  )


; (ASX::soundlength-sd2 "G3-Daten02:sun.sd")

;;***********************************************************************

(om::defmethod! ASX::numChannels-sd2 ((soundname string))
  
     (print "function soundlength-sd2 must be replaced by another function" )
     
  )


;;***********************************************************************

(om::defmethod! ASX::refnum-of-open-file (f)
  (rref (uvref (cdr (ccl::column.fblock f)) 1) :cinfopbrec.iofrefnum))

; ****************************************************************

(om::defmethod! ASX::read-ostype ((s t))
  (let ((a (read-byte s))
        (b (read-byte s))
        (c (read-byte s))
        (d (read-byte s)))
    (coerce (list (code-char a) (code-char b) (code-char c) (code-char d)) 'string)))

; ****************************************************************

(om::defmethod! ASX::read-short ((s t))
  (let ((a (read-byte s))
        (b (read-byte s)))
    (logior (ash a 8) b)))

; ****************************************************************

(om::defmethod! ASX::read-extended ((s t))
  (%stack-block ((x 8))
    (dotimes (i 8)
      (%put-byte x (read-byte s) i))
    (ccl::%get-x2float x)))

; ****************************************************************

(om::defmethod! ASX::read-long ((s t))
  (let ((a (read-byte s))
        (b (read-byte s))
        (c (read-byte s))
        (d (read-byte s)))
    (logior (ash a 24) (ash b 16) (ash c 8) d)))

(defmethod stream-init ((s t))
  (file-position s 12))

; ****************************************************************

(om::defmethod! ASX::look-for-chunck ((s t) ckname)
  (stream-init s)
  (loop while (and (not (stream-eofp s)) (not (string-equal ckname (ASX::read-ostype s)))) do
        (let ((sizeck (ASX::read-long s)))
          (loop for i from 1 to sizeck do
                (read-byte s))))
  (if (stream-eofp s)
    (error "no lo encontre"))
  (ASX::read-long s))

; ****************************************************************

(om::defmethod! ASX::check-soundlength (name)
  :initvals '( "G3-Daten01:Diphone")
  :indoc '("soundname")
  :icon 999 
  :doc  "gives the length of a sound (Aiff or Sd2f)"
  
  (let ((soundlnge nil)
        (number-of-samples 0)
        (number-of-channels 0)
        (sample-rate 5)
        (thesound (open name :element-type 'unsigned-byte)))
    (ASX::read-ostype thesound)
    (ASX::read-ostype thesound)
    (cond
     ((string-equal "AIFF" (ASX::read-ostype thesound))
      
      (ASX::look-for-chunck thesound "COMM")
 
      (ASX::read-short thesound)

      (setf number-of-samples (ASX::read-long thesound))
      (ASX::read-short thesound)
      (setf sample-rate (ASX::read-extended thesound))
      (setf soundl�nge (/ (om::round (* 1000 (/ number-of-samples sample-rate))) 1000.0)))
     (t
      (setf soundl�nge (om::om-round (ASX::soundlength-sd2 name) 3))
      )
     )
    (close thesound)
    soundlnge
    )
  )


; ****************************************************************

(om::defmethod! ASX::check-soundlength-mod (name)
  :initvals '( "G3-Daten01:Diphone")
  :indoc '("soundname")
  :icon 999 
  :doc  "givs the length of a sound (Aiff or Sd2f)"
  
  (let ((soundl�nge nil)
        (number-of-samples 0)
        (sample-rate 5)
        (thesound (open name :element-type 'unsigned-byte)))
    (ASX::read-ostype thesound)
    (ASX::read-ostype thesound)
    (cond
     ((string-equal "AIFF" (ASX::read-ostype thesound))
      
      (ASX::look-for-chunck thesound "COMM")
      ;(setf number-of-channels (ASX::read-short thesound))
      (ASX::read-short thesound)
      (setf number-of-samples (ASX::read-long thesound))
      (ASX::read-short thesound)
      (setf sample-rate (ASX::read-extended thesound))
      (setf soundl�nge (/ (om::round (* 1000 (/ number-of-samples sample-rate))) 1000.0)))
     (t
      (message-dialog "Modalys can't handle SD2-files. Use AIFF!" :ok-text "ok")
      )
     )
    (close thesound)
    soundl�nge
    )
  )

; ****************************************************************

(om::defmethod! ASX::check-numChannels (name)
  :initvals '( "G3-Daten01:Diphone")
  :indoc '("soundname")
  :icon 999 
  :doc  "givs number of Channels of a sound (Aiff or Sd2f)"
  
  (let ((number-of-channels 0)
        (thesound (open name :element-type 'unsigned-byte)))
    (ASX::read-ostype thesound)
    (ASX::read-ostype thesound)
    (cond
     ((string-equal "AIFF" (ASX::read-ostype thesound))
      (ASX::look-for-chunck thesound "COMM")
      (setf number-of-channels (ASX::read-short thesound))
      )
     (t
      (setf number-of-channels (ASX::numChannels-sd2 name))
      )
     )
    (close thesound)
    number-of-channels
    )
  )


; ****************************************************************

(om::defmethod! ASX::look-for-snd2 (lista)
  (if (endp lista)
    nil
    (cons
     (if (directoryp (car lista))
                          (ASX::look-for-snd (car lista))
       (when (or (equalp :aiff (mac-file-type (car lista)))
                 (equalp :|Sd2f| (mac-file-type (car lista))))
         (list  (file-namestring (car lista)) 
                           (ASX::check-numChannels(mac-namestring(car lista)))
                           (ASX::check-soundlength(mac-namestring(car lista)))
                           )
         ))
     (ASX::look-for-snd2 (cdr lista)))))

; ****************************************************************

(om::defmethod! ASX::look-for-snd (folder)
  :initvals '( "G3-Daten01:Diphone")
  :indoc '("soundname")
  :icon 999 
  :doc  ""
  
  (let* ((dir+wildcard (coerce (append (coerce (mac-namestring folder) 'list) '(#\*)) 'string))
         (ergebnis nil)
         (all (directory dir+wildcard :directories "*")))
    (setf ergebnis (remove nil    
                           (om::flat
                            (if (endp all)
                              nil
                              (list
                               (list (mac-namestring folder) 999 " ")
                               (if (directoryp (car all))
                                 (ASX::look-for-snd (car all))
                                 (when (or (equalp :aiff (mac-file-type (car all)))
                                           (equalp :|Sd2f| (mac-file-type (car all))))
                                   (list  (file-namestring (car all)) 
                                          (ASX::check-numChannels(mac-namestring(car all)))
                                          (ASX::check-soundlength(mac-namestring(car all)))
                                          )
                                   ))
                               (ASX::look-for-snd2 (cdr all)))))))
    (om::list-explode ergebnis (/ (length ergebnis) 3)) 
    ))
    

; ****************************************************************

(om::defmethod! ASX::list-sounds (folder myfilename)
  :initvals '( "G3-Daten01:Diphone" "soundlist.txt")
  :indoc '("folder" "soundname")
  :icon 999 
  :doc  ""

  (let (fichier)
    (setq fichier (ccl:choose-new-file-dialog  :directory myfilename :prompt "Save txt"))
    (delete-file fichier)
    (when fichier
      (with-open-file  (fd fichier
                           :direction :output :if-exists :supersede 
                           :if-does-not-exist :create)
        
        (mapcar #'(lambda (z)
                    (if (eq (second z)  999)
                      (format fd "~%~%~A~%~%" (coerce (remove '#\� (coerce (first z) 'list)) 'string))
                      (format fd "~A ~30T~D~34T~D~%" (coerce (remove '#\� (coerce (first z) 'list)) 'string)
                              (second z) (third z))
                      )) (ASX::look-for-snd folder))
        ))))


|#

; ******************* S D I F *****************

; ******************* get-SDIF-fund *****************
(in-package :ASX)

(om::defmethod! ASX::get-SDIF-fund ((self om::sdiffile))
  :doc "Return a list of fundamental freq"
  :icon 999
  (let ((freqlist ())(freqs (make-hash-table)) time freqval mlist bmat
        (ptrfile (om::dynamic-open self)))
    (sdif::SdifFReadGeneralHeader ptrfile)
    (sdif::SdifFReadAllASCIIChunks ptrfile)
    (loop for item in (om::framesdesc self) do
          (when (equal "1FQ0" (car item))                         ; wenn Matrix typ ? fundamental
            (setf time (om::om-round (nth 1 item) 3))
            (om::sdif-set-pos ptrfile (nth 3 item))
            ;;(print (nth 4 item))
            ;;(print item)
            (setf mlist (nth 4 item))
            (loop for mat in mlist do
                  (cond ((equal "1FQ0" (car mat)) (setf bmat mat))
                        (t nil)))
            ;;(print mlist)
            ;;(print (sdif-read-headers ptrfile (nth 4 item) (fifth bmat)))
      
   
            (when bmat 
              ;;; a begin matrix :
              ;;; ajoute (onset (note1 note2 ...)) dans tmplist 
              ;;; avec des valeurs de amp (0) freq (0) et dur (1000) arbitraires pour l'instant
              (om::sdif-read-headers ptrfile (nth 3 item) (fifth bmat))
              (loop for i = 0 then (+ i 1) while (< i (second bmat)) do
                    (sdif::SdifFReadOneRow ptrfile)
                    (setf freqval (om::om-round (sdif::SdifFCurrOneRowCol ptrfile 1) 3 ))
                    (om::sethash freqs (sdif::SdifFCurrOneRowCol ptrfile 1) (list time freqval) )
                    )
              )
            (setf bmat nil)
            ))
    
    
    (maphash #'(lambda (key val)
                 (push val freqlist)) 
             freqs)
    
    
    (om::mat-trans (om::sort. freqlist '< 'first) )

    
    ))

; ******************* get-SDIF-partials *****************



(om::defmethod! ASX::get-SDIF-partials ((self om::sdiffile))
  :doc "Return a list of fundamental freq"
  :icon 999
  (let ((freqlist ())(freqs (make-hash-table)) time freqval ampval mlist bmat freq-aux amp-aux
        (ptrfile (om::dynamic-open self)))
    (sdif::SdifFReadGeneralHeader ptrfile)
    (sdif::SdifFReadAllASCIIChunks ptrfile)
    (loop for item in (om::framesdesc self) do
          (when (equal "1TRC" (car item))                         ; wenn Matrix typ = 1TRC
            (setf time (om::om-round (nth 1 item) 4))
            (om::sdif-set-pos ptrfile (nth 3 item))
            ;;(print (nth 4 item))
            ;;(print item)
            (setf mlist (nth 4 item))
            (loop for mat in mlist do
                  (cond ((equal "1TRC" (car mat)) (setf bmat mat))
                        (t nil)))
            ;;(print mlist)
            ;;(print (sdif-read-headers ptrfile (nth 4 item) (fifth bmat)))
            ;;(print bmat)
            
            (when bmat 
              ;;(print bmat)
              ;;; a begin matrix :
              ;;; ajoute (onset (note1 note2 ...)) dans tmplist 
              ;;; avec des valeurs de amp (0) freq (0) et dur (1000) arbitraires pour l'instant
              (om::sdif-read-headers ptrfile (nth 3 item) (fifth bmat))
              (loop for i = 0 then (+ i 1) while (< i (second bmat)) do
                    (sdif::SdifFReadOneRow ptrfile)
                    (push (om::om-round (sdif::SdifFCurrOneRowCol ptrfile 2) 0) freq-aux)
                    (push (om::om-round (ASX::lin->dB (sdif::SdifFCurrOneRowCol ptrfile 3)) 3) amp-aux)   ;umwandlung von linearen amplituden in dB
                    ;(print ampval)
                    ;(om::sethash freqs (SdifFCurrOneRowCol ptrfile 2) (list time freqval ampval) )
                    )
              ;(print (om::sort. (om::mat-trans (list freq-aux amp-aux)) '< 'first))
              (om::sethash freqs (sdif::SdifFCurrOneRowCol ptrfile 2) (list time (om::sort. (om::mat-trans (list freq-aux amp-aux)) '< 'first)) )
              (setf amp-aux nil)
              (setf freq-aux nil)
              )
            
            (setf bmat nil)
            ))
    
    
    
    
    (maphash #'(lambda (key val)
                 (push val freqlist)) 
             freqs)
    (om::sort. freqlist '< 'first)
    
     ))


;(om::sort. '((1.37 262.338) (0.168 171.455) (0.104 178.288) (0.964 203.631)) '< 'first)

     #|

                 
                 
                 
                 (when bmat 
                     ;;; a begin matrix :
                     ;;; ajoute (onset (note1 note2 ...)) dans tmplist 
                     ;;; avec des valeurs de amp (0) freq (0) et dur (1000) arbitraires pour l'instant
                     (sdif-read-headers ptrfile (nth 3 item) (fifth bmat))
                     (loop for i = 0 then (+ i 1) while (< i (second bmat)) do
                             (SdifFReadOneRow ptrfile)
                             (sethash mrk-partials (floor (SdifFCurrOneRowCol ptrfile 1)) (list 0 time time 0))
                             )
                     )
                 (when pmat 
                     ;;; a parameter matrix :
                     ;;; cherche les notes dans tmplist et set pitch et velocity
                     (sdif-read-headers ptrfile (nth 3 item) (fifth pmat))
                     (loop for i = 0 then (+ i 1) while (< i (second pmat)) do
                             (SdifFReadOneRow ptrfile)
                             (let* ((ind (floor (SdifFCurrOneRowCol  ptrfile 1))) 
                                    (freq (SdifFCurrOneRowCol ptrfile 2))
                                    (amp (SdifFCurrOneRowCol ptrfile 3)) 
                                    (par (gethash ind mrk-partials)))
                               (when par
                                 (setf (car par) freq)
                                 (setf (fourth par) amp))
                               ))
                     )
                 (when emat 
                     ;;; a end matrix :
                     ;;; find the notes, set duration and put int the final notes list 
                     (sdif-read-headers ptrfile (nth 3 item) (fifth emat))
                     (loop for i = 0 then (+ i 1) while (< i (second emat)) do
                             (SdifFReadOneRow ptrfile)
                             (let* ((ind (floor (SdifFCurrOneRowCol ptrfile 1)))
                                   (par (gethash ind mrk-partials)))
                               (when par
                                 (setf (third par) time))
                               ))
                     )
                 (setf bmat nil)
                 (setf emat nil)
                 (setf pmat nil))
             
             (when (or (equal "1TRC" (car item)) (equal "1HRM" (car item)))
               (setf time (nth 1 item))
               (sdif-set-pos ptrfile (nth 3 item))
               (setf mlist (nth 4 item))
               (loop for m in mlist do
                     (when (or (equal "1TRC" (car m)) (equal "1HRM" (car m)))
                       (sdif-read-headers ptrfile (nth 3 item) (fifth m))
                       (loop for i = 0 then (+ i 1) while (< i (second m)) do
                             (SdifFReadOneRow ptrfile)
                             (let* ((ind (floor (SdifFCurrOneRowCol ptrfile 1)))
                                    (freq (SdifFCurrOneRowCol ptrfile 2))
                                    (amp (SdifFCurrOneRowCol ptrfile 3))
                                    (par (gethash ind trc-partials)))
                               (if par
                                 (progn 
                                   (setf (car par) (push freq (car par)))
                                   (setf (fourth par) (push amp (fourth par)))
                                   (setf (third par) time)
                                   )
                                 (sethash trc-partials ind (list (list freq) time time (list amp))))
                               )
                             ))
                     )
               )
             )

     
     
     
     (maphash #'(lambda (key val)
                  (push val chords)) 
              mrk-partials)

     (maphash #'(lambda (key val)
                  (push (list (om-mean (car val)) 
                              (second val) (third val) 
                              (om-mean (fourth val)))  
                        chords)) 
              trc-partials)

     (dynamic-close self ptrfile)
     (sort chords '< :key 'cadr)
     )
|#


;********************************************************************************************
(om::defmethod! ASX::sdif-fundextract ((self om::sdiffile)) 
  (let* ((fundlist (ASX::fund-zero-filter (om::flat (om::mat-trans (ASX::get-SDIF-fund self))) "test" "don't save")))
         ;;(number (om::om/ (om::length fundlist) 2))
         (om::mat-trans (om::group-list 
                                   fundlist (om::repeat-n 2 (om::om/ (length fundlist) 2)) 'linear)))
         )
         

;********************************************************************************************

;(in-package AS)
(om::defmethod! ASX::dB->lin (x)
  (cond((numberp x)(expt 10 (/ x 20)))
       ((listp x)(mapcar #'(lambda(y)(expt 10 (/ y 20))) x))
       (t(error "illegal arg ~a" x))))

(om::defmethod! ASX::lin->dB (x)
  (cond((numberp x)(lintodb x))
       ((listp x)(mapcar #'ASX::lintodb x))
       (t(error "illegal arg ~a" x))))

(om::defmethod! ASX::lintodB (x)
  (let((y (if (= 0.0 x) 0.00000001 x)))
  (* (log y 10) 20)))



;********************************************************************************************
;Jacopo's function "mk-data-pass"
(om::defmethod! ASX::mk-data-pass ((elements t))
  
 "It�s a simple gate not to lose the graphical connexions."
  
  :icon 999
  
  elements)



;;;================================================================================================================
;;; TRANSIENT DETECTION : GENERATE MARKERS
;;;================================================================================================================
(in-package ASX)
(om::defmethod! ASX::trans-detect ((fileName string)
                                 treshold minfreq maxfreq
                                 &optional windowsize fftsize step-oversamp windowtype outfile)
  :initvals '(nil 1.4 0.0 22050.0 4096 4096 8 "hanning" "markers.sdif")
  :menuins (list (list 6 '(("1/4" 4) ("1/8" 8) ("1/16" 16) ("1/32" 32)))
                 (list 7 '(("Blackman" "blackman")("Hanning" "hanning")("Hamming" "hamming"))))
  :icon 999
  :doc "Generates markers using transient detection

- FileName : the pathname of the sound file (Aiff) to be analysed

- treshold : transient detection treshold (1-10)

- minfreq : minimal detection frequency (Hz)

- maxfreq : maximal detection frequency (Hz)

- windowsize : the number of samples of the analysis window

- fftsize : the number of points of fft

- step-oversamp : the oversampling proportion between two successive analysis windows

- windowtype : the shape of the analysis window
"
;AudioSculpt(2) supervp -t -U  -S"/Users/hans/Documents/OpenMusic/WorkSpaces/Morelia/in-files/eisenbimmel-kurz.aif" 
;-Afft  -N8192 -M2000 -oversamp 8 -Wblackman  -OT -td_thresh 1.39999998 -td_G 2.5 -td_band 0.0,8049.9921875 
;-td_nument 10.0 "/Applications/AudioSculpt 2.6.1/Markers/tempmark-RcGj.sdif

  (if (and om::*SVP-PATH* (probe-file om::*SVP-PATH*))
    (let* ((outname (if outfile 
                      (if (pathnamep outfile) outfile (om::outfile outfile))
                      (om::om-CHOOSE-NEW-FILE-DIALOG :prompt "Choose a new SDIF markers file")))
           (unix-outname (namestring outname))
           (fftstr (format nil "-N~D -M~D -W~D -oversamp ~D " 
                           (if fftsize fftsize 8192) (if windowsize windowsize 2000) 
                           (if windowtype windowtype "blackman") (if step-oversamp step-oversamp 8)))
           (transient-params (format nil "-OT -td_thresh ~D -td_G 2.5 -td_band ~D,~D -td_nument 10.0"
                                     treshold minfreq maxfreq))
           (cmd (format nil "~s -v -t -ns -S~s -Afft ~A ~A ~s" 
                        (namestring om::*SVP-PATH*)
                        (namestring fileName)
                        fftstr transient-params
                        unix-outname))
           
           )
      (print (om::string+ "SVP PROCESS : " cmd))
      (om::om-cmd-line cmd om::*sys-console*)
      outname)
    (om::om-beep-msg "SUPERVP not found!"))
  )


(om::defmethod! ASX::trans-detect ((fileName pathName) 
                         treshold minfreq maxfreq
                         &optional windowsize fftsize step windowtype outfile)
  (trans-detect (om::om-namestring filename) 
               treshold minfreq maxfreq
               windowsize fftsize step windowtype outfile))

(om::defmethod! ASX::trans-detect ((self om::sound)  
                         treshold minfreq maxfreq
                         &optional windowsize fftsize step windowtype outfile)
  (trans-detect (om::sound-path self) 
               treshold minfreq maxfreq
               windowsize fftsize step windowtype outfile))