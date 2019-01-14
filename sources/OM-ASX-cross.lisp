;****************************
;    OM-functions by Hans Tutschku 15/10/98 IRCAM

(in-package ASX)


;********************************************************************************************

(om::defmethod! ASX::cross-rand-integer  ((soundlength number) 
                                         (steps integer)
                                         (filename string) 
                                         (saveflag string)) 
  :initvals '(5.0 10 "cross.par" "save")
  :indoc '("soundlength" "steps" "filename" "saveflag")
  :menuins '( (3 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"calculates random float-values for generalized cross synthesis with a irregular timeline"
  
  (let ((aux1 nil)(aux2 nil) (ergebnis nil)(old nil)(new nil)(final nil)
        (timeline (ASX::timeline-irr soundlength steps)))          ;creates irregular timeline
    
    
    (setf final (dolist (n timeline (nreverse ergebnis))            ;for every timepoint X x Y y and q (= 0)
                  (setf new (om::om-round 
                             (om::list
                              (setf aux1 (om::om-random 0 1))       ; X or x
                              (om::om- 1 aux1)
                              (setf aux2 (om::om-random 0 1))       ; Y or y
                              (om::om- 1 aux2)
                              0)))                                  ; q
                             
                  (if (equalp old new)                              ;test that new values are not the same as last lime
                    (setf new (om::x-append
                               (om::om-abs 
                                (om::om- (om::first-n new 4) 1)) (fifth new))))
                  (setf old new)
                  (push 
                   (om::x-append n new) ergebnis)))
    
    ;(ASX::save-or-not final filename saveflag)
    )
  )


;(ASX::cross-rand-integer 5.0 10 "bla" 1)

;********************************************************************************************

(om::defmethod! ASX::cross-rand-integer2  ((soundlength number) 
                                         (steps integer)
                                         (filename string) 
                                         ) 
  :initvals '(5.0 10 "cross.par")
  :indoc '("soundlength" "steps" "filename")
  :menuins '( (3 (("save" "save") ("don't save"))))
  :icon 999
  :doc"calculates random float-values for generalized cross synthesis with a irregular timeline"
  
  (let ((aux1 nil)(aux2 nil) (ergebnis nil)(old nil)(new nil)(final nil)
        (timeline (ASX::timeline-irr soundlength steps)))          ;creates irregular timeline
    
    
    (setf final (dolist (n timeline (nreverse ergebnis))            ;for every timepoint X x Y y and q (= 0)
                  (setf new (om::om-round 
                             (om::list
                              (setf aux1 (om::om-random 0 1))       ; X or x
                              (om::om- 1 aux1)
                              (setf aux2 (om::om-random 0 1))       ; Y or y
                              (om::om- 1 aux2)
                              0)))                                  ; q
                             
                  (if (equalp old new)                              ;test that new values are not the same as last lime
                    (setf new (om::x-append
                               (om::om-abs 
                                (om::om- (om::first-n new 4) 1)) (fifth new))))
                  (setf old new)
                  (push 
                   (om::x-append n new) ergebnis)))
    
    ;(ASX::write-lists3 final filename)    
)
  )

;(ASX::cross-rand-integer 5.0 10 "bla" 1)

;********************************************************************************************
(om::defmethod! ASX::cross-rand-float  ((soundlength number) 
                                       (steps integer)
                                       (filename string) 
                                       (saveflag string)
                                       ) 
  :initvals '(5.0 10 "cross.par" "save")
  :indoc '("soundlength" "steps" "filename" "saveflag")
  :menuins '( (3 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"calculates random float-values for generalized cross synthesis with a irregular timeline"
  
  (let ((aux1 nil)(aux2 nil)(ergebnis nil)(final nil)
        (timeline (ASX::timeline-irr soundlength steps)))             ;creates irregular timeline
    
    
    (setf final (dolist (n timeline (nreverse ergebnis)) 
                  (push 
                   (om::x-append n
                                 (om::om-round 
                                  (om::list
                                   (setf aux1 (om::om-random 0.0 1.0))       ; balancing X and x
                                   (om::om- 1 aux1)
                                   (setf aux2 (om::om-random 0.0 1.0))       ; balancing Y and y
                                   (om::om- 1 aux2)
                                   0)                                        ; q
                                  1)) 
                   ergebnis)))
    
    ;(ASX::save-or-not final filename saveflag)
    )
  )

;(ASX::cross-rand-float 5.0 10 "bla" 1)

;********************************************************************************************

(om::defmethod! ASX::cross-rand-int-rhythm  ((rhythm-for-change list)
                                           (filename string) 
                                           (saveflag string)
                                           ) 
  :initvals '((1000 333 333 333 1000 333 333 333) "cross.par" "save")
  :indoc '("in ms" "filename" "saveflag")
  :menuins '( (2 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"calculates random int-values for generalized cross synthesis with spezified rhythm"
  
  (let* ((aux1 nil)(aux2 nil)(werte nil)(old nil)
         (timeline (ASX::timeline-melodie (om::flat rhythm-for-change)))          ;creates limeline von rhythm
         (ergebnis (om::flat
                    (loop for i from -1 to (- (length timeline) 2) collect      ;loop to manage two adjacent timelinevalues
                          (progn                                                ;with the same values for X x Y y q
                            (incf i)
                            (setf werte (om::list
                                         (setf aux1 (om::om-random 0 1))
                                         (om::om- 1 aux1)
                                         (setf aux2 (om::om-random 0 1))
                                         (om::om- 1 aux2)
                                         0))
                            (if (equalp old werte)                              ; test that new values are not the same as last line
                              (setf werte (om::x-append
                                           (om::om-abs 
                                            (om::om- (om::first-n werte 4) 1)) (fifth werte))))
                            (setf old werte)
                            
                            (om::list (nth i timeline)                          ;two adjacent timelinevalues with the
                                      werte                                     ;same values for X x Y y q
                                      (nth (+ 1 i) timeline)
                                      werte)))))
         
         (number (om::om/ (om::length ergebnis) 6))
         (pattern (om::repeat-n 6 number)))                                     ;pattern for group-list (list-explode does not yet exist)
    
    (setf ergebnis (om::group-list ergebnis pattern 'linear))
    
    (ASX::save-or-not ergebnis filename saveflag)   
    )
  )

;(ASX::cross-rand-int-rhythm '(1000 333 333 333 1000 333 333 333) "cross.par" "don't save")

;(ASX::list-explode '(1000 333 333 333 1000 333 333 333) 3)
;********************************************************************************************

(om::defmethod! ASX::cross-rand-float-rhythm  ((rhythm-for-change list)
                                             (filename string) 
                                             (saveflag string)
                                             ) 
  :initvals '((1000 333 333 333 1000 333 333 333) "cross.par" "save")
  :indoc '("in ms" "filename" "saveflag")
  :menuins '( (2 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"calculates random float-values for generalized cross synthesis with spezified rhythm"
  
  (let* ((werte nil)
         (timeline (ASX::timeline-melodie (om::flat rhythm-for-change)))               ;creates limeline von rhythm
         (ergebnis (om::flat
                    (loop for i from -1 to (- (length timeline) 2) collect           ;loop to manage two adjacent timelinevalues
                          (progn                                                     ;with the same values for X x Y y q
                            (incf i)
                            (setf werte (om::om-round
                                         (om::list
                                          (om::om-random 0.0 1.0)                    ; four random values for X x Y y
                                          (om::om-random 0.0 1.0)
                                          (om::om-random 0.0 1.0)
                                          (om::om-random 0.0 1.0)
                                          0)                                         ; q
                                         1))
                            (om::list (nth i timeline)                               ;two adjacent timelinevalues with the
                                      werte                                          ;same values for X x Y y q
                                      (nth (+ 1 i) timeline)
                                      werte)))))
         
         (number (om::om/ (om::length ergebnis) 6))
         (pattern (om::repeat-n 6 number)))                                          ;pattern for group-list (list-explode does not yet exist)

    
      (setf ergebnis (om::group-list ergebnis pattern 'linear))
 
   (ASX::save-or-not ergebnis filename saveflag)
    )
  )

;(ASX::cross-rand-float-rhythm '(1000 333 333 333 1000 333 333 333) "cross.par" "don't save")


;********************************************************************************************

(om::defmethod! ASX::cross-rand-combinations  ((soundlength number) 
                                              (steps integer)
                                              (possibles list)
                                              (filename string) 
                                              (saveflag string)
                                              ) 
  :initvals '(5.0 10 ((0 1 0 0) (0 1 1 0) (1 0 0 0) (1 0 0 1) (1 0 1 1) (1 1 0 0) (1 1 0 1) (1 1 1 1)) "cross.par" "save")
  :indoc '("soundlength" "steps" "list of possibles combinations" "filename" "saveflag")
  :menuins '( (4 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"calculates random values for generalized cross synthesis - this version has a irregular timeline"
  
  (let* ((werte nil)(ergebnis nil)(old nil)(final nil)
         (timeline (ASX::timeline-irr soundlength steps)))              ;creates irregular timeline
    
    (setf final (dolist (n timeline (nreverse ergebnis))
                  (setf werte (nth (om::om-random 0 (- (length possibles) 1)) possibles ))
                  (if (equalp old werte)                               ;test that new values are not the same as last line
                    (setf werte 
                          (om::om-round (om::om-abs (om::om- werte 1)) 1)))
                  (setf old werte)
                  (push (om::flat (om::list n werte 0)) ergebnis)))

    ;(ASX::save-or-not final filename saveflag)
    )
  )

;(ASX::cross-rand-combinations 5.0 20 "cross.par" 1)

;********************************************************************************************
(om::defmethod! ASX::cross-rand-comb-rhythm  ((rhythm-for-change list)
                                             (possibles list)
                                             (filename string) 
                                             (saveflag string)
                                             ) 
  :initvals '((1000 333 333 333 1000 333 333 333) ((0 1 0 0) (0 1 1 0) (1 0 0 0) (1 0 0 1) (1 0 1 1) (1 1 0 0) (1 1 0 1) (1 1 1 1))  "cross.par" "save")
  :indoc '("in ms" "list of possibles combinations" "filename" "saveflag")
  :menuins '( (4 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc""
  (if (atom (first possibles))
    "you have to give at least 2 choices for <possibles>"
    
    (if (> (length possibles) 1)
      
      (let* ((werte nil)(old nil)(i 0)(aux1 nil)(aux2 nil)(final nil)
             (timeline (ASX::timeline-melodie (om::flat rhythm-for-change))) )         ;creates limeline von rhythm
        
        (ccl::while (< i (/ (length timeline) 2))
                    (setf aux1 (om::x-append (nth (om::om-random 0 (- (length possibles) 1)) possibles ) 0))
                    (if (not (equalp old aux1))                        ; appending q to possibles
                      (progn                                           ; if it is not equal to last choice, push into aux2
                        (incf i)                                       ; increment i and set old to actual choice
                        (push aux1 aux2)
                        (setf old aux1)))
                    )
        (setf werte (om::flat-once (om::mat-trans (om::list aux2 aux2))))
        
        
        (setf final (mapcar #'(lambda (x)
                                (om::flat x))
                            (om::mat-trans (om::list timeline werte))))
        
        ;(ASX::save-or-not final filename saveflag)
        
        )
      "you have to give at least 2 choices for <possibles>"
      )
    )
  )


;********************************************************************************************

(om::defmethod! ASX::cross-bpflib  ((tab t)
                                   (soundlength number) ;with irregular timeline
                                   (steps integer)
                                   (filename string) 
                                   (saveflag string)
                                   ) 
  :initvals '(nil 5.0 10 "cross.par" "save")
  :indoc '("bpf" "soundlength" "steps" "filename" "saveflag")
  :menuins '( (4 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"takes a BPF-LIB with four curves which represents X x Y y for general cross synthesis"
  

(if (= 4 (length tab))
    (let ((final nil)
          (timeline (ASX::timeline soundlength (- steps 1)))                ; regular timeline to scan BPF in equal steps
          
          (amp1 (om::om-round                                                 ; sampling the four curves
                 (om::om/
                  (om::bpf-sample (first tab) 0 100 steps)
                  100.0)
                 2))
          
          (amp2 (om::om-round 
                 (om::om/
                  (om::bpf-sample (second tab) 0 100 steps)
                  100.0)
                 2))
          
          (freq1 (om::om-round 
                  (om::om/
                   (om::bpf-sample (third tab) 0 100 steps)
                   100.0)
                  2))
          
          (freq2 (om::om-round 
                  (om::om/
                   (om::bpf-sample (fourth tab) 0 100 steps)
                   100.0)
                  2))
          
          (aux (ASX::repeat 0.0 steps)))
          
      (setf final (om::mat-trans
                   (om::list
                    timeline amp1 amp2 freq1 freq2 aux)))    

      (if saveflag
          (ASX::save-or-not final filename)
        final)
      )
    '("ERROR:there are not four bpf-functions in bpf-lib")
    )
  )

;(ASX::cross-bpflib '(1 3 88 6) 5.0 10 "bla" 1)



;;;=================================================================================================
;;; CROSS SYNTHESIS
;;;=================================================================================================

(in-package :om)

(om::defmethod! supervp-cross-synthesis11 ((sound1 t) (sound2 t) (param pathname) windowsize fftsize windowstep-oversamp 
                                          window-type normalize crossmode outfile begin end)
  :icon 950 
  :menuins '((5 (("1/4" 4) ("1/8" 8) ("1/16" 16) ("1/32" 32)))
             (6 (("Blackman" "blackman") ("Hanning" "hanning")("Hamming" "hamming")))
             (7 (("Normalize On" t) ("Normalize Off" nil)))
             (8 (("Cross" t) ("Add" nil)))
             )
  :initvals '(nil nil "" 4096 4096 8 "hanning" nil t "out.aiff" 0.0 2.0)

 (if (and om::*SVP-PATH* (probe-file om::*SVP-PATH*))

   (let ((srcpath (get-src-path sound1))
         (srcpath2 (get-src-path sound2)))
     
     (if (and (probe-file srcpath) (probe-file srcpath2))
       (let ((outpath (if (pathnamep outfile) outfile (outfile outfile)))
             (command ""))
         (setf command (om::string+ command  (format nil "~s -t -v -Z -U -u " 
                                                     (namestring om::*SVP-PATH*))))
         
         (when normalize
           (setf command (om::string+ command "-norm ")))
         
         
         (setf command (om::string+ command  (format nil "-S~s -s~s -A -a "  
                                                     (namestring srcpath)
                                                     (namestring srcpath2))))
         
         (if crossmode
           (setf command (om::string+ command "-Gcross "))
           (setf command (om::string+ command "-Gadd ")))
         
         (setf command (om::string+ command  (format nil "~s -B~D -E~D -M~D -N~D -m~D -n~D -oversamp ~D -J~s -W~s  -w~s ~D"  
                                                     (namestring param) begin end
                                                     windowsize fftsize windowsize fftsize 
                                                     windowstep-oversamp 
                                                     window-type window-type window-type
                                                     (namestring outpath))))
         (print command)
         (om::om-cmd-line command om::*svp-console*)
         (loop for file in *tmpparfiles* do (delete-file file))
         (setf *tmpparfiles* nil)
         outpath)
       (om::om-beep-msg "sound files no found"))
     )

    (om-beep-msg "SUPERVP not found!"))

)



(om::defmethod! supervp-cross-synthesis11 ((sound1 t) (sound2 t) (param list) windowsize fftsize windowstep-oversamp 
                                     window-type normalize crossmode outfile begin end)
  (let ((parampath (om::paramfile "tempcross.par"))
        (rep nil))
    (om::save-params param parampath)
    (setf rep (supervp-cross-synthesis11 sound1 sound2 parampath windowsize fftsize windowstep-oversamp 
                                       window-type normalize crossmode outfile begin end))
    (delete-file parampath)
    rep))



(om::defmethod! supervp-cross-synthesis11 ((sound1 t) (sound2 t) (param string) windowsize fftsize windowstep-oversamp 
                                     window-type normalize crossmode outfile begin end)
  (let ((parampath (if (probe-file (pathname param)) (pathname param) (paramfile param))))
    (supervp-cross-synthesis11 sound1 sound2 parampath windowsize fftsize windowstep-oversamp 
                             window-type normalize crossmode outfile begin end)))


(om::defmethod! supervp-cross-synthesis11 ((sound1 t) (sound2 t) (param om::bpf) windowsize fftsize windowstep-oversamp 
                                     window-type normalize crossmode outfile begin end)
  (let ((parampath (om::paramfile "tempcross.par"))
        (rep nil))
    (om::save-params param parampath)
    (setf rep (supervp-cross-synthesis11 sound1 sound2 parampath windowsize fftsize windowstep-oversamp 
                                       window-type normalize crossmode outfile begin end))
    (delete-file parampath)
    rep))


(om::defmethod! supervp-cross-synthesis11 ((sound1 t) (sound2 t) (param om::textfile) windowsize fftsize windowstep-oversamp 
                                     window-type normalize crossmode outfile begin end)
  (let ((parampath (om::paramfile "tempcross.par"))
        (rep nil))
    (om::save-params param parampath)
    (setf rep (supervp-cross-synthesis11 sound1 sound2 parampath windowsize fftsize windowstep-oversamp 
                                       window-type normalize crossmode outfile begin end))
    (delete-file parampath)
    rep))


;;;=================================================================================================
;;; SYNTHESE CROISEE SOURCE FILTRE
;;;=================================================================================================

(in-package :om)
(defmethod get-src-path ((self pathname)) self)
(defmethod get-src-path ((self om::sound)) (sound-path self))
(defmethod get-src-path ((self string)) (pathname self))

(om::defmethod! supervp-sourcefilter-synthesis4 ((source t) (filtre t)  windowsize fftsize windowstep-oversamp 
                                                 window-type normalize crossmode spectenv-type outfile beg end lpc-factor)
  :icon 950 
  :menuins '((4 (("1/4" 4) ("1/8" 8) ("1/16" 16) ("1/32" 32)))
             (5 (("Blackman" "blackman") ("Hanning" "hanning")("Hamming" "hamming")))
             (6 (("Normalize On" t) ("Normalize Off" nil)))
             (7 (("Amplitude+Phase" 0) ("Amplitude" 1)("Phase" 2)))
             (8 (("LPC" t) ("True Enveloppe" nil)))
             )
  :initvals '(nil nil 4096 4096 8 "hanning" nil nil t "out.aiff" 0.0 2.0 40)
  (if (and om::*SVP-PATH* (probe-file om::*SVP-PATH*))
    (let ((srcpath (get-src-path source))
          (srcpath2 (get-src-path filtre)))
      (if (and (probe-file srcpath) (probe-file srcpath2))
        (let ((outpath (if (pathnamep outfile) outfile (outfile outfile)))
              (command ""))
          
          (setf command (om::string+ command  (format nil "~s -t -v -Z -U -u " 
                                                      (namestring om::*SVP-PATH*))))
          
          (when normalize
            (setf command (om::string+ command "-norm ")))
          
          
          (setf command (string+ command  (format nil "-S~s -s~s -A " 
                                                  (namestring srcpath)
                                                  (namestring srcpath2))))
          
          (if spectenv-type (setf command (string+ command "-alpc ")) (setf command (om::string+ command "-atenv ")))
           
  #|
        (case crossmode
            (0 (setf command (om::string+ command " -Gmul ")))
            (1 (setf command (om::string+ command " -Gamul ")))
            (2 (setf command (om::string+ command " -Gpmul "))))
          
 
|#         
          (setf command (om::string+ command  (format nil "~D -Gmul -B~D -E~D -M~D -N~D -m~D -n~D -oversamp ~D -J~s -W~s  -w~s ~D"  
                                                      lpc-factor beg end windowsize fftsize windowsize fftsize 
                                                      windowstep-oversamp 
                                                      window-type window-type window-type
                                                      (namestring outpath))))
          (print command)
          (om::om-cmd-line command om::*svp-console*)
          (loop for file in *tmpparfiles* do (delete-file file))
          (setf *tmpparfiles* nil)
          outpath)
        (om::om-beep-msg "Source / filter sound files no found")
        ))
    (om-beep-msg "SUPERVP not found!"))
  )
