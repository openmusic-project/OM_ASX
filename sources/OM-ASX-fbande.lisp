;****************************
;    OM-functions by Hans Tutschku 15/10/98 IRCAM

(in-package ASX)


;********************************************************************************************
(om::defmethod! ASX::fund-bands-alea ((fundamental number)
                                     (rang list)
                                     (rand number))
  :initvals '(200 '(1 3 5) 0.1)
  :indoc '("fundamental" "rang of bands" "rand" )
  :icon 999 
  :doc"calculates frequencies <fund * rang> upon a fundamental with random deviation"
  
  (om::om-round
   (om::om+
    (om::om* fundamental rang)
    (om::om- fundamental (om::perturbation fundamental rand))))
  )
;(ASX::fund-bands-alea 200 '(1 3 5) 0.1)

;********************************************************************************************
(om::defmethod! ASX::fund-zero-filter ((fundamental list)
                                      (filename string)
                                      (saveflag string))
  :initvals '(nil "newfund" "don't save")
  :indoc '("fundamentalanalysis" "filename" "saveflag")
  :menuins '((2 (("save" "save") ("don't save" "don't save"))))
  
  :icon 999 
  :doc"filters zeros out of fundamental analysis"
  (let ((ergebnis nil)(final nil)(aux nil))
    (setf ergebnis (om::group-list 
                    fundamental (om::repeat-n 2 (om::om/ (length fundamental) 2)) 'linear))
    (setf final (dolist (n ergebnis (nreverse aux))
                  (if (< 10 (second n) )
                    (push n aux))))
    
    (if (string= "don't save" saveflag)
      (om::flat final)
      (progn ()
             (ASX::write-lists filename
                              final)              
             
             '("saved")
             )
      )
    
    )
  )
;(ASX::fund-zero-filter 200 20 0.3)

;********************************************************************************************
(om::defmethod! ASX::fund-bands ((fundamental number)  ;JUST ONE FUNDAMENTAL
                                (bands list))
  :initvals '(200 '(1 3 5 7 9 11))
  :indoc '("fundamental" "bands" )
  :icon 999 
  :doc"calculates frequencies upon a fundamental"
  
  
  ;(om::rest
  ;(om::arithm-ser 0 
  (om::om* 
   (om::om-round fundamental )
   bands) 
  ;(om::om-round fundamental ))
  ;)
  
  )
;(ASX::fund-bands 200 '(1 3 5 7 9 11))
;(ASX::fund-bands 200 1 )
;********************************************************************************************
(om::defmethod! ASX::freqs-to-bandwith ((freqs list)
                                       (bw number))
  :initvals '((200 400) 20)
  :indoc '("freqs" "bw" )
  :icon 999 
  :doc"calculates new frequencies around a list of freqs depending on the bandwith"
  
  (om::om-round
   (om::sort.
    (om::x-append
     (om::om- freqs (om::om/ bw 2)) 
     (om::om+ freqs (om::om/ bw 2)))))
  
  )
;(ASX::freqs-to-bandwith '(200 400) 20)
;********************************************************************************************

(om::defmethod! ASX::fbande-parallel  ((soundlength number) 
                                      (steps integer)
                                      (rang t)
                                      (fundamental number)
                                      (bw t)
                                      (rand number)
                                      (bandswitch string)
                                      (fftsize integer)
                                      (filename string)
                                      (saveflag string)) 
  :initvals '(3.4 3 '(1 3 5) 800 nil 0.0 "band" 4096 "fbande.par" "save")
  :indoc '("soundlength" "steps" "rang of bands" "fundamental" "bw as BPF" "rand" "band or notchfilter" "fftsize" "filename" "saveflag")
  :menuins '( (6 (("notch" "notch") ("band" "band")))
             (7 (("1024" 1024) ("2048" 2048) ("4096" 4096) ("8192" 8192) ("16384" 16384) ("32768" 32768)))
             (9 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"this patch is creating a parameterfile for Audiosculpt Fbande-filter. Starting from a fundamental frequency, a  number of bands centered around specified harmonics <rang> will be created.If random is > 0 you have to choose a number of random steps to get variations over time - this variation is parallel for all frequencies"
  
  (let* ((final nil)(freqs nil)(ergebnis nil)(bandwith nil)
         (bwminimum (cond 
                     ((= fftsize 1024) 44)
                     ((= fftsize 2048) 22)
                     ((= fftsize 4096) 11)
                     ((= fftsize 8192) 6)
                     ((= fftsize 16384) 3)
                     ((= fftsize 32768) 2)
                     (t 44)
                     ))
         
         (rand (/ rand 100.0))
         (rang (if (atom rang) (om::list! rang) rang))
         
         (timeline (ASX::timeline soundlength steps))                ; timeline with regular steps
         (switch (if (string= "band" bandswitch) 0 1)) )            ; switch to keep or reject partials
    
    
    (setf final  (dolist (n timeline (nreverse ergebnis))            
                   (setf freqs  (om::sort. (ASX::fund-bands-alea fundamental rang rand)))      ;creating random "harmonics" on fundamental
                   (setf bandwith (om::om-max bwminimum
                                              (om::om-min               ;sampling BPF, value must be between bwminimum and (f0 - bwminimum)
                                               (ASX::f-transfer bw (om::om-round (/ (* n 100.0) soundlength) 3 ))
                                               (- (first freqs) bwminimum))))    
                   (ASX::freqs-to-bandwith freqs bandwith)
                   (push (om::list n                                                  ;time
                                   (* 2 (length freqs))                               ;number of points
                                   switch                                             ;keep or reject
                                   (ASX::freqs-to-bandwith freqs bandwith)) ergebnis)  ;freqs
                   ))
    
    (setf final (mapcar #'(lambda (x)
                            (om::flat x))
                        final))
    
    (ASX::save-or-not final filename saveflag)
     
    ))

;(ASX::fbande-parallel 3.4 3 5 800 30 0.0 "band" "fbande.par" "don't save")
;********************************************************************************************

(om::defmethod! ASX::fbande-not-parallel  ((soundlength number) 
                                          (steps integer)
                                          (rang t)
                                          (fundamental number)
                                          (bw number)
                                          (rand integer)
                                          (bandswitch string)
                                          (fftsize integer)
                                          (filename string)
                                          (saveflag string)) 
  :initvals '(3.4 3 '(2 3 6) 200 30 0.2 "band" 4096 "fbande.par" "save")
  :indoc '("soundlength" "steps" "rang of bands" "fundamental" "bw" "rand" "band or notchfilter" "fftsize" "filename" "saveflag")
  :menuins '( (6 (("notch" "notch") ("band" "band")))
             (7 (("1024" 1024) ("2048" 2048) ("4096" 4096) ("8192" 8192) ("16384" 16384) ("32768" 32768)))
             (9 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"this patch is creating a parameterfile for Audiosculpt Fbande-filter. 
Starting from a fundamental frequency a certain number of bands centered around the 
harmonics will be created.If random is > 0 you have to choose a number of random steps 
to get variations over time - this variation is parallel for all frequencies"
  (let* ((final nil)
         (bwminimum (cond 
                     ((= fftsize 1024) 44)
                     ((= fftsize 2048) 22)
                     ((= fftsize 4096) 11)
                     ((= fftsize 8192) 6)
                     ((= fftsize 16384) 3)
                     ((= fftsize 32768) 2)
                     (t 44)
                     ))
        (bw (om::om-max bwminimum bw))

        (rand (/ rand 100.0))
        (rang (if (atom rang) (om::list! rang) rang))
        (aux (length (ASX::timeline soundlength steps)))
        (switch (if (string= "band" bandswitch)
                  0 1)))
    
    (setf final (om::mapcar 'om::flat  
                            (om::mat-trans
                             (om::list
                              (ASX::timeline soundlength steps)
                              (om::repeat-n (om::om* (length rang) 2) aux)
                              (om::repeat-n switch aux)
                              (loop for i to (- aux 1)
                                    collect
                                    (om::sort.                                    
                                     (om::om-round
                                      (ASX::freqs-to-bandwith 
                                       (om::perturbation(ASX::fund-bands fundamental rang) rand) 
                                       bw)
                                      
                                      #|
(om::perturbation  
                                       (ASX::freqs-to-bandwith (ASX::fund-bands fundamental rang) bw)
                                       rand)
|#
                                      )))))))
    
    (ASX::save-or-not final filename saveflag)
    
    )
  )

;(ASX::fbande-not-parallel 3.4 3 '(2 3 6) 200 30 0.2 "band" "fbande.par" "don't save")


;********************************************************************************************

(om::defmethod! ASX::fbande-melody  ((LMIDIC list) 
                                    (LDUR list)
                                    (rang t)
                                    (bw number)
                                    (bandswitch string)
                                    (fftsize integer)
                                    (filename string)
                                    (saveflag string)) 
  :initvals '((5000 6000)(50 100) 10 30 "band" 4096 "fbande.par" "save")
  :indoc '("LMIDIC" "LDUR" "bands" "bw" "rand" "band or notchfilter" "fft-size" "filename" "saveflag")
  :menuins '( (4 (("notch" "notch") ("band" "band")))
             (5 (("1024" 1024) ("2048" 2048) ("4096" 4096) ("8192" 8192) ("16384" 16384) ("32768" 32768)))
             (7 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"this patch is creating a parameterfile for Audiosculpt Fbande-filter from a list of Midicents and durations."
  (let* ((ergebnis nil)(final nil)
         (bwminimum (cond 
                     ((= fftsize 1024) 44)
                     ((= fftsize 2048) 22)
                     ((= fftsize 4096) 11)
                     ((= fftsize 8192) 6)
                     ((= fftsize 16384) 3)
                     ((= fftsize 32768) 2)
                     (t 44)
                     ))
         (bw (om::om-max bwminimum bw))
         (rang (if (atom rang) (om::list! rang) rang))
         (timeline (ASX::timeline-melodie (om::flat LDUR)))
         (switch (if (string= "band" bandswitch) 0 1))
         (freqs (dolist (n (om::flat LMIDIC) (nreverse ergebnis)) ;conversion of MIDIcents in frequencies
                  (push                                           ;serie of harmonic bands on every frequence
                   (ASX::freqs-to-bandwith                         ;with a bandwith   
                    (ASX::fund-bands
                     (om::om-round (om::mc->f n))
                     rang)
                    bw)       
                   ergebnis)))
         (shortest (om::om-min (length timeline) (om::om* 2 (length freqs)))))
    
    (setf final (mapcar 'om::flat
                        (loop for i to (- shortest 1)
                              collect  (om::list
                                        (nth i timeline)
                                        (om::om* (length rang) 2)
                                        switch
                                        (nth (om::floor (om::om/ i 2)) freqs)))))  
    (ASX::save-or-not final filename saveflag)
    )
  )
;(ASX::fbande-melody '(5000 6000) '(50 100) 10 30 "band" "fbande.par" "don't save")


;********************************************************************************************

(om::defmethod! ASX::fund-fbande  ((self om::sdiffile)                                      
                                   (rang list)
                                   (bw number)
                                   (bandswitch string)
                                   (snd-dur number)
                                   (scaleswitch string)
                                   (fftsize integer)
                                   (filename string) 
                                   ) 
  :initvals '(nil '(1 3 5) 30 "band" 5.0 "orig" 4096 "fbande.par")
  :indoc '("fundamental" "harmonic rang of bands" "bandwith" "band or notchfilter" "sound duration" "scale or not" "fftsize" "filename")
  :menuins '( (3 (("notch" "notch") ("band" "band")))
             (5 (("orig" "orig") ("scaled" "scaled"))))
  :icon 999
  :doc"this patch is creating a parameterfile for Audiosculpt Fbande-filter from a fundamental analysis. orig means original duration of the fundamental file, scaled means fundamental duration scaled to new soundfile duration"
  (let* ((ergebnis nil)(final nil)
         (bwminimum (cond 
                     ((= fftsize 1024) 44)
                     ((= fftsize 2048) 22)
                     ((= fftsize 4096) 11)
                     ((= fftsize 8192) 6)
                     ((= fftsize 16384) 3)
                     ((= fftsize 32768) 2)
                     (t 44)
                     ))
         (bw (om::om-max bwminimum bw))
         (switch (if (string= "notch" bandswitch) 0 1))
         (fundlist (ASX::sdif-fundextract self))
         (timeline (first fundlist))
         (timeline (om::om-round (om::om- timeline (first timeline)) 3))  ; falls erstes Fenster nicht mit 0 anfŠngt, verschiebe timeline auf Null
         (freqs (second fundlist))
         (fund-dur (first (last timeline)))
         (dur-ratio (om::om/ snd-dur fund-dur))
         (dur-multiplyer (if (string= "orig" scaleswitch) 1 dur-ratio))
         (timeline (om::om-round (om::om* timeline dur-multiplyer) 3))
         (aux2 (dolist (n freqs (nreverse ergebnis)) 
                 (push  (ASX::fund-bands n rang) ergebnis))) ;korrektur, da hohe freq-bŠnder zu tief
         
         )
    
    
    
    (setf ergebnis (dolist (n aux2 (nreverse final))
                     (push (om::om-round
                            (om::sort.
                             (om::x-append 
                              (om::om- (om::om- n bw) (om::om* n 0.005))   ;open bandwith a bit for higher freqs
                              (om::om+ (om::om+ n bw) (om::om* n 0.005))
                              ))) final)))
    
    (setf final (om::mapcar 'om::flat
                            (om::mat-trans    
                             (om::list
                              timeline
                              
                              (om::repeat-n (om::om* (length rang) 2) (length timeline)) 
                              (om::repeat-n switch (length timeline))    
                              ergebnis))))
    
    
    ;(ASX::save-or-not final filename)
    
    )
  )
  
;(ASX::fund-fbande  '(0.04 174.56 0.13 174.09 0.22 169.94 0.31 168.14) '(1 3 5) 30 "band" "fbande.par" "don't save")

;********************************************************************************************

(om::defmethod! ASX::chord-to-fbande  ((liste list) 
                                       (bw number)   ;BANDWITH AS NUMBER
                                       (bandswitch string)
                                       (duration number)
                                       (fftsize integer)
                                       (filename string)
                                       ) 
  :initvals '(nil  44 "band" 1 4096 "fbande.par")
  :indoc '("list of MIDICENTS" "bandwith in Hz" "band or notch filter" "in seconds" "fftsize" "filename")
  :menuins '((2 (("notch" "notch") ("band" "band")))
            (4 (("1024" 1024) ("2048" 2048) ("4096" 4096) ("8192" 8192) ("16384" 16384) ("32768" 32768))))
  :icon 999
  :doc"takes a list of MIDICENTs and converts it into a bandfilterfile"
  (let* ((final nil)
         (bwminimum (cond 
                     ((= fftsize 1024) 44)
                     ((= fftsize 2048) 22)
                     ((= fftsize 4096) 11)
                     ((= fftsize 8192) 6)
                     ((= fftsize 16384) 3)
                     ((= fftsize 32768) 2)
                     (t 44)
                     ))
         (bw (om::om-max bwminimum bw))
         (switch (if (string= "band" bandswitch) 1 0)))
    
    
    (setf final 
          (om::sort.
           (om::flat
            (om::list
             (om::om-round (om::om- (om::MC->F liste) (om::om/ bw 2)))
             (om::om-round (om::om+ (om::MC->F liste) (om::om/ bw 2)))))))
    
    
    
    (setf final
          (om::list
           (om::flat (om::list 0.0 (length final) switch final))
           (om::flat (om::list duration (length final) switch final))))
    
    ;(ASX::save-or-not final filename)
    )
  )
  
;********************************************************************************************

(om::defmethod! ASX::freq-fbande  ((freq number) 
                                    (rang list)
                                    (bw number)   ;BANDWITH AS NUMBER
                                    (bandswitch string)
                                    (duration number)
                                    (fftsize integer)
                                    (filename string)
                                     ) 
  :initvals '(220 '(1 3 5) 44 "band" 3.0 4096 "fbande.par" )
  :indoc '("list" "harmonic rang of bands" "bandwith in Hz" "band or notchfilter" "in seconds" "fftsize" "filename" )
  :menuins '((3 (("notch" "notch") ("band" "band"))))
  :icon 999
  :doc"takes a frequency and converts it into a bandfilterfile"
  (let* ((final nil)
         (bwminimum (cond 
                     ((= fftsize 1024) 44)
                     ((= fftsize 2048) 22)
                     ((= fftsize 4096) 11)
                     ((= fftsize 8192) 6)
                     ((= fftsize 16384) 3)
                     ((= fftsize 32768) 2)
                     (t 44)
                     ))
         (bw (om::om-max bwminimum bw))
         (switch (if (string= "notch" bandswitch) 0 1))
         (freq (om::om* freq rang)))



    
    (setf final 
          (om::sort.
           (om::flat
            (om::list
             (om::om-round (om::om- freq (om::om/ bw 2)))
             (om::om-round (om::om+ freq (om::om/ bw 2)))))))

    (setf final
          (om::list
           (om::flat (om::list 0.0 (length final) switch final))
           (om::flat (om::list duration (length final) switch final))))

   ;; (ASX::save-or-not final filename)


    )
  )

