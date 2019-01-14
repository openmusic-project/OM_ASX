;****************************
;    OM-functions by Hans Tutschku 25/10/98 IRCAM

(in-package ASX)



;********************************************************************************************
(om::defmethod! ASX::rand-trans-gliss   ((soundlength number) 
                                        (steps integer)
                                        (randintervall integer)
                                        (filename string)
                                        (saveflag string)) 
  :initvals '(3.4 10 100 "trans.par" "save")
  :indoc '("soundlength" "steps" "intervall around normal pitch in midicents" "filename" "saveflag")
  :menuins '( (4 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"creates parameterfile for random-timestretch"
  
  (let* ((aux1 nil) (final nil)
         (randintervall (/ randintervall 2))
         (timeline (om::remove-dup
                   (om::om-round  
                    (om::om-scale    
                     (om::dx->x 0.0   
                                (loop for i to (- steps 2)
                                      collect
                                      (om::om-random 0.01 (om::om/ soundlength steps))))          
                     0.0 soundlength)
                    3)
                   '= 1)))
    
    (setf final (dolist (n timeline (nreverse aux1))
                  (push (om::list n (om::om-random (om::om* -1 randintervall) randintervall)) aux1)))
    
    (ASX::save-or-not final filename saveflag)
    )
  )


;(ASX::rand-trans-gliss 3.0 100 100 "dilat.par" "save")


;********************************************************************************************
(om::defmethod! ASX::rand-trans-steps   ((soundlength number) 
                                        (steps integer)
                                        (randintervall integer)
                                        (filename string)
                                        (saveflag string)) 
  :initvals '(3.4 10 100 "trans.par" "save")
  :indoc '("soundlength" "steps" "intervall around normal pitch in midicents" "filename" "saveflag")
  :menuins '( (4 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"creates parameterfile for random-timestretch"
  
  (let* ((aux2 nil)(aux3 nil)(final nil)
         (randintervall (/ randintervall 2))
         (timeline(om::remove-dup
                   (om::om-round  
                    (om::om-scale    
                     (om::dx->x 0.0   
                                (loop for i to (- steps 2)
                                      collect
                                      (om::om-random 0.01 (om::om/ soundlength steps))))          
                     0.0 soundlength)
                    3)
                   '= 1)))
    (setf timeline
          (om::mat-trans
           (om::list  (butlast timeline)    
                      (om::rest (om::om-round (om::om- timeline 0.001) 3)))))
    
    (setf final (dolist (n timeline (nreverse aux2))
                  (progn ()
                         (setf aux3 (om::om-random (om::om* -1 randintervall) randintervall))
                         (push (om::list (first n) aux3 ) aux2)
                         (push (om::list (second n) aux3 ) aux2))))
    
    (ASX::save-or-not final filename saveflag)
    )
  )

;(ASX::rand-trans-steps 3.0 10 100 "dilat.par" "dfdf")

;********************************************************************************************
(om::defmethod! ASX::trans-melody   ((LMIDIC list) 
                                     (LDUR list)
                                     (mirror integer)
                                     (filename string)) 
  :initvals '(nil nil 6000 "trans.par")
  :indoc '("LMIDIC" "LDUR" "transposition axis in midicents" "filename")
  :icon 999
  :doc"creates parameterfile for random-timestretch"
  
  (let* ((final nil)
         (timeline   
          (om::om-round     
           (om::mat-trans    
            (om::list                
             (butlast(om::om/ (om::dx->x 0 (om::flat LDUR)) 1000.0))
             (rest(om::om- (om::om/ (om::dx->x 0 (om::flat LDUR)) 1000.0) 0.001))))
           3)))
    
    (setf final (om::flat-once (mapcar #'(lambda (x y)
                                           (om::list (om::list (first x) (- y mirror)) (om::list (second x) (- y mirror))))
                                       timeline (om::flat LMIDIC))))
    
    (ASX::save-or-not final filename)
    )
  )

;(ASX::rand-trans-melodie '(6100 6200 6300) '(1000 800 1200) 6000 "trans.par" "save")
;********************************************************************************************
(om::defmethod! ASX::vibrato   ((vibfunc om::bpf)
                               (vibfreq number)
                               (freqrand number)
                               (vibamp integer)
                               (amprand number)
                               (soundlength number)
                               (filename string)) 
  :initvals '(nil  7.0 0.0 100 0.0 5.0 "trans.par")
  :indoc '("vibratofunction as bpf"  "in herz" "in %" "in cents" "in %" "in sec" "filename")
  :icon 999
  :doc"creates parameterfile for transposition"
  
  (let* ((final nil) (steps-pro-cycle 20)
         (transfaktoren 
          (om::flat (loop for i from 1 to (om::ceiling (om::om* vibfreq soundlength)) 
                          collect (om::om-round
                                   (om::om* 
                                    (om::om-scale
                                     (om::bpf-sample vibfunc 0.0 (om::list-max (om::x-points vibfunc)) 
                                                     (om::om-round 
                                                      (om::perturbation steps-pro-cycle (om::om/ freqrand 100.0))))
                                     -1 1) 
                                    (om::perturbation vibamp (om::om/ amprand 100.0)))
                                   2))))
         
         (timeline (ASX::timeline soundlength (length transfaktoren))))
    
    (setf final (butlast (om::mat-trans (om::list timeline transfaktoren))))
    
    (ASX::save-or-not final filename)
    )
  )

;********************************************************************************************

(om::defmethod! ASX::fund-trans  ((self om::sdiffile)                                      
                                 (direction string)
                                 (transoffset number)
                                 (stretch number)
                                 (snd-dur number)
                                 (scaleswitch string)
                                 (filename string))
  
  :initvals '(nil "normal" 0 1 5.0 "orig" "trans.par")
  :indoc '("sdiffile" "direction of transposition"  "transposition offset" "spectral stretch" "sound duration" "durationscale or not"  "filename")
  :menuins '( (1 (("normal" "normal") ("mirror" "mirror")))
             (5 (("orig" "orig") ("scaled" "scaled"))))
  :icon 999
  :doc"this patch is creating a parameterfile for Audiosculpt transposition from a fundamental analysis"
  (let* ((fundlist (ASX::sdif-fundextract self))
         (timeline (butlast (first fundlist)))
         
         (freqs (om::F->MC (butlast (second fundlist))))
         (fund-dur (first (last timeline)))
         (dur-ratio (om::om/ snd-dur fund-dur))
         
         (dur-multiplyer (if (string= "orig" scaleswitch) 1 dur-ratio))
         (timeline (om::om-round (om::om* timeline dur-multiplyer) 3))
         (maxfreq (om::LIST-MIN freqs))
         (minfreq (om::LIST-MAX freqs))
         (referenznote (om::om+ (om::om+ (om::om/ (om::om- maxfreq minfreq) 2) minfreq) transoffset))
         
         
         
         )       
    
    (if (string= "normal" direction) 
      (setf freqs1 (om::om* (om::om-   freqs referenznote) stretch))
      (setf freqs1 (om::om* (om::om-  referenznote freqs ) stretch)))
    
    (setf final (om::mapcar 'om::flat
                            (om::mat-trans    
                             (om::list
                              timeline freqs1))))
    
    (ASX::save-or-not final filename)
    
    
    
    )
  )