#| 
   File to control generation of 'times' as they are used in SVP parameter files
   some PW-functions written by (HPST 22/05/95 IRCAM)
   OM-functions by Hans Tutschku 15/10/98 IRCAM
|#

(in-package ASX)

;********************************************************************************************
;========================
; DURATION OF 'timestretch'
;========================


;********************************************************************************************
(om::defmethod! ASX::randserie  ((rand1min number) 
                                (rand1max number)
                                (rand2min number)
                                (rand2max number)
                                (rand3min number)
                                (rand3max number)
                                (repetitions number)) 
  :initvals '(0.2 0.5 0.5 1 1 20 4)
  :indoc '("rand1min" "rand1max" "rand2min" "rand2max" "rand3min" "rand3max")
  :icon 999
  :doc"creates three random lists"
  
   (om::flat
   (loop for i to (- repetitions 1)
         collect
         (om::list        
          (om::om-round        
           (om::om-random rand1min rand1max)
           2)
          (om::om-round        
           (om::om-random rand2min rand2max)
           2)
          (om::om-round        
           (om::om-random rand3min rand3max)
           2))))
  
  )
;(ASX::randserie 0.2 0.5 0.5 1 1 20 40)
;********************************************************************************************
(om::defmethod! ASX::timeline ((soundlength number) 
                              (steps number))
  :initvals '(3.0 20)
  :indoc '("soundlength" "steps" )
  :icon 999 
  :doc"calculates the timeline - soundlength devidet by number of steps 
as stepsize for the arithmetic serie from 0.0 to soundlenght"
  
  (om::om-round (om::arithm-ser 0 soundlength (om::om/ soundlength steps)) 3)
  )
;(ASX::timeline 3.0 20)
;********************************************************************************************
(om::defmethod! ASX::timeline-melodie ((LDUR list))
  
  :initvals '(500 1000 500)
  :indoc '("durations")
  :icon 999 
  :doc"calculates the timeline from a list of durations"
  
  (butlast    
   (rest
    (om::om-round     
     (om::sort.    
      (om::x-append    
       (om::om- (om::om/ (om::dx->x 0 LDUR) 1000.0) 0.001)
       (om::om/ (om::dx->x 0 LDUR) 1000.0)))
     3)))
  )
;(ASX::timeline-melodie '(500 1000 500))

;********************************************************************************************
(om::defmethod! ASX::timeline-irr ((soundlength number) ;irregular timeline
                                  (steps number))
  :initvals '(3.0 20)
  :indoc '("soundlength" "steps" )
  :icon 999 
  :doc"calculates the timeline which has irregular intervals"
  
  
  (om::remove-dup
   (om::om-round  
    (om::om-scale    
     (om::dx->x 0.0   
                (loop for i to (- steps 2)
                      collect
                      (om::nth-random 
                       (om::list 
                        (om::om-random 0.01 0.05)
                        (om::om-random 0.05 1)
                        (om::om-random 1.0 5)
                        (om::om-random 5.0 20)))
                      )          
                )          
     0.0 soundlength)
    3)
   '= 1)
  )
;(ASX::timeline-irr 3.0 10)
;********************************************************************************************
(om::defmethod! time-onsets ((times list) (facts list))
  :initvals '(nil nil)
  :indoc '("times" "facts")
  :icon 999 
  :doc" "

  (loop for i to (- (length times) 2)
        collect  (/ (* (- (nth (1+ i) times)
                          (nth     i  times))
                       (+ (nth (1+ i) facts)
                          (nth     i  facts))) 2)))

;(ASX::time-onsets '(1.0 2.0 3.0 4.0) '(2 1.2 1 2))
;********************************************************************************************

(om::defmethod! find-dur  ((times  t) 
                           (facts  t))  
  
  :initvals '(nil nil)
  :indoc '("times" "facts")
  :icon 999 
  :doc"calculate duration from of <times> and <facts>"               
  
  (apply '+ (time-onsets times facts))
  )

;(ASX::find-dur '(1 2 3 3.3) '(2.0 3.3 4.4 1.0))
;********************************************************************************************

(om::defmethod! find-coefs ((times  t)
                            (facts  t)
                            (dur    number)) 
  
  :initvals '(nil nil nil)
  :indoc '("times" "facts" "dur")
  :icon 999 
  :doc"calculate coefficients from a <tlist> and <facts> to fit with a desired <dur>"               
  
  (om::om* facts (/ dur (find-dur times facts)))
  )
  

;(ASX::find-coefs '(1 2 3 3.3) '(2.0 3.3 4.4 1.0) 10.0)

;********************************************************************************************
(om::defmethod! ASX::stretch-dyn-random  ((soundlength number) 
                       (steps integer)
                       (filename string)) 
  :initvals '(3.4 10 "stretch.par")
  :indoc '("soundlength" "steps" "filename")
  :icon 999
  :doc"creates parameterfile for random-timestretch"
  (let* ((final nil)
         (faktoren (om::remove-dup
                    (om::sort.   
                     (om::om-round    
                      (om::om-scale    
                       (om::flat
                        (loop for i to (- (* steps 3) 1)
                              collect
                              (om::om-random 0 1000000)))
                       0.0 soundlength)
                      3)
                     )
                    '= 1))
         (faktleng (length faktoren)))
    
    
    (setf final (om::mat-trans
                 (om::list
                  faktoren
                  (om::first-n (ASX::randserie 0.2 0.5 0.5 1 1 20 steps) faktleng)
                  ;in case that not enough timevalues without repetition could be
                  ;calculatet, just as many stretchvalues are taken
                  )))
    
    (ASX::save-or-not final filename)
    )
  )



;(ASX::stretch-dyn-random 3.4 10 1 )

;(om::first-n (ASX::randserie 0.2 0.5 0.5 1 1 20 30) 3)
;********************************************************************************************
(om::defmethod! ASX::stretch-dyn-random2  ((soundlength number) ; ohne Fileselector
                                          (steps integer)
                                          (filename string)
                                          (saveflag string)) 
  :initvals '(3.4 10 "stretch.par" "save")
  :indoc '("soundlength" "steps" "filename" "saveflag")
  :menuins '( (3 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"creates parameterfile for random-timestretch"
  (let* ((final nil)
         (faktoren (om::remove-dup
                    (om::sort.   
                     (om::om-round    
                      (om::om-scale    
                       (om::flat
                        (loop for i to (- (* steps 3) 1)
                              collect
                              (om::om-random 0 1000000)))
                       0.0 soundlength)
                      3)
                     )
                    '= 1))
         (faktleng (length faktoren)))
    
    
    (setf final (om::mat-trans
                 (om::list
                  faktoren
                  (om::first-n (ASX::randserie 0.2 0.6 0.6 1 1 30 steps) faktleng)
                  ;in case that not enough timevalues without repetition could be
                  ;calculatet, just as many stretchvalues are taken
                  )))
    
    (if (string=  "don't save" saveflag)
      final
      (progn ()      
             (ASX::write-lists3 final filename)
             '("saved")
             ))
    
    )
  )



;(ASX::stretch-dyn-random 3.4 10 1 )

;(om::first-n (ASX::randserie 0.2 0.5 0.5 1 1 20 30) 3)

;********************************************************************************************
(om::defmethod! ASX::stretch-dyn-random3  ((soundlength number) ; ohne Fileselector
                                          (steps integer)
                                          (filename string)
                                          (saveflag string)) 
  :initvals '(3.4 10 "stretch.par" "save")
  :indoc '("soundlength" "steps" "filename" "saveflag")
  :menuins '( (3 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"creates parameterfile for random-timestretch"
  (let* ((final nil)
         (faktoren (om::remove-dup
                    (om::sort.   
                     (om::om-round    
                      (om::om-scale    
                       (om::flat
                        (loop for i to (- (* steps 3) 1)
                              collect
                              (om::om-random 0 1000000)))
                       0.0 soundlength)
                      3)
                     )
                    '= 1))
         (faktleng (length faktoren)))
    
    
    (setf final (om::mat-trans
                 (om::list
                  faktoren
                  (om::first-n (ASX::randserie 0.2 0.6 0.6 1 1 30 steps) faktleng)
                  ;in case that not enough timevalues without repetition could be
                  ;calculatet, just as many stretchvalues are taken
                  )))
    
    (if (string=  "don't save" saveflag)
      final
      (progn ()      
             (ASX::write-lists3 final filename)
             '("saved")
             ))
    
    )
  )



;(ASX::stretch-dyn-random 3.4 10 1 )

;(om::first-n (ASX::randserie 0.2 0.5 0.5 1 1 20 30) 3)

;********************************************************************************************
(om::defmethod! ASX::stretch-dyn-exact  ((soundlength number) 
                                        (steps integer)
                                        (newlength number)
                                        (filename string)) 
  :initvals '(3.4 20 5.5 "stretch.par")
  :indoc '("soundlength" "steps" "soundlength" "filename" )
  :menuins '( (4 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"creates parameterfile for random-timestretch"
  (let* ((newfaktoren nil)(final nil)
         (timeline (om::remove-dup
                    (om::sort.   
                     (om::om-round    
                      (om::om-scale    
                       (om::flat
                        (loop for i to (- (* steps 3) 1)
                              collect
                              (om::om-random 0 1000000)))
                       0.0 soundlength)
                      4)
                     )
                    '= 1))
         (timeleng (length timeline))
         (faktoren (om::x-append (om::first-n (ASX::randserie 0.2 0.5 0.5 1 1 20 steps) (- timeleng 2)) 1 1)))
    ;in case that not enough timevalues without repetition could be
    ;calculatet, just as many stretchvalues are taken
    ;(print parampath)
    
    
    (setf newfaktoren 
          (om::om-round
           (ASX::find-coefs 
            timeline
            faktoren
            newlength)
           4))
    
    (setf final (om::mat-trans
                 (om::list  
                  timeline 
                  newfaktoren)))
    
    (ASX::save-or-not final filename)
    
    
    ))

;(ASX::stretch-dyn-exact 3.4 3 5.5 1 )

;********************************************************************************************
(om::defmethod! ASX::stretch-dyn-exact-loop  ((steps number)
                                             (stretchfact number)
                                             (filename string)
                                             (soundpath string) 
                                             (soundname t)) 
  :initvals '(20.0 2.0 "stretch.par" "G3-Daten02" "toto")
  :indoc '("number" "number" "string" "string" "string")
  :icon 999
  :doc"creates parameterfile for random-timestretch"
  (let* ((newfaktoren nil)(final nil)
         (newsoundname (format nil "~A:~A" soundpath soundname))
         (soundlength (ASX::check-soundlength newsoundname))
         (steps (+ 1 (* steps soundlength)))
         (timeline (om::remove-dup
                    (om::sort.   
                     (om::om-round    
                      (om::om-scale    
                       (om::flat
                        (loop for i to (- (* steps 3) 1)
                              collect
                              (om::om-random 0 1000000)))
                       0.0 soundlength)
                      4)
                     )
                    '= 1))
         (timeleng (length timeline))
         (faktoren (om::x-append (om::first-n (ASX::randserie 0.2 0.5 0.5 1 1 20 steps) (- timeleng 2)) 1 1)))
    ;in case that not enough timevalues without repetition could be
    ;calculatet, just as many stretchvalues are taken
    (print soundlength)
    (setf newfaktoren 
          (om::om-round
           (ASX::find-coefs 
            timeline
            faktoren
            (* soundlength stretchfact))
           4))
    
    (setf final (om::mat-trans
                 (om::list  
                  timeline 
                  newfaktoren)))
    
    
    (let (fichier) 
      (delete-file filename)
      (when filename)
      (with-open-file  (fd filename
                           :direction :output :if-exists :supersede 
                           :if-does-not-exist :create)
        (dotimes (n (length final))
          (dotimes (j (length (nth n final)))
            (format fd "~D "(nth j (nth n final))))
          (format fd "~%"))
        ))
    
    
    
    ))

;(ASX::stretch-dyn-exact 3.4 3 5.5 1 )



;********************************************************************************************

(om::defmethod! ASX::marker-stretch  ((markers list) 
                                     (zone number)
                                     (factor number)
                                     (soundlength number)
                                     (filename string)
                                     (saveflag string)
                                     ) 
  :initvals '(( MARKERS 5 0.5 1.1 1.8 2.4 3.7) 0.1 5.0 6.0 "stretch.par" "save")
  :indoc '("marker" "zone" "factor" "soundlength" "filename" "saveflag")
  :menuins '( (5 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"this patch is creating a parameterfile for Audiosculpt timestretch. Starting from a markerfile - which represents the attacks to keep untouched during stretch"
  
  (let* ((faktoren nil)(final nil)
         
         (markers 
          (om::rest (om::rest (om::flat markers))))
         (number (length markers))
         
         
         (vor (om::om- markers (om::om/ zone 2.0)))
         (nach (om::om+ markers (om::om/ zone 2.0)))
         (vorvor (om::om- vor 0.005))
         
         (timeline (om::x-append 0.0    
                                 (om::om-round    
                                  (om::rest
                                   (om::sort.     
                                    (om::flat
                                     (om::list vor nach vorvor))))
                                  3)
                                 soundlength)))

    (setf faktoren (om::x-append 1.0
                                 (om::flat
                                  (om::mat-trans    
                                   (om::list    
                                    (om::repeat-n 1.0 number)
                                    (om::repeat-n 1.0 number)
                                    (om::repeat-n factor number))))))
    
    (setf final (om::mat-trans
                 (om::list
                  timeline faktoren)))

    (ASX::save-or-not final filename saveflag)
    
    ))

