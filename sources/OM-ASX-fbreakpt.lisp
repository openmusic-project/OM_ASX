;****************************
;    OM-functions by Hans Tutschku 15/10/98 IRCAM

(in-package ASX)



;********************************************************************************************

(om::defmethod! ASX::rhythm-fbreakpt  ((LDUR list) 
                                      (cutfreq number)
                                      (openfreq number)
                                      (filename string)
                                      (saveflag string)
                                      ) 
  :initvals '((100 33 33 100 33 33) 100 20000 "fbreakpt.par" "save")
  :indoc '("LDUR" "cutfreq" "openfreq" "filename" "saveflag")
  :menuins '( (4 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"A rhythm is converted into a rhythmical sequenz of opening and closing filter for Audiosculpt Fbreakpt-filter"
  
  (let* ((final nil)(freq nil)
         (timeline (om::om/ LDUR 10)))
    
    (setf timeline (om::flat 
                    (dolist (n timeline (nreverse final))
                      (push (om::repeat-n n 10) final))))
    
    (setf timeline  (om::om/ (om::dx->x 0 timeline) 1000.0))
    
    (setf timeline (om::list-explode timeline (/ (- (length timeline) 1) 10)))
    
    (setf final nil)   
    (setf timeline (om::om-round (rest (om::flat
                                        (dolist (n timeline (nreverse final))
                                          (push (om::x-append (om::om-  (first n) 0.001) n) final))))
                                 4))
    
    ; (setf timeline (om::flat (om::list-explode timeline (/ (length timeline) 11))))
    (setf freq (om::flat (ASX::repeat
                           (reverse (om::arithm-ser cutfreq openfreq (/ (om::om- openfreq cutfreq) 10)))
                           (/ (length timeline) 11))))
    
    (setf final (mapcar #'(lambda (x y)    
                            (om::list x 0 3 0 0 (- cutfreq 14) 0 y -90))
                        timeline freq))
    
    
    (ASX::save-or-not final filename saveflag)
    
    
    )
  )

;(ASX::rhythm-fbreakpt '(100 33 33 100 33 33) 100 20000 )
;********************************************************************************************
(om::defmethod! ASX::groupe-freqs ((liste list))
  :initvals '(nil)
  :indoc '("liste des paires temps freqs")
  :icon 999 
  :doc"liste est une liste de paires temps freqs
la liste est déjà ordonnée en fonction des temps"
  ; aux0 liste d'attente
  
  (let ((aux0 nil) (aux1 nil) (mem -1))
    
    (dolist (n liste aux1)
      (if (not (=  (first n) mem))
        (progn ()
               (push (remove nil aux0) aux1)
               (setf aux0 nil)
               (push (second n) aux0)
               (setf mem (first n)))
        (progn ()
               (push (second n) aux0))))
    
    (om::mat-trans (list (om::remove-dup  (first (om::mat-trans liste)) 'equalp 1)
                         (remove nil (reverse (push aux0 aux1)))))
    
    )
  )
;********************************************************************************************
(om::defmethod! ASX::groupe-freqs+amp ((liste list))
  :initvals '(nil)
  :indoc '("liste des triplets temps freqs amp")
  :icon 999 
  :doc"liste est une liste de paires temps freqs
la liste est déjà ordonnée en fonction des temps"
  ; aux0 liste d'attente
  
  (let ((aux0 nil) (aux1 nil) (mem -1))
    
    (dolist (n liste aux1)
      (if (not (=  (first n) mem))
        (progn ()
               (push (remove nil aux0) aux1)
               (setf aux0 nil)
               (push (om::x-append (second n)(third n)) aux0)
               (setf mem (first n)))
        (progn ()
               (push (om::x-append (second n)(third n)) aux0))))
    
    (om::mat-trans (list (om::remove-dup  (first (om::mat-trans liste)) 'equalp 1)
                         (remove nil (reverse (push aux0 aux1)))))
    
    )
  )

;********************************************************************************************
(om::defmethod! ASX::round-freqs ((liste list))
  :initvals '(nil)
  :indoc '("liste")
  :icon 999 
  :doc"keeps times and rounds frequencies"
  
  (om::sort.
   (mapcar #'(lambda (x)
               (list (first x) (om::om-round (second x)))) liste)
   '< 'first)
  )
;********************************************************************************************
(om::defmethod! ASX::round-freqs+amp ((liste list))
  :initvals '(nil)
  :indoc '("liste")
  :icon 999 
  :doc"keeps times,rounds frequencies and keeps amplitudes"
  
  (om::sort.
   (mapcar #'(lambda (x)
               (list (first x) (om::om-round (second x)) (om::om-round (third x) 1))) liste)
   '< 'first)
  )
;********************************************************************************************
(om::defmethod! ASX::sort-freqs ((liste list))
  :initvals '(nil)
  :indoc '("liste")
  :icon 999 
  :doc"keeps times and sorts frequencies"
  
   (mapcar #'(lambda (x)
               (list (first x) (om::sort. (second x)))) liste)
  )
;********************************************************************************************
(om::defmethod! ASX::sort-freqs+amp ((liste list))
  :initvals '(nil)
  :indoc '("liste")
  :icon 999 
  :doc"keeps times and sorts frequencies"
  
   (mapcar #'(lambda (x)
               (list (first x) (om::sort. (second x) '< 'first))) liste)
  )

;********************************************************************************************
(om::defmethod! ASX::just-times ((liste list))
  :initvals '(nil)
  :indoc '("liste")
  :icon 999 
  :doc"gives just the times back"
   (mapcar #'(lambda (x)
              (first x)) liste)
  )
;********************************************************************************************
(om::defmethod! ASX::cut-first-two-elements ((daten list))
  :initvals '(nil)
  :indoc '("liste")
  :icon 999 
  :doc""
  
  (mapcar #'(lambda (x)
              (nthcdr 2 x)) daten)
  )

;(ASX::cut-first-two-elements '((1 2 3 4)(a b c d)))
;********************************************************************************************
(om::defmethod! ASX::bandwith-for-fbreakpt ((daten list)
                                           (point1 number)
                                           (point2 number)
                                           (point3 number)
                                           (point4 number)
                                           (bw number)
                                           (bandswitch om::bpf)
                                           (verschiebung number))
  :initvals '(nil -60 -43 43 60 40 0 0.046)
  :indoc '("liste" "point1" "point2" "point3" "point4" "bandwith" "keep or reject partials" "verschiebung")
  :icon 999 
  :doc"calculates frequencies and amplitudes around the central freq 
<freq - point1><freq - point2><freq + point3><freq + point4>"
  
  (let* ((subgroup nil)(filter nil)(laenge nil)
         (newdata (mapcar #'(lambda (x)                        ;calculate new four freqs around the original freq
                              (om::flat                
                               (mapcar #'(lambda (y)
                                           (om::list
                                            (om::om+ point1 y) 
                                            (om::om+ point2 y)
                                            (om::om+ point3 y)
                                            (om::om+ point4 y)) )
                                       (second x) ))) 
                          daten ))
         
         
         
         (timeline (om::om-round (om::om- (ASX::just-times daten) verschiebung) 3))) ;verschiebung um 4 analysefenster
    
    
    (setf newdata 
          (mapcar #'(lambda (x y)
                      (if (= 4 (length x))
                        (list y x)
                        (list y 
                              (om::flat (progn ()
                                               (setf subgroup 
                                                     (om::group-list x 
                                                                     (om::flat (om::list 2 (om::repeat-n 4  (- (/ (length x) 4) 1)) 2)) 'linear))
                                               (remove 9999 (mapcar #'(lambda (z)
                                                                        (if (= 2 (length z))
                                                                          z
                                                                          (if (> (+ bw 36) (- (third z) (second z)))
                                                                            9999
                                                                            z)))
                                                                    subgroup))                 
                                               )))
                        )
                      )
                  newdata timeline))  
    
    
    (om::sort.
     (mapcar #'(lambda (x)
                 (setf filter (om::om-round
                               (ASX::vel-db (ASX::f-transfer bandswitch (/ (* 100 (first x)) (first (last timeline)))))
                               2))
                 (setf filter (om::om-round 
                               (om::list  (-  -76.2 filter ) filter filter (-  -76.2 filter ) )1))
                 (setf laenge (length (second x)))
                 
                 
                 (om::flat
                  (list (first x) 0  laenge                          ; list together "time" "0"
                        (om::flat
                         (om::mat-trans                        
                          (om::list (second x)                          ;put freqs and amps together
                                    (om::flat (ASX::repeat filter (om::om/ laenge 4)))))))))
             newdata )
     '< 'first)
    )
  )
;********************************************************************************************
(om::defmethod! ASX::bandwith-for-fbreakpt2 ((daten list)
                                            (point1 number)
                                            (point2 number)
                                            (point3 number)
                                            (point4 number)
                                            (bw number)
                                            (filterflag number)
                                            (verschiebung number))
  :initvals '(nil -60 -43 43 60 40 0 0.046)
  :indoc '("liste" "point1" "point2" "point3" "point4" "bandwith" "keep or reject partials" "verschiebung")
  :icon 999 
  :doc"calculates frequencies and amplitudes around the central freq 
<freq - point1><freq - point2><freq + point3><freq + point4>
depending on filterflag the amplitudes around each freq are (0 -90 -90 0) or (-90 0 0 -90)"
  
  (let* ((subgroup nil)
         (newdata (mapcar #'(lambda (x)                        ;calculate new four freqs around the original freq
                              (om::flat 
                               (mapcar #'(lambda (y)
                                           (om::list
                                            (om::om+ point1 (first y)) 
                                            (om::om+ point2 (first y))
                                            (om::om+ point3 (first y))
                                            (om::om+ point4 (first y))) )
                                       (second x))
                               )
                              ) 
                          daten ))
         
         
         
         (timeline (om::om-round (om::om- (ASX::just-times daten) verschiebung) 3))) ;verschiebung um 4 analysefenster
    
    ;(print daten)
    ;(print newdata)
    
    
    
    (setf newdata 
          (mapcar #'(lambda (x y)
                      (if (= 4 (length x))
                        (list y (om::flat x))
                        (list y 
                              (om::flat (progn ()
                                               (setf subgroup 
                                                     (om::group-list x 
                                                                     (om::flat (om::list 2 (om::repeat-n 4  (- (/ (length x) 4) 1)) 2)) 'linear))
                                               (remove 9999 (mapcar #'(lambda (z)
                                                                        (if (= 2 (length z))
                                                                          z
                                                                          (if (> (+ bw 36) (- (third z) (second z)))
                                                                            9999
                                                                            z)))
                                                                    subgroup))                 
                                               )))
                        )
                      
                      )
                  newdata timeline))
    
    
    ;(print newdata)
    
     (om::sort.
     (mapcar #'(lambda (x)
                 (om::flat
                  (list (first x) 0 
                        (/ (length  (om::flat
                                     (om::mat-trans                        
                                      (om::list (second x)                          ;put freqs and amps together
                                                (om::flat
                                                 (if (= 0 filterflag)      ;depending on filterflag reject or keep 
                                                   (om::repeat-n  '(0 -96 -96 0)              
                                                                  (om::om/ (length (second x)) 4))
                                                   (om::repeat-n  '(-96 0 0 -96)              
                                                                  (om::om/ (length (second x)) 4))
                                                   
                                                   )))))) 2)                          
                        (om::flat
                         (om::mat-trans                        
                          (om::list (second x)                          ;put freqs and amps together
                                    (om::flat
                                     (if (= 0 filterflag)      ;depending on filterflag reject or keep 
                                       (om::repeat-n  '(0 -96 -96 0)              
                                                      (om::om/ (length (second x)) 4))
                                       (om::repeat-n  '(-96 0 0 -96)              
                                                      (om::om/ (length (second x)) 4))
                                       
                                       ))))))))
             newdata )
     '< 'first)
    
    
    
    )
  )

;********************************************************************************************

(om::defmethod! ASX::bandwith-for-fbreakpt+amp ((daten list)   ; function for "keep with amplitudes"
                                               (point1 number)
                                               (point2 number)
                                               (point3 number)
                                               (point4 number)
                                               (bw number)
                                               (bwminimum number)
                                               (verschiebung number))
  :initvals '(nil -60 -43 43 60 50 46 0.046)
  :indoc '("liste" "point1" "point2" "point3" "point4" "bandwith" "bwminimum" "verschiebung")
  :icon 999 
  :doc"calculates frequencies and amplitudes around the central freq 
<freq - point1><freq - point2><freq + point3><freq + point4>"
  
  (let* ((subgroup nil)(aux nil)
         (timeline (om::om-round (om::om- (ASX::just-times daten) verschiebung) 3)) ;verschiebung um 4 analysefenster
         
         (newdata (mapcar #'(lambda (x)                        ;calculate new four freqs around the original freq
                              (om::flat (mapcar #'(lambda (y)
                                                    (om::mat-trans
                                                     (om::list
                                                      (om::list
                                                       (om::om-round (om::om+ point1 (first y))) 
                                                       (om::om-round (om::om+ point2 (first y)))
                                                       (om::om-round (om::om+ point3 (first y)))
                                                       (om::om-round (om::om+ point4 (first y))))
                                                      (om::list -96 (second y) (second y) -96 ))))
                                                (second x))))
                          daten )))

    

  (setf newdata (dolist (n newdata (nreverse aux))
                    (push (om::list-explode n (/ (length n) 2)) aux)))
    
    
   ;(print newdata)
    (setf newdata 
          (mapcar #'(lambda (x)
                      (if (= 4 (length x))
                        (om::flat x)
                         (om::flat (progn ()
                                         (setf subgroup 
                                               (om::group-list x 
                                                               (om::flat (om::list 2 (om::repeat-n 4  (- (/ (length x) 4) 1)) 2)) 'linear))
                                         (remove 9999 (mapcar #'(lambda (z)
                                                                  (if (= 2 (length z))
                                                                    z
                                                                    (if (> (+ bw (* 3 bwminimum)) (- (first (third z)) (first (second z))))
                                                                      9999
                                                                      z)))
                                                              subgroup))                 
                                         ))
                        
                        )
                      )
                  newdata))

    
   
(setf newdata (mapcar #'(lambda (x y)
                              (om::x-append y 0 (/ (length (om::flat x)) 2) x))
                          newdata timeline))

    )
  )
;********************************************************************************************
(om::defmethod! ASX::scramble-data ((daten list))
  
  :initvals '(nil)
  :indoc '("partilas")
  :icon 999
  :doc""
  (let* ((daten 
          (om::flat
           (ASX::cut-first-two-elements daten) ;cut "partials <number>" from every sublist
           ))
         
         (number (om::om/ (om::length daten) 3))
         
         (pattern (om::repeat-n 3 number)))   ;for "grouplist" - listexplode is not yet ported
    
    (setf daten (om::group-list daten pattern 'linear))
    ))
;********************************************************************************************

(om::defmethod! ASX::partials-fbreakpt-bpf  ((partials list) 
                                        (bw number)
                                        (bandswitch om::bpf)
                                        (filename string)
                                        (saveflag string)
                                        ) 
  :initvals '(nil 40 nil "fbreakpt.par" "save")
  :indoc '("partilas" "bandwith in Hz" "keep or reject partials" "filename" "saveflag")
  :menuins '((4 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"takes a partial-file from Audiosculpt and converts it into a fbreakpt-filterfile"
  
  (if (= 0 (length partials))
    "no partials"
    (let* ((final nil)
           (daten (ASX::scramble-data (rest (rest (first partials)))))
           (point2 (om::om* -1 (om::om/ bw 2)))
           (point3 (om::om/ bw 2))
           (point4 (om::om+  point3 12))
           (point1 (om::om- point2 12)))
      
      
      (setf final (ASX::bandwith-for-fbreakpt                         ;puts freqs and amps together
                   (ASX::sort-freqs
                    (ASX::groupe-freqs                              ;groupes all frequencies which are simultanously on the same timepoint
                     (ASX::round-freqs daten)))
                   point1 point2 point3 point4 bw bandswitch 0.046)
            )
 
      (if (not (= (first (first final)) 0))
        (setf final (om::x-append 
                     '((0.0 0 2 100 -96 200 -96)) 
                     final)))
     
      (ASX::save-or-not final filename saveflag)
      )
    )
  )
;********************************************************************************************

(om::defmethod! ASX::partials-fbreakpt  ((partials list) 
                                        (bw number)
                                        (bandswitch string)
                                        (fftsize integer)
                                        (filename string)
                                        (saveflag string)
                                        ) 
  :initvals '(nil 40 "band" 4096 "fbreakpt.par" "save")
  :indoc '("partilas" "bandwith in Hz" "keep or reject partials" "fft-size" "filename" "saveflag")
  :menuins '( (2 (("reject" "reject") ("keep" "keep")("keep with amp" "keep with amp")))
             (3 (("1024" 1024) ("2048" 2048) ("4096" 4096) ("8192" 8192) ("16384" 16384) ("32768" 32768)))
             (5 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"takes a partial-file from Audiosculpt and converts it into a fbreakpt-filterfile"
  
  (if (= 0 (length partials))
    "no partials"
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
           
           (daten (ASX::scramble-data (rest (rest (first partials)))))
           (point2 (om::om* -1 (om::om/ bw 2)))
           (point3 (om::om/ bw 2))
           (point4 (om::om+  point3 bwminimum))
           (point1 (om::om- point2 bwminimum))
           (filterflag  (if (string= "reject" bandswitch)
                          0 1))) 
     
    

(if (string= "keep with amp" bandswitch)
        (setf final (ASX::bandwith-for-fbreakpt+amp
                     (ASX::sort-freqs+amp
                      (ASX::groupe-freqs+amp
                       (ASX::round-freqs+amp daten)))
                     point1 point2 point3 point4 bw bwminimum 0.046))
        
        
        (setf final               
              (ASX::bandwith-for-fbreakpt2                         ;puts freqs and amps together
               (ASX::sort-freqs
                (ASX::groupe-freqs                              ;groupes all frequencies which are simultanously on the same timepoint
                 (ASX::round-freqs daten)))
               point1 point2 point3 point4 bw filterflag 0.046)))
          

      (if (not (= (first (first final)) 0))
        (setf final (om::x-append 
                     '((0.0 0 2 100 -96 200 -96)) 
                     final)))
     
      (ASX::save-or-not final filename saveflag)

           
      )
    )
  )

;********************************************************************************************

(om::defmethod! ASX::partials-fbreakpt2  ((self om::sdiffile) 
                                          (bw number)
                                          (bandswitch string)
                                          (fftsize integer)
                                          (filename string) 
                                          ) 
  :initvals '(nil 40 "band" 4096 "fbreakpt.par")
  :indoc '("partilas" "bandwith in Hz" "keep or reject partials" "fft-size" "filename" )
  :menuins '( (2 (("reject" "reject") ("keep" "keep")("keep with amp" "keep with amp"))))
  :icon 999
  :doc"takes a partial-file from Audiosculpt and converts it into a fbreakpt-filterfile"
  
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
         
         (daten (ASX::get-sdif-partials self))
         (point2 (om::om* -1 (om::om/ bw 2)))
         (point3 (om::om/ bw 2))
         (point4 (om::om+  point3 bwminimum))
         (point1 (om::om- point2 bwminimum))
         (filterflag  (if (string= "reject" bandswitch)
                        0 1))) 
    
    (setf daten (butlast daten))
    (setf daten (rest daten))    
    
    
    
    (if (string= "keep with amp" bandswitch)
      (setf final (ASX::bandwith-for-fbreakpt+amp
                   daten
                   point1 point2 point3 point4 bw bwminimum 0))
      
      (setf final               
            (ASX::bandwith-for-fbreakpt2                         ;puts freqs and amps together
             daten
             point1 point2 point3 point4 bw filterflag 0)))
    
    
    (setf final (remove nil (mapcar #'(lambda (x)                  ;falls beim herausfiltern der Punkte keine Frequenzen übrig bleiben, nimm die ganze Zeile raus
                            (if (< 5 (length x))       ;ansonsten hat SVP ein Problem und kann das Filterfile nicht akzeptieren
                              x))
                            final)))
   
    (setf final (om::x-append 
                 '((0.0 0 2 100 -96 200 -96)) 
                 final))
    
    ; (ASX::save-or-not final filename)
    
    
    
    
    )
  )
  

;********************************************************************************************

(om::defmethod! ASX::partials-amp-filter  ((partials list) 
                                          (ampmin number)
                                          ) 
  :initvals '(nil -40)
  :indoc '("partilas" "minimum amplitude")
  :icon 999
  :doc"keep partials which average amplitude is higher then ampmin"
  
  (let ((daten 
         (ASX::cut-first-two-elements (rest (rest (first partials) ))))
        (ampmean nil))
    
    (setf daten (mapcar #'(lambda (x)
                            (om::group-list x 
                                            (om::repeat-n 3 
                                                          (om::om/ (om::length x) 3))
                                            'linear))
                        daten ))
    
    
    (setf ampmean (mapcar #'(lambda (x)
                              (om::om-round                
                               (om::om-mean                
                                (om::third
                                 (om::mat-trans x)))
                               3))
                          daten ))
    
    (setf daten (remove nil
                        (mapcar #'(lambda (x y)         ;just keep the partials above a certain amplitude level
                                    
                                    (om::flat
                                     (if (< ampmin y)
                                       (list 'points (length x) x))))
                                daten ampmean )))
     
    (if (= 0 (length daten))
      (progn ()
             (print "no partials above this amplitude")
             ())
      (om::list (om::x-append 'partials (om::x-append (length daten) daten)) ))
    
    )
  )    
   
  ;********************************************************************************************


(om::defmethod! ASX::fbreakpt-rand  ((soundlength number) 
                                     (steps integer)
                                     (minpoints integer)
                                     (maxpoints integer)
                                     (minamp number)
                                     (maxamp number)
                                     (minfreq number)
                                     (maxfreq number)
                                     (bandswitch string)
                                     (filename string)
                                     (saveflag string)) 
  :initvals '(3.4 10 5 10 -50 0.0 30 20000 "keep" "fbreakpt.par" "save")
  :indoc '("soundlength" "steps" "min number of freqpoints" "max number of freqpoints" "in dB" "in dB"
           "low limit freq" "high limit freq" "keep or reject bands" "filename" "saveflag")
  :menuins '((8 (("keep" "keep") ("reject" "reject")))
             (10 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"random evolution for breakpointfilter"
  
  (let* ((aux nil)(numbofbands nil)
         (timeline (ASX::timeline-irr soundlength steps))
         (keepamp (if (string= "keep" bandswitch) -96 0))
         
         (daten (dolist (n timeline (nreverse aux))
                  (progn ()
                         (setf numbofbands (om::om-random minpoints maxpoints))
                         (push (om::list n 0
                                         (+ 4 numbofbands) 0 keepamp minfreq keepamp
                                         (om::mat-trans
                                          (om::list
                                           (om::sort.
                                            (loop for i from 1 to numbofbands
                                                  collect
                                                  (om::om-round                         
                                                   (om::om* 12.0 (om::om-random (+ (/ minfreq 12.0) 12) (- (/ maxfreq 12.0) 12))))))
                                           (om::om-round                    
                                            (loop for i from 1 to numbofbands
                                                  collect
                                                  (om::om-random minamp maxamp)) 1)))
                                         maxfreq keepamp 22000 keepamp)
                               aux)))))
    
    ;(ASX::save-or-not (om::mapcar 'om::flat daten) filename saveflag)
    (om::mapcar 'om::flat daten)
    
    )
  )    
   

  
;(ASX::fbreakpt-rand 3.4 10 2 5 -50 0.0 30 20000 "fbreakpt.par" "save")

   ;********************************************************************************************


(om::defmethod! ASX::fbreakpt-rand-rhythm  ((soundlength number) 
                                     (steps integer)
                                     (minpoints integer)
                                     (maxpoints integer)
                                     (minamp number)
                                     (maxamp number)
                                     (minfreq number)
                                     (maxfreq number)
                                     (bandswitch string)
                                     (timeline list)
                                     ) 
  :initvals '(3.4 10 5 10 -50 0.0 30 20000 "keep" '(1 2 3 4) )
  :indoc '("soundlength" "steps" "min number of freqpoints" "max number of freqpoints" "in dB" "in dB"
           "low limit freq" "high limit freq" "keep or reject bands" "filename" "saveflag")
  :menuins '((8 (("keep" "keep") ("reject" "reject")))
             )
  :icon 999
  :doc"random evolution for breakpointfilter - following the rhythm of timeline"
  
  (let* ((aux nil)(numbofbands nil)
         
         (keepamp (if (string= "keep" bandswitch) -96 0))
         
         (daten (dolist (n timeline (nreverse aux))
                  (progn ()
                         (setf numbofbands (om::om-random minpoints maxpoints))
                         (push (om::list n 0
                                         (+ 4 numbofbands) 0 keepamp minfreq keepamp
                                         (om::mat-trans
                                          (om::list
                                           (om::sort.
                                            (loop for i from 1 to numbofbands
                                                  collect
                                                  (om::om-round                         
                                                   (om::om* 12.0 (om::om-random (+ (/ minfreq 12.0) 12) (- (/ maxfreq 12.0) 12))))))
                                           (om::om-round                    
                                            (loop for i from 1 to numbofbands
                                                  collect
                                                  (om::om-random minamp maxamp)) 1)))
                                         maxfreq keepamp 22000 keepamp)
                               aux)))))
    
    ;(ASX::save-or-not (om::mapcar 'om::flat daten) filename saveflag)
    (om::mapcar 'om::flat daten)
    
    )
  )    
   

  
;(ASX::fbreakpt-rand 3.4 10 2 5 -50 0.0 30 20000 "fbreakpt.par" "save")

;********************************************************************************************
(om::defmethod! ASX::fbreakpt-rand-loop  ((steps number)
                                         (minpoints integer)
                                         (maxpoints integer)
                                         (minamp number)
                                         (maxamp number)
                                         (minfreq number)
                                         (maxfreq number)
                                         (bandswitch string)
                                         (filename string)
                                         (soundpath string) 
                                         (soundname t)) 
  :initvals '(2.0 5 10 -50 0.0 30 20000 "keep" "fbreakpt.par" "G3-Daten02" "toto")
  :indoc '("number" "min number of freqpoints" "max number of freqpoints" "in dB" "in dB"
           "low limit freq" "high limit freq" "keep or reject bands" "filename" "string" "string")
  :menuins '((7 (("keep" "keep") ("reject" "reject"))))
  :icon 999
  :doc"random evolution for breakpointfilter"
  
  (let* ((aux nil)(numbofbands nil)
         (newsoundname (format nil "~A:~A" soundpath soundname))
         (soundlength (ASX::check-soundlength newsoundname))
         (steps (+ 1 (* steps soundlength)))
         (timeline (ASX::timeline-irr soundlength steps))
         (keepamp (if (string= "keep" bandswitch) -96 0))
         
         (daten (dolist (n timeline (nreverse aux))
                  (progn ()
                         (setf numbofbands (om::om-random minpoints maxpoints))
                         (push (om::list n 0
                                         (+ 4 numbofbands) 0 keepamp minfreq keepamp
                                         (om::mat-trans
                                          (om::list
                                           (om::sort.
                                            (loop for i from 1 to numbofbands
                                                  collect
                                                  (om::om-round                         
                                                   (om::om* 12.0 (om::om-random (+ (/ minfreq 12.0) 12) (- (/ maxfreq 12.0) 12))))))
                                           (om::om-round                    
                                            (loop for i from 1 to numbofbands
                                                  collect
                                                  (om::om-random minamp maxamp)) 1)))
                                         maxfreq keepamp 22000 keepamp)
                               aux))))
         (final (om::mapcar 'om::flat daten)))
    
    
    
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
    
    
    )
  )    
   

  
;(ASX::fbreakpt-rand 3.4 10 2 5 -50 0.0 30 20000 "fbreakpt.par" "save")

;********************************************************************************************
(om::defmethod! ASX::fbreakpt-rand-nurattack  ((steps number)
                                              (minpoints integer)
                                              (maxpoints integer)
                                              (minamp number)
                                              (maxamp number)
                                              (minfreq number)
                                              (maxfreq number)
                                              (bandswitch string)
                                              (filename string)
                                              (attacklnge number) 
                                              (randomvalue number)) 
  :initvals '(2.0 5 10 -50 0.0 30 20000 "keep" "fbreakpt.par" 0.2 0.1)
  :indoc '("number" "min number of freqpoints" "max number of freqpoints" "in dB" "in dB"
           "low limit freq" "high limit freq" "keep or reject bands" "filename" "number" "number")
  :menuins '((7 (("keep" "keep") ("reject" "reject"))))
  :icon 999
  :doc"random evolution for breakpointfilter - nur Attacks"
  
  (let* ((aux nil)(numbofbands nil)
         
         (soundlength (+ 0.05 (om::perturbation attacklnge randomvalue)))
         (steps (+ 1 (* steps soundlength)))
         (timeline (ASX::timeline-irr soundlength steps))
         (keepamp (if (string= "keep" bandswitch) -96 0))
         
         (daten (dolist (n timeline (nreverse aux))
                  (progn ()
                         (setf numbofbands (om::om-random minpoints maxpoints))
                         (push (om::list n 0
                                         (+ 4 numbofbands) 0 keepamp minfreq keepamp
                                         (om::mat-trans
                                          (om::list
                                           (om::sort.
                                            (loop for i from 1 to numbofbands
                                                  collect
                                                  (om::om-round                         
                                                   (om::om* 12.0 (om::om-random (+ (/ minfreq 12.0) 12) (- (/ maxfreq 12.0) 12))))))
                                           (om::om-round                    
                                            (loop for i from 1 to numbofbands
                                                  collect
                                                  (om::om-random minamp maxamp)) 1)))
                                         maxfreq keepamp 22000 keepamp)
                               aux))))
         (final (om::mapcar 'om::flat daten)))
    
    
    
    #|
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
|#
    final
    
    )
  )    
   

  
;(ASX::fbreakpt-rand 3.4 10 2 5 -50 0.0 30 20000 "fbreakpt.par" "save")


  ;********************************************************************************************

(om::defmethod! ASX::seq-to-fbreakpt  ((liste list) 
                                      (bw number)   ;BANDWITH AS NUMBER
                                      (amplow number)
                                      (amphigh number)
                                      (fftsize integer)
                                      (scanspeed integer)    ; special feature for Ketty
                                      (scanrand number)
                                      (filename string)
                                      (saveflag string)
                                      ) 
  :initvals '(nil 20 -40.0 0.0 4096 10 0.0 "fbreakptpar" "save")
  :indoc '("list" "bandwith in Hz" "scaling amplitude low in dB" "scaling amplitude high in dB" "fftsize" "in milliseconds" "random 0.0 - 1.0"
           "filename" "saveflag")
  :menuins '((4 (("1024" 1024) ("2048" 2048) ("4096" 4096) ("8192" 8192) ("16384" 16384) ("32768" 32768)))
            (8 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"takes the output of a multiseq and converts it into a fbreakpt-filterfile"
  (let* ((aux1 nil)(aux2 nil)(aux3 nil)(final nil)
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
         (point2 (om::om* -1 (om::om/ bw 2)))
         (point3 (om::om/ bw 2))
         (point4 (om::om+  point3 bwminimum))
         (point1 (om::om- point2 bwminimum))
         
         (daten (om::sort.
                 (dolist (n liste (nreverse aux1))
                   (if (not (= 1 (length n)))                   
                     (if (= 1 (length (first n)))
                       (push (om::flat (om::list (second (om::flat n)) (om::om-round (om::MC->F (om::flat (first n))) 1)
                                                 (om::flat (third n)) 
                                                 (om::om-round (om::om-scale (om::flat (fourth n)) amplow amphigh 0 127) 1)
                                                 ))  aux1)
                       (loop for i from 0 to (- (length (first n)) 1) do
                             (push (om::list (second n) (om::om-round (om::MC->F (nth i (first n))) 1)
                                             (nth i (third n)) 
                                             (om::om-round (om::om-scale (nth i (fourth n)) amplow amphigh 0 127) 1) 
                                             ) aux1)))))
                 '< 'first))
         
          
         
(zeitpunkte (dolist (n daten (nreverse aux3))
                       (push (om::flat (om::list (first n) (om::om+ (first n)(third n)))) aux3)))
         
         ;(myserie (om::arithm-ser (first (first zeitpunkte)) (second (first (last zeitpunkte ))) scanspeed))   ; was 20 milliseconds
         
         (myserie (om::om-round 
                   (om::sort. (om::perturbation 
                               (om::arithm-ser (first (first zeitpunkte)) (second (first (last zeitpunkte ))) scanspeed) scanrand)) 3)) 
         
         
         (zeitscan (dolist (n zeitpunkte (nreverse aux2))
                     (push (om::om-round 
                            (om::om/ (ASX::listfilter 
                                      myserie
                                      (first n) (second n)) 1000.0) 3) aux2))))
    
    
    
    
    
    (setf daten (om::flat-once 
                 (mapcar #'(lambda (x y)
                             (om::mat-trans 
                              (om::list x
                                        (om::repeat-n (second (om::flat y)) (length x))
                                        (om::repeat-n (fourth (om::flat y)) (length x))))  
                             )
                         zeitscan daten)))
    
  

      
    (setf final (ASX::bandwith-for-fbreakpt+amp
                 (ASX::sort-freqs+amp
                  (ASX::groupe-freqs+amp
                   (ASX::round-freqs+amp daten)))
                 point1 point2 point3 point4 bw bwminimum 0.0))


  
    (setf final (mapcar #'(lambda (x)
                            (om::flat x))
                        final))
    
    (if (not (= (first (first final)) 0))
        (setf final (om::x-append 
                     '((0.0 0 2 100 -96 200 -96)) 
                     final)))
    
    (ASX::save-or-not final filename saveflag)

    
    )
  )
