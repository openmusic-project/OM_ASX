;****************************
;    OM-functions by Hans Tutschku 15/10/98 IRCAM

(in-package ASX)

;********************************************************************************************
(om::defmethod! ASX::groupe-fof-freqs ((liste list))
  :initvals '(nil)
  :indoc '("liste des temps freqs amplitude bw")
  :icon 999 
  :doc""
  
  (let ((aux0 nil) (aux1 nil) (mem -1))
    
    (dolist (n liste aux1)
      (if (not (=  (first n) mem))
        (progn ()
               (push (remove nil aux0) aux1)
               (setf aux0 nil)
               (push (om::list (second n) (third n) (fourth n)) aux0)
               (setf mem (first n)))
        (progn ()
               (push (om::list (second n) (third n) (fourth n)) aux0))))
    
    (om::mat-trans (list (om::remove-dup  (first (om::mat-trans liste)) 'equalp 1)
                         (remove nil (reverse (push aux0 aux1)))))
    )
  )

;********************************************************************************************

(om::defmethod! ASX::listfilter  ((liste list) 
                                 (low number)
                                 (high number)) 
  :initvals '(nil 0 1000)
  :indoc '("list" "low"  "high")
  :icon 999
  :doc"filters elements of a flat list and keeps just values between two limits"
  
  (let ((aux1 nil))
    (dolist (n liste (nreverse aux1))
      (if (and
           (>= n low)
           (<= n high))
        (push n aux1)))
    )
  )

;********************************************************************************************

(om::defmethod! ASX::seq-to-fifof  ((midicent list)   ; methode fŸr Bandwidth als number
                                    (onset list)
                                    (duration list)
                                    (vel list)
                                    (bw number)   ;BANDWITH AS NUMBER
                                    (amplow number)
                                    (amphigh number)
                                    (scanspeed integer)    ; special future/feature for Ketty (neo-post-moderne)
                                    (filename string) 
                                    ) 
  :initvals '(nil nil nil nil 0.01 -40.0 0.0 20 0.0 "fifof.par")
  :indoc '("midicent" "onsettimes" "durations" "velocity" "bandwith in Hz" "scaling amplitude low in dB" "scaling amplitude high in dB" "in milliseconds" "random 0.0 - 1.0"
           "filename" )
  :icon 999
  :doc"takes the output of a chordseq and converts it into a fomant-filterfile"
  (let* ((aux1 nil)(aux2 nil)(aux3 nil)(aux4 nil)(aux5 nil)(final nil)
         (scanspeed (om::om- scanspeed 1))
         (vel (om::om-round (om::om-scale vel amplow amphigh 0 127) 1))
         (onsetneu 
          (loop for i from 0 to (- (length midicent) 1) collect
                (om::repeat-n (nth i onset) (length (om::nth i midicent)))
                )) ; alle onsetzeiten werden so oft wiederholt, wie Noten in Akkorden sind
         
         (daten (om::flat (mapcar 'om::mat-trans (om::mat-trans (list onsetneu duration midicent vel))) 1))
         
         (daten2 (dolist (n daten (nreverse aux3))
                   (push (list (first n) (om::om+ (first n) (second n)) (third n) (fourth n)) aux3))) ; addiere die Dauern zum Onset (also noteon, noteoff)
         
         
         ; wŠhle alle Noten, deren Zeiten der jeweiligen Scanzeit enstspricht
         (daten3
          (loop for i from (om::om* scanspeed -1) to (second (first (last daten2))) collect
                (progn ()
                       (setf i (+ scanspeed i))
                       (setf aux2 nil)
                       (push (dolist (n daten2 (nreverse aux2))
                               ;(print (third n))
                               
                               (if (and (<= (first n) i) (>= (second n) i))
                                 (push (list (om::om-round (om::mc->f (third n))) (fourth n) (om::om-round (/ (* bw (om::mc->f (third n))) 1000.0))) aux2)
                                 
                                 )
                               ) aux4)
                       (push i aux5)
                       )
                ))
         (aux3 nil)
         (daten3 (dolist (n (reverse aux4) (nreverse aux3))
                   (push (om::sort. n '< 'first) aux3)))      ; sortiere die frequenzen aufsteigend
         (daten3 (om::mat-trans (list (om::om/ (reverse aux5) 1000.0) daten3)))
         
         (aux1 nil)
         
         (daten4 (dolist (n daten3 (nreverse aux1))
                   (if (= (length n) 2)
                     (push (om::flat (list (first n)  (length (second n)) 
                                           (second n))) aux1))))
         
         
         )
    ; (print daten4)
    daten4
    )
  )

;********************************************************************************************

(om::defmethod! ASX::seq-to-fifof  ((midicent list)   ; methode fŸr Bandwidth als bpf
                                    (onset list)
                                    (duration list)
                                    (vel list)
                                    (bw om::bpf)   ;BANDWITH AS LIST
                                    (amplow number)
                                    (amphigh number)
                                    (scanspeed integer)    ; special future/feature for Ketty (neo-post-moderne)
                                    (filename string) 
                                    ) 
  :initvals '(nil nil nil nil 0.01 -40.0 0.0 20 0.0 "fifof.par")
  :indoc '("midicent" "onsettimes" "durations" "velocity" "bandwith in Hz" "scaling amplitude low in dB" "scaling amplitude high in dB" "in milliseconds" "random 0.0 - 1.0"
           "filename" )
  :icon 999
  :doc"takes the output of a chordseq and converts it into a fomant-filterfile"
  (let* ((aux1 nil)(aux2 nil)(aux3 nil)(aux4 nil)(aux5 nil)(final nil)
         (scanspeed (om::om- scanspeed 1))
         (vel (om::om-round (om::om-scale vel amplow amphigh 0 127) 1))
         (onsetneu 
          (loop for i from 0 to (- (length midicent) 1) collect
                (om::repeat-n (nth i onset) (length (om::nth i midicent)))
                )) ; alle onsetzeiten werden so oft wiederholt, wie Noten in Akkorden sind
         
         (daten (om::flat (mapcar 'om::mat-trans (om::mat-trans (list onsetneu duration midicent vel))) 1))
         
         (daten2 (dolist (n daten (nreverse aux3))
                   (push (list (first n) (om::om+ (first n) (second n)) (third n) (fourth n)) aux3))) ; addiere die Dauern zum Onset (also noteon, noteoff)
         
         
         ; wŠhle alle Noten, deren Zeiten der jeweiligen Scanzeit enstspricht
         (daten3
          (loop for i from (om::om* scanspeed -1) to (second (first (last daten2))) collect
                (progn ()
                       (setf i (+ scanspeed i))
                       (push i aux5)
                       )
                ))
         
         ; aux5 enthŠlt die Anfangszeiten
         (bpfvalues   (om::om/ (om::bpf-sample bw 1 99 (length aux5)) 10000.0))   ;sample die bpf mit der nštigen Anzahl von Zeitpunkten
  

          (daten3
          (loop for i from 0 to (- (length aux5) 1) collect
                (progn ()
                       
                       (setf aux2 nil)
                       (push (dolist (n daten2 (nreverse aux2))
                               ;(print aux5)
                               
                               
                               (if (and (<= (first n) (om::posn-match aux5 i) ) (>= (second n) (om::posn-match aux5 i)))
                                 (push (list (om::om-round (om::mc->f (third n)) 2) (fourth n) (om::om-round (* (om::posn-match bpfvalues i) (om::mc->f (third n))) 2)) aux2)
                                 ;(push (list (om::om-round (om::mc->f (third n)) 2) (fourth n) (om::om-round (* 1 (om::mc->f (third n))) 2)) aux2)
                                 )
                               
                               ) aux4)
                       
                       )
                ))
         
         
         
         (aux3 nil)
         (daten3 (dolist (n aux4 (nreverse aux3))
                   (push (om::sort. n '< 'first) aux3)))      ; sortiere die frequenzen aufsteigend
         (daten3 (om::mat-trans (list (om::om/ (reverse aux5) 1000.0) daten3)))
         
         (aux1 nil)
         
         
         (daten4 (dolist (n daten3 (nreverse aux1))
                   (if (= (length n) 2)
                     (push (om::flat (list (first n)  (length (second n)) 
                                           (second n))) aux1))))
         
         )
;(print (list (length aux5)(length bpfvalues)))
    daten4
    )
  )
 

;********************************************************************************************

(om::defmethod! ASX::seq-to-fifof-bpf  ((liste list) 
                                       (bw OM::BPF)   ;BANDWITH AS BPF
                                       (lowscale number)                                     
                                       (highscale number)
                                       (amplow number)
                                       (amphigh number)
                                       (filename string)
                                       (saveflag string)
                                       ) 
  :initvals '(nil nil 0.01 20.0 -40.0 0.0 "fifof.par" "save")
  :indoc '("list" "bandwith as bpf" "low limit bw" "high limit bw" 
           "scaling amplitude low in dB" "scaling amplitude high in dB" "filename" "saveflag")
  :menuins '((7 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"takes the output of a multiseq and converts it into a formant-filterfile"
  
  (let* ((aux1 nil)(aux2 nil)(aux3 nil)(aux4 nil)(ergebnis nil)(final nil)
         (daten (om::sort.
                 (dolist (n liste (nreverse aux1))
                   (if (not (= 1 (length n)))
                     (if (= 1 (length (first n)))
                       (push (om::flat (om::list (second (om::flat n)) (om::om-round (om::MC->F (om::flat (first n))) 1)
                                                 (om::flat (third n)) 
                                                 (om::om-round (om::om-scale (om::flat (fourth n)) amplow amphigh 0 127) 1)))  aux1)
                       (loop for i from 0 to (- (length (first n)) 1) do
                             (push (om::list (second n) (om::om-round (om::MC->F (nth i (first n))) 1)
                                             (nth i (third n)) 
                                             (om::om-round (om::om-scale (nth i (fourth n)) amplow amphigh 0 127) 1)) aux1)))))
                 '< 'first))
         
         (zeitpunkte (dolist (n daten (nreverse aux3))
                       (push (om::flat (om::list (first n) (om::om+ (first n)(third n)))) aux3)))
         (anfang (first (first zeitpunkte)))
         (ende (second (first (last zeitpunkte ))))
         (myserie (om::arithm-ser anfang ende 20))
         
         (zeitscan (dolist (n zeitpunkte (nreverse aux2))
                     (push (om::om-round 
                            (om::om/ (ASX::listfilter 
                                      myserie
                                      (first n) (second n)) 1000.0) 3) aux2))))
    
    
    
    (setf daten (om::sort.
                 (om::flat-once 
                  (mapcar #'(lambda (x y)
                              (om::mat-trans 
                               (om::list x
                                         (om::repeat-n (second (om::flat y)) (length x))
                                         (om::repeat-n (fourth (om::flat y)) (length x))
                                         (om::repeat-n (fifth (om::flat y)) (length x))))  
                              )
                          zeitscan daten))
                 '< 'first))
    
    (setf daten (ASX::groupe-fof-freqs
                 (dolist (n daten (nreverse ergebnis))
                   (push (om::flat
                          (om::list
                           n
                           (om::om-round
                            (ASX::f-transfer bw (/ (* 100000 (first n)) ende))
                            2))) ergebnis))))
    
    
    (setf daten (dolist (x daten (nreverse aux4))
                  (if (= 1 (length (second x)))
                    (push x aux4)
                    (push (om::list (first x) 
                                    (om::sort. (second x) '< 'first)) aux4))))
    
    (setf final(mapcar #'(lambda (x)
                           (om::flat x))
                       (mapcar #'(lambda (x)
                                   (om::list (first x) (/ (- (length (om::flat x)) 1) 3) (rest x)))
                               daten)))
    
    (ASX::save-or-not final filename saveflag)    
    )
  )

;********************************************************************************************

(om::defmethod! ASX::fifof-rand  ((soundlength number) 
                                 (steps integer)
                                 (minpoints integer)
                                 (maxpoints integer)
                                 (minfreq number)
                                 (maxfreq number)
                                 (minamp number)
                                 (maxamp number)
                                 (minbw number)
                                 (maxbw number)
                                 (filename string)
                                 (saveflag string)) 
  :initvals '(3.4 10 5 10 30 20000 -50 0.0 14 200 "fifof.par" "save")
  :indoc '("soundlength" "steps" "min number of freqpoints" "max number of freqpoints" "low limit freq" "high limit freq"
           "in dB" "in dB" "in Hz" "in Hz" "filename" "saveflag")
  :menuins '((11 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"random evolution for formantfilter"
  
  (let* ((aux nil)(numboffofs nil)(daten nil)
         (timeline (ASX::timeline-irr soundlength steps)))
    
    (setf timeline (om::flat
                    (om::mat-trans
                     (om::list  (butlast timeline)    
                                (om::rest (om::om-round (om::om- timeline 0.001) 3))))))
    
    (setf daten (dolist (n timeline (nreverse aux))
                  (progn ()
                         (setf numboffofs (om::om-random minpoints maxpoints))
                         (push (om::list n numboffofs
                                         (om::sort.
                                          (loop for i from 1 to numboffofs
                                                collect
                                                (om::x-append
                                                 (om::om-random minfreq maxfreq)
                                                 (om::om-round (om::om-random minamp maxamp)1)
                                                 (om::om-random minbw maxbw)))
                                          '< 'first))
                               aux))))
    
    
    (ASX::save-or-not (om::mapcar 'om::flat daten) filename saveflag)
    )
  )

;(ASX::fifof-rand 3.4 5 5 10 30 20000 -50 0.0 14 200 "fbreakpt.par" "don't save")

;********************************************************************************************

(om::defmethod! ASX::fifof-rand2  ((soundlength number) 
                                 (steps integer)
                                 (minpoints integer)
                                 (maxpoints integer)
                                 (minfreq number)
                                 (maxfreq number)
                                 (minamp number)
                                 (maxamp number)
                                 (minbw number)
                                 (maxbw number)
                                 (filename string)
                                 (saveflag string)) 
  :initvals '(3.4 10 5 10 30 20000 -50 0.0 14 200 "fifof.par" "save")
  :indoc '("soundlength" "steps" "min number of freqpoints" "max number of freqpoints" "low limit freq" "high limit freq"
           "in dB" "in dB" "in Hz" "in Hz" "filename" "saveflag")
  :menuins '((11 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"random evolution for formantfilter"
  
  (let* ((aux nil)(numboffofs nil)(daten nil)
         (timeline (ASX::timeline-irr soundlength steps)))
    
    (setf timeline (om::flat
                    (om::mat-trans
                     (om::list  (butlast timeline)    
                                (om::rest (om::om-round (om::om- timeline 0.001) 3))))))
    
    (setf daten (dolist (n timeline (nreverse aux))
                  (progn ()
                         (setf numboffofs (om::om-random minpoints maxpoints))
                         (push (om::flat (om::x-append n numboffofs
                                         (om::sort.
                                          (loop for i from 1 to numboffofs
                                                collect
                                                (om::x-append
                                                 (om::om-random minfreq maxfreq)
                                                 (om::om-round (om::om-random minamp maxamp))
                                                 (om::om-random minbw maxbw)))
                                          '< 'first)))
                               aux))))
    ;(print daten)
    
    (ASX::write-lists3 daten filename)
    )
  )

;(ASX::fifof-rand 3.4 5 5 10 30 20000 -50 0.0 14 200 "fbreakpt.par" "don't save")


;********************************************************************************************

(om::defmethod! ASX::fundamental-fof  ((self om::sdiffile)                                      
                                      (rang list)
                                      (bw number)
                                      (snd-dur number)
                                      (scaleswitch string)
                                      (fftsize integer)
                                      (filename string) 
                                      ) 
  :initvals '(nil '(1 3 5) 30 5 "orig" 4096 "fof.par")
  :indoc '("fundamental" "harmonic rang of bands" "bandwith" "sound duration" "scale or not" "fft size" "filename")
  :menuins '((4 (("orig" "orig") ("scaled" "scaled"))))
  :icon 999
  :doc"this patch is creating a parameterfile for Audiosculpt formant-filter from a fundamental analysis"
  (let* ((ergebnis nil)(final nil) (aux1 nil) (aux3 nil)
         (fundlist (ASX::sdif-fundextract self))
         (timeline (first fundlist))
         (freqs (second fundlist))
         (fund-dur (first (last timeline)))
         (dur-ratio (om::om/ snd-dur fund-dur))
         (dur-multiplyer (if (string= "orig" scaleswitch) 1 dur-ratio))
         (timeline (om::om-round (om::om* timeline dur-multiplyer) 3))
         
         (aux2 (dolist (n freqs (nreverse ergebnis)) 
                 (push  (ASX::fund-bands n rang) ergebnis))) ;korrektur, da hohe freq-bŠnder zu tief
         )
    
    
    
    (setf ergebnis (dolist (n aux2 (nreverse final))
                     (push 
                      (om::x-append 
                       (length n)
                       (loop for i from 0 to (- (length n) 1) collect
                             (om::list (nth i n) 0 bw)))
                      final)))
    
    
    
    (setf final (mapcar #'(lambda (x y)
                            (om::flat (om::x-append x y)))
                        timeline ergebnis))
    
    (if (not (= (first (first final)) 0))
      (progn()
            (setf aux1 (second (first final)))                 ;wieviele Formanten?
            (setf aux2 (om::arithm-ser 100 (* 100 aux1) 100))  ; serie von frequenzen
            (setf aux2 (mapcar #'(lambda (x)                   ; Klammern um jeden Wert
                                   (om::list x)  
                                   )
                               aux2))
            
            
            (setf aux3 (ASX::repeat '(-96 1) aux1))             ;Liste von Amp und bw
            (setf aux3 (om::list (om::x-append 0.0 aux1 (om::flat (om::mat-trans (om::list aux2 aux3))))))
            
            
            (setf final (om::x-append 
                         aux3 
                         final))))
    
    
    ;(ASX::save-or-not final filename)   
    
     )
  )

;********************************************************************************************

(om::defmethod! ASX::fund-fifof-rand  ((fundamental list)                                      
                                      (numb integer)
                                      (bw number)
                                      (filename string)
                                      (saveflag string)
                                      ) 
  :initvals '((0.04 174.56 0.13 174.09 0.22 169.94 0.31 168.14) 5 30 "fifof.par" "save")
  :indoc '("fundamental" "number of formants" "bandwith" "filename" "saveflag")
  :menuins '((4 (("save" "save") ("don't save" "don't save"))))
  :icon 999
  :doc"this patch is creating a parameterfile for Audiosculpt formant-filter from a fundamental analysis"
  (let* ((ergebnis nil)(final nil)
         (number (om::om/ (om::length fundamental) 2))
         (aux (om::mat-trans (om::list-explode fundamental number)))
         (timeline (om::om-round (first aux) 3))
         (freqs (om::om-round(second aux)))
         
         (rang (loop for i from 0 to number collect
                     (om::sort.
                      (om::remove-dup 
                       (loop for i from 1 to (om::om-random 1 numb) collect
                             (om::om-random 1 20))
                       'equal 1))))
         
         (aux2 (mapcar #'(lambda (x y)
                           (ASX::fund-bands x y)) freqs rang)))
    
    (setf ergebnis (dolist (n aux2 (nreverse final))
                     (push 
                      (om::x-append 
                       (length n)
                       (loop for i from 0 to (- (length n) 1) collect
                             (om::list (nth i n) 0 bw)))
                      final)))
    
    (setf final (mapcar #'(lambda (x y)
                            (om::flat (om::x-append x y)))
                        timeline ergebnis))
    
    (ASX::save-or-not final filename saveflag)

    
    )
  )



