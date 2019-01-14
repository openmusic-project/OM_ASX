(in-package :ASX)

; many of those functions have been first developed by Benoit, then modified by Hans

;;; methodes modifiées dans la librairie omas :
;;; write-lists
;;; save-or-not
;;; stretch-dyn-random
   

(defun path2unixpath (path) path)


(om::defmethod! ASX::svp-command (cmd)
  (let ((cmdline (om::string+ "cd '" ASX::*svp_unixpath* "'; " cmd)))
    ;(print cmdline)
    (om::shell cmdline)
    ))


(om::defmethod! ASX::snd-duration ((self om::sound))
  :icon 999
  (/ (om::get-obj-dur self) 1000.0))


;;; *************************** TIME STRETCH **************************************************************

(om::defmethod! ASX::t-stretch (self begin end fftsize windowsize normswitch normfact transient-thresh out-name param)
 
  (let* ((out (if out-name out-name "snd-out"))
         (outpath (om::unique-pathname (pathname-directory ASX::*om-as-result-sound-folder*) out ".aiff"))
         
         
         (cmd (if (string= "no" normswitch) 
                
                (format nil "./supervp -t -v -Z -U -Afft -S'~A' -D'~D' -B~D -E~D -M~D -N~D -nn -Wblackman -Jblackman -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize transient-thresh
                        (path2unixpath outpath))
                (format nil "./supervp -t -v -Z -U -Afft -S'~A' -D'~D' -B~D -E~D -M~D -N~D -norm ~D -Wblackman -Jblackman -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize normfact transient-thresh
                        (path2unixpath outpath))
                
                )
         ))
    
    (print (om::string+ "Unixpath " ASX::*svp_unixpath*))
    (svp-command cmd)
    (print (list cmd (mac-namestring outpath)))
    (mac-namestring outpath)
    )
  )



(om::defmethod! ASX::time-stretch ((self om::sound) 
                                    (begin number) 
                                    (end number) 
                                    (fftsize number) 
                                    (windowsize number) 
                                    (normswitch string) 
                                    (normfact number) 
                                    (transient-thresh number) 
                                    (out-name string) 
                                    (param pathname))
  :icon 999
  :initvals '(nil 0 1 4096 2000 "no" 1 1.4 "test.aiff" nil)
  :indoc '("soundfile" "begin" "end" "fftsize" "windowsize" "normalization" "normalization factor" 
           "transient threshold" "name for resulting soundfile" "value or path for parameterfile" )
  :menuins '((5 (("no" "no") ("yes" "yes"))))
  (ASX::t-stretch self begin end fftsize windowsize normswitch normfact transient-thresh out-name param))

(om::defmethod! ASX::time-stretch ((self om::sound) 
                                    (begin number) 
                                    (end number) 
                                    (fftsize number) 
                                    (windowsize number) 
                                    (normswitch string) 
                                    (normfact number) 
                                    (transient-thresh number) 
                                    (out-name string) 
                                    (param number))
  :icon 999
  :initvals '(nil 0 1 4096 2000 "no" 1 1.4 "test.aiff" nil)
  :indoc '("soundfile" "begin" "end" "fftsize" "windowsize" "normalization" "normalization factor" 
           "transient threshold" "name for resulting soundfile" "value or path for parameterfile" )
  :menuins '((5 (("no" "no") ("yes" "yes"))))
  (ASX::t-stretch self begin end fftsize windowsize normswitch normfact transient-thresh out-name param))




;;; *************************** TRANSPOSITION **************************************************************


(om::defmethod! ASX::f-shift (self begin end fftsize windowsize normswitch normfact transient-thresh out-name param)
  
  (let* ((out (if out-name out-name "snd-out"))
         (outpath (om::unique-pathname (pathname-directory ASX::*om-as-result-sound-folder*) out ".aiff"))
         
         
         (cmd (if (string= "no" normswitch) 
                (format nil "./supervp -t -v -Z -U -Afft 30 -S'~A' -transke '~D' -B~D -E~D -M~D -N~D -nn -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize transient-thresh
                        (path2unixpath outpath))
                (format nil "./supervp -t -v -Z -U -Afft 30 -S'~A' -transke '~D' -B~D -E~D -M~D -N~D -norm ~D -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize normfact transient-thresh
                        (path2unixpath outpath))
                
                )))
    
    (print (om::string+ "Unixpath " ASX::*svp_unixpath*))
    (svp-command cmd)
    (print (list cmd (mac-namestring outpath)))
    (mac-namestring outpath)
    )
  )

(om::defmethod! ASX::transposition ((self om::sound) 
                                    (begin number) 
                                    (end number) 
                                    (fftsize number) 
                                    (windowsize number) 
                                    (normswitch string) 
                                    (normfact number) 
                                    (transient-thresh number) 
                                    (out-name string) 
                                    (param pathname))
  :icon 999
  :initvals '(nil 0 1 4096 2000 "no" 1 1.4 "test.aiff" nil)
  :indoc '("soundfile" "begin" "end" "fftsize" "windowsize" "normalization" "normalization factor" 
           "transient threshold" "name for resulting soundfile" "value or path for parameterfile" )
  :menuins '((5 (("no" "no") ("yes" "yes"))))
  (ASX::f-shift self begin end fftsize windowsize normswitch normfact transient-thresh out-name param))

(om::defmethod! ASX::transposition ((self om::sound) 
                                    (begin number) 
                                    (end number) 
                                    (fftsize number) 
                                    (windowsize number) 
                                    (normswitch string) 
                                    (normfact number) 
                                    (transient-thresh number) 
                                    (out-name string) 
                                    (param number))
  :icon 999
  :initvals '(nil 0 1 4096 2000 "no" 1 1.4 "test.aiff" nil)
  :indoc '("soundfile" "begin" "end" "fftsize" "windowsize" "normalization" "normalization factor" 
           "transient threshold" "name for resulting soundfile" "value or path for parameterfile" )
  :menuins '((5 (("no" "no") ("yes" "yes"))))
  (ASX::f-shift self begin end fftsize windowsize normswitch normfact transient-thresh out-name param)

)




;;; *************************** CLIPPING **************************************************************

(om::defmethod! ASX::clipping-filter (self begin end fftsize windowsize normswitch normfact transient-thresh out-name param)
  
  (let* ((out (if out-name out-name "snd-out"))
         (outpath (om::unique-pathname (pathname-directory ASX::*om-as-result-sound-folder*) out ".aiff"))
         
         (cmd (if (string= "no" normswitch) 
                (format nil "./supervp -t -v -Z -U -Afft -S'~A' -Fclip '~D' -B~D -E~D -M~D -N~D -nn -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize transient-thresh
                        (path2unixpath outpath))
                (format nil "./supervp -t -v -Z -U -Afft -S'~A' -Fclip '~D' -B~D -E~D -M~D -N~D -norm ~D -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize normfact transient-thresh
                        (path2unixpath outpath))
                ))
         )
    
    (print (om::string+ "Unixpath " ASX::*svp_unixpath*))
    (svp-command cmd)
    (print (list cmd (mac-namestring outpath)))
    (mac-namestring outpath)
    )
  )



(om::defmethod! ASX::clipping ((self om::sound) 
                             (begin number) 
                             (end number) 
                             (fftsize number) 
                             (windowsize number) 
                             (normswitch string) 
                             (normfact number) 
                             (transient-thresh number) 
                             (out-name string) 
                             (param pathname))
  :icon 999
  :initvals '(nil 0 1 4096 2000 "no" 1 1.4 "test.aiff" nil)
  :indoc '("soundfile" "begin" "end" "fftsize" "windowsize" "normalization" "normalization factor" 
           "transient threshold" "name for resulting soundfile" "path for parameterfile" )
  :menuins '((5 (("no" "no") ("yes" "yes"))))
  (ASX::clipping-filter self begin end fftsize windowsize normswitch normfact transient-thresh out-name param))



;;; ***************************BAND FILTER**************************************************************

(om::defmethod! ASX::band-filter (self begin end fftsize windowsize normswitch normfact transient-thresh out-name param)
  
  (let* ((out (if out-name out-name "snd-out"))
         (outpath (om::unique-pathname (pathname-directory ASX::*om-as-result-sound-folder*) out ".aiff"))
         
         
         (cmd (if (string= "no" normswitch) 
                (format nil "./supervp -t -v -Z -U -Afft -S'~A' -Fbande-noex '~D' -B~D -E~D -M~D -N~D -nn -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize transient-thresh
                        (path2unixpath outpath))
                (format nil "./supervp -t -v -Z -U -Afft -S'~A' -Fbande-noex '~D' -B~D -E~D -M~D -N~D -norm ~D -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize normfact transient-thresh
                        (path2unixpath outpath))
                ))
         )
    (print (om::string+ "Unixpath " ASX::*svp_unixpath*))
    (svp-command cmd)
    (print (list cmd (mac-namestring outpath)))
    (mac-namestring outpath)
    )
  )

(om::defmethod! ASX::Fbande ((self om::sound) 
                             (begin number) 
                             (end number) 
                             (fftsize number) 
                             (windowsize number) 
                             (normswitch string) 
                             (normfact number) 
                             (transient-thresh number) 
                             (out-name string) 
                             (param pathname))
  :icon 999
  :initvals '(nil 0 1 4096 2000 "no" 1 1.4 "test.aiff" nil)
  :indoc '("soundfile" "begin" "end" "fftsize" "windowsize" "normalization" "normalization factor" 
           "transient threshold" "name for resulting soundfile" "path for parameterfile" )
  :menuins '((5 (("no" "no") ("yes" "yes"))))
  (ASX::band-filter self begin end fftsize windowsize normswitch normfact transient-thresh out-name param))




;;; ************************** BREAKPOINT FILTER *************************************************************

(om::defmethod! ASX::brkpt-filter (self begin end fftsize windowsize normswitch normfact transient-thresh out-name param)
  :icon 999  
  
  (let* ((out (if out-name out-name "snd-out"))
         (outpath (om::unique-pathname (pathname-directory ASX::*om-as-result-sound-folder*) out ".aiff"))
         
         (cmd (if (string= "no" normswitch) 
                (format nil "./supervp -t -v -Z -U -Afft -S'~A' -Fbreakpt '~D' -B~D -E~D -M~D -N~D -nn -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize transient-thresh
                        (path2unixpath outpath))
                (format nil "./supervp -t -v -Z -U -Afft -S'~A' -Fbreakpt '~D' -B~D -E~D -M~D -N~D -norm ~D -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize normfact transient-thresh
                        (path2unixpath outpath))
                ))
         )
    
    (print (om::string+ "Unixpath " ASX::*svp_unixpath*))
    (svp-command cmd)
    (print (list cmd (mac-namestring outpath)))
    (mac-namestring outpath)
    )
  )


(om::defmethod! ASX::Fbreakpt ((self om::sound) 
                               (begin number) 
                               (end number) 
                               (fftsize number) 
                               (windowsize number) 
                               (normswitch string) 
                               (normfact number) 
                               (transient-thresh number) 
                               (out-name string) 
                               (param pathname))
  :icon 999
  :initvals '(nil 0 1 4096 2000 "no" 1 1.4 "test.aiff" nil)
  :indoc '("soundfile" "begin" "end" "fftsize" "windowsize" "normalization" "normalization factor" 
           "transient threshold" "name for resulting soundfile" "path for parameterfile" )
  :menuins '((5 (("no" "no") ("yes" "yes"))))
  (ASX::brkpt-filter self begin end fftsize windowsize normswitch normfact transient-thresh out-name param))


;;; *************************** FOF FILTER **************************************************************

(om::defmethod! ASX::fof-filter (self begin end fftsize windowsize normswitch normfact transient-thresh out-name param)
  
  (let* ((out (if out-name out-name "snd-out"))
         (outpath (om::unique-pathname (pathname-directory ASX::*om-as-result-sound-folder*) out ".aiff"))
         
         (cmd (if (string= "no" normswitch) 
                (format nil "./supervp -t -v -Z -U -Afft -S'~A' -Ffof-noex '~D' -B~D -E~D -M~D -N~D -nn -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize transient-thresh
                        (path2unixpath outpath))
                (format nil "./supervp -t -v -Z -U -Afft -S'~A' -Ffof-noex '~D' -B~D -E~D -M~D -N~D -norm ~D -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize normfact transient-thresh
                        (path2unixpath outpath))
                ))
         
         )
    (print (om::string+ "Unixpath " ASX::*svp_unixpath*))
    (svp-command cmd)
    (print (list cmd (mac-namestring outpath)))
    (mac-namestring outpath)
    )
  )



(om::defmethod! ASX::FOF ((self om::sound) 
                          (begin number) 
                          (end number) 
                          (fftsize number) 
                          (windowsize number) 
                          (normswitch string) 
                          (normfact number) 
                          (transient-thresh number) 
                          (out-name string) 
                          (param pathname))
  :icon 999
  :initvals '(nil 0 1 4096 2000 "no" 1 1.4 "test.aiff" nil)
  :indoc '("soundfile" "begin" "end" "fftsize" "windowsize" "normalization" "normalization factor" 
           "transient threshold" "name for resulting soundfile" "path for parameterfile" )
  :menuins '((5 (("no" "no") ("yes" "yes"))))
  (ASX::fof-filter self begin end fftsize windowsize normswitch normfact transient-thresh out-name param))

;;; *************************** FIFOF FILTER **************************************************************

(om::defmethod! ASX::fifof-filter (self begin end fftsize windowsize normswitch normfact transient-thresh out-name param)
  
  (let* ((out (if out-name out-name "snd-out"))
         (outpath (om::unique-pathname (pathname-directory ASX::*om-as-result-sound-folder*) out ".aiff"))
         (cmd (if (string= "no" normswitch) 
                (format nil "./supervp -t -Z -U -Afft -S'~A' -Ffifof '~D' -B~D -E~D -M~D -N~D -nn -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end windowsize fftsize transient-thresh
                        (path2unixpath outpath))
                (format nil "./supervp -t -Z -U -Afft -S'~A' -Ffifof '~D' -B~D -E~D -norm ~D -M~D -N~D -Wblackman  -Jblackman  -P1 -td_thresh ~D -td_G 2.5 -td_band 0.0,22050.0 -td_nument 10.0 -td_ampfac 1.5 -FCombineMul '~D'" 
                        (path2unixpath (om::sound-path self))
                        (if (pathnamep param) (path2unixpath param) param)
                        begin end normfact windowsize fftsize transient-thresh
                        (path2unixpath outpath))
                ))         
         )
    (print (om::string+ "Unixpath " ASX::*svp_unixpath*))
    (svp-command cmd)
    (print (list cmd (mac-namestring outpath)))
    (mac-namestring outpath)
    )
  )

(om::defmethod! ASX::FIFOF ((self om::sound) 
                            (begin number) 
                            (end number) 
                            (fftsize number) 
                            (windowsize number) 
                            (normswitch string) 
                            (normfact number) 
                            (transient-thresh number) 
                            (out-name string) 
                            (param pathname))
  :icon 999
  :initvals '(nil 0 1 4096 2000 "no" 1 1.4 "test.aiff" nil)
  :indoc '("soundfile" "begin" "end" "fftsize" "windowsize" "normalization" "normalization factor" 
           "transient threshold" "name for resulting soundfile" "path for parameterfile" )
  :menuins '((5 (("no" "no") ("yes" "yes"))))
  (ASX::fifof-filter self begin end fftsize windowsize normswitch normfact transient-thresh out-name param))

;;; **************************  CROSS SYNTHESIS *************************************************************
 
(om::defmethod! ASX::c-synth ((self om::sound) (self2 om::sound) param-file out-name fftsize windowsize)
  
  (let* ((out (if out-name out-name "snd-out"))
         (outpath (om::unique-pathname (pathname-directory *outfilesfolder*) out ".aiff")))
    (svp-command (format nil "./supervp -t -v -Z -a -A -S~D -s~D -Gcross ~D -Jhanning -M~D -N~D -Whanning -I2000 -m~D -n~D -whanning ~D" 
                         (path2unixpath (om::sound-path self))
                         (path2unixpath (om::sound-path self2))
                         (path2unixpath param-file)
                         windowsize fftsize windowsize fftsize 
                         (path2unixpath outpath)))
    (mac-namestring outpath)))


(om::defmethod! ASX::cross-synth ((self om::sound) (self2 om::sound) (param pathname) &optional out-name (fftsize 4096) (windowsize 4000))
  :icon 999  
  (ASX::c-synth self self2 param out-name windowsize fftsize))