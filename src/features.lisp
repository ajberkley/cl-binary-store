;; To enable debugging execute the below line and recompile everything
;; (pushnew :dribble-csf *features*)
;; (pushnew :debug-csf *features*)
;; (pushnew :info-csf *features*)
;; To disable debugging execute the below and recompile everything which can be done
;; by saving this file and quickload'ing the package again
;; (setf *features*
;;       (remove-if (lambda (x) (member x '(:dribble-csf :debug-csf :info-csf))) *features*))
