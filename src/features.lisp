;; To enable debugging execute the below line and recompile everything
;; (pushnew :dribble-cbs *features*)
;; (pushnew :debug-cbs *features*)
;; (pushnew :info-cbs *features*)
;; To disable debugging execute the below and recompile everything which can be done
;; by saving this file and quickload'ing the package again
;; (setf *features* (remove-if (lambda (x) (member x '(:dribble-cbs :debug-cbs :info-cbs))) *features*))
