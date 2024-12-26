;; To enable debugging execute the below line and recompile everything
;; (pushnew :dribble-clb *features*)
;; (pushnew :debug-clb *features*)
;; (pushnew :info-clb *features*)
;; To disable debugging execute the below and recompile everything which can be done
;; by saving this file and quickload'ing the package again
;; (setf *features* (remove-if (lambda (x) (member x '(:dribble-clb :debug-clb :info-clb))) *features*))
