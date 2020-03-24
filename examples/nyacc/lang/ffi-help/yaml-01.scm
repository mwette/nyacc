;; demo1.scm - yaml demo

(add-to-load-path (getcwd))

(use-modules (yaml))
(use-modules (ice-9 pretty-print))

(let ((yaml (read-yaml-file "demo1.yml")))
  (pretty-print yaml))

;; --- last line ---
