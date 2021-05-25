;; getpass.scm

;; Copyright (C) 2020 Matthew R. Wette

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(use-modules (ffi termios))
(use-modules (system ffi-help-rt))
(use-modules (ice-9 rdelim))

(define* (get-passwd #:optional (prompt "password: "))
  (let* ((pt (open "/dev/tty" (logior O_RDWR O_NOCTTY)))
	 (fd (port->fdes pt))
	 (old (make-struct-termios))
	 (new (make-struct-termios))
	 (TCSANOW (ffi-termios-symbol-val 'TCSANOW)))
    (dynamic-wind
      (lambda ()
	(tcgetattr fd (pointer-to old))
	(tcgetattr fd (pointer-to new))
	(let* ((ECHO (ffi-termios-symbol-val 'ECHO))
	       (old_flag (fh-object-ref new 'c_lflag))
	       (new_flag (logand old_flag (lognot ECHO))))
	  (fh-object-set! new 'c_lflag new_flag)
	  (tcsetattr fd TCSANOW (pointer-to new))))
      (lambda ()
	(display "password: " pt) (force-output pt)
	(read-line pt))
      (lambda ()
	(tcsetattr fd TCSANOW (pointer-to old))))))


(let ((pw (get-passwd)))
  (simple-format #t "\nread password ~S\n" pw))

;; --- last line ---

