;;

(use-modules (ffi lightning))
(use-modules (system ffi-help-rt))

(define _jit (make-jit_state_t*))

(init_jit "ldemo.scm")
(fh-object-set! _jit '* (jit_new_state))
(define start (jit_note "ldemo.scm" 10))
(jit_prolog)
(define in (jit_arg))
(jit_getarg JIT_R1 in)

(jit_pushargi "generated %d bytes\n") ;; ???
(jit_ellipsis)

(jit_pushargr JIT_R1)
(jit_finishi (dynamic-func "printf" (dynamic-link)))
(jit_ret)
(jit_epilog)
(define end (jit_note "ldemo.scm" 22))

(define myFunction (jit_emit))

;; Call the generated code.
;;TODO: myFunction((char*)jit_address(end) - (char*)jit_address(start));
(jit_clear_state)
(jit_disassemble)
(jit_destroy_state)
(finish_jit)

;; --- last line ---
