;; libelf-01.scm
;;    https://bitbucket.org/developer2developer/elfcodegenerator/
;;                downloads/libelf-by-example.pdf

(use-modules (ffi libelf))
(use-modules (system ffi-help-rt))
;;(use-modules (bytestructures guile))
(use-modules ((system foreign) #:prefix ffi:))

(define (sf fmt . args) (apply simple-format #t fmt args))

(define go-port (open-input-file "/usr/local/lib/guile/2.2/ccache/texinfo.go"))
(define go-fd (fileno go-port))

(define elf (elf_begin go-fd 'ELF_C_READ NULL))

(define kind (elf_kind elf))
(sf "kind = ~S\n" kind)

(elf_end elf)

;; --- last line ---
