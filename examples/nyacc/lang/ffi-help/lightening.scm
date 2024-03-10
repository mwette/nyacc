;; generated with `guild compile-ffi ffi/lightening.ffi'

(define-module (lightening)
  #:use-module (system ffi-help-rt)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile))

(define ffi-lightening-llibs (delay (list)))


;; typedef int64_t jit_word_t;

;; typedef uint64_t jit_uword_t;

;; typedef float jit_float32_t;
(define-public jit_float32_t-desc float)

;; typedef double jit_float64_t;
(define-public jit_float64_t-desc double)

;; typedef void *jit_pointer_t;
(define-public jit_pointer_t-desc (fh:pointer 'void))
(define-fh-pointer-type jit_pointer_t jit_pointer_t-desc jit_pointer_t? 
 make-jit_pointer_t)
(export jit_pointer_t jit_pointer_t? make-jit_pointer_t)

;; typedef int jit_bool_t;
(define-public jit_bool_t-desc int)

;; typedef void *jit_addr_t;
(define-public jit_addr_t-desc (fh:pointer 'void))
(define-fh-pointer-type jit_addr_t jit_addr_t-desc jit_addr_t? make-jit_addr_t
 )
(export jit_addr_t jit_addr_t? make-jit_addr_t)

;; typedef long jit_off_t;
(define-public jit_off_t-desc long)

;; typedef intptr_t jit_imm_t;

;; typedef uintptr_t jit_uimm_t;

;; typedef struct jit_gpr {
;;   uint8_t regno;
;; } jit_gpr_t;
(define-public jit_gpr_t-desc
  (bs:struct (list `(regno ,uint8))))
(define-fh-compound-type jit_gpr_t jit_gpr_t-desc jit_gpr_t? make-jit_gpr_t)
(export jit_gpr_t jit_gpr_t? make-jit_gpr_t)
(define-public jit_gpr_t*-desc
  (fh:pointer jit_gpr_t-desc))
(define-fh-pointer-type jit_gpr_t* jit_gpr_t*-desc jit_gpr_t*? make-jit_gpr_t*
 )
(export jit_gpr_t* jit_gpr_t*? make-jit_gpr_t*)
(fh-ref<=>deref!
  jit_gpr_t*
  make-jit_gpr_t*
  jit_gpr_t
  make-jit_gpr_t)
(define-public struct-jit_gpr-desc
  jit_gpr_t-desc)
(define-fh-compound-type struct-jit_gpr struct-jit_gpr-desc struct-jit_gpr? 
 make-struct-jit_gpr)
(export struct-jit_gpr struct-jit_gpr? make-struct-jit_gpr)
(define-public struct-jit_gpr*-desc
  jit_gpr_t*-desc)
(define-fh-pointer-type struct-jit_gpr* struct-jit_gpr*-desc struct-jit_gpr*? 
 make-struct-jit_gpr*)
(export struct-jit_gpr* struct-jit_gpr*? make-struct-jit_gpr*)
(fh-ref<=>deref!
  struct-jit_gpr*
  make-struct-jit_gpr*
  struct-jit_gpr
  make-struct-jit_gpr)

;; typedef struct jit_fpr {
;;   uint8_t regno;
;; } jit_fpr_t;
(define-public jit_fpr_t-desc
  (bs:struct (list `(regno ,uint8))))
(define-fh-compound-type jit_fpr_t jit_fpr_t-desc jit_fpr_t? make-jit_fpr_t)
(export jit_fpr_t jit_fpr_t? make-jit_fpr_t)
(define-public jit_fpr_t*-desc
  (fh:pointer jit_fpr_t-desc))
(define-fh-pointer-type jit_fpr_t* jit_fpr_t*-desc jit_fpr_t*? make-jit_fpr_t*
 )
(export jit_fpr_t* jit_fpr_t*? make-jit_fpr_t*)
(fh-ref<=>deref!
  jit_fpr_t*
  make-jit_fpr_t*
  jit_fpr_t
  make-jit_fpr_t)
(define-public struct-jit_fpr-desc
  jit_fpr_t-desc)
(define-fh-compound-type struct-jit_fpr struct-jit_fpr-desc struct-jit_fpr? 
 make-struct-jit_fpr)
(export struct-jit_fpr struct-jit_fpr? make-struct-jit_fpr)
(define-public struct-jit_fpr*-desc
  jit_fpr_t*-desc)
(define-fh-pointer-type struct-jit_fpr* struct-jit_fpr*-desc struct-jit_fpr*? 
 make-struct-jit_fpr*)
(export struct-jit_fpr* struct-jit_fpr*? make-struct-jit_fpr*)
(fh-ref<=>deref!
  struct-jit_fpr*
  make-struct-jit_fpr*
  struct-jit_fpr
  make-struct-jit_fpr)

;; enum jit_reloc_kind {
;;   JIT_RELOC_ABSOLUTE,
;;   JIT_RELOC_REL8,
;;   JIT_RELOC_REL16,
;;   JIT_RELOC_REL32,
;;   JIT_RELOC_REL64,
;;   JIT_RELOC_MASK = 15,
;;   JIT_RELOC_FLAG_0 = 16,
;; };
(define enum-jit_reloc_kind-enum-nvl
  '((JIT_RELOC_ABSOLUTE . 0)
    (JIT_RELOC_REL8 . 1)
    (JIT_RELOC_REL16 . 2)
    (JIT_RELOC_REL32 . 3)
    (JIT_RELOC_REL64 . 4)
    (JIT_RELOC_MASK . 15)
    (JIT_RELOC_FLAG_0 . 16))
  )
(define enum-jit_reloc_kind-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-jit_reloc_kind-enum-nvl))
(define-public (unwrap-enum-jit_reloc_kind n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-jit_reloc_kind-enum-nvl n)
        (throw 'ffi-help-error "bad arg: ~A" n)))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-jit_reloc_kind v)
  (assq-ref enum-jit_reloc_kind-enum-vnl v))

;; typedef struct jit_reloc {
;;   uint8_t kind;
;;   uint8_t inst_start_offset;
;;   uint8_t pc_base_offset;
;;   uint8_t rsh;
;;   uint32_t offset;
;; } jit_reloc_t;
(define-public jit_reloc_t-desc
  (bs:struct
    (list `(kind ,uint8)
          `(inst_start_offset ,uint8)
          `(pc_base_offset ,uint8)
          `(rsh ,uint8)
          `(offset ,uint32))))
(define-fh-compound-type jit_reloc_t jit_reloc_t-desc jit_reloc_t? 
 make-jit_reloc_t)
(export jit_reloc_t jit_reloc_t? make-jit_reloc_t)
(define-public jit_reloc_t*-desc
  (fh:pointer jit_reloc_t-desc))
(define-fh-pointer-type jit_reloc_t* jit_reloc_t*-desc jit_reloc_t*? 
 make-jit_reloc_t*)
(export jit_reloc_t* jit_reloc_t*? make-jit_reloc_t*)
(fh-ref<=>deref!
  jit_reloc_t*
  make-jit_reloc_t*
  jit_reloc_t
  make-jit_reloc_t)
(define-public struct-jit_reloc-desc
  jit_reloc_t-desc)
(define-fh-compound-type struct-jit_reloc struct-jit_reloc-desc 
 struct-jit_reloc? make-struct-jit_reloc)
(export struct-jit_reloc struct-jit_reloc? make-struct-jit_reloc)
(define-public struct-jit_reloc*-desc
  jit_reloc_t*-desc)
(define-fh-pointer-type struct-jit_reloc* struct-jit_reloc*-desc 
 struct-jit_reloc*? make-struct-jit_reloc*)
(export struct-jit_reloc* struct-jit_reloc*? make-struct-jit_reloc*)
(fh-ref<=>deref!
  struct-jit_reloc*
  make-struct-jit_reloc*
  struct-jit_reloc
  make-struct-jit_reloc)

;; typedef struct jit_state jit_state_t;
(define-public jit_state_t-desc 'void)
(define-fh-type-alias jit_state_t fh-void)
(define-public jit_state_t? fh-void?)
(define-public make-jit_state_t make-fh-void)
(define-public jit_state_t*-desc (fh:pointer jit_state_t-desc))
(define-fh-pointer-type jit_state_t* jit_state_t*-desc jit_state_t*? 
 make-jit_state_t*)
(export jit_state_t* jit_state_t*? make-jit_state_t*)

;; enum jit_operand_abi {
;;   JIT_OPERAND_ABI_UINT8,
;;   JIT_OPERAND_ABI_INT8,
;;   JIT_OPERAND_ABI_UINT16,
;;   JIT_OPERAND_ABI_INT16,
;;   JIT_OPERAND_ABI_UINT32,
;;   JIT_OPERAND_ABI_INT32,
;;   JIT_OPERAND_ABI_UINT64,
;;   JIT_OPERAND_ABI_INT64,
;;   JIT_OPERAND_ABI_POINTER,
;;   JIT_OPERAND_ABI_FLOAT,
;;   JIT_OPERAND_ABI_DOUBLE,
;;   JIT_OPERAND_ABI_WORD = JIT_OPERAND_ABI_INT64,
;; };
(define enum-jit_operand_abi-enum-nvl
  '((JIT_OPERAND_ABI_UINT8 . 0)
    (JIT_OPERAND_ABI_INT8 . 1)
    (JIT_OPERAND_ABI_UINT16 . 2)
    (JIT_OPERAND_ABI_INT16 . 3)
    (JIT_OPERAND_ABI_UINT32 . 4)
    (JIT_OPERAND_ABI_INT32 . 5)
    (JIT_OPERAND_ABI_UINT64 . 6)
    (JIT_OPERAND_ABI_INT64 . 7)
    (JIT_OPERAND_ABI_POINTER . 8)
    (JIT_OPERAND_ABI_FLOAT . 9)
    (JIT_OPERAND_ABI_DOUBLE . 10)
    (JIT_OPERAND_ABI_WORD . 7))
  )
(define enum-jit_operand_abi-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-jit_operand_abi-enum-nvl))
(define-public (unwrap-enum-jit_operand_abi n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-jit_operand_abi-enum-nvl n)
        (throw 'ffi-help-error "bad arg: ~A" n)))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-jit_operand_abi v)
  (assq-ref enum-jit_operand_abi-enum-vnl v))

;; enum jit_operand_kind {
;;   JIT_OPERAND_KIND_IMM,
;;   JIT_OPERAND_KIND_GPR,
;;   JIT_OPERAND_KIND_FPR,
;;   JIT_OPERAND_KIND_MEM,
;; };
(define enum-jit_operand_kind-enum-nvl
  '((JIT_OPERAND_KIND_IMM . 0)
    (JIT_OPERAND_KIND_GPR . 1)
    (JIT_OPERAND_KIND_FPR . 2)
    (JIT_OPERAND_KIND_MEM . 3))
  )
(define enum-jit_operand_kind-enum-vnl
  (map (lambda (pair) (cons (cdr pair) (car pair)))
       enum-jit_operand_kind-enum-nvl))
(define-public (unwrap-enum-jit_operand_kind n)
  (cond
   ((symbol? n)
    (or (assq-ref enum-jit_operand_kind-enum-nvl n)
        (throw 'ffi-help-error "bad arg: ~A" n)))
   ((integer? n) n)
   (else (error "bad arg"))))
(define-public (wrap-enum-jit_operand_kind v)
  (assq-ref enum-jit_operand_kind-enum-vnl v))

;; typedef struct jit_operand {
;;   enum jit_operand_abi abi;
;;   enum jit_operand_kind kind;
;;   union {
;;     intptr_t imm;
;;     struct {
;;       jit_gpr_t gpr;
;;       long addend;
;;     } gpr;
;;     jit_fpr_t fpr;
;;     struct {
;;       jit_gpr_t base;
;;       long offset;
;;       long addend;
;;     } mem;
;;   } loc;
;; } jit_operand_t;
(define-public jit_operand_t-desc
  (bs:struct
    (list `(abi ,int)
          `(kind ,int)
          `(loc ,(bs:union
                   (list `(imm ,intptr_t)
                         `(gpr ,(bs:struct
                                  (list `(gpr ,jit_gpr_t-desc)
                                        `(addend ,long))))
                         `(fpr ,jit_fpr_t-desc)
                         `(mem ,(bs:struct
                                  (list `(base ,jit_gpr_t-desc)
                                        `(offset ,long)
                                        `(addend ,long))))))))))
(define-fh-compound-type jit_operand_t jit_operand_t-desc jit_operand_t? 
 make-jit_operand_t)
(export jit_operand_t jit_operand_t? make-jit_operand_t)
(define-public jit_operand_t*-desc
  (fh:pointer jit_operand_t-desc))
(define-fh-pointer-type jit_operand_t* jit_operand_t*-desc jit_operand_t*? 
 make-jit_operand_t*)
(export jit_operand_t* jit_operand_t*? make-jit_operand_t*)
(fh-ref<=>deref!
  jit_operand_t*
  make-jit_operand_t*
  jit_operand_t
  make-jit_operand_t)
(define-public struct-jit_operand-desc
  jit_operand_t-desc)
(define-fh-compound-type struct-jit_operand struct-jit_operand-desc 
 struct-jit_operand? make-struct-jit_operand)
(export struct-jit_operand struct-jit_operand? make-struct-jit_operand)
(define-public struct-jit_operand*-desc
  jit_operand_t*-desc)
(define-fh-pointer-type struct-jit_operand* struct-jit_operand*-desc 
 struct-jit_operand*? make-struct-jit_operand*)
(export struct-jit_operand* struct-jit_operand*? make-struct-jit_operand*)
(fh-ref<=>deref!
  struct-jit_operand*
  make-struct-jit_operand*
  struct-jit_operand
  make-struct-jit_operand)

;; extern jit_bool_t init_jit(void);
(define init_jit
  (let ((~init_jit
          (delay (fh-link-proc
                   ffi:int
                   "init_jit"
                   (list)
                   (force ffi-lightening-llibs)))))
    (lambda () (let () ((force ~init_jit))))))
(export init_jit)

;; extern jit_state_t *jit_new_state(void *(*alloc_fn)(size_t), void (*free_fn)
;;     (void *));
(define jit_new_state
  (let ((~jit_new_state
          (delay (fh-link-proc
                   ffi-void*
                   "jit_new_state"
                   (list ffi-void* ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (alloc_fn free_fn)
      (let ((~alloc_fn
              ((make-fctn-param-unwrapper
                 ffi-void*
                 (list ffi:long))
               alloc_fn))
            (~free_fn
              ((make-fctn-param-unwrapper
                 ffi:void
                 (list ffi-void*))
               free_fn)))
        ((fht-wrap jit_state_t*)
         ((force ~jit_new_state) ~alloc_fn ~free_fn))))))
(export jit_new_state)

;; extern void jit_destroy_state(jit_state_t *);
(define jit_destroy_state
  (let ((~jit_destroy_state
          (delay (fh-link-proc
                   ffi:void
                   "jit_destroy_state"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0)))
        ((force ~jit_destroy_state) ~arg0)))))
(export jit_destroy_state)

;; extern void jit_begin(jit_state_t *, uint8_t *, size_t);
(define jit_begin
  (let ((~jit_begin
          (delay (fh-link-proc
                   ffi:void
                   "jit_begin"
                   (list ffi-void* ffi-void* ffi:size_t)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 arg1 arg2)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~arg1 (unwrap~pointer arg1))
            (~arg2 (unwrap~fixed arg2)))
        ((force ~jit_begin) ~arg0 ~arg1 ~arg2)))))
(export jit_begin)

;; extern jit_bool_t jit_has_overflow(jit_state_t *);
(define jit_has_overflow
  (let ((~jit_has_overflow
          (delay (fh-link-proc
                   ffi:int
                   "jit_has_overflow"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0)))
        ((force ~jit_has_overflow) ~arg0)))))
(export jit_has_overflow)

;; extern void jit_reset(jit_state_t *);
(define jit_reset
  (let ((~jit_reset
          (delay (fh-link-proc
                   ffi:void
                   "jit_reset"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0)))
        ((force ~jit_reset) ~arg0)))))
(export jit_reset)

;; extern void *jit_end(jit_state_t *, size_t *);
(define jit_end
  (let ((~jit_end
          (delay (fh-link-proc
                   ffi-void*
                   "jit_end"
                   (list ffi-void* ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 arg1)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~arg1 (unwrap~pointer arg1)))
        ((force ~jit_end) ~arg0 ~arg1)))))
(export jit_end)

;; extern void jit_align(jit_state_t *, unsigned);
(define jit_align
  (let ((~jit_align
          (delay (fh-link-proc
                   ffi:void
                   "jit_align"
                   (list ffi-void* ffi:unsigned-int)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 arg1)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~arg1 (unwrap~fixed arg1)))
        ((force ~jit_align) ~arg0 ~arg1)))))
(export jit_align)

;; extern jit_pointer_t jit_address(jit_state_t *);
(define jit_address
  (let ((~jit_address
          (delay (fh-link-proc
                   ffi-void*
                   "jit_address"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0)))
        ((fht-wrap jit_pointer_t)
         ((force ~jit_address) ~arg0))))))
(export jit_address)

;; typedef void (*jit_function_pointer_t)();
(define-public jit_function_pointer_t-desc
  (fh:pointer (delay (fh:function 'void (list))))
  )
(define-fh-function*-type
  jit_function_pointer_t
  jit_function_pointer_t-desc
  jit_function_pointer_t?
  make-jit_function_pointer_t)
(export jit_function_pointer_t jit_function_pointer_t? 
 make-jit_function_pointer_t)

;; extern jit_function_pointer_t jit_address_to_function_pointer(jit_pointer_t)
;;     ;
(define jit_address_to_function_pointer
  (let ((~jit_address_to_function_pointer
          (delay (fh-link-proc
                   ffi-void*
                   "jit_address_to_function_pointer"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0)
      (let ((~arg0 ((fht-unwrap jit_pointer_t) arg0)))
        ((fht-wrap jit_function_pointer_t)
         ((force ~jit_address_to_function_pointer) ~arg0))))))
(export jit_address_to_function_pointer)

;; extern void jit_patch_here(jit_state_t *, jit_reloc_t);
(define jit_patch_here
  (let ((~jit_patch_here
          (delay (fh-link-proc
                   ffi:void
                   "jit_patch_here"
                   (list ffi-void*
                         (list ffi:uint8
                               ffi:uint8
                               ffi:uint8
                               ffi:uint8
                               ffi:uint32))
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 arg1)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~arg1 ((fht-unwrap jit_reloc_t) arg1)))
        ((force ~jit_patch_here) ~arg0 ~arg1)))))
(export jit_patch_here)

;; extern void jit_patch_there(jit_state_t *, jit_reloc_t, jit_pointer_t);
(define jit_patch_there
  (let ((~jit_patch_there
          (delay (fh-link-proc
                   ffi:void
                   "jit_patch_there"
                   (list ffi-void*
                         (list ffi:uint8
                               ffi:uint8
                               ffi:uint8
                               ffi:uint8
                               ffi:uint32)
                         ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 arg1 arg2)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~arg1 ((fht-unwrap jit_reloc_t) arg1))
            (~arg2 ((fht-unwrap jit_pointer_t) arg2)))
        ((force ~jit_patch_there) ~arg0 ~arg1 ~arg2)))))
(export jit_patch_there)

;; extern void jit_move_operands(jit_state_t *_jit, jit_operand_t *dst, 
;;     jit_operand_t *src, size_t argc);
(define jit_move_operands
  (let ((~jit_move_operands
          (delay (fh-link-proc
                   ffi:void
                   "jit_move_operands"
                   (list ffi-void* ffi-void* ffi-void* ffi:size_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit dst src argc)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~dst ((fht-unwrap jit_operand_t*) dst))
            (~src ((fht-unwrap jit_operand_t*) src))
            (~argc (unwrap~fixed argc)))
        ((force ~jit_move_operands)
         ~_jit
         ~dst
         ~src
         ~argc)))))
(export jit_move_operands)

;; extern size_t jit_align_stack(jit_state_t *_jit, size_t expand);
(define jit_align_stack
  (let ((~jit_align_stack
          (delay (fh-link-proc
                   ffi:size_t
                   "jit_align_stack"
                   (list ffi-void* ffi:size_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit expand)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~expand (unwrap~fixed expand)))
        ((force ~jit_align_stack) ~_jit ~expand)))))
(export jit_align_stack)

;; extern void jit_shrink_stack(jit_state_t *_jit, size_t diff);
(define jit_shrink_stack
  (let ((~jit_shrink_stack
          (delay (fh-link-proc
                   ffi:void
                   "jit_shrink_stack"
                   (list ffi-void* ffi:size_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit diff)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~diff (unwrap~fixed diff)))
        ((force ~jit_shrink_stack) ~_jit ~diff)))))
(export jit_shrink_stack)

;; extern size_t jit_enter_jit_abi(jit_state_t *_jit, size_t v, size_t vf, 
;;     size_t frame_size);
(define jit_enter_jit_abi
  (let ((~jit_enter_jit_abi
          (delay (fh-link-proc
                   ffi:size_t
                   "jit_enter_jit_abi"
                   (list ffi-void* ffi:size_t ffi:size_t ffi:size_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit v vf frame_size)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~v (unwrap~fixed v))
            (~vf (unwrap~fixed vf))
            (~frame_size (unwrap~fixed frame_size)))
        ((force ~jit_enter_jit_abi)
         ~_jit
         ~v
         ~vf
         ~frame_size)))))
(export jit_enter_jit_abi)

;; extern void jit_leave_jit_abi(jit_state_t *_jit, size_t v, size_t vf, size_t
;;      frame_size);
(define jit_leave_jit_abi
  (let ((~jit_leave_jit_abi
          (delay (fh-link-proc
                   ffi:void
                   "jit_leave_jit_abi"
                   (list ffi-void* ffi:size_t ffi:size_t ffi:size_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit v vf frame_size)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~v (unwrap~fixed v))
            (~vf (unwrap~fixed vf))
            (~frame_size (unwrap~fixed frame_size)))
        ((force ~jit_leave_jit_abi)
         ~_jit
         ~v
         ~vf
         ~frame_size)))))
(export jit_leave_jit_abi)

;; extern void jit_calli(jit_state_t *, jit_pointer_t f, size_t argc, 
;;     jit_operand_t args[]);
(define jit_calli
  (let ((~jit_calli
          (delay (fh-link-proc
                   ffi:void
                   "jit_calli"
                   (list ffi-void* ffi-void* ffi:size_t ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 f argc args)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~f ((fht-unwrap jit_pointer_t) f))
            (~argc (unwrap~fixed argc))
            (~args (unwrap~array args)))
        ((force ~jit_calli) ~arg0 ~f ~argc ~args)))))
(export jit_calli)

;; extern void jit_callr(jit_state_t *, jit_gpr_t f, size_t argc, jit_operand_t
;;      args[]);
(define jit_callr
  (let ((~jit_callr
          (delay (fh-link-proc
                   ffi:void
                   "jit_callr"
                   (list ffi-void*
                         (list ffi:uint8)
                         ffi:size_t
                         ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 f argc args)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~f ((fht-unwrap jit_gpr_t) f))
            (~argc (unwrap~fixed argc))
            (~args (unwrap~array args)))
        ((force ~jit_callr) ~arg0 ~f ~argc ~args)))))
(export jit_callr)

;; extern void jit_locate_args(jit_state_t *, size_t argc, jit_operand_t args[]
;;     );
(define jit_locate_args
  (let ((~jit_locate_args
          (delay (fh-link-proc
                   ffi:void
                   "jit_locate_args"
                   (list ffi-void* ffi:size_t ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 argc args)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~argc (unwrap~fixed argc))
            (~args (unwrap~array args)))
        ((force ~jit_locate_args) ~arg0 ~argc ~args)))))
(export jit_locate_args)

;; extern void jit_load_args(jit_state_t *, size_t argc, jit_operand_t dst[]);
(define jit_load_args
  (let ((~jit_load_args
          (delay (fh-link-proc
                   ffi:void
                   "jit_load_args"
                   (list ffi-void* ffi:size_t ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 argc dst)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~argc (unwrap~fixed argc))
            (~dst (unwrap~array dst)))
        ((force ~jit_load_args) ~arg0 ~argc ~dst)))))
(export jit_load_args)

;; extern void jit_addr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t 
;;     c);
(define jit_addr
  (let ((~jit_addr
          (delay (fh-link-proc
                   ffi:void
                   "jit_addr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_addr) ~_jit ~a ~b ~c)))))
(export jit_addr)

;; extern void jit_addr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b, 
;;     jit_fpr_t c);
(define jit_addr_f
  (let ((~jit_addr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_addr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_addr_f) ~_jit ~a ~b ~c)))))
(export jit_addr_f)

;; extern void jit_addr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b, 
;;     jit_fpr_t c);
(define jit_addr_d
  (let ((~jit_addr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_addr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_addr_d) ~_jit ~a ~b ~c)))))
(export jit_addr_d)

;; extern void jit_addi(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_imm_t 
;;     c);
(define jit_addi
  (let ((~jit_addi
          (delay (fh-link-proc
                   ffi:void
                   "jit_addi"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_addi) ~_jit ~a ~b ~c)))))
(export jit_addi)

;; extern void jit_addcr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t
;;      c);
(define jit_addcr
  (let ((~jit_addcr
          (delay (fh-link-proc
                   ffi:void
                   "jit_addcr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_addcr) ~_jit ~a ~b ~c)))))
(export jit_addcr)

;; extern void jit_addci(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_imm_t
;;      c);
(define jit_addci
  (let ((~jit_addci
          (delay (fh-link-proc
                   ffi:void
                   "jit_addci"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_addci) ~_jit ~a ~b ~c)))))
(export jit_addci)

;; extern void jit_addxr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t
;;      c);
(define jit_addxr
  (let ((~jit_addxr
          (delay (fh-link-proc
                   ffi:void
                   "jit_addxr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_addxr) ~_jit ~a ~b ~c)))))
(export jit_addxr)

;; extern void jit_addxi(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_imm_t
;;      c);
(define jit_addxi
  (let ((~jit_addxi
          (delay (fh-link-proc
                   ffi:void
                   "jit_addxi"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_addxi) ~_jit ~a ~b ~c)))))
(export jit_addxi)

;; extern void jit_subr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t 
;;     c);
(define jit_subr
  (let ((~jit_subr
          (delay (fh-link-proc
                   ffi:void
                   "jit_subr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_subr) ~_jit ~a ~b ~c)))))
(export jit_subr)

;; extern void jit_subr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b, 
;;     jit_fpr_t c);
(define jit_subr_f
  (let ((~jit_subr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_subr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_subr_f) ~_jit ~a ~b ~c)))))
(export jit_subr_f)

;; extern void jit_subr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b, 
;;     jit_fpr_t c);
(define jit_subr_d
  (let ((~jit_subr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_subr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_subr_d) ~_jit ~a ~b ~c)))))
(export jit_subr_d)

;; extern void jit_subi(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_imm_t 
;;     c);
(define jit_subi
  (let ((~jit_subi
          (delay (fh-link-proc
                   ffi:void
                   "jit_subi"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_subi) ~_jit ~a ~b ~c)))))
(export jit_subi)

;; extern void jit_subcr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t
;;      c);
(define jit_subcr
  (let ((~jit_subcr
          (delay (fh-link-proc
                   ffi:void
                   "jit_subcr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_subcr) ~_jit ~a ~b ~c)))))
(export jit_subcr)

;; extern void jit_subci(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_imm_t
;;      c);
(define jit_subci
  (let ((~jit_subci
          (delay (fh-link-proc
                   ffi:void
                   "jit_subci"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_subci) ~_jit ~a ~b ~c)))))
(export jit_subci)

;; extern void jit_subxr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t
;;      c);
(define jit_subxr
  (let ((~jit_subxr
          (delay (fh-link-proc
                   ffi:void
                   "jit_subxr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_subxr) ~_jit ~a ~b ~c)))))
(export jit_subxr)

;; extern void jit_subxi(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_imm_t
;;      c);
(define jit_subxi
  (let ((~jit_subxi
          (delay (fh-link-proc
                   ffi:void
                   "jit_subxi"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_subxi) ~_jit ~a ~b ~c)))))
(export jit_subxi)

;; extern void jit_mulr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t 
;;     c);
(define jit_mulr
  (let ((~jit_mulr
          (delay (fh-link-proc
                   ffi:void
                   "jit_mulr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_mulr) ~_jit ~a ~b ~c)))))
(export jit_mulr)

;; extern void jit_mulr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b, 
;;     jit_fpr_t c);
(define jit_mulr_f
  (let ((~jit_mulr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_mulr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_mulr_f) ~_jit ~a ~b ~c)))))
(export jit_mulr_f)

;; extern void jit_mulr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b, 
;;     jit_fpr_t c);
(define jit_mulr_d
  (let ((~jit_mulr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_mulr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_mulr_d) ~_jit ~a ~b ~c)))))
(export jit_mulr_d)

;; extern void jit_muli(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_imm_t 
;;     c);
(define jit_muli
  (let ((~jit_muli
          (delay (fh-link-proc
                   ffi:void
                   "jit_muli"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_muli) ~_jit ~a ~b ~c)))))
(export jit_muli)

;; extern void jit_qmulr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t
;;      c, jit_gpr_t d);
(define jit_qmulr
  (let ((~jit_qmulr
          (delay (fh-link-proc
                   ffi:void
                   "jit_qmulr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c d)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c))
            (~d ((fht-unwrap jit_gpr_t) d)))
        ((force ~jit_qmulr) ~_jit ~a ~b ~c ~d)))))
(export jit_qmulr)

;; extern void jit_qmuli(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t
;;      c, jit_imm_t d);
(define jit_qmuli
  (let ((~jit_qmuli
          (delay (fh-link-proc
                   ffi:void
                   "jit_qmuli"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c d)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c))
            (~d (unwrap~fixed d)))
        ((force ~jit_qmuli) ~_jit ~a ~b ~c ~d)))))
(export jit_qmuli)

;; extern void jit_qmulr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c, jit_gpr_t d);
(define jit_qmulr_u
  (let ((~jit_qmulr_u
          (delay (fh-link-proc
                   ffi:void
                   "jit_qmulr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c d)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c))
            (~d ((fht-unwrap jit_gpr_t) d)))
        ((force ~jit_qmulr_u) ~_jit ~a ~b ~c ~d)))))
(export jit_qmulr_u)

;; extern void jit_qmuli_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c, jit_uimm_t d);
(define jit_qmuli_u
  (let ((~jit_qmuli_u
          (delay (fh-link-proc
                   ffi:void
                   "jit_qmuli_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c d)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c))
            (~d (unwrap~fixed d)))
        ((force ~jit_qmuli_u) ~_jit ~a ~b ~c ~d)))))
(export jit_qmuli_u)

;; extern void jit_divr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t 
;;     c);
(define jit_divr
  (let ((~jit_divr
          (delay (fh-link-proc
                   ffi:void
                   "jit_divr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_divr) ~_jit ~a ~b ~c)))))
(export jit_divr)

;; extern void jit_divr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b, 
;;     jit_fpr_t c);
(define jit_divr_f
  (let ((~jit_divr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_divr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_divr_f) ~_jit ~a ~b ~c)))))
(export jit_divr_f)

;; extern void jit_divr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b, 
;;     jit_fpr_t c);
(define jit_divr_d
  (let ((~jit_divr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_divr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_divr_d) ~_jit ~a ~b ~c)))))
(export jit_divr_d)

;; extern void jit_divi(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_imm_t 
;;     c);
(define jit_divi
  (let ((~jit_divi
          (delay (fh-link-proc
                   ffi:void
                   "jit_divi"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_divi) ~_jit ~a ~b ~c)))))
(export jit_divi)

;; extern void jit_divr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_divr_u
  (let ((~jit_divr_u
          (delay (fh-link-proc
                   ffi:void
                   "jit_divr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_divr_u) ~_jit ~a ~b ~c)))))
(export jit_divr_u)

;; extern void jit_divi_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_uimm_t c);
(define jit_divi_u
  (let ((~jit_divi_u
          (delay (fh-link-proc
                   ffi:void
                   "jit_divi_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_divi_u) ~_jit ~a ~b ~c)))))
(export jit_divi_u)

;; extern void jit_qdivr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t
;;      c, jit_gpr_t d);
(define jit_qdivr
  (let ((~jit_qdivr
          (delay (fh-link-proc
                   ffi:void
                   "jit_qdivr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c d)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c))
            (~d ((fht-unwrap jit_gpr_t) d)))
        ((force ~jit_qdivr) ~_jit ~a ~b ~c ~d)))))
(export jit_qdivr)

;; extern void jit_qdivi(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t
;;      c, jit_imm_t d);
(define jit_qdivi
  (let ((~jit_qdivi
          (delay (fh-link-proc
                   ffi:void
                   "jit_qdivi"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c d)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c))
            (~d (unwrap~fixed d)))
        ((force ~jit_qdivi) ~_jit ~a ~b ~c ~d)))))
(export jit_qdivi)

;; extern void jit_qdivr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c, jit_gpr_t d);
(define jit_qdivr_u
  (let ((~jit_qdivr_u
          (delay (fh-link-proc
                   ffi:void
                   "jit_qdivr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c d)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c))
            (~d ((fht-unwrap jit_gpr_t) d)))
        ((force ~jit_qdivr_u) ~_jit ~a ~b ~c ~d)))))
(export jit_qdivr_u)

;; extern void jit_qdivi_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c, jit_uimm_t d);
(define jit_qdivi_u
  (let ((~jit_qdivi_u
          (delay (fh-link-proc
                   ffi:void
                   "jit_qdivi_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c d)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c))
            (~d (unwrap~fixed d)))
        ((force ~jit_qdivi_u) ~_jit ~a ~b ~c ~d)))))
(export jit_qdivi_u)

;; extern void jit_remr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t 
;;     c);
(define jit_remr
  (let ((~jit_remr
          (delay (fh-link-proc
                   ffi:void
                   "jit_remr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_remr) ~_jit ~a ~b ~c)))))
(export jit_remr)

;; extern void jit_remi(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_imm_t 
;;     c);
(define jit_remi
  (let ((~jit_remi
          (delay (fh-link-proc
                   ffi:void
                   "jit_remi"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_remi) ~_jit ~a ~b ~c)))))
(export jit_remi)

;; extern void jit_remr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_remr_u
  (let ((~jit_remr_u
          (delay (fh-link-proc
                   ffi:void
                   "jit_remr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_remr_u) ~_jit ~a ~b ~c)))))
(export jit_remr_u)

;; extern void jit_remi_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_uimm_t c);
(define jit_remi_u
  (let ((~jit_remi_u
          (delay (fh-link-proc
                   ffi:void
                   "jit_remi_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_remi_u) ~_jit ~a ~b ~c)))))
(export jit_remi_u)

;; extern void jit_andr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t 
;;     c);
(define jit_andr
  (let ((~jit_andr
          (delay (fh-link-proc
                   ffi:void
                   "jit_andr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_andr) ~_jit ~a ~b ~c)))))
(export jit_andr)

;; extern void jit_andi(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_uimm_t
;;      c);
(define jit_andi
  (let ((~jit_andi
          (delay (fh-link-proc
                   ffi:void
                   "jit_andi"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_andi) ~_jit ~a ~b ~c)))))
(export jit_andi)

;; extern void jit_orr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t c
;;     );
(define jit_orr
  (let ((~jit_orr
          (delay (fh-link-proc
                   ffi:void
                   "jit_orr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_orr) ~_jit ~a ~b ~c)))))
(export jit_orr)

;; extern void jit_ori(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_uimm_t 
;;     c);
(define jit_ori
  (let ((~jit_ori
          (delay (fh-link-proc
                   ffi:void
                   "jit_ori"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_ori) ~_jit ~a ~b ~c)))))
(export jit_ori)

;; extern void jit_xorr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t 
;;     c);
(define jit_xorr
  (let ((~jit_xorr
          (delay (fh-link-proc
                   ffi:void
                   "jit_xorr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_xorr) ~_jit ~a ~b ~c)))))
(export jit_xorr)

;; extern void jit_xori(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_uimm_t
;;      c);
(define jit_xori
  (let ((~jit_xori
          (delay (fh-link-proc
                   ffi:void
                   "jit_xori"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_xori) ~_jit ~a ~b ~c)))))
(export jit_xori)

;; extern void jit_lshr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t 
;;     c);
(define jit_lshr
  (let ((~jit_lshr
          (delay (fh-link-proc
                   ffi:void
                   "jit_lshr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_lshr) ~_jit ~a ~b ~c)))))
(export jit_lshr)

;; extern void jit_lshi(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_uimm_t
;;      c);
(define jit_lshi
  (let ((~jit_lshi
          (delay (fh-link-proc
                   ffi:void
                   "jit_lshi"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_lshi) ~_jit ~a ~b ~c)))))
(export jit_lshi)

;; extern void jit_rshr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_gpr_t 
;;     c);
(define jit_rshr
  (let ((~jit_rshr
          (delay (fh-link-proc
                   ffi:void
                   "jit_rshr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_rshr) ~_jit ~a ~b ~c)))))
(export jit_rshr)

;; extern void jit_rshi(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, jit_uimm_t
;;      c);
(define jit_rshi
  (let ((~jit_rshi
          (delay (fh-link-proc
                   ffi:void
                   "jit_rshi"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_rshi) ~_jit ~a ~b ~c)))))
(export jit_rshi)

;; extern void jit_rshr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_rshr_u
  (let ((~jit_rshr_u
          (delay (fh-link-proc
                   ffi:void
                   "jit_rshr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_rshr_u) ~_jit ~a ~b ~c)))))
(export jit_rshr_u)

;; extern void jit_rshi_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_uimm_t c);
(define jit_rshi_u
  (let ((~jit_rshi_u
          (delay (fh-link-proc
                   ffi:void
                   "jit_rshi_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_rshi_u) ~_jit ~a ~b ~c)))))
(export jit_rshi_u)

;; extern void jit_negr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_negr
  (let ((~jit_negr
          (delay (fh-link-proc
                   ffi:void
                   "jit_negr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_negr) ~_jit ~a ~b)))))
(export jit_negr)

;; extern void jit_comr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_comr
  (let ((~jit_comr
          (delay (fh-link-proc
                   ffi:void
                   "jit_comr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_comr) ~_jit ~a ~b)))))
(export jit_comr)

;; extern void jit_movr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_movr
  (let ((~jit_movr
          (delay (fh-link-proc
                   ffi:void
                   "jit_movr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_movr) ~_jit ~a ~b)))))
(export jit_movr)

;; extern void jit_movi(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_movi
  (let ((~jit_movi
          (delay (fh-link-proc
                   ffi:void
                   "jit_movi"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((force ~jit_movi) ~_jit ~a ~b)))))
(export jit_movi)

;; extern jit_reloc_t jit_mov_addr(jit_state_t *_jit, jit_gpr_t a);
(define jit_mov_addr
  (let ((~jit_mov_addr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_mov_addr"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_mov_addr) ~_jit ~a))))))
(export jit_mov_addr)

;; extern void jit_extr_c(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_extr_c
  (let ((~jit_extr_c
          (delay (fh-link-proc
                   ffi:void
                   "jit_extr_c"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_extr_c) ~_jit ~a ~b)))))
(export jit_extr_c)

;; extern void jit_extr_uc(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_extr_uc
  (let ((~jit_extr_uc
          (delay (fh-link-proc
                   ffi:void
                   "jit_extr_uc"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_extr_uc) ~_jit ~a ~b)))))
(export jit_extr_uc)

;; extern void jit_extr_s(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_extr_s
  (let ((~jit_extr_s
          (delay (fh-link-proc
                   ffi:void
                   "jit_extr_s"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_extr_s) ~_jit ~a ~b)))))
(export jit_extr_s)

;; extern void jit_extr_us(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_extr_us
  (let ((~jit_extr_us
          (delay (fh-link-proc
                   ffi:void
                   "jit_extr_us"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_extr_us) ~_jit ~a ~b)))))
(export jit_extr_us)

;; extern void jit_extr_i(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_extr_i
  (let ((~jit_extr_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_extr_i"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_extr_i) ~_jit ~a ~b)))))
(export jit_extr_i)

;; extern void jit_extr_ui(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_extr_ui
  (let ((~jit_extr_ui
          (delay (fh-link-proc
                   ffi:void
                   "jit_extr_ui"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_extr_ui) ~_jit ~a ~b)))))
(export jit_extr_ui)

;; extern void jit_bswapr_us(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bswapr_us
  (let ((~jit_bswapr_us
          (delay (fh-link-proc
                   ffi:void
                   "jit_bswapr_us"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_bswapr_us) ~_jit ~a ~b)))))
(export jit_bswapr_us)

;; extern void jit_bswapr_ui(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bswapr_ui
  (let ((~jit_bswapr_ui
          (delay (fh-link-proc
                   ffi:void
                   "jit_bswapr_ui"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_bswapr_ui) ~_jit ~a ~b)))))
(export jit_bswapr_ui)

;; extern void jit_bswapr_ul(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bswapr_ul
  (let ((~jit_bswapr_ul
          (delay (fh-link-proc
                   ffi:void
                   "jit_bswapr_ul"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_bswapr_ul) ~_jit ~a ~b)))))
(export jit_bswapr_ul)

;; extern void jit_ldr_c(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_ldr_c
  (let ((~jit_ldr_c
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldr_c"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_ldr_c) ~_jit ~a ~b)))))
(export jit_ldr_c)

;; extern void jit_ldi_c(jit_state_t *_jit, jit_gpr_t a, jit_pointer_t b);
(define jit_ldi_c
  (let ((~jit_ldi_c
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldi_c"
                   (list ffi-void* (list ffi:uint8) ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_pointer_t) b)))
        ((force ~jit_ldi_c) ~_jit ~a ~b)))))
(export jit_ldi_c)

;; extern void jit_ldr_uc(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_ldr_uc
  (let ((~jit_ldr_uc
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldr_uc"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_ldr_uc) ~_jit ~a ~b)))))
(export jit_ldr_uc)

;; extern void jit_ldi_uc(jit_state_t *_jit, jit_gpr_t a, jit_pointer_t b);
(define jit_ldi_uc
  (let ((~jit_ldi_uc
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldi_uc"
                   (list ffi-void* (list ffi:uint8) ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_pointer_t) b)))
        ((force ~jit_ldi_uc) ~_jit ~a ~b)))))
(export jit_ldi_uc)

;; extern void jit_ldr_s(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_ldr_s
  (let ((~jit_ldr_s
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldr_s"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_ldr_s) ~_jit ~a ~b)))))
(export jit_ldr_s)

;; extern void jit_ldi_s(jit_state_t *_jit, jit_gpr_t a, jit_pointer_t b);
(define jit_ldi_s
  (let ((~jit_ldi_s
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldi_s"
                   (list ffi-void* (list ffi:uint8) ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_pointer_t) b)))
        ((force ~jit_ldi_s) ~_jit ~a ~b)))))
(export jit_ldi_s)

;; extern void jit_ldr_us(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_ldr_us
  (let ((~jit_ldr_us
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldr_us"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_ldr_us) ~_jit ~a ~b)))))
(export jit_ldr_us)

;; extern void jit_ldi_us(jit_state_t *_jit, jit_gpr_t a, jit_pointer_t b);
(define jit_ldi_us
  (let ((~jit_ldi_us
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldi_us"
                   (list ffi-void* (list ffi:uint8) ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_pointer_t) b)))
        ((force ~jit_ldi_us) ~_jit ~a ~b)))))
(export jit_ldi_us)

;; extern void jit_ldr_i(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_ldr_i
  (let ((~jit_ldr_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldr_i"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_ldr_i) ~_jit ~a ~b)))))
(export jit_ldr_i)

;; extern void jit_ldi_i(jit_state_t *_jit, jit_gpr_t a, jit_pointer_t b);
(define jit_ldi_i
  (let ((~jit_ldi_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldi_i"
                   (list ffi-void* (list ffi:uint8) ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_pointer_t) b)))
        ((force ~jit_ldi_i) ~_jit ~a ~b)))))
(export jit_ldi_i)

;; extern void jit_ldr_ui(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_ldr_ui
  (let ((~jit_ldr_ui
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldr_ui"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_ldr_ui) ~_jit ~a ~b)))))
(export jit_ldr_ui)

;; extern void jit_ldi_ui(jit_state_t *_jit, jit_gpr_t a, jit_pointer_t b);
(define jit_ldi_ui
  (let ((~jit_ldi_ui
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldi_ui"
                   (list ffi-void* (list ffi:uint8) ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_pointer_t) b)))
        ((force ~jit_ldi_ui) ~_jit ~a ~b)))))
(export jit_ldi_ui)

;; extern void jit_ldr_l(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_ldr_l
  (let ((~jit_ldr_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldr_l"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_ldr_l) ~_jit ~a ~b)))))
(export jit_ldr_l)

;; extern void jit_ldi_l(jit_state_t *_jit, jit_gpr_t a, jit_pointer_t b);
(define jit_ldi_l
  (let ((~jit_ldi_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldi_l"
                   (list ffi-void* (list ffi:uint8) ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_pointer_t) b)))
        ((force ~jit_ldi_l) ~_jit ~a ~b)))))
(export jit_ldi_l)

;; extern void jit_ldr_f(jit_state_t *_jit, jit_fpr_t a, jit_gpr_t b);
(define jit_ldr_f
  (let ((~jit_ldr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_ldr_f) ~_jit ~a ~b)))))
(export jit_ldr_f)

;; extern void jit_ldi_f(jit_state_t *_jit, jit_fpr_t a, jit_pointer_t b);
(define jit_ldi_f
  (let ((~jit_ldi_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldi_f"
                   (list ffi-void* (list ffi:uint8) ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_pointer_t) b)))
        ((force ~jit_ldi_f) ~_jit ~a ~b)))))
(export jit_ldi_f)

;; extern void jit_ldr_d(jit_state_t *_jit, jit_fpr_t a, jit_gpr_t b);
(define jit_ldr_d
  (let ((~jit_ldr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_ldr_d) ~_jit ~a ~b)))))
(export jit_ldr_d)

;; extern void jit_ldi_d(jit_state_t *_jit, jit_fpr_t a, jit_pointer_t b);
(define jit_ldi_d
  (let ((~jit_ldi_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldi_d"
                   (list ffi-void* (list ffi:uint8) ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_pointer_t) b)))
        ((force ~jit_ldi_d) ~_jit ~a ~b)))))
(export jit_ldi_d)

;; extern void jit_ldxr_c(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_ldxr_c
  (let ((~jit_ldxr_c
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxr_c"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_ldxr_c) ~_jit ~a ~b ~c)))))
(export jit_ldxr_c)

;; extern void jit_ldxi_c(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_off_t c);
(define jit_ldxi_c
  (let ((~jit_ldxi_c
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxi_c"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:long)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_ldxi_c) ~_jit ~a ~b ~c)))))
(export jit_ldxi_c)

;; extern void jit_ldxr_uc(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_ldxr_uc
  (let ((~jit_ldxr_uc
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxr_uc"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_ldxr_uc) ~_jit ~a ~b ~c)))))
(export jit_ldxr_uc)

;; extern void jit_ldxi_uc(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_off_t c);
(define jit_ldxi_uc
  (let ((~jit_ldxi_uc
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxi_uc"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:long)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_ldxi_uc) ~_jit ~a ~b ~c)))))
(export jit_ldxi_uc)

;; extern void jit_ldxr_s(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_ldxr_s
  (let ((~jit_ldxr_s
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxr_s"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_ldxr_s) ~_jit ~a ~b ~c)))))
(export jit_ldxr_s)

;; extern void jit_ldxi_s(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_off_t c);
(define jit_ldxi_s
  (let ((~jit_ldxi_s
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxi_s"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:long)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_ldxi_s) ~_jit ~a ~b ~c)))))
(export jit_ldxi_s)

;; extern void jit_ldxr_us(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_ldxr_us
  (let ((~jit_ldxr_us
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxr_us"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_ldxr_us) ~_jit ~a ~b ~c)))))
(export jit_ldxr_us)

;; extern void jit_ldxi_us(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_off_t c);
(define jit_ldxi_us
  (let ((~jit_ldxi_us
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxi_us"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:long)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_ldxi_us) ~_jit ~a ~b ~c)))))
(export jit_ldxi_us)

;; extern void jit_ldxr_i(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_ldxr_i
  (let ((~jit_ldxr_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxr_i"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_ldxr_i) ~_jit ~a ~b ~c)))))
(export jit_ldxr_i)

;; extern void jit_ldxi_i(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_off_t c);
(define jit_ldxi_i
  (let ((~jit_ldxi_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxi_i"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:long)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_ldxi_i) ~_jit ~a ~b ~c)))))
(export jit_ldxi_i)

;; extern void jit_ldxr_ui(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_ldxr_ui
  (let ((~jit_ldxr_ui
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxr_ui"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_ldxr_ui) ~_jit ~a ~b ~c)))))
(export jit_ldxr_ui)

;; extern void jit_ldxi_ui(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_off_t c);
(define jit_ldxi_ui
  (let ((~jit_ldxi_ui
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxi_ui"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:long)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_ldxi_ui) ~_jit ~a ~b ~c)))))
(export jit_ldxi_ui)

;; extern void jit_ldxr_l(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_ldxr_l
  (let ((~jit_ldxr_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxr_l"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_ldxr_l) ~_jit ~a ~b ~c)))))
(export jit_ldxr_l)

;; extern void jit_ldxi_l(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_off_t c);
(define jit_ldxi_l
  (let ((~jit_ldxi_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxi_l"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:long)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_ldxi_l) ~_jit ~a ~b ~c)))))
(export jit_ldxi_l)

;; extern void jit_ldxr_f(jit_state_t *_jit, jit_fpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_ldxr_f
  (let ((~jit_ldxr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_ldxr_f) ~_jit ~a ~b ~c)))))
(export jit_ldxr_f)

;; extern void jit_ldxi_f(jit_state_t *_jit, jit_fpr_t a, jit_gpr_t b, 
;;     jit_off_t c);
(define jit_ldxi_f
  (let ((~jit_ldxi_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxi_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:long)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_ldxi_f) ~_jit ~a ~b ~c)))))
(export jit_ldxi_f)

;; extern void jit_ldxr_d(jit_state_t *_jit, jit_fpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_ldxr_d
  (let ((~jit_ldxr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_ldxr_d) ~_jit ~a ~b ~c)))))
(export jit_ldxr_d)

;; extern void jit_ldxi_d(jit_state_t *_jit, jit_fpr_t a, jit_gpr_t b, 
;;     jit_off_t c);
(define jit_ldxi_d
  (let ((~jit_ldxi_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldxi_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         ffi:long)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c (unwrap~fixed c)))
        ((force ~jit_ldxi_d) ~_jit ~a ~b ~c)))))
(export jit_ldxi_d)

;; extern void jit_ldr_atomic(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_ldr_atomic
  (let ((~jit_ldr_atomic
          (delay (fh-link-proc
                   ffi:void
                   "jit_ldr_atomic"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_ldr_atomic) ~_jit ~a ~b)))))
(export jit_ldr_atomic)

;; extern void jit_str_atomic(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_str_atomic
  (let ((~jit_str_atomic
          (delay (fh-link-proc
                   ffi:void
                   "jit_str_atomic"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_str_atomic) ~_jit ~a ~b)))))
(export jit_str_atomic)

;; extern void jit_swap_atomic(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_swap_atomic
  (let ((~jit_swap_atomic
          (delay (fh-link-proc
                   ffi:void
                   "jit_swap_atomic"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_swap_atomic) ~_jit ~a ~b ~c)))))
(export jit_swap_atomic)

;; extern void jit_cas_atomic(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c, jit_gpr_t d);
(define jit_cas_atomic
  (let ((~jit_cas_atomic
          (delay (fh-link-proc
                   ffi:void
                   "jit_cas_atomic"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c d)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c))
            (~d ((fht-unwrap jit_gpr_t) d)))
        ((force ~jit_cas_atomic) ~_jit ~a ~b ~c ~d)))))
(export jit_cas_atomic)

;; extern void jit_str_c(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_str_c
  (let ((~jit_str_c
          (delay (fh-link-proc
                   ffi:void
                   "jit_str_c"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_str_c) ~_jit ~a ~b)))))
(export jit_str_c)

;; extern void jit_sti_c(jit_state_t *_jit, jit_pointer_t a, jit_gpr_t b);
(define jit_sti_c
  (let ((~jit_sti_c
          (delay (fh-link-proc
                   ffi:void
                   "jit_sti_c"
                   (list ffi-void* ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_pointer_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_sti_c) ~_jit ~a ~b)))))
(export jit_sti_c)

;; extern void jit_str_s(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_str_s
  (let ((~jit_str_s
          (delay (fh-link-proc
                   ffi:void
                   "jit_str_s"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_str_s) ~_jit ~a ~b)))))
(export jit_str_s)

;; extern void jit_sti_s(jit_state_t *_jit, jit_pointer_t a, jit_gpr_t b);
(define jit_sti_s
  (let ((~jit_sti_s
          (delay (fh-link-proc
                   ffi:void
                   "jit_sti_s"
                   (list ffi-void* ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_pointer_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_sti_s) ~_jit ~a ~b)))))
(export jit_sti_s)

;; extern void jit_str_i(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_str_i
  (let ((~jit_str_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_str_i"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_str_i) ~_jit ~a ~b)))))
(export jit_str_i)

;; extern void jit_sti_i(jit_state_t *_jit, jit_pointer_t a, jit_gpr_t b);
(define jit_sti_i
  (let ((~jit_sti_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_sti_i"
                   (list ffi-void* ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_pointer_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_sti_i) ~_jit ~a ~b)))))
(export jit_sti_i)

;; extern void jit_str_l(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_str_l
  (let ((~jit_str_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_str_l"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_str_l) ~_jit ~a ~b)))))
(export jit_str_l)

;; extern void jit_sti_l(jit_state_t *_jit, jit_pointer_t a, jit_gpr_t b);
(define jit_sti_l
  (let ((~jit_sti_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_sti_l"
                   (list ffi-void* ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_pointer_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_sti_l) ~_jit ~a ~b)))))
(export jit_sti_l)

;; extern void jit_str_f(jit_state_t *_jit, jit_gpr_t a, jit_fpr_t b);
(define jit_str_f
  (let ((~jit_str_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_str_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_str_f) ~_jit ~a ~b)))))
(export jit_str_f)

;; extern void jit_sti_f(jit_state_t *_jit, jit_pointer_t a, jit_fpr_t b);
(define jit_sti_f
  (let ((~jit_sti_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_sti_f"
                   (list ffi-void* ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_pointer_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_sti_f) ~_jit ~a ~b)))))
(export jit_sti_f)

;; extern void jit_str_d(jit_state_t *_jit, jit_gpr_t a, jit_fpr_t b);
(define jit_str_d
  (let ((~jit_str_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_str_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_str_d) ~_jit ~a ~b)))))
(export jit_str_d)

;; extern void jit_sti_d(jit_state_t *_jit, jit_pointer_t a, jit_fpr_t b);
(define jit_sti_d
  (let ((~jit_sti_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_sti_d"
                   (list ffi-void* ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_pointer_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_sti_d) ~_jit ~a ~b)))))
(export jit_sti_d)

;; extern void jit_stxr_c(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_stxr_c
  (let ((~jit_stxr_c
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxr_c"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_stxr_c) ~_jit ~a ~b ~c)))))
(export jit_stxr_c)

;; extern void jit_stxi_c(jit_state_t *_jit, jit_off_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_stxi_c
  (let ((~jit_stxi_c
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxi_c"
                   (list ffi-void*
                         ffi:long
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a (unwrap~fixed a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_stxi_c) ~_jit ~a ~b ~c)))))
(export jit_stxi_c)

;; extern void jit_stxr_s(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_stxr_s
  (let ((~jit_stxr_s
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxr_s"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_stxr_s) ~_jit ~a ~b ~c)))))
(export jit_stxr_s)

;; extern void jit_stxi_s(jit_state_t *_jit, jit_off_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_stxi_s
  (let ((~jit_stxi_s
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxi_s"
                   (list ffi-void*
                         ffi:long
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a (unwrap~fixed a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_stxi_s) ~_jit ~a ~b ~c)))))
(export jit_stxi_s)

;; extern void jit_stxr_i(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_stxr_i
  (let ((~jit_stxr_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxr_i"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_stxr_i) ~_jit ~a ~b ~c)))))
(export jit_stxr_i)

;; extern void jit_stxi_i(jit_state_t *_jit, jit_off_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_stxi_i
  (let ((~jit_stxi_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxi_i"
                   (list ffi-void*
                         ffi:long
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a (unwrap~fixed a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_stxi_i) ~_jit ~a ~b ~c)))))
(export jit_stxi_i)

;; extern void jit_stxr_l(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_stxr_l
  (let ((~jit_stxr_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxr_l"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_stxr_l) ~_jit ~a ~b ~c)))))
(export jit_stxr_l)

;; extern void jit_stxi_l(jit_state_t *_jit, jit_off_t a, jit_gpr_t b, 
;;     jit_gpr_t c);
(define jit_stxi_l
  (let ((~jit_stxi_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxi_l"
                   (list ffi-void*
                         ffi:long
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a (unwrap~fixed a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_gpr_t) c)))
        ((force ~jit_stxi_l) ~_jit ~a ~b ~c)))))
(export jit_stxi_l)

;; extern void jit_stxr_f(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_fpr_t c);
(define jit_stxr_f
  (let ((~jit_stxr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_stxr_f) ~_jit ~a ~b ~c)))))
(export jit_stxr_f)

;; extern void jit_stxi_f(jit_state_t *_jit, jit_off_t a, jit_gpr_t b, 
;;     jit_fpr_t c);
(define jit_stxi_f
  (let ((~jit_stxi_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxi_f"
                   (list ffi-void*
                         ffi:long
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a (unwrap~fixed a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_stxi_f) ~_jit ~a ~b ~c)))))
(export jit_stxi_f)

;; extern void jit_stxr_d(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b, 
;;     jit_fpr_t c);
(define jit_stxr_d
  (let ((~jit_stxr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_stxr_d) ~_jit ~a ~b ~c)))))
(export jit_stxr_d)

;; extern void jit_stxi_d(jit_state_t *_jit, jit_off_t a, jit_gpr_t b, 
;;     jit_fpr_t c);
(define jit_stxi_d
  (let ((~jit_stxi_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_stxi_d"
                   (list ffi-void*
                         ffi:long
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b c)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a (unwrap~fixed a))
            (~b ((fht-unwrap jit_gpr_t) b))
            (~c ((fht-unwrap jit_fpr_t) c)))
        ((force ~jit_stxi_d) ~_jit ~a ~b ~c)))))
(export jit_stxi_d)

;; extern jit_reloc_t jit_bltr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bltr
  (let ((~jit_bltr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bltr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bltr) ~_jit ~a ~b))))))
(export jit_bltr)

;; extern jit_reloc_t jit_bltr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_bltr_f
  (let ((~jit_bltr_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bltr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bltr_f) ~_jit ~a ~b))))))
(export jit_bltr_f)

;; extern jit_reloc_t jit_bltr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_bltr_d
  (let ((~jit_bltr_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bltr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bltr_d) ~_jit ~a ~b))))))
(export jit_bltr_d)

;; extern jit_reloc_t jit_blti(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_blti
  (let ((~jit_blti
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_blti"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_blti) ~_jit ~a ~b))))))
(export jit_blti)

;; extern jit_reloc_t jit_bltr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bltr_u
  (let ((~jit_bltr_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bltr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bltr_u) ~_jit ~a ~b))))))
(export jit_bltr_u)

;; extern jit_reloc_t jit_blti_u(jit_state_t *_jit, jit_gpr_t a, jit_uimm_t b);
;;     
(define jit_blti_u
  (let ((~jit_blti_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_blti_u"
                   (list ffi-void* (list ffi:uint8) ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_blti_u) ~_jit ~a ~b))))))
(export jit_blti_u)

;; extern jit_reloc_t jit_bler(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bler
  (let ((~jit_bler
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bler"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bler) ~_jit ~a ~b))))))
(export jit_bler)

;; extern jit_reloc_t jit_bler_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_bler_f
  (let ((~jit_bler_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bler_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bler_f) ~_jit ~a ~b))))))
(export jit_bler_f)

;; extern jit_reloc_t jit_bler_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_bler_d
  (let ((~jit_bler_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bler_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bler_d) ~_jit ~a ~b))))))
(export jit_bler_d)

;; extern jit_reloc_t jit_blei(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_blei
  (let ((~jit_blei
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_blei"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_blei) ~_jit ~a ~b))))))
(export jit_blei)

;; extern jit_reloc_t jit_bler_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bler_u
  (let ((~jit_bler_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bler_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bler_u) ~_jit ~a ~b))))))
(export jit_bler_u)

;; extern jit_reloc_t jit_blei_u(jit_state_t *_jit, jit_gpr_t a, jit_uimm_t b);
;;     
(define jit_blei_u
  (let ((~jit_blei_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_blei_u"
                   (list ffi-void* (list ffi:uint8) ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_blei_u) ~_jit ~a ~b))))))
(export jit_blei_u)

;; extern jit_reloc_t jit_beqr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_beqr
  (let ((~jit_beqr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_beqr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_beqr) ~_jit ~a ~b))))))
(export jit_beqr)

;; extern jit_reloc_t jit_beqr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_beqr_f
  (let ((~jit_beqr_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_beqr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_beqr_f) ~_jit ~a ~b))))))
(export jit_beqr_f)

;; extern jit_reloc_t jit_beqr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_beqr_d
  (let ((~jit_beqr_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_beqr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_beqr_d) ~_jit ~a ~b))))))
(export jit_beqr_d)

;; extern jit_reloc_t jit_beqi(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_beqi
  (let ((~jit_beqi
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_beqi"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_beqi) ~_jit ~a ~b))))))
(export jit_beqi)

;; extern jit_reloc_t jit_bger(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bger
  (let ((~jit_bger
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bger"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bger) ~_jit ~a ~b))))))
(export jit_bger)

;; extern jit_reloc_t jit_bger_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_bger_f
  (let ((~jit_bger_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bger_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bger_f) ~_jit ~a ~b))))))
(export jit_bger_f)

;; extern jit_reloc_t jit_bger_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_bger_d
  (let ((~jit_bger_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bger_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bger_d) ~_jit ~a ~b))))))
(export jit_bger_d)

;; extern jit_reloc_t jit_bgei(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_bgei
  (let ((~jit_bgei
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bgei"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bgei) ~_jit ~a ~b))))))
(export jit_bgei)

;; extern jit_reloc_t jit_bger_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bger_u
  (let ((~jit_bger_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bger_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bger_u) ~_jit ~a ~b))))))
(export jit_bger_u)

;; extern jit_reloc_t jit_bgei_u(jit_state_t *_jit, jit_gpr_t a, jit_uimm_t b);
;;     
(define jit_bgei_u
  (let ((~jit_bgei_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bgei_u"
                   (list ffi-void* (list ffi:uint8) ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bgei_u) ~_jit ~a ~b))))))
(export jit_bgei_u)

;; extern jit_reloc_t jit_bgtr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bgtr
  (let ((~jit_bgtr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bgtr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bgtr) ~_jit ~a ~b))))))
(export jit_bgtr)

;; extern jit_reloc_t jit_bgtr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_bgtr_f
  (let ((~jit_bgtr_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bgtr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bgtr_f) ~_jit ~a ~b))))))
(export jit_bgtr_f)

;; extern jit_reloc_t jit_bgtr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_bgtr_d
  (let ((~jit_bgtr_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bgtr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bgtr_d) ~_jit ~a ~b))))))
(export jit_bgtr_d)

;; extern jit_reloc_t jit_bgti(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_bgti
  (let ((~jit_bgti
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bgti"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bgti) ~_jit ~a ~b))))))
(export jit_bgti)

;; extern jit_reloc_t jit_bgtr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bgtr_u
  (let ((~jit_bgtr_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bgtr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bgtr_u) ~_jit ~a ~b))))))
(export jit_bgtr_u)

;; extern jit_reloc_t jit_bgti_u(jit_state_t *_jit, jit_gpr_t a, jit_uimm_t b);
;;     
(define jit_bgti_u
  (let ((~jit_bgti_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bgti_u"
                   (list ffi-void* (list ffi:uint8) ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bgti_u) ~_jit ~a ~b))))))
(export jit_bgti_u)

;; extern jit_reloc_t jit_bner(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bner
  (let ((~jit_bner
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bner"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bner) ~_jit ~a ~b))))))
(export jit_bner)

;; extern jit_reloc_t jit_bner_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_bner_f
  (let ((~jit_bner_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bner_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bner_f) ~_jit ~a ~b))))))
(export jit_bner_f)

;; extern jit_reloc_t jit_bner_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_bner_d
  (let ((~jit_bner_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bner_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bner_d) ~_jit ~a ~b))))))
(export jit_bner_d)

;; extern jit_reloc_t jit_bnei(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_bnei
  (let ((~jit_bnei
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bnei"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bnei) ~_jit ~a ~b))))))
(export jit_bnei)

;; extern jit_reloc_t jit_bunltr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_bunltr_f
  (let ((~jit_bunltr_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bunltr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bunltr_f) ~_jit ~a ~b))))))
(export jit_bunltr_f)

;; extern jit_reloc_t jit_bunltr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_bunltr_d
  (let ((~jit_bunltr_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bunltr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bunltr_d) ~_jit ~a ~b))))))
(export jit_bunltr_d)

;; extern jit_reloc_t jit_bunler_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_bunler_f
  (let ((~jit_bunler_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bunler_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bunler_f) ~_jit ~a ~b))))))
(export jit_bunler_f)

;; extern jit_reloc_t jit_bunler_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_bunler_d
  (let ((~jit_bunler_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bunler_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bunler_d) ~_jit ~a ~b))))))
(export jit_bunler_d)

;; extern jit_reloc_t jit_buneqr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_buneqr_f
  (let ((~jit_buneqr_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_buneqr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_buneqr_f) ~_jit ~a ~b))))))
(export jit_buneqr_f)

;; extern jit_reloc_t jit_buneqr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_buneqr_d
  (let ((~jit_buneqr_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_buneqr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_buneqr_d) ~_jit ~a ~b))))))
(export jit_buneqr_d)

;; extern jit_reloc_t jit_bunger_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_bunger_f
  (let ((~jit_bunger_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bunger_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bunger_f) ~_jit ~a ~b))))))
(export jit_bunger_f)

;; extern jit_reloc_t jit_bunger_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_bunger_d
  (let ((~jit_bunger_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bunger_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bunger_d) ~_jit ~a ~b))))))
(export jit_bunger_d)

;; extern jit_reloc_t jit_bungtr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_bungtr_f
  (let ((~jit_bungtr_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bungtr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bungtr_f) ~_jit ~a ~b))))))
(export jit_bungtr_f)

;; extern jit_reloc_t jit_bungtr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_bungtr_d
  (let ((~jit_bungtr_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bungtr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bungtr_d) ~_jit ~a ~b))))))
(export jit_bungtr_d)

;; extern jit_reloc_t jit_bltgtr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_bltgtr_f
  (let ((~jit_bltgtr_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bltgtr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bltgtr_f) ~_jit ~a ~b))))))
(export jit_bltgtr_f)

;; extern jit_reloc_t jit_bltgtr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b)
;;     ;
(define jit_bltgtr_d
  (let ((~jit_bltgtr_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bltgtr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bltgtr_d) ~_jit ~a ~b))))))
(export jit_bltgtr_d)

;; extern jit_reloc_t jit_bordr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
;;     
(define jit_bordr_f
  (let ((~jit_bordr_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bordr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bordr_f) ~_jit ~a ~b))))))
(export jit_bordr_f)

;; extern jit_reloc_t jit_bordr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
;;     
(define jit_bordr_d
  (let ((~jit_bordr_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bordr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bordr_d) ~_jit ~a ~b))))))
(export jit_bordr_d)

;; extern jit_reloc_t jit_bunordr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b
;;     );
(define jit_bunordr_f
  (let ((~jit_bunordr_f
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bunordr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bunordr_f) ~_jit ~a ~b))))))
(export jit_bunordr_f)

;; extern jit_reloc_t jit_bunordr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b
;;     );
(define jit_bunordr_d
  (let ((~jit_bunordr_d
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bunordr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bunordr_d) ~_jit ~a ~b))))))
(export jit_bunordr_d)

;; extern jit_reloc_t jit_bmsr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bmsr
  (let ((~jit_bmsr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bmsr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bmsr) ~_jit ~a ~b))))))
(export jit_bmsr)

;; extern jit_reloc_t jit_bmsi(jit_state_t *_jit, jit_gpr_t a, jit_uimm_t b);
(define jit_bmsi
  (let ((~jit_bmsi
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bmsi"
                   (list ffi-void* (list ffi:uint8) ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bmsi) ~_jit ~a ~b))))))
(export jit_bmsi)

;; extern jit_reloc_t jit_bmcr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bmcr
  (let ((~jit_bmcr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bmcr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bmcr) ~_jit ~a ~b))))))
(export jit_bmcr)

;; extern jit_reloc_t jit_bmci(jit_state_t *_jit, jit_gpr_t a, jit_uimm_t b);
(define jit_bmci
  (let ((~jit_bmci
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bmci"
                   (list ffi-void* (list ffi:uint8) ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bmci) ~_jit ~a ~b))))))
(export jit_bmci)

;; extern jit_reloc_t jit_boaddr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_boaddr
  (let ((~jit_boaddr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_boaddr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_boaddr) ~_jit ~a ~b))))))
(export jit_boaddr)

;; extern jit_reloc_t jit_boaddi(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_boaddi
  (let ((~jit_boaddi
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_boaddi"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_boaddi) ~_jit ~a ~b))))))
(export jit_boaddi)

;; extern jit_reloc_t jit_boaddr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b)
;;     ;
(define jit_boaddr_u
  (let ((~jit_boaddr_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_boaddr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_boaddr_u) ~_jit ~a ~b))))))
(export jit_boaddr_u)

;; extern jit_reloc_t jit_boaddi_u(jit_state_t *_jit, jit_gpr_t a, jit_uimm_t b
;;     );
(define jit_boaddi_u
  (let ((~jit_boaddi_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_boaddi_u"
                   (list ffi-void* (list ffi:uint8) ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_boaddi_u) ~_jit ~a ~b))))))
(export jit_boaddi_u)

;; extern jit_reloc_t jit_bxaddr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bxaddr
  (let ((~jit_bxaddr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bxaddr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bxaddr) ~_jit ~a ~b))))))
(export jit_bxaddr)

;; extern jit_reloc_t jit_bxaddi(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_bxaddi
  (let ((~jit_bxaddi
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bxaddi"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bxaddi) ~_jit ~a ~b))))))
(export jit_bxaddi)

;; extern jit_reloc_t jit_bxaddr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b)
;;     ;
(define jit_bxaddr_u
  (let ((~jit_bxaddr_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bxaddr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bxaddr_u) ~_jit ~a ~b))))))
(export jit_bxaddr_u)

;; extern jit_reloc_t jit_bxaddi_u(jit_state_t *_jit, jit_gpr_t a, jit_uimm_t b
;;     );
(define jit_bxaddi_u
  (let ((~jit_bxaddi_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bxaddi_u"
                   (list ffi-void* (list ffi:uint8) ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bxaddi_u) ~_jit ~a ~b))))))
(export jit_bxaddi_u)

;; extern jit_reloc_t jit_bosubr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bosubr
  (let ((~jit_bosubr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bosubr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bosubr) ~_jit ~a ~b))))))
(export jit_bosubr)

;; extern jit_reloc_t jit_bosubi(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_bosubi
  (let ((~jit_bosubi
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bosubi"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bosubi) ~_jit ~a ~b))))))
(export jit_bosubi)

;; extern jit_reloc_t jit_bosubr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b)
;;     ;
(define jit_bosubr_u
  (let ((~jit_bosubr_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bosubr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bosubr_u) ~_jit ~a ~b))))))
(export jit_bosubr_u)

;; extern jit_reloc_t jit_bosubi_u(jit_state_t *_jit, jit_gpr_t a, jit_uimm_t b
;;     );
(define jit_bosubi_u
  (let ((~jit_bosubi_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bosubi_u"
                   (list ffi-void* (list ffi:uint8) ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bosubi_u) ~_jit ~a ~b))))))
(export jit_bosubi_u)

;; extern jit_reloc_t jit_bxsubr(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b);
(define jit_bxsubr
  (let ((~jit_bxsubr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bxsubr"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bxsubr) ~_jit ~a ~b))))))
(export jit_bxsubr)

;; extern jit_reloc_t jit_bxsubi(jit_state_t *_jit, jit_gpr_t a, jit_imm_t b);
(define jit_bxsubi
  (let ((~jit_bxsubi
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bxsubi"
                   (list ffi-void* (list ffi:uint8) ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bxsubi) ~_jit ~a ~b))))))
(export jit_bxsubi)

;; extern jit_reloc_t jit_bxsubr_u(jit_state_t *_jit, jit_gpr_t a, jit_gpr_t b)
;;     ;
(define jit_bxsubr_u
  (let ((~jit_bxsubr_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bxsubr_u"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bxsubr_u) ~_jit ~a ~b))))))
(export jit_bxsubr_u)

;; extern jit_reloc_t jit_bxsubi_u(jit_state_t *_jit, jit_gpr_t a, jit_uimm_t b
;;     );
(define jit_bxsubi_u
  (let ((~jit_bxsubi_u
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_bxsubi_u"
                   (list ffi-void* (list ffi:uint8) ffi:uintptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b (unwrap~fixed b)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_bxsubi_u) ~_jit ~a ~b))))))
(export jit_bxsubi_u)

;; extern void jit_jmpr(jit_state_t *_jit, jit_gpr_t a);
(define jit_jmpr
  (let ((~jit_jmpr
          (delay (fh-link-proc
                   ffi:void
                   "jit_jmpr"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a)))
        ((force ~jit_jmpr) ~_jit ~a)))))
(export jit_jmpr)

;; extern void jit_jmpi(jit_state_t *_jit, jit_pointer_t a);
(define jit_jmpi
  (let ((~jit_jmpi
          (delay (fh-link-proc
                   ffi:void
                   "jit_jmpi"
                   (list ffi-void* ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_pointer_t) a)))
        ((force ~jit_jmpi) ~_jit ~a)))))
(export jit_jmpi)

;; extern jit_reloc_t jit_jmp(jit_state_t *_jit);
(define jit_jmp
  (let ((~jit_jmp
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_jmp"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit)))
        ((fht-wrap jit_reloc_t) ((force ~jit_jmp) ~_jit))))))
(export jit_jmp)

;; extern void jit_jmpi_with_link(jit_state_t *_jit, jit_pointer_t a);
(define jit_jmpi_with_link
  (let ((~jit_jmpi_with_link
          (delay (fh-link-proc
                   ffi:void
                   "jit_jmpi_with_link"
                   (list ffi-void* ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_pointer_t) a)))
        ((force ~jit_jmpi_with_link) ~_jit ~a)))))
(export jit_jmpi_with_link)

;; extern void jit_pop_link_register(jit_state_t *_jit);
(define jit_pop_link_register
  (let ((~jit_pop_link_register
          (delay (fh-link-proc
                   ffi:void
                   "jit_pop_link_register"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit)))
        ((force ~jit_pop_link_register) ~_jit)))))
(export jit_pop_link_register)

;; extern void jit_push_link_register(jit_state_t *_jit);
(define jit_push_link_register
  (let ((~jit_push_link_register
          (delay (fh-link-proc
                   ffi:void
                   "jit_push_link_register"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit)))
        ((force ~jit_push_link_register) ~_jit)))))
(export jit_push_link_register)

;; extern void jit_ret(jit_state_t *_jit);
(define jit_ret
  (let ((~jit_ret
          (delay (fh-link-proc
                   ffi:void
                   "jit_ret"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit)))
        ((force ~jit_ret) ~_jit)))))
(export jit_ret)

;; extern void jit_retr(jit_state_t *_jit, jit_gpr_t a);
(define jit_retr
  (let ((~jit_retr
          (delay (fh-link-proc
                   ffi:void
                   "jit_retr"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a)))
        ((force ~jit_retr) ~_jit ~a)))))
(export jit_retr)

;; extern void jit_retr_f(jit_state_t *_jit, jit_fpr_t a);
(define jit_retr_f
  (let ((~jit_retr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_retr_f"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a)))
        ((force ~jit_retr_f) ~_jit ~a)))))
(export jit_retr_f)

;; extern void jit_retr_d(jit_state_t *_jit, jit_fpr_t a);
(define jit_retr_d
  (let ((~jit_retr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_retr_d"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a)))
        ((force ~jit_retr_d) ~_jit ~a)))))
(export jit_retr_d)

;; extern void jit_reti(jit_state_t *_jit, jit_imm_t a);
(define jit_reti
  (let ((~jit_reti
          (delay (fh-link-proc
                   ffi:void
                   "jit_reti"
                   (list ffi-void* ffi:intptr_t)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a (unwrap~fixed a)))
        ((force ~jit_reti) ~_jit ~a)))))
(export jit_reti)

;; extern void jit_retval_c(jit_state_t *_jit, jit_gpr_t a);
(define jit_retval_c
  (let ((~jit_retval_c
          (delay (fh-link-proc
                   ffi:void
                   "jit_retval_c"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a)))
        ((force ~jit_retval_c) ~_jit ~a)))))
(export jit_retval_c)

;; extern void jit_retval_uc(jit_state_t *_jit, jit_gpr_t a);
(define jit_retval_uc
  (let ((~jit_retval_uc
          (delay (fh-link-proc
                   ffi:void
                   "jit_retval_uc"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a)))
        ((force ~jit_retval_uc) ~_jit ~a)))))
(export jit_retval_uc)

;; extern void jit_retval_s(jit_state_t *_jit, jit_gpr_t a);
(define jit_retval_s
  (let ((~jit_retval_s
          (delay (fh-link-proc
                   ffi:void
                   "jit_retval_s"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a)))
        ((force ~jit_retval_s) ~_jit ~a)))))
(export jit_retval_s)

;; extern void jit_retval_us(jit_state_t *_jit, jit_gpr_t a);
(define jit_retval_us
  (let ((~jit_retval_us
          (delay (fh-link-proc
                   ffi:void
                   "jit_retval_us"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a)))
        ((force ~jit_retval_us) ~_jit ~a)))))
(export jit_retval_us)

;; extern void jit_retval_i(jit_state_t *_jit, jit_gpr_t a);
(define jit_retval_i
  (let ((~jit_retval_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_retval_i"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a)))
        ((force ~jit_retval_i) ~_jit ~a)))))
(export jit_retval_i)

;; extern void jit_retval_ui(jit_state_t *_jit, jit_gpr_t a);
(define jit_retval_ui
  (let ((~jit_retval_ui
          (delay (fh-link-proc
                   ffi:void
                   "jit_retval_ui"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a)))
        ((force ~jit_retval_ui) ~_jit ~a)))))
(export jit_retval_ui)

;; extern void jit_retval_l(jit_state_t *_jit, jit_gpr_t a);
(define jit_retval_l
  (let ((~jit_retval_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_retval_l"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a)))
        ((force ~jit_retval_l) ~_jit ~a)))))
(export jit_retval_l)

;; extern void jit_retval_f(jit_state_t *_jit, jit_fpr_t a);
(define jit_retval_f
  (let ((~jit_retval_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_retval_f"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a)))
        ((force ~jit_retval_f) ~_jit ~a)))))
(export jit_retval_f)

;; extern void jit_retval_d(jit_state_t *_jit, jit_fpr_t a);
(define jit_retval_d
  (let ((~jit_retval_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_retval_d"
                   (list ffi-void* (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a)))
        ((force ~jit_retval_d) ~_jit ~a)))))
(export jit_retval_d)

;; extern void jit_breakpoint(jit_state_t *_jit);
(define jit_breakpoint
  (let ((~jit_breakpoint
          (delay (fh-link-proc
                   ffi:void
                   "jit_breakpoint"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit)))
        ((force ~jit_breakpoint) ~_jit)))))
(export jit_breakpoint)

;; extern void jit_negr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_negr_f
  (let ((~jit_negr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_negr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_negr_f) ~_jit ~a ~b)))))
(export jit_negr_f)

;; extern void jit_negr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_negr_d
  (let ((~jit_negr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_negr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_negr_d) ~_jit ~a ~b)))))
(export jit_negr_d)

;; extern void jit_absr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_absr_f
  (let ((~jit_absr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_absr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_absr_f) ~_jit ~a ~b)))))
(export jit_absr_f)

;; extern void jit_absr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_absr_d
  (let ((~jit_absr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_absr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_absr_d) ~_jit ~a ~b)))))
(export jit_absr_d)

;; extern void jit_sqrtr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_sqrtr_f
  (let ((~jit_sqrtr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_sqrtr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_sqrtr_f) ~_jit ~a ~b)))))
(export jit_sqrtr_f)

;; extern void jit_sqrtr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_sqrtr_d
  (let ((~jit_sqrtr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_sqrtr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_sqrtr_d) ~_jit ~a ~b)))))
(export jit_sqrtr_d)

;; extern void jit_truncr_f_i(jit_state_t *_jit, jit_gpr_t a, jit_fpr_t b);
(define jit_truncr_f_i
  (let ((~jit_truncr_f_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_truncr_f_i"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_truncr_f_i) ~_jit ~a ~b)))))
(export jit_truncr_f_i)

;; extern void jit_extr_f(jit_state_t *_jit, jit_fpr_t a, jit_gpr_t b);
(define jit_extr_f
  (let ((~jit_extr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_extr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_extr_f) ~_jit ~a ~b)))))
(export jit_extr_f)

;; extern void jit_extr_d(jit_state_t *_jit, jit_fpr_t a, jit_gpr_t b);
(define jit_extr_d
  (let ((~jit_extr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_extr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_gpr_t) b)))
        ((force ~jit_extr_d) ~_jit ~a ~b)))))
(export jit_extr_d)

;; extern void jit_extr_d_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_extr_d_f
  (let ((~jit_extr_d_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_extr_d_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_extr_d_f) ~_jit ~a ~b)))))
(export jit_extr_d_f)

;; extern void jit_extr_f_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_extr_f_d
  (let ((~jit_extr_f_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_extr_f_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_extr_f_d) ~_jit ~a ~b)))))
(export jit_extr_f_d)

;; extern void jit_movr_f(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_movr_f
  (let ((~jit_movr_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_movr_f"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_movr_f) ~_jit ~a ~b)))))
(export jit_movr_f)

;; extern void jit_movr_d(jit_state_t *_jit, jit_fpr_t a, jit_fpr_t b);
(define jit_movr_d
  (let ((~jit_movr_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_movr_d"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_movr_d) ~_jit ~a ~b)))))
(export jit_movr_d)

;; extern void jit_movi_f(jit_state_t *_jit, jit_fpr_t a, jit_float32_t b);
(define jit_movi_f
  (let ((~jit_movi_f
          (delay (fh-link-proc
                   ffi:void
                   "jit_movi_f"
                   (list ffi-void* (list ffi:uint8) ffi:float)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b (unwrap~float b)))
        ((force ~jit_movi_f) ~_jit ~a ~b)))))
(export jit_movi_f)

;; extern void jit_movi_d(jit_state_t *_jit, jit_fpr_t a, jit_float64_t b);
(define jit_movi_d
  (let ((~jit_movi_d
          (delay (fh-link-proc
                   ffi:void
                   "jit_movi_d"
                   (list ffi-void* (list ffi:uint8) ffi:double)
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_fpr_t) a))
            (~b (unwrap~float b)))
        ((force ~jit_movi_d) ~_jit ~a ~b)))))
(export jit_movi_d)

;; extern void jit_truncr_d_i(jit_state_t *_jit, jit_gpr_t a, jit_fpr_t b);
(define jit_truncr_d_i
  (let ((~jit_truncr_d_i
          (delay (fh-link-proc
                   ffi:void
                   "jit_truncr_d_i"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_truncr_d_i) ~_jit ~a ~b)))))
(export jit_truncr_d_i)

;; extern void jit_truncr_f_l(jit_state_t *_jit, jit_gpr_t a, jit_fpr_t b);
(define jit_truncr_f_l
  (let ((~jit_truncr_f_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_truncr_f_l"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_truncr_f_l) ~_jit ~a ~b)))))
(export jit_truncr_f_l)

;; extern void jit_truncr_d_l(jit_state_t *_jit, jit_gpr_t a, jit_fpr_t b);
(define jit_truncr_d_l
  (let ((~jit_truncr_d_l
          (delay (fh-link-proc
                   ffi:void
                   "jit_truncr_d_l"
                   (list ffi-void*
                         (list ffi:uint8)
                         (list ffi:uint8))
                   (force ffi-lightening-llibs)))))
    (lambda (_jit a b)
      (let ((~_jit ((fht-unwrap jit_state_t*) _jit))
            (~a ((fht-unwrap jit_gpr_t) a))
            (~b ((fht-unwrap jit_fpr_t) b)))
        ((force ~jit_truncr_d_l) ~_jit ~a ~b)))))
(export jit_truncr_d_l)

;; void jit_begin_data(jit_state_t *, size_t max_size_or_zero);
(define jit_begin_data
  (let ((~jit_begin_data
          (delay (fh-link-proc
                   ffi:void
                   "jit_begin_data"
                   (list ffi-void* ffi:size_t)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 max_size_or_zero)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~max_size_or_zero
              (unwrap~fixed max_size_or_zero)))
        ((force ~jit_begin_data) ~arg0 ~max_size_or_zero)))))
(export jit_begin_data)

;; void jit_end_data(jit_state_t *);
(define jit_end_data
  (let ((~jit_end_data
          (delay (fh-link-proc
                   ffi:void
                   "jit_end_data"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0)))
        ((force ~jit_end_data) ~arg0)))))
(export jit_end_data)

;; void jit_emit_u8(jit_state_t *, uint8_t);
(define jit_emit_u8
  (let ((~jit_emit_u8
          (delay (fh-link-proc
                   ffi:void
                   "jit_emit_u8"
                   (list ffi-void* ffi:uint8)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 arg1)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~arg1 (unwrap~fixed arg1)))
        ((force ~jit_emit_u8) ~arg0 ~arg1)))))
(export jit_emit_u8)

;; void jit_emit_u16(jit_state_t *, uint16_t);
(define jit_emit_u16
  (let ((~jit_emit_u16
          (delay (fh-link-proc
                   ffi:void
                   "jit_emit_u16"
                   (list ffi-void* ffi:uint16)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 arg1)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~arg1 (unwrap~fixed arg1)))
        ((force ~jit_emit_u16) ~arg0 ~arg1)))))
(export jit_emit_u16)

;; void jit_emit_u32(jit_state_t *, uint32_t);
(define jit_emit_u32
  (let ((~jit_emit_u32
          (delay (fh-link-proc
                   ffi:void
                   "jit_emit_u32"
                   (list ffi-void* ffi:uint32)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 arg1)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~arg1 (unwrap~fixed arg1)))
        ((force ~jit_emit_u32) ~arg0 ~arg1)))))
(export jit_emit_u32)

;; void jit_emit_u64(jit_state_t *, uint64_t);
(define jit_emit_u64
  (let ((~jit_emit_u64
          (delay (fh-link-proc
                   ffi:void
                   "jit_emit_u64"
                   (list ffi-void* ffi:uint64)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 arg1)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~arg1 (unwrap~fixed arg1)))
        ((force ~jit_emit_u64) ~arg0 ~arg1)))))
(export jit_emit_u64)

;; void jit_emit_ptr(jit_state_t *, void *);
(define jit_emit_ptr
  (let ((~jit_emit_ptr
          (delay (fh-link-proc
                   ffi:void
                   "jit_emit_ptr"
                   (list ffi-void* ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0 arg1)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0))
            (~arg1 (unwrap~pointer arg1)))
        ((force ~jit_emit_ptr) ~arg0 ~arg1)))))
(export jit_emit_ptr)

;; jit_reloc_t jit_emit_addr(jit_state_t *);
(define jit_emit_addr
  (let ((~jit_emit_addr
          (delay (fh-link-proc
                   (list ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint8
                         ffi:uint32)
                   "jit_emit_addr"
                   (list ffi-void*)
                   (force ffi-lightening-llibs)))))
    (lambda (arg0)
      (let ((~arg0 ((fht-unwrap jit_state_t*) arg0)))
        ((fht-wrap jit_reloc_t)
         ((force ~jit_emit_addr) ~arg0))))))
(export jit_emit_addr)

;; access to enum symbols and #define'd constants:
(define ffi-lightening-symbol-tab
  '((JIT_OPERAND_KIND_MEM . 3)
    (JIT_OPERAND_KIND_FPR . 2)
    (JIT_OPERAND_KIND_GPR . 1)
    (JIT_OPERAND_KIND_IMM . 0)
    (JIT_OPERAND_ABI_WORD . 7)
    (JIT_OPERAND_ABI_DOUBLE . 10)
    (JIT_OPERAND_ABI_FLOAT . 9)
    (JIT_OPERAND_ABI_POINTER . 8)
    (JIT_OPERAND_ABI_INT64 . 7)
    (JIT_OPERAND_ABI_UINT64 . 6)
    (JIT_OPERAND_ABI_INT32 . 5)
    (JIT_OPERAND_ABI_UINT32 . 4)
    (JIT_OPERAND_ABI_INT16 . 3)
    (JIT_OPERAND_ABI_UINT16 . 2)
    (JIT_OPERAND_ABI_INT8 . 1)
    (JIT_OPERAND_ABI_UINT8 . 0)
    (JIT_RELOC_FLAG_0 . 16)
    (JIT_RELOC_MASK . 15)
    (JIT_RELOC_REL64 . 4)
    (JIT_RELOC_REL32 . 3)
    (JIT_RELOC_REL16 . 2)
    (JIT_RELOC_REL8 . 1)
    (JIT_RELOC_ABSOLUTE . 0)))

(define ffi-lightening-symbol-val
  (lambda (k)
    (or (assq-ref ffi-lightening-symbol-tab k))))
(export ffi-lightening-symbol-val)

(define (unwrap-enum obj)
  (cond ((number? obj) obj)
        ((symbol? obj) (ffi-lightening-symbol-val obj))
        ((fh-object? obj) (struct-ref obj 0))
        (else (error "type mismatch"))))

(define ffi-lightening-types
  '("jit_pointer_t" "jit_addr_t" (struct . "jit_gpr") (pointer . "jit_gpr_t") 
    "jit_gpr_t" (struct . "jit_fpr") (pointer . "jit_fpr_t") "jit_fpr_t" 
    (struct . "jit_reloc") (pointer . "jit_reloc_t") "jit_reloc_t" (pointer . 
    "jit_state_t") "jit_state_t" (struct . "jit_operand") (pointer . 
    "jit_operand_t") "jit_operand_t" "jit_function_pointer_t"))
(export ffi-lightening-types)

;; --- last line ---
