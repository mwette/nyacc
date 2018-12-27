;; nyacc/lang/ffi-help/libssh-01.scm - not finished (i.e., not working)

;; Copyright (C) 2018 Matthew R. Wette

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Notes:

;; see http://api.libssh.org/stable/libssh_tutorial.html
;; I have version 0.6.3, so the tutorial does not work as it is for v0.8.X.
;; (And why does Ubuntu not upgrade this for 16.04?)

;;; Code:

(use-modules (ffi libssh))
(use-modules (system ffi-help-rt))

(define sess (ssh_new))
(define hash (make-char*))
(define hlen (make-size_t))
(define key (make-ssh_key))

(ssh_options_set sess 'SSH_OPTIONS_HOST "localhost")
;;(ssh_options_set sess 'SSH_OPTIONS_USER "root")

(define conn (ssh_connect sess))

(let* ((state (ssh_is_server_known sess))
       (hlen (ssh_get_pubkey_hash sess (pointer-to hash))))
  (if (negative? hlen) (error "no hash"))
  (case state
    ((SSH_SERVER_KNOWN_OK) #t)
    ;;((SSH_SERVER_KNOWN_CHANGED) #t)
    ;;((SSH_SERVER_FOUND_OTHER) #t)
    ;;((SSH_SERVER_FILE_NOT_FOUND) #f)
    ;;((SSH_SERVER_NOT_KNOWN) #f)
    ;;((SSH_SERVER_ERROR) #f)
    (else (error "nope")))
  ;;(free hash)
  #t)

(define chan (ssh_channel_new sess))
(if (zero? (fh-object-ref chan)) (error "nope"))

(let ((rc (ssh_channel_request_exec "ps aux")))
  #t)

(let* ((buffer (make-bytevector 128))
       (&buffer (bytevector->pointer buffer))
       )
  (let loop ((n (ssh_channel_read chan &buffer 128 0)))
    (when (positive? n)
      ;; write buffer
      (loop (ssh_channel_read chan &buffer 128 0))))
  #f)

(ssh_channel_close chan)
(ssh_channel_free chan)

(ssh_disconnect sess)
(ssh_free sess)


;; (foo k)
;; (bar k)
;; (baz k)

;; (foo ((bar) z))

;; --- last line ---
