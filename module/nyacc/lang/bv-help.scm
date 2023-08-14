;;; nyacc/lang/bv-help.scm - nicknames for bytevector access routines

;; Copyright (C) 2023 Matthew Wette
;;
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Notes:

;;; Code:

(define-module (nyacc lang bv-help)
  #:export (
            bv-i8-ref bv-i8-set!
            bv-i16-ref bv-i16-set!
            bv-i32-ref bv-i32-set!
            bv-i64-ref bv-i64-set!
            bv-f32-ref bv-f32-set!
            bv-f64-ref bv-f64-set!

            bv-i8le-ref bv-i8le-set!
            bv-i16le-ref bv-i16le-set!
            bv-i32le-ref bv-i32le-set!
            bv-i64le-ref bv-i64le-set!
            bv-f32le-ref bv-f32le-set!
            bv-f64le-ref bv-f64le-set!

            bv-i8be-ref bv-i8be-set!
            bv-i16be-ref bv-i16be-set!
            bv-i32be-ref bv-i32be-set!
            bv-i64be-ref bv-i64be-set!
            bv-f32be-ref bv-f32be-set!
            bv-f64be-ref bv-f64be-set!

            bv-le-ref-map bv-le-set-map
            bv-be-ref-map bv-be-set-map
            lkup-bv-le-getter lkup-bv-le-setter
            lkup-bv-be-getter lkup-bv-le-setter
            )

  #:use-module (rnrs bytevectors))

(define bv-i8-ref bytevector-s8-ref)
(define bv-i8-set! bytevector-s8-set!)
(define bv-u8-ref bytevector-u8-ref)
(define bv-u8-set! bytevector-u8-set!)
(define bv-i16-ref bytevector-s16-native-ref)
(define bv-i16-set! bytevector-s16-native-set!)
(define bv-u16-ref bytevector-u16-native-ref)
(define bv-u16-set! bytevector-u16-native-set!)
(define bv-i32-ref bytevector-s32-native-ref)
(define bv-i32-set! bytevector-s32-native-set!)
(define bv-u32-ref bytevector-u32-native-ref)
(define bv-u32-set! bytevector-u32-native-set!)
(define bv-i64-ref bytevector-s64-native-ref)
(define bv-i64-set! bytevector-s64-native-set!)
(define bv-u64-ref bytevector-u64-native-ref)
(define bv-u64-set! bytevector-u64-native-set!)
(define bv-f32-ref bytevector-ieee-single-native-ref)
(define bv-f32-set! bytevector-ieee-single-native-set!)
(define bv-f64-ref bytevector-ieee-double-native-ref)
(define bv-f64-set! bytevector-ieee-double-native-set!)

(define bv-i8le-ref bytevector-s8-ref)
(define bv-i8le-set! bytevector-s8-set!)
(define bv-u8le-ref bytevector-u8-ref)
(define bv-u8le-set! bytevector-u8-set!)
(define (bv-i16le-ref bv ix) (bytevector-s16-ref bv ix 'little))
(define (bv-i16le-set! bv ix v) (bytevector-s16-set! bv ix v 'little))
(define (bv-u16le-ref bv ix) (bytevector-u16-ref bv ix 'little))
(define (bv-u16le-set! bv ix v) (bytevector-u16-set! bv ix v 'little))
(define (bv-i32le-ref bv ix) (bytevector-s32-ref bv ix 'little))
(define (bv-i32le-set! bv ix v) (bytevector-s32-set! bv ix v 'little))
(define (bv-u32le-ref bv ix) (bytevector-u32-ref bv ix 'little))
(define (bv-u32le-set! bv ix v) (bytevector-u32-set! bv ix v 'little))
(define (bv-i64le-ref bv ix) (bytevector-s64-ref bv ix 'little))
(define (bv-i64le-set! bv ix v) (bytevector-s64-set! bv ix v 'little))
(define (bv-u64le-ref bv ix) (bytevector-u64-ref bv ix 'little))
(define (bv-u64le-set! bv ix v) (bytevector-u64-set! bv ix v 'little))
(define (bv-f32le-ref bv ix) (bytevector-ieee-single-ref bv ix 'little))
(define (bv-f32le-set! bv ix v) (bytevector-ieee-single-set! bv ix v 'little))
(define (bv-f64le-ref bv ix) (bytevector-ieee-double-ref bv ix 'little))
(define (bv-f64le-set! bv ix v) (bytevector-ieee-double-set! bv ix v 'little))

(define bv-le-ref-map
  `((i8 . ,bv-i8-ref) (u8 . ,bv-u8-ref)
    (i16 . ,bv-i16le-ref) (u16 . ,bv-u16le-ref)
    (i32 . ,bv-i32le-ref) (u32 . ,bv-u32le-ref)
    (i64 . ,bv-i64le-ref) (u64 . ,bv-u64le-ref)
    (f32 . ,bv-f32le-ref)
    (f64 . ,bv-f64le-ref)))

(define bv-le-set-map
  `((i8 . ,bv-i8-set!) (u8 . ,bv-u8-set!)
    (i16 . ,bv-i16le-set!) (u16 . ,bv-u16le-set!)
    (i32 . ,bv-i32le-set!) (u32 . ,bv-u32le-set!)
    (i64 . ,bv-i64le-set!) (u64 . ,bv-u64le-set!)
    (f32 . ,bv-f32le-set!)
    (f64 . ,bv-f64le-set!)))

(define bv-i8be-ref bytevector-s8-ref)
(define bv-i8be-set! bytevector-s8-set!)
(define bv-u8be-ref bytevector-u8-ref)
(define bv-u8be-set! bytevector-u8-set!)
(define (bv-i16be-ref bv ix) (bytevector-s16-ref bv ix 'big))
(define (bv-i16be-set! bv ix v) (bytevector-s16-set! bv ix v 'big))
(define (bv-u16be-ref bv ix) (bytevector-u16-ref bv ix 'big))
(define (bv-u16be-set! bv ix v) (bytevector-u16-set! bv ix v 'big))
(define (bv-i32be-ref bv ix) (bytevector-s32-ref bv ix 'big))
(define (bv-i32be-set! bv ix v) (bytevector-s32-set! bv ix v 'big))
(define (bv-u32be-ref bv ix) (bytevector-u32-ref bv ix 'big))
(define (bv-u32be-set! bv ix v) (bytevector-u32-set! bv ix v 'big))
(define (bv-i64be-ref bv ix) (bytevector-s64-ref bv ix 'big))
(define (bv-i64be-set! bv ix v) (bytevector-s64-set! bv ix v 'big))
(define (bv-u64be-ref bv ix) (bytevector-u64-ref bv ix 'big))
(define (bv-u64be-set! bv ix v) (bytevector-u64-set! bv ix v 'big))
(define (bv-f32be-ref bv ix) (bytevector-ieee-single-ref bv ix 'big))
(define (bv-f32be-set! bv ix v) (bytevector-ieee-single-set! bv ix v 'big))
(define (bv-f64be-ref bv ix) (bytevector-ieee-double-ref bv ix 'big))
(define (bv-f64be-set! bv ix v) (bytevector-ieee-double-set! bv ix v 'big))

(define bv-be-ref-map
  `((i8 . ,bv-i8-ref) (u8 . ,bv-u8-ref)
    (i16 . ,bv-i16be-ref) (u16 . ,bv-u16be-ref)
    (i32 . ,bv-i32be-ref) (u32 . ,bv-u32be-ref)
    (i64 . ,bv-i64be-ref) (u64 . ,bv-u64be-ref)
    (f32 . ,bv-f32be-ref) (f64 . ,bv-f64be-ref)))

(define bv-be-set-map
  `((i8 . ,bv-i8-set!) (u8 . ,bv-u8-set!)
    (i16 . ,bv-i16be-set!) (u16 . ,bv-u16be-set!)
    (i32 . ,bv-i32be-set!) (u32 . ,bv-u32be-set!)
    (i64 . ,bv-i64be-set!) (u64 . ,bv-u64be-set!)
    (f32 . ,bv-f32be-set!) (f64 . ,bv-f64be-set!)))

(define (lkup-bv-le-getter type)  (assoc-ref bv-le-ref-map type))
(define (lkup-bv-le-setter type)  (assoc-ref bv-le-set-map type))
(define (lkup-bv-be-getter type)  (assoc-ref bv-be-ref-map type))
(define (lkup-bv-be-setter type)  (assoc-ref bv-be-set-map type))

;; ---last line ---
