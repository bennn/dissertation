#lang typed/racket
;; guile-jpeg
;; Copyright (C) 2014 Andy Wingo <wingo at pobox dot com>

;; This library is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A parser for JPEG.
;;
;;; Code:

(require
  require-typed-check
  "../base/typedefs.rkt"
  (only-in "../base/math/array.rkt" Array)
  (only-in racket/file file->bytes))

(require/typed/check "jfif.rkt"
  (#:struct jfif ((frame : frame) (misc-segments : (Listof misc)) (mcu-array : (Array MCU))))
  (#:struct frame
    ((marker : Natural)
     (precision : Byte)
     (y : Natural)
     (x : Natural)
     (components : (Vectorof component))
     (samp-x : Natural)
     (samp-y : Natural)))
  (#:struct component
    ((id : Byte)
     (index : Natural)
     (samp-x : Natural)
     (samp-y : Natural)
     (q-table : Natural)))
  (#:struct misc
    ((marker : Natural)
     (bytes : Bytes)))
  (#:struct params
    ((q-tables : QT*)
     (dc-tables : H*)
     (ac-tables : H*)
     (restart-interval : Natural)
     (misc-segments : (Listof misc))))
  (read-jfif (->* [(U String Bytes Input-Port)] [#:with-body? Boolean #:with-misc-sections? Boolean] jfif))
  (write-jfif (-> (U String Output-Port) jfif Void)))

(require/typed/check "exif.rkt"
  (parse-exif (-> Bytes (Listof PTs))))

(define-type Jpeg jfif)

(: jpeg-dimensions (-> Jpeg (Values Natural Natural)))
(define (jpeg-dimensions jpeg)
  (let* ((jfif (if (jfif? jpeg)
                   jpeg
                   (read-jfif jpeg #:with-body? #f #:with-misc-sections? #f)))
         (frame (jfif-frame jfif)))
    (values (frame-x frame)
            (frame-y frame))))

(: read-jpeg (-> (U Bytes String Input-Port) Jpeg))
(define (read-jpeg jpeg)
  (read-jfif jpeg))

(: write-jpeg (-> (U String Output-Port) Jpeg Void))
(define (write-jpeg port jpeg)
  (write-jfif port jpeg))

(: find-exif (-> (Listof misc) Any))
(define (find-exif misc-segments)
  (: bv-prefix? (-> Bytes Bytes Boolean))
  (define (bv-prefix? prefix bv)
    (and (>= (bytes-length bv) (bytes-length prefix))
         (let lp ((n 0))
           (or (= n (bytes-length prefix))
               (and (eqv? (bytes-ref prefix n) (bytes-ref bv n))
                    (lp (add1 n)))))))
  (filter-map (lambda ((misc : misc))
                (and (= (misc-marker misc) #xffe1) ; APP1
                     (bv-prefix? #"Exif\0\0" (misc-bytes misc))
                     (parse-exif (subbytes (misc-bytes misc) 6))))
              misc-segments))

(: jpeg-dimensions-and-exif (-> Jpeg (Values Any Any Any)))
(define (jpeg-dimensions-and-exif jpeg)
  (let* ((jfif (if (jfif? jpeg)
                   jpeg
                   (read-jfif jpeg #:with-body? #f)))
         (frame (jfif-frame jfif)))
    (values (frame-x frame)
            (frame-y frame)
            (match (find-exif (jfif-misc-segments jfif))
              ((list (list main thumbnail)) main)
              ((list (list main)) main)
              (_ '())))))

;(define (jpeg->rgb in
;                   #:argb? (argb? #f)
;                   #:stride-for-width (stride-for-width
;                                       (lambda (width)
;                                         (* width (if argb? 4 3)))))
;  (let ((jfif (if (jfif? in) in (read-jfif in))))
;    (yuv->rgb (jpeg->planar-image jfif)
;              #:argb? argb?
;              #:stride (stride-for-width (frame-x (jfif-frame jfif))))))
;
;(define (rgb->jpeg rgb #:samp-x (samp-x 2) #:samp-y (samp-y 2)
;                   #:quality (quality 85))
;  (planar-image->jpeg (rgb->yuv rgb #:samp-x samp-x #:samp-y samp-y)
;                      #:quality quality))

;; -----------------------------------------------------------------------------

(: main (-> Bytes Void))
(define (main bytes)
  (define outb (open-output-bytes))
  (define j1 (read-jpeg bytes))
  (void
    ;;; TODO keep this? ;;; (rgb->jpeg (jpeg->rgb j1))
    (let-values (((_a _b _c) (jpeg-dimensions-and-exif j1))) (void))
    (write-jpeg outb j1)
    (close-output-port outb)))

(define bytes (file->bytes "../base/test.jpg"))

(time (main bytes))
