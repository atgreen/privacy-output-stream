;;; privacy-output-stream
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2023  Anthony Green <green@moxielogic.com>

(in-package :privacy-output-stream)

(defclass privacy-output-stream (fundamental-character-output-stream)
  ((stream :initarg :stream :reader stream-of)
   (secrets :initarg :secrets :reader secrets :initform nil)))

(defmethod stream-element-type ((stream privacy-output-stream))
  (stream-element-type (stream-of stream)))

(defmethod close ((stream privacy-output-stream) &key abort)
  (close (stream-of stream) :abort abort))

(defmethod stream-force-output ((stream privacy-output-stream))
  (force-output (stream-of stream)))

(defmethod stream-write-char ((stream privacy-output-stream)
                              char)
  (write-char char (stream-of stream)))

(defmethod stream-write-string ((stream privacy-output-stream)
                                string &optional start end)
  (dolist (secret (secrets stream))
    (setf string (with-output-to-string (out)
                   (loop with start = 0
                         for pos = (search secret string :start2 start)
                         do (write-string string out :start start :end pos)
                         when pos do (dotimes (x (length secret)) (write-char #\* out))
                           while pos do (setf start (+ pos (length secret)))))))
  (write-string string (stream-of stream) :start start :end end))
