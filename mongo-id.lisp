;;; This is a library for generating MongoDB ObjectIDs per the client
;;; specification:
;;;
;;;   http://www.mongodb.org/display/DOCS/Object+IDs
;;;
;;; Full documentation on github:
;;;
;;;   https://github.com/orthecreedence/cl-mongo-id
;;;
;;; Please enjoy irresponsibly =].
;;;
;;; Andrew Lyon <orthecreedence@gmail.com>

(defpackage :cl-mongo-id
  (:use :cl)
  (:export :oid
           :oid-str
           :get-timestamp
           :get-hostname
           :get-pid
           :get-inc)
  (:nicknames :mongoid))
(in-package :cl-mongo-id)

(defvar *id-inc* (secure-random:number (ash 1 24)))

(defun oid (&optional id)
  "Generate a mongo id, in byte vector format."
  (cond
    ((stringp id) (convert-hex-vector id))
    ((vectorp id) id)
    ((null id)    (create-new-id))))

(defun oid-str (oid)
  "Given a vector ID, convert it to a string."
  (let ((hex-string (make-string 24)))
    (loop for byte across oid
          for i from 0 by 2 do
      (let ((byte-hex (format nil "~2,'0X" byte)))
        (setf (aref hex-string i) (aref byte-hex 0)
              (aref hex-string (1+ i)) (aref byte-hex 1))))
    hex-string))

(defun get-timestamp (oid &key bytes)
  "Grab the timestamp out of a vector oid. Passing :bytes t will return an array
  of bytes corresponding to the timestamp part of the ID instead of parsing it
  as an integer."
  (let ((timestamp (subseq oid 0 4)))
    (if bytes
        timestamp
        (convert-vector-int timestamp))))

(defun get-hostname (oid &key bytes)
  "Grab the hostname int out of a vector oid. Passing :bytes t will return an
  array of bytes corresponding to the hostname part of the ID instead of parsing
  it as an integer."
  (let ((host (subseq oid 4 7)))
    (if bytes
        host
        (convert-vector-int host))))

(defun get-pid (oid &key bytes)
  "Grab the pid out of a vector oid. Passing :bytes t will return an array of
  bytes corresponding to the PID part of the ID instead of parsing it as an
  integer."
  (let ((pid (subseq oid 7 9)))
    (if bytes
        pid
        (convert-vector-int pid))))

(defun get-inc (oid &key bytes)
  "Grab the inc value out of a vector oid. Passing :bytes t will return an array
  of bytes corresponding to the inc value of the ID instead of parsing it as an
  integer."
  (let ((inc (subseq oid 9)))
    (if bytes
        inc
        (convert-vector-int inc))))

(defun convert-hex-vector (hex-string)
  "Takes a hex string, IE 4f2b8096 and converts it into a byte array:
      4f2b8096 -> #(79 43 128 150)
  Hex string *must* have even number of bytes."
  (assert (evenp (length hex-string)))
  (let* ((octet-count (floor (length hex-string) 2))
         (vector (make-array octet-count :element-type '(unsigned-byte 8))))
    (loop for i below octet-count
          for j from 0 by 2
          for k from 2 by 2 do
      (setf (aref vector i)
            (parse-integer hex-string :start j :end k :radix 16)))
    vector))

(defun convert-vector-int (vector)
  "Convert a byte array to an integer:
      #(79 150 243 81) -> 1335292753"
  (reduce (lambda (x y)
            (+ (ash x 8) y))
          vector))

(defun create-new-id ()
  "Create a brand-spankin-new ObjectId using the current timestamp/inc values,
  along with hostname and process pid."
  (let ((hostname (get-current-hostname))
        (pid (logand #xFFFF (get-current-pid)))
        (timestamp (logand #xFFFFFFFF (get-current-timestamp)))
        (inc (get-inc-val)))
    (let ((hostname-bytes (subseq (md5:md5sum-sequence hostname) 0 3))
          (pid-bytes (convert-hex-vector (format nil "~4,'0X" pid)))
          (timestamp-bytes (convert-hex-vector (format nil "~8,'0X" timestamp)))
          (inc-bytes (convert-hex-vector (format nil "~6,'0X" inc))))
      (concatenate 'vector timestamp-bytes hostname-bytes pid-bytes inc-bytes))))

(defun get-current-hostname ()
  "Get hostname of machine."
  (machine-instance))

(defun get-current-timestamp ()
  "Get current unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))

(defun get-inc-val ()
  "Thread-safe method to get current ObjectId inc value. Takes an optional
  timestamp value to calculate inc for."
  (logand #xFFFFFF
          (atomics:atomic-incf *id-inc*)))

(defun get-current-pid (&key if-not-exists-return)
  "Get the current process' PID. This function does it's best to be cross-
   implementation. If it isn't able to grab the PID from the system, it defaults
   to returning whatever value is passed into the :if-not-exists-return key."
  #+clisp
  (system::process-id)
  #+(and lispworks unix)
  (system::getpid)
  #+(and sbcl unix)
  (sb-unix:unix-getpid)
  #+(and cmu unix)
  (unix:unix-getpid)
  #+openmcl
  (ccl::getpid)
  #+ecl
  (ext:getpid)
  #-(or clisp (and lispworks unix) (and sbcl unix) (and cmu unix) (and openmcl unix) openmcl ecl)
  if-not-exists-return)
