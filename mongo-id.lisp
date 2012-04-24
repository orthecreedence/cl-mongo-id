(defpackage :cl-mongo-id
  (:use :cl)
  (:export :oid :oid-str)
  (:nicknames :mongo-id))
(in-package :cl-mongo-id)

(defvar *id-inc* 0)
(defvar *id-inc-lock* (bt:make-lock))

(defvar *hostname* nil)
(defvar *hostname-lock* (bt:make-lock))

(defun oid (&optional id)
  "Generate a mongo id."
  (cond
    ((stringp id) (convert-hex-vector id))
    ((vectorp id) id)
    ((null id)    (create-new-id))))

(defun oid-str (id &key downcase)
  "Given a vector ID, convert it to a string."
  (let ((str ""))
    (loop for byte across id do
      (setf str (concatenate 'string str (format nil "~2,'0X" byte))))
    (if downcase
        (string-downcase str)
        str)))

(define-condition hex-conversion-error (error) ()
  (:report (lambda (o s)
             (format s "Conversion of hex string to byte array failed: ~a" o))))

(defun convert-hex-vector (str)
  "Takes a hex string, IE 4f2b8096 and converts it into a byte array:
      4f2b8096 -> #(79 43 128 150)
  Hex string *must* have even number of bytes."
  (when (oddp (length str))
    (error 'hex-conversion-error :text (format nil "Odd-length string given in hex conversion: ~a" str)))
  (let ((vec (make-array 0 :fill-pointer t :adjustable t)))
    (loop for i from 0 to (1- (/ (length str) 2)) do
      (vector-push-extend (read-from-string (format nil "#x~a" (subseq str (* 2 i) (* 2 (1+ i)))) :base 10) vec))
    vec))

(defun create-new-id ()
  "Create a brand-spankin-new ObjectId using the current timestamp/inc values,
  along with hostname and process pid."
  (declare (optimize (speed 3) (safety 1)))
  (let ((hostname (get-hostname))
        (pid (logand #xFFFF (get-current-pid)))
        (timestamp (logand #xFFFFFFFF (get-current-timestamp)))
        (inc (logand #xFFFFFF (get-inc-val))))
    (let ((hostname-bytes (subseq (md5:md5sum-sequence hostname) 0 3))
          (pid-bytes (convert-hex-vector (format nil "~4,'0X" pid)))
          (timestamp-bytes (convert-hex-vector (format nil "~8,'0X" timestamp)))
          (inc-bytes (convert-hex-vector (format nil "~6,'0X" inc))))
      (concatenate 'vector timestamp-bytes hostname-bytes pid-bytes inc-bytes))))

(defun get-hostname ()
  "Get hostname of machine (and cache it)."
  (declare (optimize (speed 3) (safety 1)))
  (bt:with-lock-held (*hostname-lock*)
    (if *hostname*
        *hostname*
        (let ((host (machine-instance)))
          (setf *hostname* (subseq host 0 (position #\  host)))
          *hostname*))))

(defun get-current-timestamp ()
  "Get current unix timestamp."
  ;; 2208988800 == (encode-universal-time 0 0 0 1 1 1970 0)
  (- (get-universal-time) 2208988800))

(defun get-inc-val ()
  "Thread-safe method to get current ObjectId inc value. Takes an optional
  timestamp value to calculate inc for."
  (declare (optimize (speed 3) (safety 1)))
  (bt:with-lock-held (*id-inc-lock*)
    (setf *id-inc* (logand #xFFFFFF (1+ *id-inc*)))
    *id-inc*))

(defun get-current-pid ()
  "Get the current process' PID. This function does it's best to be cross-
  implementation."
  #+clisp
  (system::process-id)
  #+(and lispworks unix)
  (system::getpid)
  #+(and sbcl unix)
  (sb-unix:unix-getpid)
  #+(and cmu unix)
  (unix:unix-getpid)
  #+openmcl
  (ccl::getpid))

