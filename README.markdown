cl-mongo-id
===========

This is a simple library for creating/handling Mongo IDs (ObjectIDs).

Usage
-----
Create byte array from string id:

    (mongoid:oid "4f9638d834322b9531000005")  ->
        #(79 150 56 216 52 50 43 149 49 0 0 5)
    
If you pass a bad string id into `(oid)`, it will most likely throw a hex-conversion-error.

Create new id using [MongoDB ObjectID specification](http://www.mongodb.org/display/DOCS/Object+IDs):

    (mongoid:oid)  ->
        #(79 150 62 92 61 196 44 20 228 0 0 0)

Grab a string representation of an ObjectID:

    (mongid:oid-str (mongoid:oid))  ->
	    "4F97001C3DC42C14E4000050"

### Quicklisp
Please note that cl-mongo-id is *not* available via quicklisp yet. I'm working
on this. In the meantime, you can do the following:

    cd ~/quicklisp/local-projects
	git clone git://github.com/orthecreedence/cl-mongo-id.git

Now you should be able to do a `(ql:quickload :cl-mongo-id)` =].

Usage with cl-mongo
-------------------
The cl-mongo-id library can be used with [cl-mongo](https://github.com/fons/cl-mongo).
There is just one caveat: You must import the `make-bson-oid` function from the
cl-mongo package before you can use the two together.

    (import 'cl-mongo::make-bson-oid)
    
    (with-mongo-connection (:host "127.0.0.1" :db "test")
      ;; querying a document of a known id
      (car (docs (iter (db.find "mycoll" (kv "_id" (make-bson-oid :oid (mongoid:oid "4f9638d834322b9531000005")))))))
    
      ;; creating a new document with an oid
	  (let ((doc (make-document :oid (make-bson-oid :oid (mongoid:oid)))))
	    (db.save "mycoll" doc)))

Thread safe
-----------
cl-mongo-id is built to be thread-safe (specifically the inc value). This means
you can create ID's via `(oid)` from several different threads without problems,
and your inc value will be unique between all of them.

Helper functions
----------------
cl-mongo-id has some helper functions that may be useful to you for either 
debugging or grabbing values from your IDs. 

Note that all of the following helper functions take a keyword argument `:bytes`,
which if set to T will return the raw bytes of that portion of the ID and not
convert them to an integer.

### get-timestamp
Get the unix timestamp from a Mongo Object ID:

    (mongoid:get-timestamp (oid "4f96f9fa3dc42c14e400004e"))  ->
        1335294458

### get-pid
Get the PID the ID was created under:

    (mongoid:get-pid (oid "4f96f9fa3dc42c14e400004e"))  ->
        5348

### get-hostname
Get the int value corresponding to the first three bytes of the MD5 of the hostname the ID was created on:

    (mongoid:get-hostname (oid "4f96f9fa3dc42c14e400004e"))  ->
        4047916
    (mongoid:get-hostname (oid "4f96f9fa3dc42c14e400004e") :bytes t)  ->
        #(61 196 44)

### get-inc
Get the ID's "inc" value:

    (mongoid:get-inc (oid "4f96f9fa3dc42c14e400004e"))  ->
        78

