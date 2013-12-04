(module libzip *

(import chicken scheme srfi-1 foreign posix lolevel extras traversal)
(use traversal bind easyffi lolevel define-structure scheme2c-compatibility)

#>
#include <errno.h>
#include "zip.h"
<#

(bind-rename/pattern "_" "-")
(bind-rename/pattern "make-zip-stat" "create-zip-stat")

(bind "typedef int time_t;")
(bind "typedef short int16_t;")
(bind "typedef uint64_t zip_uint64_t;")
(bind "typedef int64_t zip_int64_t;")
(bind "typedef uint32_t zip_uint32_t;")
(bind "typedef int32_t zip_int32_t;")
(bind "typedef unsigned short zip_uint16_t;")
(bind "typedef short zip_int16_t;")

(bind "
struct zip_stat {
    zip_uint64_t valid;			/* which fields have valid values */
    const char *name;			/* name of the file */
    zip_uint64_t index;			/* index within archive */
    zip_uint64_t size;			/* size of file (uncompressed) */
    zip_uint64_t comp_size;		/* size of file (compressed) */
    time_t mtime;			/* modification time */
    zip_uint32_t crc;			/* crc of file data */
    zip_uint16_t comp_method;		/* compression method used */
    zip_uint16_t encryption_method;	/* encryption method used */
    zip_uint32_t flags;			/* reserved for future use */
};
")

(define-structure zip-file-stat filename index size compressed-size)
(define-structure zip-archive handle)
(define-foreign-type zip c-pointer zip-archive-handle make-zip-archive)
(define-structure zip-source handle)
(define-foreign-type zip-source c-pointer zip-source-handle make-zip-source)
(define-structure zip-file handle)
(define-foreign-type zip-file c-pointer zip-file-handle make-zip-file)

(define (zip:read-file zip name-or-index)
 (lines (c-string->string (vector-ref (zip:read-file-to-buffer zip name-or-index) 0))))

(define (zip:read-binary-file zip name-or-index)
 (c-string->string (vector-ref (zip:read-file-to-buffer zip name-or-index) 0)))

(define (zip:read-file-to-buffer zip name-or-index)
 (let* ((index (zip:index zip name-or-index))
	(stat (zip:stat-index zip index))
	(zip-file (zip:fopen-index zip index))
	(data (zip:fread-to-buffer zip-file (zip-file-stat-size stat))))
  (zip:fclose zip-file)
  (vector data (zip-file-stat-size stat))))

(define (zip:get-archive-comment archive)
 ((c-function c-string ("zip_get_archive_comment" zip c-pointer int))
  archive #f 0))

;; only in the new libzip
;; (define (zip:set-archive-comment zip string)
;;  (when
;;    (= -1 ((c-function int ("zip_set_archive_comment" zip c-string int))
;; 	  zip string (string-length string)))
;;   (fuck-up)))

(define zip:error-clear (c-function void ("zip_error_clear" zip)))

(define (zip:error-get zip)
 (with-alloc
  c-sizeof-int
  (lambda (zip-error-ptr)
   (with-alloc
    c-sizeof-int
    (lambda (system-error-ptr)
     ((c-function void ("zip_error_get" c-pointer c-pointer c-pointer))
      zip zip-error-ptr system-error-ptr)
     (cons (c-int-ref zip-error-ptr 0) (c-int-ref system-error-ptr 0)))))))

(define (zip:delete zip name-or-index)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function int ("zip_delete" zip integer64)) zip
    (zip:index zip name-or-index)))
  zip))

(define (zip:rename zip name-or-index destination-name)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function int ("zip_rename" zip integer64 c-string)) zip
    (zip:index zip name-or-index) destination-name))
  zip))

;; TODO zip:write-* which either adds or replaces

(define (zip:add-file-from-buffer zip name buffer size free?)
 (zip:add-source zip name (zip:source-buffer zip buffer size free?)))
(define (zip:add-file-from-pathname zip name source-name)
 (zip:add-source zip name (zip:source-file zip source-name)))
(define (zip:add-file-from-zip zip zip-source name source-index)
 (zip:add-source zip name (zip:source-zip zip zip-source source-index)))
(define (zip:add-file zip name strings)
 (let* ((string (unlines strings))
	(buffer (malloc (string-length string))))
  ((foreign-lambda* void ((c-pointer ptr) (c-string str) (int size))
                    "memcpy(ptr,str,size);")
   buffer string (string-length string))
  (zip:add-source
   zip name
   (zip:source-buffer zip buffer (string-length string) #t))))

(define (zip:replace-file-from-buffer zip name buffer size free?)
 (zip:replace-source zip name (zip:source-buffer zip buffer size free?)))
(define (zip:replace-file-from-pathname zip name source-name)
 (zip:replace-source zip name (zip:source-file zip source-name)))
(define (zip:replace-file-from-zip zip zip-source name source-index)
 (zip:replace-source zip name (zip:source-zip zip zip-source source-index)))
(define (zip:replace-file zip name strings)
 (let ((string (unlines strings)))
  (zip:replace-source
   zip name
   (zip:source-buffer zip string (string-length string) #t))))

(define (zip:add-directory zip name)
 ;; TODO should return #F when already exists
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function integer64 ("zip_add_dir" zip c-string)) zip name))
  zip))

(define (zip:ls zip)
 (map-n (lambda (n) (zip:get-name zip n)) (zip:get-num-entries zip)))

(define *zip:mode-create-if-necessary* (c-value int "ZIP_CREATE"))
(define *zip:mode-open* 0)
(define *zip:mode-create-new* (bit-or (c-value int "ZIP_CREATE") (c-value int "ZIP_EXCL")))
(define *zip:mode-extra-checks* (c-value int "ZIP_CHECKCONS"))

(define (zip:open filename mode)
 (with-alloc
  c-sizeof-int
  (lambda (error-ptr)
   (let ((zip ((c-function c-pointer ("zip_open" c-string int c-pointer))
	       filename mode error-ptr))
	 (errno (c-value int "errno")))
    (unless zip
     (let ((buffer-size
	    (+ (zip:error-to-str #f 0 (c-int-ref error-ptr 0) errno) 1)))
      (with-alloc
       buffer-size
       (lambda (buffer)
	(zip:error-to-str buffer buffer-size (c-int-ref error-ptr 0) errno)
	(panic "zip-open error ~a ~a~%" filename (c-string->string buffer))))))
    (make-zip-archive zip)))))

(define (zip:close zip)
 (zip:with-fail-on-error!
  (lambda (zip)
   ;; TODO zip_close doesn't free up the memory if it fails
   ((c-function int ("zip_close" zip)) zip))
  zip)
 (set-zip-archive-handle! zip #f)
 zip)

(define (zip:stat zip name-or-index)
 (zip:stat-index zip (zip:index zip name-or-index)))

;;; zip low level API

;; flags for zip_open, ORed together
(define zip:const-create (c-value int "ZIP_CREATE"))
(define zip:const-excl (c-value int "ZIP_EXCL"))
(define zip:const-checkcons (c-value int "ZIP_CHECKCONS"))
;; flags for zip_name_locate, zip_fopen, zip_stat, ...
(define zip:const-fl-nocase (c-value int "ZIP_FL_NOCASE"))
(define zip:const-fl-nodir (c-value int "ZIP_FL_NODIR"))
(define zip:const-fl-compressed (c-value int "ZIP_FL_COMPRESSED"))
(define zip:const-fl-unchanged (c-value int "ZIP_FL_UNCHANGED"))
(define zip:const-fl-recompress (c-value int "ZIP_FL_RECOMPRESS"))
;; only in the new libzip
;; (define zip:const-fl-encrypted (c-value int "ZIP_FL_ENCRYPTED"))
;; archive global flags flags
(define zip:const-afl-torrent (c-value int "ZIP_AFL_TORRENT"))
;; only in the new libzip
;; (define zip:const-afl-rdonly (c-value int "ZIP_AFL_RDONLY"))
;; flags for compression and encryption sources
;; only in the new libzip
;; (define zip:const-codec-encode (c-value int "ZIP_CODEC_ENCODE"))
;; libzip error codes
(define zip:const-er-ok (c-value int "ZIP_ER_OK"))
(define zip:const-er-multidisk (c-value int "ZIP_ER_MULTIDISK"))
(define zip:const-er-rename (c-value int "ZIP_ER_RENAME"))
(define zip:const-er-close (c-value int "ZIP_ER_CLOSE"))
(define zip:const-er-seek (c-value int "ZIP_ER_SEEK"))
(define zip:const-er-read (c-value int "ZIP_ER_READ"))
(define zip:const-er-write (c-value int "ZIP_ER_WRITE"))
(define zip:const-er-crc (c-value int "ZIP_ER_CRC"))
(define zip:const-er-zipclosed (c-value int "ZIP_ER_ZIPCLOSED"))
(define zip:const-er-noent (c-value int "ZIP_ER_NOENT"))
(define zip:const-er-exists (c-value int "ZIP_ER_EXISTS"))
(define zip:const-er-open (c-value int "ZIP_ER_OPEN"))
(define zip:const-er-tmpopen (c-value int "ZIP_ER_TMPOPEN"))
(define zip:const-er-zlib (c-value int "ZIP_ER_ZLIB"))
(define zip:const-er-memory (c-value int "ZIP_ER_MEMORY"))
(define zip:const-er-changed (c-value int "ZIP_ER_CHANGED"))
(define zip:const-er-compnotsupp (c-value int "ZIP_ER_COMPNOTSUPP"))
(define zip:const-er-eof (c-value int "ZIP_ER_EOF"))
(define zip:const-er-inval (c-value int "ZIP_ER_INVAL"))
(define zip:const-er-nozip (c-value int "ZIP_ER_NOZIP"))
(define zip:const-er-internal (c-value int "ZIP_ER_INTERNAL"))
(define zip:const-er-incons (c-value int "ZIP_ER_INCONS"))
(define zip:const-er-remove (c-value int "ZIP_ER_REMOVE"))
(define zip:const-er-deleted (c-value int "ZIP_ER_DELETED"))
;; only in the new libzip
;; (define zip:const-er-encrnotsupp (c-value int "ZIP_ER_ENCRNOTSUPP"))
;; (define zip:const-er-rdonly (c-value int "ZIP_ER_RDONLY"))
;; (define zip:const-er-nopasswd (c-value int "ZIP_ER_NOPASSWD"))
;; (define zip:const-er-wrongpasswd (c-value int "ZIP_ER_WRONGPASSWD"))

;; type of system error value
(define zip:const-et-none (c-value int "ZIP_ET_NONE"))
(define zip:const-et-sys (c-value int "ZIP_ET_SYS"))
(define zip:const-et-zlib (c-value int "ZIP_ET_ZLIB"))
;; compression methods
(define zip:const-cm-default (c-value int "ZIP_CM_DEFAULT"))
(define zip:const-cm-store (c-value int "ZIP_CM_STORE"))
(define zip:const-cm-shrink (c-value int "ZIP_CM_SHRINK"))
(define zip:const-cm-reduce-1 (c-value int "ZIP_CM_REDUCE_1"))
(define zip:const-cm-reduce-2 (c-value int "ZIP_CM_REDUCE_2"))
(define zip:const-cm-reduce-3 (c-value int "ZIP_CM_REDUCE_3"))
(define zip:const-cm-reduce-4 (c-value int "ZIP_CM_REDUCE_4"))
(define zip:const-cm-implode (c-value int "ZIP_CM_IMPLODE"))
(define zip:const-cm-deflate (c-value int "ZIP_CM_DEFLATE"))
(define zip:const-cm-deflate64 (c-value int "ZIP_CM_DEFLATE64"))
(define zip:const-cm-pkware-implode (c-value int "ZIP_CM_PKWARE_IMPLODE"))
(define zip:const-cm-bzip2 (c-value int "ZIP_CM_BZIP2"))
(define zip:const-cm-lzma (c-value int "ZIP_CM_LZMA"))
(define zip:const-cm-terse (c-value int "ZIP_CM_TERSE"))
(define zip:const-cm-lz77 (c-value int "ZIP_CM_LZ77"))
(define zip:const-cm-wavpack (c-value int "ZIP_CM_WAVPACK"))
(define zip:const-cm-ppmd (c-value int "ZIP_CM_PPMD"))
;; encryption methods
(define zip:const-em-none (c-value int "ZIP_EM_NONE"))
(define zip:const-em-trad-pkware (c-value int "ZIP_EM_TRAD_PKWARE"))
(define zip:const-em-unknown (c-value int "ZIP_EM_UNKNOWN"))
;;
(define zip:const-source-open (c-value int "ZIP_SOURCE_OPEN"))
(define zip:const-source-read (c-value int "ZIP_SOURCE_READ"))
(define zip:const-source-close (c-value int "ZIP_SOURCE_CLOSE"))
(define zip:const-source-stat (c-value int "ZIP_SOURCE_STAT"))
(define zip:const-source-error (c-value int "ZIP_SOURCE_ERROR"))
(define zip:const-source-free (c-value int "ZIP_SOURCE_FREE"))

;; only in the new libzip
;; (define zip:const-source-err-lower (c-value int "ZIP_SOURCE_ERR_LOWER"))

;; only in the new libzip
;; (define zip:const-stat-name (c-value int "ZIP_STAT_NAME"))
;; (define zip:const-stat-index (c-value int "ZIP_STAT_INDEX"))
;; (define zip:const-stat-size (c-value int "ZIP_STAT_SIZE"))
;; (define zip:const-stat-comp-size (c-value int "ZIP_STAT_COMP_SIZE"))
;; (define zip:const-stat-mtime (c-value int "ZIP_STAT_MTIME"))
;; (define zip:const-stat-crc (c-value int "ZIP_STAT_CRC"))
;; (define zip:const-stat-comp-method (c-value int "ZIP_STAT_COMP_METHOD"))
;; (define zip:const-stat-encryption-method (c-value int "ZIP_STAT_ENCRYPTION_METHOD"))
;; (define zip:const-stat-flags (c-value int "ZIP_STAT_FLAGS"))

;; TODO integer64 should be uint64_t or int64_t all over
(define (zip:add-source zip name zip-source)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function integer64 ("zip_add" zip c-string zip-source)) zip name zip-source))
  zip))
(define (zip:replace-source zip name-or-index zip-source)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function int ("zip_replace" zip integer64 zip-source))
    zip (zip:index zip name-or-index) zip-source))
  zip))

(define zip:error-get-sys-type (c-function int ("zip_error_get_sys_type" int)))
(define zip:error-to-str (c-function int ("zip_error_to_str" c-pointer integer64 int int)))

(define zip:file-error-clear (c-function void ("zip_file_error_clear" c-pointer)))
(define zip:file-error-get (c-function void ("zip_file_error_get" c-pointer c-pointer c-pointer)))
(define zip:file-strerror (c-function c-string ("zip_file_strerror" c-pointer)))

;; only in the new libzip
;; (define zip:fopen-encrypted (c-function c-pointer ("zip_fopen_encrypted" c-pointer string int string)))
;; (define zip:fopen-index-encrypted (c-function c-pointer ("zip_fopen_index_encrypted" c-pointer integer64 int string)))

(define (zip:fopen zip name-or-index)
 (zip:fopen-index zip (zip:index zip name-or-index)))
(define (zip:fopen-index zip index)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function zip-file ("zip_fopen_index" zip integer64 int)) zip index 0))
  zip))
(define (zip:fclose zip-file)
 (unless (zero? ((c-function int ("zip_fclose" zip-file)) zip-file))
  (fuck-up)))
(define (zip:fread-to-buffer zip-file size)
 (let* ((buffer (set-finalizer! (malloc (+ size 1)) free))
	(read ((c-function int ("zip_fread" zip-file c-pointer integer64))
	       zip-file buffer (+ size 1))))
  (when (= -1 read) (panic "read failed"))
  (c-byte-set! buffer size 0)
  buffer))
(define (zip:fread zip-file size)
 (with-alloc
  (+ size 1)
  (lambda (buffer)
   (let ((read ((c-function int ("zip_fread" zip-file c-pointer integer64))
		zip-file buffer (+ size 1))))
    (when (= -1 read) (panic "failed to read"))
    (let ((string (make-string (+ size 1))))
     (for-each-n
      (lambda (n) (string-set! string n
			  (integer->char (c-byte-ref buffer n))))
      size)
     string)))))

(define zip:get-archive-flag (c-function int ("zip_get_archive_flag" c-pointer int int)))

;; only in the new libzip
;; (define zip:get-file-comment (c-function c-string ("zip_get_file_comment" c-pointer integer64 c-pointer int)))
;; (define zip:get-file-extra (c-function c-string ("zip_get_file_extra" c-pointer integer64 c-pointer int)))

(define (zip:get-name zip index)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function c-string ("zip_get_name" zip integer64 int)) zip index 0))
  zip))

(define (zip:get-num-entries zip)
 ((c-function integer64 ("zip_get_num_files" zip)) zip)
 ;; only in the new libzip
 ;; ((c-function integer64 ("zip_get_num_entries" zip int)) zip 0)
 )

(define (zip:name-locate zip name)
 ;; TODO should return #F when not found
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function int ("zip_name_locate" zip c-string int)) zip name 0))
  zip
  name))

;; only in the new libzip
;; (define zip:fdopen (c-function c-pointer ("zip_fdopen" int int c-pointer)))

(define zip:set-archive-flag (c-function int ("zip_set_archive_flag" zip int int)))
;; only in the new libzip
;; (define zip:set-default-password (c-function int ("zip_set_default_password" zip c-string)))
(define zip:set-file-comment (c-function int ("zip_set_file_comment" zip integer64 c-string int)))
;; only in the new libzip
;; (define zip:set-file-extra (c-function int ("zip_set_file_extra" zip integer64 c-string int)))

(define (zip:source-buffer zip buffer size free?)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function zip-source ("zip_source_buffer" zip c-pointer integer64 bool))
    zip buffer size free?))
  zip))
(define (zip:source-file zip filename . range)
 ;; reads the entire file when no range is passed in
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function zip-source ("zip_source_file" zip c-string integer64 integer64))
    zip filename
    (if (null? range) 0 (first range))
    (if (null? range) 0 (second range))))
  zip))
(define (zip:source-zip zip-destination zip-source source-index . range)
 ;; reads the entire file when no range is passed in
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function zip-source ("zip_source_zip" zip zip integer64 int integer64 integer64))
    zip-destination zip-source source-index 0
    (if (null? range) 0 (first range))
    (if (null? range) 0 (second range))))
  zip))
(define zip:source-function (c-function zip-source ("zip_source_function" zip c-pointer c-pointer)))
(define zip:source-filep (c-function zip-source ("zip_source_filep" zip c-pointer integer64 integer64)))
;; Note that ownership of sources is transferred with zip_add or
;; zip_replace, no need to free in that case
(define zip:source-free (c-function void ("zip_source_free" zip-source)))

(define (zip:index zip name-or-index)
 (cond ((number? name-or-index) name-or-index)
       ((string? name-or-index) (zip:name-locate zip name-or-index))
       (else (panic "bad index"))))

(define (zip:stat-index zip index)
 (with-alloc
  (c-sizeof "struct zip_stat")
  (lambda (zip-stat)
   (zip:stat-init zip-stat)
   (zip:with-fail-on-error!
    (lambda (zip)
     ((c-function int ("zip_stat_index" zip integer64 int c-pointer))
      zip index 0 zip-stat))
    zip)
   (make-zip-file-stat (zip-stat-name zip-stat)
		       (zip-stat-index zip-stat)
		       (zip-stat-size zip-stat)
		       (zip-stat-comp-size zip-stat)))))

(define zip:stat-init (c-function void ("zip_stat_init" c-pointer)))

(define zip:unchange (c-function int ("zip_unchange" zip integer64)))
(define zip:unchange-all (c-function int ("zip_unchange_all" zip)))
(define zip:unchange-archive (c-function int ("zip_unchange_archive" zip)))

(define zip:strerror (c-function c-string ("zip_strerror" zip)))

(define (zip:fail-on-error! zip)
 (unless (equal? (zip:strerror zip) "No error")
  (panic "Zip error: ~a~%" (zip:strerror zip))))

(define (zip:with-fail-on-error! f zip . message)
 (let ((result (f zip)))
  (unless (equal? (zip:strerror zip) "No error")
   (panic "Zip error: ~a~a~%"
	  (zip:strerror zip)
	  (if (null? message) "" (format #f ". ~a" (first message)))))
  result))

(define (with-zip-file f filename mode)
 (let*  ((zip (zip:open filename mode)) (result (f zip)))
  (zip:close zip)
  result))
)
