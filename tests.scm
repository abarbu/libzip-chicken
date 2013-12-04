(use libzip)
(with-zip-file
 (lambda (zip) (display zip)(newline)
    (pp (zip:ls zip))
    (unless (member "afile" (zip:ls zip))
     (display "wrote")(newline)
     (zip:add-file zip "afile" '("hello" "cruel" "world" "of" "zip files"))))
 "/tmp/a.zip"
 *zip:mode-create-if-necessary*)

(with-zip-file
 (lambda (zip) (display zip)(newline)
    (pp (zip:ls zip))
    (zip:read-file zip "afile"))
 "/tmp/a.zip"
 *zip:mode-create-if-necessary*)

(c-byte-ref
 (vector-ref
  (with-zip-file
   (lambda (zip) (display zip)(newline)
      (pp (zip:ls zip))
      (zip:read-file-to-buffer zip "afile"))
   "/tmp/a.zip"
   *zip:mode-create-if-necessary*)
  0)
 5)

(with-zip-file
 (lambda (zip) (display zip)(newline)
    (pp (zip:ls zip))
    (zip:stat-index zip 0))
 "/tmp/a.zip"
 *zip:mode-create-if-necessary*)

(c-byte-ref
 (with-zip-file
  (lambda (zip) (display zip)(newline)
     (pp (zip:ls zip))
     (zip:fread-to-buffer (zip:fopen-index zip 0) 0))
  "/tmp/a.zip"
  *zip:mode-create-if-necessary*)
 0)
