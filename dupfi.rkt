#lang racket

(require file/md5)

(define-struct file
  (name
   path
   chksum)
  #:transparent)

(define-struct dir
  (name
   path
   contents)
  #:transparent)

(define (file< f1 f2)
  ; sort two files alphabetically
  (string<? (file-name f1) (file-name f2)))

(define (file= f1 f2)
  (and (file? f1) (file? f2)
       (string=? (file-name f1) (file-name f2))
       (bytes=? (touch (file-chksum f1)) (touch (file-chksum f2)))))

(define (chkfile p)
  ; path -> file
  ; given a path to a file that exists, it returns its structure
  (when (not (file-exists? p))
    (error 'chkfile "path ~a doesn't point to valid file" (path->string p)))
  
  (let-values ([(base name must-be-dir?) (split-path p)])
    (make-file (path->string name) base 
               (future (lambda () (call-with-input-file p md5))))))
    
(define (file-sig f)
  (bytes-append (string->bytes/utf-8 (string-append (file-name f) ":")) (touch (file-chksum f))))

(define (dir< d1 d2)
  ; sort two directories alphabetically with the directories coming first
  (string<? (dir-name d1) (dir-name d2)))

(define (dir-contents< d1c d2c)
  (cond [(and (dir? d1c) (dir? d2c))
         (dir< d1c d2c)]
        [(and (file? d1c) (file? d2c))
         (file< d1c d2c)]
        [(and (file? d1c) (dir? d2c))
         #f]
        [(and (dir? d1c) (file? d2c))
         #t]))

(define (dir= d1 d2)
  ; two directories are equal if their names are equal and their contents are equal
  (and (dir? d1) (dir? d2)
       (string=? (dir-name d1) (dir-name d2))
       (= (length (dir-contents d1)) (length (dir-contents d2)))
       (andmap (lambda (d1i d2i)
                 (cond [(and (file? d1i) (file? d2i))
                        (file= d1i d2i)]
                       [(and (dir? d1i) (dir? d2i))
                        (dir= d1i d2i)]
                       [else #f]))
               (dir-contents d1) (dir-contents d2))))
                         
(define (chkdir p)
  ; path -> dir
  ; given a path to a dir that exists, it returns its structure
  (when (not (directory-exists? p))
    (error 'chkdir "path ~a doesn't point to a valid directory" (path->string p)))
  
  (let-values ([(base name must-be-dir?) (split-path p)])
    (make-dir (path->string name) base
              (sort (map (lambda (path)
                           (let ([path (build-path p path)])
                             (cond [(file-exists? path)
                                    (printf "checking file ~a~n" path)
                                    (chkfile path)]
                                   [(directory-exists? path)
                                    (printf "checking dir ~a~n" path)
                                    (chkdir path)])))
                         (filter (lambda (path) (not (link-exists? (build-path p path)))) (directory-list p)))
                    dir-contents<))))
    
(define (bytestring-xor* . bss)
  (define (bytestring-xor bs1 bs2)
    (let* ([bs1-lst (bytes->list bs1)]
           [bs2-lst (bytes->list bs2)]
           [bs1-len (length bs1-lst)]
           [bs2-len (length bs2-lst)]
           [diff (abs (- bs2-len bs1-len))])
      (list->bytes (map bitwise-xor
                        (if (> bs2-len bs1-len)
                            (append bs1-lst (build-list diff (lambda (x) 0)))
                            bs1-lst)
                        (if (> bs1-len bs2-len)
                            (append bs2-lst (build-list diff (lambda (x) 0)))
                            bs2-lst)))))
  (cond [(null? bss)
         (error 'bytestring-xor* "needs at least 1 argument")]
        [(null? (cdr bss))
         (car bss)]
        [else
         (bytestring-xor (car bss) (apply bytestring-xor* (cdr bss)))]))
      
         
(define (dir-sig d)
  ; the signature of a directory is the xor of all its contents, together with its name preppended
  (let ([sigs (map (lambda (i)
                     (cond [(file? i)
                            (file-sig i)]
                           [(dir? i)
                            (dir-sig i)]))
                   (dir-contents d))])
    (if (null? sigs)
        (string->bytes/utf-8 (dir-name d))
        (bytes-append (string->bytes/utf-8 (string-append (dir-name d) "@"))
                      (apply bytestring-xor* sigs)))))

(define (item-sig i)
  (if (file? i)
      (file-sig i)
      (dir-sig i)))

(define (partition-items i)
  (define (flatten-item i) 
    ; return a list of dir and of all its content objects
    (if (file? i)
        (list i)
        (cons i (append-map flatten-item (dir-contents i)))))
  
  (define (group-equal groups rem)
    (if (null? rem)
        groups
        (let* ([item (caar rem)]
               [sig (cdar rem)]
               [rst (cdr rem)])
          (let-values ([(item-dup others) (partition (lambda (i/sig) (bytes=? sig (cdr i/sig)))
                                                     rst)])
            (let ([dupitems (map car item-dup)])
              (if (null? dupitems)
                  (group-equal groups others)
                  (group-equal (cons (cons item dupitems) groups)
                               others)))))))
  
  (let* ([flat (flatten-item i)]
         [sigs (map item-sig flat)]
         [flat (map cons flat sigs)])
    (group-equal '() flat)))

(define (display-results groups)
  (if (null? groups)
      (printf "no duplicate files~n")
      (for-each (lambda (group)
                  (printf "GROUP:~n")
                  (for-each (lambda (i)
                              (if (file? i)
                                  (printf "F ~a~a: ~a~n"
                                          (file-path i)
                                          (file-name i)
                                          (touch (file-chksum i)))
                                  (printf "D ~a~a~n"
                                          (dir-path i)
                                          (dir-name i))))
                            group)
                  (printf "--~n~n"))
                groups)))

;(current-command-line-arguments (vector "/localhome/pmatos/empty"))
(display-results (partition-items (chkdir (vector-ref (current-command-line-arguments) 0))))