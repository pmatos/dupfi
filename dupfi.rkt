#lang typed/racket

;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require file/md5)
(require (planet synx/stat))

(define *cache-file* (build-path (find-system-path 'home-dir) ".dupfi.cache"))
(define *cache* (make-parameter (make-hash)))
(define *cache-diff-max* 1000)

(struct: file
  ((name : String)
   (path : Path)
   (chksum : Bytes))
  #:transparent)

(struct: dir
  ((name : String)
   (path : Path)
   (contents : (Listof file)))
  #:transparent)

(define old-cache?
  (let ([cache-counter 0])
    (lambda ()
      (if (= cache-counter *cache-diff-max*)
          (begin
            (set! cache-counter 0)
            #t)
          (begin
            (set! cache-counter (+ cache-counter 1))
            #f)))))

(define (file< f1 f2)
  ; sort two files alphabetically
  (string<? (file-name f1) (file-name f2)))

(define (file= f1 f2)
  (and (file? f1) (file? f2)
       (string=? (file-name f1) (file-name f2))
       (bytes=? (file-chksum f1) (file-chksum f2))))

;;
;; Returns hash table
;; (<path> (<modtime> . <hash>))
;;
(define (read-cache)
  (*cache* (make-hash))
  (when (file-exists? *cache-file*)
    (call-with-input-file *cache-file*
      (lambda (in)
        (let: loop : Void ([line : String (read-line in)])
          (when (not (eof-object? line))
            (let ([split-str (string-split line ",")])
              (cond 
                [(not (= (length split-str) 3))
                 (printf "[1] read-cache fails, unexpected line in cache file: ~a, ignoring.~n" line)
                 (loop (read-line in))]
                [else 
                 (let ([path (first split-str)]
                       [modtime (string->number (string-trim (second split-str)))]
                       [md5 (string->bytes/utf-8 (string-trim (third split-str)))])
                   (cond 
                     [(or (not path) (not modtime) (not md5))
                      (printf "[2] read-cache fails, unexpected line in cache file: ~a, ignoring.~n" line)
                      (loop (read-line in))]
                     [(not modtime)
                      (error "fail")] ;; unreachable?
                     [else 
                      (hash-set! (*cache*) path (cons modtime md5))
                      (loop (read-line in))]))])))))
      #:mode 'text))
  (printf "cache has ~a entries~n" (hash-count (*cache*))))

(define (write-cache)
  (call-with-output-file *cache-file*
    (lambda (out)
      (hash-for-each (*cache*)
                     (lambda (key val)
                       (when (not (string? key))
                         (error "writing cache failed, key is not string: " key))
                       (when (not (number? (car val)))
                         (error "writing cache failed, modtime is not number: " (car val)))
                       (when (not (bytes? (cdr val)))
                         (error "writing cache failed, md5 is not bytes: " (cdr val)))
                       (fprintf out "~a, ~a, ~a~n" key (car val) (bytes->string/utf-8 (cdr val))))))
    #:mode 'text
    #:exists 'replace))

(define (dump-cache)
  (printf "CACHE DUMP:~n")
  (hash-for-each (*cache*)
   (lambda (key val)
     (let ([modtime (car val)]
           [md5 (cdr val)])
       (printf "~a: ~a,~a~n"
               key modtime md5))))
  (printf "CACHE DUMP DONE~n"))

(define (make-file name path)
  ; Checks if file is in cache, if it is and modification seconds are the same, returns cached file.
  ; Otherwise, it calculates its md5sum and adds file to cache.
  (let* ([fullpath (build-path path name)]
         [modtime (file-or-directory-modify-seconds fullpath)]
         [val (hash-ref (*cache*) (path->string fullpath) (lambda () #f))])
    (if (and val (= (car val) modtime))
        (begin
          (printf "found cache for file ~a~n" (path->string fullpath))
          (file name path (cdr val)))
        (let ([md5 (begin
                     (printf "computing md5 for ~a~n" fullpath)
                     (if (normal-file? (type-bits fullpath))
                       (call-with-input-file fullpath md5)
                       (string->bytes/utf-8 "0")))])
          (if (not val)
            (printf "file not in cache: ~a~n" (path->string fullpath))
            (printf "file is in cache but modify times are different: old ~a, new ~a~n" (car val) modtime))
              
          (when val
            (hash-remove! (*cache*) fullpath))
          (hash-set! (*cache*) (path->string fullpath) (cons modtime md5))
          (when (old-cache?)
            (printf "found old cache, WRITING CACHE~n")
            (write-cache))
          (file name path md5)))))
  
(define (chkfile p)
  ; path -> file
  ; given a path to a file that exists, it returns its structure
  (when (not (file-exists? p))
    (error 'chkfile "path ~a doesn't point to valid file" (path->string p)))
  
  (let-values ([(base name must-be-dir?) (split-path p)])
    (make-file (path->string name) base)))
    
(define (file-sig f)
  (bytes-append (string->bytes/utf-8 (string-append (file-name f) ":")) (file-chksum f)))

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
    (dir (path->string name) base
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
    (printf "f")
    (if (file? i)
        (list i)
        (cons i (append-map flatten-item (dir-contents i)))))
  
  (define (group-equal groups rem)
    (printf "g")
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
  (printf "Partitioning items")
  (let* ([flat (begin (printf "Flattening~n") (flatten-item i))]
         [sigs (begin (printf "Sigs~n") (map item-sig flat))]
         [flat (map cons flat sigs)])
    (group-equal '() flat)))

(define (simplify-groups groups)
  (printf "Simplifying groups~n")
  ; Given a list of groups, it ensures the groups are as simple as possible.
  ; It follows the following rules:
  (sort groups (lambda (a b)
                 (cond [(and (dir? (car a)) (dir? (car b)))
                        (< (apply min (map (compose length explode-path dir-path) a))
                           (apply min (map (compose length explode-path dir-path) b)))]
                       [else 
                        (and (dir? (car a)) (file? (car b)))]))))

(define (display-results groups)
  (printf "Displaying groups~n")
  (if (null? groups)
      (printf "no duplicate files~n")
      (for-each (lambda (group)
                  (printf "GROUP:~n")
                  (for-each (lambda (i)
                              (if (file? i)
                                  (printf "F ~a~a: ~a~n"
                                          (file-path i)
                                          (file-name i)
                                          (file-chksum i))
                                  (printf "D ~a~a~n"
                                          (dir-path i)
                                          (dir-name i))))
                            group)
                  (printf "--~n~n"))
                groups)))

(read-cache)

(current-command-line-arguments (vector "/home/pmatos/Desktop/pmatos"))
(display-results (simplify-groups (partition-items (chkdir (vector-ref (current-command-line-arguments) 0)))))