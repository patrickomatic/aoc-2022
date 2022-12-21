#lang racket
(require "shared/number.rkt")

(struct command (name argument output) #:transparent)

(struct file (path size directory?) #:transparent)

(define command-regexp (const #px"^\\$\\s+(\\w+)\\s?(.+)?$"))

(define (parse-command-output input [output '()])
  (let ([next-byte (peek-char input)])
    (if (or (eof-object? next-byte) (eq? next-byte #\$))
      output
      (parse-command-output input (cons (read-line input) output)))))

(define (parse-command input)
  (let* ([line (read-line input)]
         [match (or (regexp-match (command-regexp) line)
                    (error "Invalid command line" line))]
         [matches (cdr match)])
    (command (string->symbol (first matches))
             (second matches)
             (parse-command-output input))))

(define (parse-terminal-history input [commands '()])
  (if (eof-object? (peek-char input))
    (reverse commands)
    (parse-terminal-history input 
                            (cons (parse-command input) commands))))

(define (change-directory cd-command current-path)
  (let ([to (command-argument cd-command)])
    (match to 
           [".." (cdr current-path)]
           ["/" '()]
           [else (cons to current-path)])))

(define (join-path path)
  (string-append "/" (string-join (reverse path) "/")))

(define (list-directory ls-command current-path)
  (map (lambda (row)
         (let* ([filename (cadr row)]
                [size (car row)]
                [path (join-path (cons filename current-path))]
                [directory? (equal? size "dir")])
           (file path 
                 (if directory? 0 (string->real size))
                 directory?)))
       (map string-split (command-output ls-command))))

(define (bubble-up-path-size! known-paths size current-path)
  (let* ([current-path-str (join-path current-path)]
         [current-value (hash-ref known-paths current-path-str 0)])
    (hash-set! known-paths 
               current-path-str
               (+ current-value size))
    (unless (null? current-path)
      (bubble-up-path-size! known-paths size (cdr current-path)))))

(define (collect-known-paths commands)
  (let ([current-path '()]
        [known-paths (make-hash)])
    (for/list ([current-command commands])
              (match (command-name current-command)
                     ['cd
                      (set! current-path (change-directory current-command current-path))]
                     ['ls
                      (let* ([paths (list-directory current-command current-path)])
                        (for/list ([path (filter file-directory? paths)])
                                  (hash-set! known-paths (file-path path) (file-size path)))
                        (bubble-up-path-size! known-paths (apply + (map file-size paths)) current-path))]
                     [else
                       (error "Unsupported command" (command-name current-command))]))
    known-paths))

(define (q7 filename strategy)
  (let* ([commands (parse-terminal-history (open-input-file filename))]
         [known-paths (collect-known-paths commands)]
         [space-left (- 70000000 (hash-ref known-paths "/"))]
         [space-needed (- 30000000 space-left)])
    (if (eq? strategy 'part1)
      (apply + (filter (curry >= 100000) (hash-values known-paths)))
      (car (sort (filter (curry < space-needed) (hash-values known-paths)) <)))))

(let* ([filename "input/day7.txt"])
  (printf "Question 7/Part 1: ~s\n" (q7 filename 'part1))
  (printf "Question 7/Part 2: ~s\n" (q7 filename 'part2)))

(provide change-directory 
         collect-known-paths
         command 
         file
         list-directory
         parse-terminal-history)
