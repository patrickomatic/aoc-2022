#lang racket

(require rackunit "day7.rkt")

(test-case "change-directory"
           (check-equal? (change-directory (command 'cd ".." '()) '(a b c)) '(b c))
           (check-equal? (change-directory (command 'cd "/" '()) '(a b c)) '())
           (check-equal? (change-directory (command 'cd "foo" '()) '(a b c)) '("foo" a b c)))

(define test-history
"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd ..
$ cd foo
$ ls
50010 bar.txt
")

(test-case "parse-terminal-history"
           (check-equal? (parse-terminal-history (open-input-string test-history))
             (list
               (command 'cd "/" '())
               (command 'ls #f '("dir d" "8504156 c.dat" "14848514 b.txt" "dir a"))
               (command 'cd ".." '())
               (command 'cd "foo" '())
               (command 'ls #f '("50010 bar.txt")))))

(test-case "list-directory"
           (check-equal? (list-directory
                           (command 'ls #f '("dir d" "8504156 c.dat" "14848514 b.txt" "dir a"))
                           '("bin" "usr"))
                         (list (file "/usr/bin/d" 0 #t)
                               (file "/usr/bin/c.dat" 8504156 #f)
                               (file "/usr/bin/b.txt" 14848514 #f)
                               (file "/usr/bin/a" 0 #t))))

(test-case "collect-known-paths"
           (check-equal? (collect-known-paths 
                           (list
                             (command 'cd "/" '())
                             (command 'ls #f '("dir d" "8504156 c.dat" "14848514 b.txt" "dir a"))
                             (command 'cd "foo" '())
                             (command 'ls #f '("50010 bar.txt"))))
                         (make-hash 
                           '(("/" . 23402680)
                             ("/a" . 0)
                             ("/d" . 0)
                             ("/foo" . 50010)))))
