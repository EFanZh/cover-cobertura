#lang racket

(require racket/path)
(require xml)

(struct coverage-line (number hits))
(struct coverage-class (name filename line-rate lines))
(struct coverage-package (name line-rate classes))
(struct coverage-coverage (line-rate lines-covered lines-valid timestamp packages))

(define (check-line-coverage coverage file start end)
  (let irrelevant ([i start])
    (if (= i end)
        'irrelevant
        (match (coverage file i)
          ['covered (if (for/or ([i (in-range (add1 i) end)])
                          (eq? (coverage file i) 'uncovered))
                        'uncovered
                        'covered)]
          ['uncovered 'uncovered]
          ['irrelevant (irrelevant (add1 i))]))))

(define (coverage-rate covered valid)
  (if (zero? valid)
      1
      (/ covered valid)))

(define (calculate-file-coverage coverage file root)
  (let ([lines-covered 0]
        [lines-valid 0]
        [lines '()]
        [file-path (cdr file)]
        [line-start 1]
        [line-number 1])
    (for ([line-length (map string-length (file->lines file-path #:mode 'text))])
      (let ([line-end (+ line-start line-length)])
        (match (check-line-coverage coverage file-path line-start line-end)
          ['covered (set! lines-covered (add1 lines-covered))
                    (set! lines-valid (add1 lines-valid))
                    (set! lines (cons (coverage-line line-number 1) lines))]
          ['uncovered (set! lines-valid (add1 lines-valid))
                      (set! lines (cons (coverage-line line-number 0) lines))]
          ['irrelevant (void)])
        (set! line-start (add1 line-end)) ; Adding the end of line character.
        (set! line-number (add1 line-number))))
    (values lines-covered
            lines-valid
            (coverage-class (car file)
                            (path->string (find-relative-path root file-path))
                            (coverage-rate lines-covered lines-valid)
                            lines))))

(define (calculate-package-coverage name coverage files root)
  (let ([lines-covered 0]
        [lines-valid 0]
        [classes '()])
    (for ([file files])
      (let-values ([(class-lines-covered class-lines-valid class) (calculate-file-coverage coverage file root)])
        (set! lines-covered (+ lines-covered class-lines-covered))
        (set! lines-valid (+ lines-valid class-lines-valid))
        (set! classes (cons class classes))))
    (values lines-covered
            lines-valid
            (coverage-package name
                              (coverage-rate lines-covered lines-valid)
                              classes))))

(define (collect-packages files root)
  (let ([result (make-hash)])
    (for ([file files])
      (let-values ([(package-path file-name _) (split-path file)])
        (hash-update! result
                      (path->string (find-relative-path root package-path #:more-than-same? #f))
                      (λ (classes)
                        (cons (cons (path->string (path-replace-extension file-name "")) file) classes))
                      '())))
    result))

(define (collect-coverage coverage files root)
  (let ([lines-covered 0]
        [lines-valid 0]
        [packages '()])
    (for ([(package-name files) (collect-packages files root)])
      (let-values ([(package-lines-covered package-lines-valid package) (calculate-package-coverage package-name
                                                                                                    coverage
                                                                                                    files
                                                                                                    root)])
        (set! lines-covered (+ lines-covered package-lines-covered))
        (set! lines-valid (+ lines-valid package-lines-valid))
        (set! packages (cons package packages))))
    (coverage-coverage (coverage-rate lines-covered lines-valid)
                       lines-covered
                       lines-valid
                       (current-milliseconds)
                       packages)))

(define (build-xml-report report)
  (document
   (prolog '()
           (document-type 'coverage
                          (external-dtd/system "http://cobertura.sourceforge.net/xml/coverage-04.dtd")
                          #f)
           '())
   (element #f
            #f
            'coverage
            (list (attribute #f #f 'line-rate (real->decimal-string (coverage-coverage-line-rate report) 4))
                  (attribute #f #f 'branch-rate "0")
                  (attribute #f #f 'lines-covered (number->string (coverage-coverage-lines-covered report)))
                  (attribute #f #f 'lines-valid (number->string (coverage-coverage-lines-valid report)))
                  (attribute #f #f 'branches-covered "0")
                  (attribute #f #f 'branches-valid "0")
                  (attribute #f #f 'complexity "0")
                  (attribute #f #f 'version "0")
                  (attribute #f #f 'timestamp (number->string (coverage-coverage-timestamp report))))
            (list (xexpr->xml
                   `(packages
                     ,@(for/list ([package (coverage-coverage-packages report)])
                         `(package
                           ([name ,(coverage-package-name package)]
                            [line-rate ,(real->decimal-string (coverage-package-line-rate package) 4)]
                            [branch-rate "0"]
                            [complexity "0"])
                           (classes
                            ,@(for/list ([class (coverage-package-classes package)])
                                `(class
                                     ([name ,(coverage-class-name class)]
                                      [filename ,(coverage-class-filename class)]
                                      [line-rate ,(real->decimal-string (coverage-class-line-rate class) 4)]
                                      [branch-rate "0"]
                                      [complexity "0"])
                                   (methods)
                                   (lines
                                    ,@(for/list ([line (coverage-class-lines class)])
                                        `(line ([number ,(number->string (coverage-line-number line))]
                                                [hits ,(number->string (coverage-line-hits line))])))))))))))))
   '()))

(define (generate-cobertura-coverage coverage files [d "coverage"])
  (let* ([root (current-directory)]
         [report (collect-coverage coverage files root)]
         [xml-report (build-xml-report report)]
         [output-directory (path->complete-path d root)])
    (make-directory* output-directory)
    (with-output-to-file (build-path output-directory "cobertura.xml")
      (thunk
       (parameterize ([empty-tag-shorthand 'always])
         (displayln "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>")
         (write-xml xml-report)))
      #:mode 'text
      #:exists 'truncate)))

(provide generate-cobertura-coverage)
