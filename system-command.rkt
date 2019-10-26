#lang racket

(require racket/system)

;; racket -l errortrace -t system-command.rkt

;; a general print function
(define (printline elems #:sep [sep " "] #:end [end "\n"] #:element-converter [element-converter identity])
  (define (iter remaining-elements result-string)
    (cond
      [(empty? remaining-elements) (string-append result-string end)]
      [(empty? (rest remaining-elements))
        (iter (rest remaining-elements)
              (string-append result-string
                             (element-converter (first remaining-elements))))]
      [else
        (iter (rest remaining-elements)
              (string-append result-string
                             (element-converter (first remaining-elements))
                             sep))]))
  (cond
    [(empty? elems) (display end)]
    [(not (list? elems)) (display (string-append (element-converter elems) end))]
    [else (display (iter elems ""))]))

;; Bash formatting stuff
(define no-formatting "\033[0m")
(define bold-intense-white-formatting "\033[1;97m")
(define message-formatting "\033[1;94m")
(define error-formatting "\033[1;91m")
(define success-formatting "\033[1;92m")

(define (bash-formatting a-string string-format)
  (cond
    [(string=? string-format message-formatting)
     (string-append message-formatting a-string no-formatting)]
    [(string=? string-format success-formatting)
     (string-append success-formatting a-string no-formatting)]
    [(string=? string-format error-formatting)
     (string-append error-formatting a-string no-formatting)]
    [else (printline "unknown string format, returning parameter as is")]))

;; The shell command function.
(define (shell-command
          command
          #:flag-print-command [flag-print-command true]

          #:flag-print-success-status [flag-print-success-status true]
          #:flag-print-error-status [flag-print-error-status true]

          #:flag-print-success-output [flag-print-success-output false]
          #:flag-print-error-output [flag-print-error-output true]

          #:flag-print-success-exit-code [flag-print-success-exit-code false]
          #:flag-print-error-exit-code [flag-print-error-exit-code true]

          #:verbose [verbose false])

  ;; prints all lines until eof is found
  (define (print-all-lines an-input-port)
    (let
      ((line (read-line an-input-port)))
      (cond
        [(not (eof-object? line))
          (printline line)
          (print-all-lines an-input-port)])))

  (if (or flag-print-command verbose)
      (printline (list "Executing command:" (bash-formatting command message-formatting)))
      (void))
  ;; defining the process object
  (define command-process (process command))
  ;; wait for the command to be finished, regardless the result
  ((last command-process) 'wait)
  ;; get the exit code
  (define command-exit-code ((last command-process) 'exit-code))
  ;; use the exit code to determin what to do next
  (cond
    [(= command-exit-code 0)
     (if (or flag-print-success-exit-code verbose)
         (printline command-exit-code #:element-converter number->string)
         (void))
     (if (or flag-print-success-status verbose)
         (printline (bash-formatting "SUCCESS!" success-formatting))
         (void))
     (if (or flag-print-success-output verbose)
         (print-all-lines (first command-process))
         (void))
     (if (or flag-print-success-status
             flag-print-success-exit-code
             flag-print-success-output
             verbose)
         (printline "")
         (void))]
    [else
      (if (or flag-print-error-status verbose)
          (printline (bash-formatting "ERROR!" error-formatting))
          (void))
      (if (or flag-print-error-exit-code verbose)
          (printline (list "exit code:" (number->string command-exit-code)))
          (void))
      (if (or flag-print-error-output verbose)
          (print-all-lines (fourth command-process))
          (void))
      (if (or flag-print-error-status
              flag-print-error-exit-code
              flag-print-error-output
              verbose)
              (printline "")
              (void))]))

;; some test runs
(shell-command "ll" #:flag-print-error-status false)
(shell-command "pushd /home/xialong/development/ && popd")
(shell-command "ls")
(shell-command "ls -al")
(shell-command "cd .. && cd system-commands" #:verbose true)