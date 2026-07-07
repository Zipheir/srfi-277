;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(import (rnrs base)
        (rnrs io ports)
        (srfi :64)
        (srfi srfi-NNN)
        )

;;; Test runner

;; The SRFI 64 implementation used by most Schemes has a very basic
;; default test runner. This is slightly more helpful on failures.

(define (my-test-runner-factory)
  (let*
   ((runner (test-runner-null))
    (test-end
     (lambda (runner)
       (case (test-result-kind runner)
         ((pass)
          (display "Pass: ")
          (display (test-runner-test-name runner))
          (newline))
         ((fail)
          (display "FAIL: ")
          (display (test-runner-test-name runner))
          (display ". Expected ")
          (display (test-result-ref runner 'expected-value))
          (display ", got ")
          (display (test-result-ref runner 'actual-value))
          (display ".\n")))))
    (test-final
     (lambda (runner)
       (display "===============================\n")
       (display "Total passes: ")
       (display (test-runner-pass-count runner))
       (newline)
       (display "Total failures: ")
       (display (test-runner-fail-count runner))
       (newline)
       (display "Total skips: ")
       (display (test-runner-skip-count runner))
       (newline))))

    (test-runner-on-test-end! runner test-end)
    (test-runner-on-final! runner test-final)
    runner))

(test-runner-factory my-test-runner-factory)


(test-begin "Cyclic ports")

(test-assert "cyclic bytevector ports are input ports"
  (call-with-port (open-cyclic-input-bytevector '#vu8(1)) input-port?))

(test-assert "cyclic bytevector ports are binary ports"
  (call-with-port (open-cyclic-input-bytevector '#vu8(1)) binary-port?))

(test-assert "cyclic string ports are input ports"
  (call-with-port (open-cyclic-input-string "a") input-port?))

(test-assert "cyclic string ports are textual ports"
  (call-with-port (open-cyclic-input-string "a") textual-port?))

(test-equal "read from cyclic bytevector port"
  '#vu8(1 2 3 1 2 3 1 2)
  (call-with-port (open-cyclic-input-bytevector '#vu8(1 2 3))
                  (lambda (p)
                    (get-bytevector-n p 8))))

(test-equal "read from cyclic string port"
  "abcabcab"
  (call-with-port (open-cyclic-input-string "abc")
                  (lambda (p)
                    (get-string-n p 8))))

(test-end)
