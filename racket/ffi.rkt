#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         racket/format)

(define-ffi-definer define-curses (ffi-lib "libncurses.so.6"))

(define _WINDOW-pointer (_cpointer 'WINDOW))

(define-curses initscr (_fun -> _WINDOW-pointer))
(define-curses waddstr (_fun _WINDOW-pointer _string -> _int))
(define-curses wrefresh (_fun _WINDOW-pointer -> _int))
(define-curses endwin (_fun -> _int))

(define win (initscr))
(define (rec str)
  (if (string? str)
      (begin
        (void (waddstr win str))
        (void (waddstr win "\n"))
        (void (wrefresh win))
        (sleep 1)
        (if (< 0 (string-length str))
            (rec (substring str 1))
            'nil)
        )
      'nil)
  )

(rec "hello world")

(void (endwin))