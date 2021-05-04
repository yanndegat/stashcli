;;; stashcli project commands
(import :gerbil/gambit
        :std/format
        :std/iter
        :std/misc/hash
        :std/misc/string
        :std/misc/symbol
        :std/ref
        :std/srfi/13
        :std/sugar
        :std/text/json
        :colorstring/colorstring)

(export #t)

(def (format-pr-approval-status pr)
  (map (lambda (r)
         (case (~ r 'status)
          (("APPROVED") "[green]\u2713[reset]")
          (("NEEDS_WORK") "[red]\u2716[reset]")
          (else "☐")))
       (~ pr 'reviewers)))

(def (display-attrs attrs default-colors?: (default-colors? #t))
  (for (attr (hash->list/sort attrs symbol<?))
    (with ([key :: val] attr)
      (displayln (color (format-line [[ key :: (json-object->string val) ]]
                                     default-colors?: default-colors?))))))

(def (display-line line default-colors?: (default-colors? #t))
  (displayln (color (format-line line default-colors?: default-colors?))))

(def (format-line line default-colors?: (default-colors? #t))
  (string-join
   (map (lambda (attr)
          (if default-colors?
            (format "[bold][blue]~a[reset]: ~a" (car attr) (cdr attr))
            (format "~a: ~a" (car attr) (cdr attr))))
        line)
   ", "))

(def (format-pr-state s)
  (match (string-downcase s)
    ((equal? "merged") (format "[green]~a[reset]" s))
    ((equal? "declined") (format "[red]~a[reset]" s))
    (_ s)))
