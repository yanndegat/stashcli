;;; stashcli project commands
(import :gerbil/gambit
        :std/sugar
        :std/format
        :std/ref
        :std/net/request
        :std/srfi/13
        :std/text/yaml
        :colorstring/colorstring)

(export #t)

(def (load-config path)
  (def yaml-data (yaml-load path))
  (try
   (def config-data (car yaml-data))
   (hash (token (hash-ref config-data "token"))
         (default-project (hash-ref config-data "default-project" #f))
         (url (hash-ref config-data "url")))
     (catch (error? exn)
     (error (format "Could not parse your config ~a" path)))))

(def config (make-parameter (hash)))

(def (default-project) (~ (config) 'default-project))

(def (stash-url path)
  (format "~a~a" (~ (config) 'url) path))

(def (default-http-headers)
  (let-hash (config)
    [["Accept" :: "*/*"]
     ["Content-type" :: "application/json"]
     ["Authorization" :: (format "Bearer ~a" .token) ]]))

(def (request-success? req)
  (and (>= (request-status req) 200) (<= (request-status req) 299)))

(def (display-line line)
  (displayln (color (format-line line))))

(def (format-line line)
  (string-join
   (map (lambda (attr) (format "[bold][blue]~a[reset]: ~a" (car attr) (cdr attr))) line)
   ", "))

(def (format-pr-state s)
     (match (string-downcase s)
            ((equal? "merged") (format "[green]~a[reset]" s))
            ((equal? "declined") (format "[red]~a[reset]" s))
            (_ s)))
