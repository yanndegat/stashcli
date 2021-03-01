;;; stashcli project commands
(import :gerbil/gambit
        :std/sugar
        :std/format
        :std/ref
        :std/net/request
        :std/text/yaml)

(export #t)

(def (load-config path)
  (def yaml-data (yaml-load path))
  (try
   (def config-data (car yaml-data))
   (hash (token (hash-ref config-data "token"))
         (url (hash-ref config-data "url")))
     (catch (error? exn)
     (error (format "Could not parse your config ~a" path)))))

(def config (make-parameter (hash)))

(def (stash-url path)
  (format "~a~a" (~ (config) 'url) path))

(def (default-http-headers)
  (let-hash (config)
    [["Accept" :: "*/*"]
     ["Content-type" :: "application/json"]
     ["Authorization" :: (format "Bearer ~a" .token) ]]))

(def (request-success? req)
  (and (>= (request-status req) 200) (<= (request-status req) 299)))
