;;; stashcli project commands
(import :gerbil/gambit
        :std/format
        :std/misc/ports
        :std/misc/process
        :std/misc/string
        :std/net/request
        :std/pregexp
        :std/ref
        :std/srfi/13
        :std/sugar
        :std/text/yaml
        :url-string/url-string
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

(def rx-upstream-track "^# branch.upstream ([[:alnum:]]+)/.+$")
(def (upstream-track)
  (let* ((status (pregexp-split "\n" (run-process '("git" "status" "--porcelain=2" "-b")
                                                  stderr-redirection: #t)))
         (upstream-track (car (filter (cut pregexp-match rx-upstream-track <>) status))))
    (pregexp-replace rx-upstream-track upstream-track "\\1")))

(def (this-repo-url)
  (let ((remote-url (string-trim-eol (run-process `("git" "remote" "get-url" "--push" ,(upstream-track))
                                                  stderr-redirection: #t))))
    (string->url remote-url)))

(def current-repo-url (make-parameter (try (this-repo-url) (catch _ #f))))

(def current-project (make-parameter
                      (let ((repo-url (current-repo-url)))
                        (if repo-url
                          (path/param-path (car (url-path repo-url)))
                          #f))))

(def current-repo (make-parameter
                   (let ((repo-url (current-repo-url)))
                     (if repo-url
                       (string-trim-suffix ".git" (path/param-path (cadr (url-path repo-url))))
                       #f))))

(def (default-project) (or (current-project) (~ (config) 'default-project)))
