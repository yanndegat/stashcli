;;; stashcli project commands
(import :gerbil/gambit
        :std/format
        :std/iter
        :std/logger
        :std/misc/hash
        :std/misc/ports
        :std/misc/process
        :std/misc/string
        :std/misc/symbol
        :std/net/request
        :std/pregexp
        :std/ref
        :std/srfi/13
        :std/sugar
        :std/text/json
        :std/text/yaml
        :url-string/url-string
        :colorstring/colorstring)

(export #t)

(def (stash-url path)
  (format "~a~a" (~ (config) 'url) path))

(def (default-http-headers)
  (let-hash (config)
    [["Accept" :: "*/*"]
     ["Content-type" :: "application/json"]
     ["Authorization" :: (format "Bearer ~a" .token) ]]))

(def (request-success? req)
  (and (>= (request-status req) 200) (<= (request-status req) 299)))

(def (format-pr-approval-status pr)
  (map (lambda (r)
         (case (~ r 'status)
          (("APPROVED") "[green]☑[reset]")
          (("NEEDS_WORK") "[red]☒[reset]")
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

(def (upstream-track)
  (and (current-upstream)
       (pregexp-replace rx-upstream-track (current-upstream) "\\1")))

(def (upstream-branch)
  (and (current-upstream)
       (pregexp-replace rx-upstream-track (current-upstream) "\\2")))

(def (default-project) (or (current-project) (~ (config) 'default-project)))

(def (default-remote-branch
      project: (project (current-project))
      repo: (repo (current-repo)))
  (and project
       repo
       (car (filter (cut ~ <> 'isDefault) (repo-branches project repo)))))

(def (repo-branches project repo)
  (def url (stash-url (format
                       "/api/1.0/projects/~a/repos/~a/branches"
                       project repo)))
  (def req (http-get url headers: (default-http-headers)))
  (def body (request-json req))
  (unless (request-success? req) (error (json-object->string body)))
  (~ body 'values))

(def rx-upstream-track "^# branch.upstream ([[:alnum:]]+)/(.+)$")

(def current-upstream (make-parameter #f))
(def (init-current-upstream)
  (current-upstream
   (try
    (let ((status (pregexp-split "\n" (run-process '("git" "status" "--porcelain=2" "-b")
                                                   stderr-redirection: #t))))
      (car (filter (cut pregexp-match rx-upstream-track <>) status)))
    (catch _ #f))))

(def current-repo-url (make-parameter #f))
(def (init-current-repo-url git-remote)
  (current-repo-url
   (let* ((track (or git-remote (upstream-track) "origin"))
          (remote-url (and track
                           (try (run-process `("git" "remote" "get-url" "--push" ,track)
                                             stderr-redirection: #t)
                                (catch _ #f)))))
     (and remote-url (string->url (string-trim-eol remote-url))))))

(def current-project (make-parameter #f))
(def (init-current-project)
  (current-project
   (let ((repo-url (current-repo-url)))
     (and repo-url (path/param-path (car (url-path repo-url)))))))

(def current-repo (make-parameter #f))
(def (init-current-repo)
  (current-repo
   (let ((repo-url (current-repo-url)))
     (and repo-url (string-trim-suffix ".git" (path/param-path (cadr (url-path repo-url))))))))

(def current-remote-branch (make-parameter #f))
(def (init-current-remote-branch)
  (current-remote-branch
   (and (current-project)
        (current-repo)
        (upstream-branch)
        (car (filter (lambda (b) (or (equal? (upstream-branch) (hash-ref b 'id #f))
                                     (equal? (upstream-branch) (hash-ref b 'displayId #f))))
                     (repo-branches (current-project) (current-repo)))))))

(def config (make-parameter #f))
(def (init-config path)
  (def yaml-data (yaml-load path))
  (try
   (def config-data (car yaml-data))
   (config (hash (token (hash-ref config-data "token"))
                 (default-project (hash-ref config-data "default-project" #f))
                 (url (hash-ref config-data "url"))))
   (catch (error? exn)
     (error (format "Could not parse your config ~a" path)))))


(def (init path
           debug?: (debug? #f)
           git-remote: (git-remote #f))
  (start-logger!)
  (and debug? (debug "init config from: ~a" path))
  (init-config path)
  (init-current-upstream)
  (and debug? (debug "init current upstream: ~a" (current-upstream)))
  (init-current-repo-url git-remote)
  (and debug? (debug "init current repo url: ~a" (url->string (current-repo-url))))
  (init-current-project)
  (and debug? (debug "init current project: ~a" (current-project)))
  (init-current-repo)
  (and debug? (debug "init current repository: ~a" (current-repo)))
  (init-current-remote-branch)
  (and debug? (debug "init current remote branch: ~a" (current-remote-branch)))
  (and debug? (debug "config default project is: ~a" (default-project))))
