;;; stashcli project commands
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/format
        :std/iter
        :std/ref
        :std/net/request
        :std/text/json
        :stash/utils)

(export (prefix-out maincmd project/))

;; display list of project repositoties
;; hash? string? -> any?
(def (list-repositories project)
  (def url (stash-url (format "/api/1.0/projects/~a/repos?limit=1000" project)))
  (def req (http-get url headers: (default-http-headers)))
  (def body (request-json req))

  (unless (request-success? req) (error (json-object->string body)))

  (for (repo (~ body 'values))
    (displayln (format "~a: ~a"
                       (~ repo 'name)
                       (hash-get repo 'description)))))

;; display list of project default reviewers conditions
;; hash? string? -> any?
(def (list-reviewers project)
  (def url (stash-url (format "/default-reviewers/1.0/projects/~a/conditions" project)))
  (def req (http-get url headers: (default-http-headers)))
  (def body (request-json req))

  (unless (request-success? req) (error (json-object->string body)))

  (for (c body)
    (displayln (format "src: ~a; target: ~a; users: ~a; req approvals: ~a"
                       (~ c 'sourceRefMatcher 'type 'name)
                       (~ c 'targetRefMatcher 'type 'name)
                       (string-join (map (cut ~ <> 'displayName) (~ c 'reviewers)) ",")
                       (~ c 'requiredApprovals)))))

(def (maincmd opt)
  (def args (~ opt 'args))

  (def listrepocmd
    (command 'list-repositories help: "list repositories in a project"
             (argument 'project help: "project")))

  (def listreviewerscmd
    (command 'list-reviewers help: "list default reviewers in a project"
             (argument 'project help: "project")))

  (def helpcmd
    (command 'help help: "display project usage help"
             (optional-argument 'command value: string->symbol)))

  (def gopt (getopt listreviewerscmd
                    listrepocmd
                    helpcmd))
  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (case cmd
       ((list-repositories) (list-repositories (~ opt 'project)))
       ((list-reviewers) (list-reviewers (~ opt 'project)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "project"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "project" (current-error-port)))))
