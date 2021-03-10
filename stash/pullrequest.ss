;;; stashcli repository commands
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/format
        :std/iter
        :std/ref
        :std/net/request
        :std/text/json
        :stash/utils)

(export (prefix-out maincmd pullrequest/))

;; display list of pull requests on a repository
;; hash? string? -> any?
(def (diff project repo id)
  (def url (stash-url (format
                       "/api/1.0/projects/~a/repos/~a/pull-requests/~a/diff"
                       project repo id)))
  (def req (http-get url headers: (default-http-headers)))
  (def body (request-json req))

  (unless (request-success? req) (error (json-object->string body)))
  (displayln (json-object->string body)))

(def (maincmd opt)
  (def args (~ opt 'args))

  (def diffcmd
    (command 'diff
             (option 'project "-p" "--project"
                     default: (default-project)
                     help: "project of the repository")
             (option 'repository "-r" "--repository"
                     default: (current-repo)
                     help: "repository")
             (argument 'id help: "id of the pull request")
             help: "shows pull request diff"))

  (def helpcmd
    (command 'help help: "display repository usage help"
             (optional-argument 'command value: string->symbol)))

  (def gopt (getopt diffcmd
                    helpcmd))
  (try
   (let* (((values cmd opt) (getopt-parse gopt args))
          (project (or (~ opt 'project) (default-project))))
     (case cmd
       ((diff) (diff (~ opt 'project)
                     (~ opt 'repository)
                     (~ opt 'id)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "pull request"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "pull request" (current-error-port)))))
