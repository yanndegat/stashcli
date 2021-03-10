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

(export (prefix-out maincmd repository/))

;; display list of pull requests on a repository
;; hash? string? -> any?
(def (list-prs project repo direction state)
  (def url (stash-url (format
                       "/api/1.0/projects/~a/repos/~a/pull-requests?direction=~a&state=~a"
                       project repo direction state)))
  (def req (http-get url headers: (default-http-headers)))
  (def body (request-json req))
  (unless (request-success? req) (error (json-object->string body)))
  (for (pr (~ body 'values))
      (display-line [["id" :: (~ pr 'id)]
                     ["ref" :: (~ pr 'toRef 'displayId)]
                     ["state" :: (format-pr-state (~ pr 'state))]
                     ["status" :: (format-pr-approval-status pr)]
                     ["title" :: (~ pr 'title)]
                     ["author" :: (~ pr 'author 'user 'name)]])))

(def (maincmd opt)
  (def args (~ opt 'args))

  (def listprscmd
    (command 'list-prs
             (option 'direction "-d" "--direction"
                               default: "incoming"
                               help: "incoming or outgoing"
                               value: (lambda (r) (if (or (equal? r "incoming") (equal? r "outgoing"))
                                                    r
                                                    (error "direction must be incoming or outgoing"))))
             (option 'state "-s" "--state"
                               default: "open"
                               help: "open, declined, merged, all"
                               value: (lambda (r) (if (or (equal? r "all")
                                                          (equal? r "merged")
                                                          (equal? r "open")
                                                          (equal? r "declined"))
                                                    r
                                                    (error "state must be open, declined, merged or all"))))
             (option 'project "-p" "--project"
                     default: (default-project)
                     help: "project of the repository")
             (optional-argument 'repository
                                default:(current-repo)
                                help: "repository")
             help: "list repository pull requests"))

  (def helpcmd
    (command 'help help: "display repository usage help"
             (optional-argument 'command value: string->symbol)))

  (def gopt (getopt listprscmd
                    helpcmd))
  (try
   (let* (((values cmd opt) (getopt-parse gopt args))
          (project (or (~ opt 'project) (default-project))))
     (case cmd
       ((list-prs) (list-prs (~ opt 'project)
                             (~ opt 'repository)
                             (~ opt 'direction)
                             (~ opt 'state)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "repository"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "repository" (current-error-port)))))
