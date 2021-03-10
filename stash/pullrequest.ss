;;; stashcli repository commands
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/format
        :std/iter
        :std/ref
        :std/misc/process
        :std/net/request
        :std/text/json
        :stash/utils)

(export
  (prefix-out maincmd pullrequest/)
  (prefix-out list pullrequest/)
  (prefix-out listcmd pullrequest/))

;; display list of pull requests on a repository
;; hash? string? -> any?
(def (diff project repo id git-mode: (git-mode #f))
  (def url (stash-url (format
                       "/api/1.0/projects/~a/repos/~a/pull-requests/~a/diff"
                       project repo id)))
  (def req (http-get url headers: (default-http-headers)))
  (def body (request-json req))

  (unless (request-success? req) (error (json-object->string body)))

  (displayln (if git-mode
               (run-process `("git" "--no-pager" "diff" ,(~ body 'fromHash) ,(~ body 'toHash))
                            pseudo-terminal: #t)
               (json-object->string body))))

(def (list project repo direction state)
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


(def (listcmd id)
  (command id
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
             (flag 'git "-g" help: "git diff mode")
             (argument 'id help: "id of the pull request")
             help: "shows pull request diff"))

  (def helpcmd
    (command 'help help: "display repository usage help"
             (optional-argument 'command value: string->symbol)))

  (def gopt (getopt diffcmd
                    (listcmd 'list)
                    helpcmd))
  (try
   (let* (((values cmd opt) (getopt-parse gopt args))
          (project (or (~ opt 'project) (default-project))))
     (case cmd
       ((diff) (diff (~ opt 'project)
                     (~ opt 'repository)
                     (~ opt 'id)
                     git-mode: (hash-ref opt 'git #f)))
       ((list) (list (~ opt 'project)
                     (~ opt 'repository)
                     (~ opt 'direction)
                     (~ opt 'state)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "pull request"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "pull request" (current-error-port)))))
