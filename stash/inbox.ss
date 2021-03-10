;;; stashcli inbox commands
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/format
        :std/iter
        :std/ref
        :std/net/request
        :std/text/json
        :stash/utils)

(export (prefix-out maincmd inbox/))

;; display list of project repositoties
;; hash? string? -> any?
(def (list-prs role)
  (def url (stash-url (format "/api/1.0/inbox/pull-requests?role=~a" role)))
  (def req (http-get url headers: (default-http-headers)))
  (def body (request-json req))

  (unless (request-success? req) (error (json-object->string body)))
  (for (pr (~ body 'values))
    (display-line [["project" :: (~ pr 'toRef 'repository 'project 'key)]
                   ["repo" :: (~ pr 'toRef 'repository 'slug)]
                   ["id" :: (~ pr 'id)]
                   ["ref" :: (~ pr 'toRef 'displayId)]
                   ["state" :: (format-pr-state (~ pr 'state))]
                   ["status" :: (format-pr-approval-status pr)]
                   ["title" :: (~ pr 'title)]
                   ["author" :: (if (equal? role "author") "me" (~ pr 'author 'user 'name))]])))

(def (maincmd opt)
  (def args (~ opt 'args))

  (def listprscmd
    (command 'list-prs (option 'role "-r" "--role"
                               default: "reviewer"
                               help: "author or reviewer"
                               value: (lambda (r) (if (or (equal? r "author") (equal? r "reviewer"))
                                                    r
                                                    (error "role must be author or reviewer"))))
             help: "list inbox pull requests"))

  (def helpcmd
    (command 'help help: "display inbox usage help"
             (optional-argument 'command value: string->symbol)))

  (def gopt (getopt listprscmd
                    helpcmd))
  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (case cmd
       ((list-prs) (list-prs (~ opt 'role)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "inbox"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "inbox" (current-error-port)))))
