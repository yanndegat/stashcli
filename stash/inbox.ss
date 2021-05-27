;;; stashcli inbox commands
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/format
        :std/iter
        :std/ref
        :std/srfi/13
        :std/text/json
        :stash/api
        :stash/context
        :stash/utils)

(export (prefix-out maincmd inbox/))

;; display list of project repositoties
;; hash? string? -> any?
(def (list-prs role all?)
     (let ((prs (filter (lambda (pr) (or all?
                                         (equal? (string-downcase (default-project))
                                                 (string-downcase (~ pr 'toRef 'repository 'project 'key)))))
                        (inbox/pull-requests (context) role))))
       (for (pr prs)
            (display-line [(if all? ["project" :: (~ pr 'toRef 'repository 'project 'key)] #f)
                           ["repo" :: (~ pr 'toRef 'repository 'slug)]
                           ["id" :: (~ pr 'id)]
                           ["version" :: (~ pr 'version)]
                           ["ref" :: (~ pr 'toRef 'displayId)]
                           ["state" :: (format-pr-state (~ pr 'state))]
                           ["status" :: (format-pr-approval-status pr)]
                           ["build-status" :: (format-build-status
                                               (build-status/commits (context) (~ pr 'fromRef 'latestCommit)))]
                           ["title" :: (~ pr 'title)]
                           ["author" :: (if (equal? role "author") "me" (~ pr 'author 'user 'name))]]))))

(def (maincmd opt)
  (def args (~ opt 'args))

  (def listprscmd
       (command 'prs
                (flag 'all "-a" help: "all projects")
                (option 'role "-r" "--role"
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
       ((prs) (list-prs (~ opt 'role) (hash-ref opt 'all #f)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "inbox"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "inbox" (current-error-port)))))
