;;; stashcli repository commands
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/format
        :std/iter
        :std/ref
        :std/net/request
        :std/text/json
        :stash/pullrequest
        :stash/utils)

(export (prefix-out maincmd repository/))

(def (maincmd opt)
  (def args (~ opt 'args))

  (def helpcmd
    (command 'help help: "display repository usage help"
             (optional-argument 'command value: string->symbol)))

  (def gopt (getopt (pullrequest/listcmd 'list-prs)
                    helpcmd))
  (try
   (let* (((values cmd opt) (getopt-parse gopt args))
          (project (or (~ opt 'project) (default-project))))
     (case cmd
       ((list-prs) (pullrequest/list (~ opt 'project)
                                     (~ opt 'repository)
                                     (~ opt 'direction)
                                     (~ opt 'state)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "repository"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "repository" (current-error-port)))))
