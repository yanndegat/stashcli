;;; stashcli command line client
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/ref
        :colorstring/colorstring
        :stash/inbox
        :stash/project
        :stash/pullrequest
        :stash/repository
        :stash/utils)

(export main)

(def (main . args)
  (def inboxcmd
    (command 'inbox help: "inbox interactions"
             (rest-arguments 'args help: "inbox args")))
  (def projectcmd
    (command 'project help: "project interactions"
             (rest-arguments 'args help: "project args")))
  (def pullrequestcmd
    (command 'pr help: "pullrequest interactions"
             (rest-arguments 'args help: "pullrequest args")))
  (def repositorycmd
    (command 'repository help: "repository interactions"
             (rest-arguments 'args help: "repository args")))
  (def helpcmd
    (command 'help help: "display usage help"
             (optional-argument 'command value: string->symbol)))

  (def gopt
    (getopt (option 'config "-c" "--config"
                    default: "~/.stashrc.yaml"
                    help: "stash config file")
            (flag 'no-color "-n" help: "disable coloured output")
            projectcmd
            pullrequestcmd
            inboxcmd
            repositorycmd
            helpcmd))
  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (config (load-config (~ opt 'config)))
     (color-disabled (hash-ref opt 'no-color #f))
     (case cmd
       ((inbox) (inbox/maincmd opt))
       ((project) (project/maincmd opt))
       ((pr) (pullrequest/maincmd opt))
       ((repository) (repository/maincmd opt))
       ((help)
        (getopt-display-help-topic gopt (hash-get opt 'command) "stashcli"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "stashcli" (current-error-port))
     (exit 1))))
