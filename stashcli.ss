;;; stashcli command line client
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/ref
        :stash/project
        :stash/inbox
        :stash/utils)

(export main)

(def (main . args)
  (def projectcmd
    (command 'project help: "project interactions"
             (rest-arguments 'args help: "project args")))
  (def inboxcmd
    (command 'inbox help: "inbox interactions"
             (rest-arguments 'args help: "inbox args")))
  (def helpcmd
    (command 'help help: "display usage help"
             (optional-argument 'command value: string->symbol)))

  (def gopt
    (getopt (option 'config "-c" "--config"
                    default: "~/.stashrc.yaml"
                    help: "stash config file")
            projectcmd
            inboxcmd
            helpcmd))
  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (config (load-config (~ opt 'config)))
     (case cmd
       ((project) (project/maincmd opt))
       ((inbox) (inbox/maincmd opt))
       ((help)
        (getopt-display-help-topic gopt (hash-get opt 'command) "stashcli"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "stashcli" (current-error-port))
     (exit 1))))
