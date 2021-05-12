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
        :stash/utils
        :stash/context)

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
                  (option 'remote "-r" "--remote"
                          default: #f
                          help: "git remote")
                  (flag 'no-color "-n" help: "disable coloured output")
                  (flag 'debug "-d" help: "debug mode")
                  (flag 'quiet "-q" help: "quiet mode: only field values are displayed")
                  projectcmd
                  pullrequestcmd
                  inboxcmd
                  repositorycmd
                  helpcmd))
     (try
      (let ((values cmd opt) (getopt-parse gopt args))
        (when (equal? 'help cmd)
          (and (getopt-display-help-topic gopt (hash-get opt 'command) "stashcli")
               (exit 0)))

        (init (~ opt 'config)
              debug?: (hash-ref opt 'debug #f)
              quiet?: (hash-ref opt 'quiet #f)
              git-remote: (hash-ref opt 'remote #f))
        (color-disabled (hash-ref opt 'no-color #f))
        (case cmd
          ((inbox) (inbox/maincmd opt))
          ((project) (project/maincmd opt))
          ((pr) (pullrequest/maincmd opt))
          ((repository) (repository/maincmd opt))))
      (catch (getopt-error? exn)
             (and (getopt-display-help exn "stashcli" (current-error-port))
                  (exit 1)))))
