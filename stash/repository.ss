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

(def (list-branches project repo)
  (let ((branches (repo-branches project repo)))
    (for (b branches)
      (if (~ b 'isDefault)
        (display-line default-colors?: #f
                      [["[green]id[reset]" :: (format "[underline]~a[reset]"(~ b 'id))]
                       ["[green]displayId[reset]" :: (~ b 'displayId)]
                       ["[underline][green]default[reset]" :: (~ b 'isDefault)]
                       ["[green]commit[reset]" :: (~ b 'latestCommit)]])
        (display-line [["id" :: (~ b 'id)]
                       ["displayId" :: (~ b 'displayId)]
                       ["default" :: (~ b 'isDefault)]
                       ["commit" :: (~ b 'latestCommit)]])))))

(def (maincmd opt)
  (def args (~ opt 'args))

  (def helpcmd
    (command 'help help: "display repository usage help"
             (optional-argument 'command value: string->symbol)))

  (def listbranchescmd
    (command 'list-branches
             (option 'project "-p" "--project"
                     default: (default-project)
                     help: "project of the repository")

             (optional-argument 'repository
                                default:(current-repo)
                                help: "repository")
             help: "list repository branches"))

  (def gopt (getopt (pullrequest/listcmd 'list-prs)
                    listbranchescmd
                    helpcmd))
  (try
   (let* (((values cmd opt) (getopt-parse gopt args))
          (project (or (~ opt 'project) (default-project))))
     (case cmd
       ((list-prs) (pullrequest/list (~ opt 'project)
                                     (~ opt 'repository)
                                     (~ opt 'direction)
                                     (~ opt 'state)))
       ((list-branches) (list-branches (~ opt 'project)
                                       (~ opt 'repository)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "repository"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "repository" (current-error-port)))))
