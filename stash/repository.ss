;;; stashcli repository commands
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/format
        :std/iter
        :std/ref
        :std/text/json
        :stash/api
        :stash/context
        :stash/pullrequest
        :stash/utils)

(export (prefix-out maincmd repository/))

(def (status project repo branch-id)
     (unless (and project repo) (error "project and repo are mandatory"))
     (let* ((branches (projects/repos/branches (context) project repo))
            (branch (find (lambda (b) (or
                                       (and (not branch-id) (~ b 'isDefault))
                                       (equal? branch-id (~ b 'displayId)))) branches))
            (statuses (and branch (build-status/commits (context) (~ branch 'latestCommit)))))
            (cond
             ((not branch) (error (format "can't find branch ~a" branch-id)))
             (else
              (for (s (~ statuses 'values))
                   (display-line [["state" :: (format-build-status (~ s 'state))]
                                  ["name" :: (~ s 'name)]
                                  ["url" :: (~ s 'url)]]))))))

(def (list-branches project repo)
     (unless (and project repo) (error "project and repo are mandatory"))
     (let ((branches (projects/repos/branches (context) project repo)))
       (for (b branches)
            (if (~ b 'isDefault)
                (display-line default-colors?: #f
                              [["[red]id[reset]" :: (format "[underline]~a[reset]"(~ b 'id))]
                               ["[blue]displayId[reset]" :: (~ b 'displayId)]
                               ["[blue]default[reset]" :: (~ b 'isDefault)]
                               ["[blue]commit[reset]" :: (~ b 'latestCommit)]])
                (display-line [["id" :: (~ b 'id)]
                               ["displayId" :: (~ b 'displayId)]
                               ["default" :: (~ b 'isDefault)]
                               ["commit" :: (~ b 'latestCommit)]])))))

(def (info project repo)
     (unless (and project repo) (error "project and repo are mandatory"))
     (display-attrs (projects/repo (context) project repo)))

(def (infocmd id)
  (command id
           (option 'project "-p" "--project"
                   default: (default-project)
                   help: "project of the repository")

           (optional-argument 'repository
                              default:(current-repo)
                              help: "repository")
           help: "show repository information"))

(def (maincmd opt)
  (def args (~ opt 'args))

  (def helpcmd
    (command 'help help: "display repository usage help"
             (optional-argument 'command value: string->symbol)))

  (def listbranchescmd
    (command 'branches
             (option 'project "-p" "--project"
                     default: (default-project)
                     help: "project of the repository")
             (option 'repository "-r" "--repository"
                     default: (current-repo)
                     help: "repository")
             help: "list repository branches"))

  (def statuscmd
    (command 'status
             (option 'project "-p" "--project"
                     default: (default-project)
                     help: "project of the repository")
             (option 'repository "-r" "--repository"
                     default: (current-repo)
                     help: "repository")

             (optional-argument 'branch
                                default:(current-remote-branch)
                                help: "branch")
             help: "build status"))

  (def gopt (getopt (pullrequest/listcmd 'prs)
                    (infocmd 'info)
                    listbranchescmd
                    statuscmd
                    helpcmd))
  (try
   (let* (((values cmd opt) (getopt-parse gopt args))
          (project (or (~ opt 'project) (default-project))))
     (case cmd
       ((prs) (pullrequest/list (~ opt 'project)
                                (~ opt 'repository)
                                (~ opt 'direction)
                                (~ opt 'state)))
       ((branches) (list-branches (~ opt 'project)
                                  (~ opt 'repository)))
       ((status) (status (~ opt 'project)
                         (~ opt 'repository)
                         (~ opt 'branch)))
       ((info) (info (~ opt 'project)
                     (~ opt 'repository)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "repository"))))
   (catch (getopt-error? exn)
          (getopt-display-help exn "repository" (current-error-port)))))
