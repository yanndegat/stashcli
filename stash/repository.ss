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

(export (prefix-out maincmd repository/)
        (prefix-out infocmd repository/)
        (prefix-out info repository/))

(def (list-branches project repo)
  (let ((branches (repo-branches project repo)))
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
  (def url (stash-url (format "/api/1.0/projects/~a/repos/~a"
                              project repo)))
  (def req (http-get url headers: (default-http-headers)))
  (def body (request-json req))
  (unless (request-success? req) (error (json-object->string body)))
  (display-attrs body))

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
    (command 'list-branches
             (option 'project "-p" "--project"
                     default: (default-project)
                     help: "project of the repository")

             (optional-argument 'repository
                                default:(current-repo)
                                help: "repository")
             help: "list repository branches"))

  (def gopt (getopt (pullrequest/listcmd 'list-prs)
                    (infocmd 'info)
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
       ((info) (info (~ opt 'project)
                     (~ opt 'repository)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "repository"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "repository" (current-error-port)))))
