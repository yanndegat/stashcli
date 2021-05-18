;;; stashcli repository commands
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/format
        :std/iter
        :std/ref
        :std/misc/process
        :std/text/json
        :stash/api
        :stash/context
        :stash/utils)

(export
  (prefix-out maincmd pullrequest/)
  (prefix-out list pullrequest/)
  (prefix-out listcmd pullrequest/))

;; display list of pull requests on a repository
;; hash? string? -> any?
(def (diff project repo id difftool-mode: (difftool-mode #f))
  (let ((body (projects/repos/pull-requests/diff (context) project repo id)))
    (unless (run-process `("git" "fetch"
                           ,(git-remote) ,(~ body 'fromHash) ,(~ body 'toHash)))
      (error "could not fetch refs ~a/~a, ~a/~a" repo (~ body 'fromHash) repo (~ body 'toHash)))
    (if difftool-mode
      (displayln (format "git difftool ~a ~a" (~ body 'fromHash) (~ body 'toHash)))
      (displayln (run-process `("git" "--no-pager" "diff"
                                ,(~ body 'fromHash) ,(~ body 'toHash))
                              pseudo-terminal: #t)))))

(def (list project repo direction state)
     (for (pr (projects/repos/pull-requests (context) project repo direction state))
          (displayln (json-object->string pr))
          (display-line [["id" :: (~ pr 'id)]
                         ["ref" :: (~ pr 'toRef 'displayId)]
                         ["state" :: (format-pr-state (~ pr 'state))]
                         ["status" :: (format-pr-approval-status pr)]
                         ["build-status" :: (format-build-status
                                             (build-status/commits (context) (~ pr 'fromRef 'latestCommit)))]
                         ["title" :: (~ pr 'title)]
                         ["author" :: (~ pr 'author 'user 'name)]])))

(def (info project repo id)
  (display-attrs (projects/repos/pull-request (context) project repo id)))

(def (delete project repo id version)
  (when (projects/repos/pull-requests/delete (context) project repo id version: version)
    (displayln "deleted!")))

(def (approve project repo id version)
  (when (projects/repos/pull-requests/approve (context) project repo id version: version)
    (displayln "approved!")))

(def (unapprove project repo id version)
  (when (projects/repos/pull-requests/unapprove (context) project repo id version: version)
    (displayln "unapproved!")))

(def (decline project repo id version)
  (when (projects/repos/pull-requests/decline (context) project repo id version: version)
    (displayln "declined!")))

(def (merge project repo id version)
  (when (projects/repos/pull-requests/merge (context) project repo id version: version)
    (displayln "merged!")))

(def (create project repo title from to reviewers desc)
  (unless project (error "no project specified."))
  (unless repo (error "no repository specified."))
  (unless from (error "no from branch specified."))
  (unless to (error "no from branch specified."))

  (let* ((conditions (default-reviewers/projects/conditions (context) project))
         (reviewers  (map (lambda (r)
                            (hash (user (hash (id  (~ r 'id))
                                              (name  (~ r 'name))
                                              (displayName  (~ r 'displayName))))))
                          (~ (car conditions) 'reviewers)))
         (pr (projects/repos/pull-requests/create (context) project repo from to title desc reviewers)))
    (display-line [["id" :: (~ pr 'id)]
                   ["href" :: (~ pr 'links 'self 0 'href)]])))


(def (actioncmd id)
     (command id
              (option 'project "-p" "--project"
                      default: (default-project)
                      help: "project of the repository")
              (option 'repository "-r" "--repository"
                      default: (current-repo)
                      help: "repository")
              (argument 'id help: "id of the pull request")
              (optional-argument 'version
                                 help: "pull request version"
                                 default: 0)
              help: (format "~a pull request" id)))

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
             (flag 'difftool "-dt" help: "git difftool mode")
             (argument 'id help: "id of the pull request")
             help: "shows pull request diff"))

  (def createcmd
    (command 'create
             (option 'project "-p" "--project"
                     default: (default-project)
                     help: "project of the repository")
             (option 'repository "-r" "--repository"
                     default: (current-repo)
                     help: "repository")
             (option 'reviewers "-R" "--reviewers"
                     default: #f
                     help: "repository")
             (option 'desc "-d" "--description"
                     default: #f
                     help: "description of the pull request. - for stdin")
             (argument 'title help: "title of the pull request")
             (optional-argument 'to
                                help: "destination branch"
                                default: (and (default-remote-branch)
                                              (hash-ref (default-remote-branch) 'id #f)))
             (optional-argument 'from help: "from branch"
                                default: (and (current-remote-branch)
                                              (hash-ref (current-remote-branch) 'id #f)))
             help: "create pull request"))

 (def infocmd
    (command 'info
             (option 'project "-p" "--project"
                     default: (default-project)
                     help: "project of the repository")
             (option 'repository "-r" "--repository"
                     default: (current-repo)
                     help: "repository")
             (argument 'id help: "id of the pull request")
             help: "info pull request"))

  (def helpcmd
    (command 'help help: "display repository usage help"
             (optional-argument 'command value: string->symbol)))

  (def gopt (getopt createcmd
                    (actioncmd 'delete)
                    (actioncmd 'unapprove)
                    (actioncmd 'approve)
                    (actioncmd 'decline)
                    (actioncmd 'merge)
                    infocmd
                    diffcmd
                    (listcmd 'list)
                    helpcmd))
  (try
   (let (((values cmd opt) (getopt-parse gopt args)))
     (case cmd
       ((diff) (diff (~ opt 'project)
                     (~ opt 'repository)
                     (~ opt 'id)
                     difftool-mode: (hash-ref opt 'difftool #f)))
       ((list) (list (~ opt 'project)
                     (~ opt 'repository)
                     (~ opt 'direction)
                     (~ opt 'state)))
       ((delete) (delete (~ opt 'project)
                         (~ opt 'repository)
                         (~ opt 'id)
                         (~ opt 'version)))
       ((merge) (merge (~ opt 'project)
                       (~ opt 'repository)
                       (~ opt 'id)
                       (~ opt 'version)))
       ((decline) (decline (~ opt 'project)
                           (~ opt 'repository)
                           (~ opt 'id)
                           (~ opt 'version)))
       ((approve) (approve (~ opt 'project)
                           (~ opt 'repository)
                           (~ opt 'id)
                           (~ opt 'version)))
       ((unapprove) (unapprove (~ opt 'project)
                               (~ opt 'repository)
                               (~ opt 'id)
                               (~ opt 'version)))
       ((info) (info (~ opt 'project)
                     (~ opt 'repository)
                     (~ opt 'id)))
       ((create) (create (~ opt 'project)
                         (~ opt 'repository)
                         (~ opt 'title)
                         (~ opt 'from)
                         (~ opt 'to)
                         (~ opt 'reviewers)
                         (~ opt 'desc)))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "pr"))))
   (catch (getopt-error? exn)
          (getopt-display-help exn "pr" (current-error-port)))))
