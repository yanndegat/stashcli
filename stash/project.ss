;;; stashcli project commands
(import :gerbil/gambit
        :std/sugar
        :std/getopt
        :std/format
        :std/iter
        :std/ref
        :stash/api
        :stash/context
        :stash/utils)

(export (prefix-out maincmd project/))

;; display list of project repositoties
;; hash? string? -> any?
(def (list-repositories project)
  (for (repo (projects/repos (context) project))
    (display-line [["repo" :: (~ repo 'name)]
                   ["desc" :: (hash-get repo 'description)]])))

;; display list of project default reviewers conditions
;; hash? string? -> any?
(def (list-reviewers project)
  (for (c (default-reviewers/projects/conditions (context) project))
    (display-line [["src" :: (~ c 'sourceRefMatcher 'type 'name)]
                   ["target" :: (~ c 'targetRefMatcher 'type 'name)]
                   ["users" :: (string-join (map (cut ~ <> 'displayName) (~ c 'reviewers)) ",")]
                   ["approvals" :: (~ c 'requiredApprovals)]])))

(def (maincmd opt)
  (def args (~ opt 'args))

  (def listreposcmd
    (command 'repositories help: "list repositories in a project"
             (optional-argument 'project help: "project"
                                default: #f)))

  (def listreviewerscmd
    (command 'reviewers help: "list default reviewers in a project"
             (optional-argument 'project help: "project"
                                default: #f)))

  (def helpcmd
    (command 'help help: "display project usage help"
             (optional-argument 'command value: string->symbol)))

  (def gopt (getopt listreviewerscmd
                    listreposcmd
                    helpcmd))
  (try
   (let* (((values cmd opt) (getopt-parse gopt args))
          (project (or (~ opt 'project) (default-project))))
     (unless project (error "no project specified."))
     (case cmd
       ((repositories) (list-repositories project))
       ((reviewers) (list-reviewers project))
       ((help)
        (getopt-display-help-topic gopt (~ opt 'command) "project"))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "project" (current-error-port)))))
