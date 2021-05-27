;;; stashcli project commands
(import :gerbil/gambit
        :std/format
        :std/iter
        :std/logger
        :std/misc/hash
        :std/misc/ports
        :std/misc/process
        :std/misc/string
        :std/misc/symbol
        :std/pregexp
        :std/ref
        :std/srfi/13
        :std/sugar
        :std/text/json
        :std/text/yaml
        :stash/api
        :url-string/url-string
        :colorstring/colorstring)

(export #t)
(def debug-mode? (make-parameter #f))
(def quiet-mode? (make-parameter #f))

(def rx-upstream-track "^# branch.upstream ([[:alnum:]]+)/(.+)$")

(def (upstream-track)
  (and (current-upstream)
       (pregexp-replace rx-upstream-track (current-upstream) "\\1")))

(def (upstream-branch)
  (and (current-upstream)
       (pregexp-replace rx-upstream-track (current-upstream) "\\2")))

(def (default-project) (or (current-project) (~ (context) 'default-project)))

(def (default-remote-branch
      project: (project (current-project))
      repo: (repo (current-repo)))
  (and project
       repo
       (car (filter (cut ~ <> 'isDefault)
                    (projects/repos/branches (context) project repo)))))

(def git-remote (make-parameter #f))
(def (init-git-remote remote)
  (unless (or (and remote (git-remote remote))
              (and (upstream-track) (git-remote (upstream-track))))
    (error "could not init git remote: try with --remote (e.g.: '--remote origin').")))

(def current-upstream (make-parameter #f))
(def (init-current-upstream)
  (current-upstream
   (try
    (let ((status (pregexp-split "\n" (run-process '("git" "status" "--porcelain=2" "-b")
                                                   stderr-redirection: #t))))
      (car (filter (cut pregexp-match rx-upstream-track <>) status)))
    (catch _ #f))))

(def current-repo-url (make-parameter #f))
(def (init-current-repo-url git-remote)
  (current-repo-url
   (let* ((track (or git-remote (upstream-track) "origin"))
          (remote-url (and track
                           (try (run-process `("git" "remote" "get-url" "--push" ,track)
                                             stderr-redirection: #t)
                                (catch _ #f)))))
     (and remote-url (string->url (string-trim-eol remote-url))))))

(def current-project (make-parameter #f))
(def (init-current-project)
  (current-project
   (let ((repo-url (current-repo-url)))
     (and repo-url (path/param-path (car (url-path repo-url)))))))

(def current-repo (make-parameter #f))
(def (init-current-repo)
     (current-repo
      (let ((repo-url (current-repo-url)))
        (and repo-url (string-trim-suffix ".git" (path/param-path (cadr (url-path repo-url))))))))

(def current-remote-branch (make-parameter #f))
(def (init-current-remote-branch)
     (current-remote-branch
      (and (current-project) (current-repo) (upstream-branch)
           (let* ((branches (projects/repos/branches (context) (current-project) (current-repo)))
                  (branch (find (lambda (b) (or (equal? (upstream-branch) (hash-ref b 'id #f))
                                                (equal? (upstream-branch) (hash-ref b 'displayId #f))))
                                branches)))
             (and branch (~ branch 'displayId))))))

(def context (make-parameter #f))
(def (init-context path)
  (def yaml-data (yaml-load path))
  (try
   (def context-data (car yaml-data))
   (context (hash (token (hash-ref context-data "token"))
                  (default-project (hash-ref context-data "default-project" #f))
                  (url (hash-ref context-data "url"))))
   (catch (error? exn)
     (error (format "Could not parse your context ~a" path)))))


(def (init path
           debug?: (debug? #f)
           quiet?: (quiet? #f)
           git-remote: (remote #f))

     (debug-mode? debug?)
     (quiet-mode? quiet?)

     (start-logger!)
     (and (debug-mode?) (debug "init context from: ~a" path))
     (init-context path)

     (let ((within-gitdir (try (run-process `("git" "status")
                                            stderr-redirection: #t)
                               (catch _ #f))))
       (cond
        ((not within-gitdir) (and (debug-mode?) (debug "not within gitdir")))
        (else
         (git-remote remote)
         (init-current-upstream)
         (and (debug-mode?) (debug "init current upstream: ~a" (current-upstream)))
         (init-current-repo-url remote)
         (and (debug-mode?) (debug "init current repo url: ~a" (url->string (current-repo-url))))
         (init-current-project)
         (and (debug-mode?) (debug "init current project: ~a" (current-project)))
         (init-current-repo)
         (and (debug-mode?) (debug "init current repo: ~a" (current-repo)))
         (init-current-remote-branch)
         (and (debug-mode?) (debug "init current remote branch: ~a" (current-remote-branch)))
         (init-git-remote remote)
         (and (debug-mode?) (debug "init git remote: ~a" (git-remote)))
         (and (debug-mode?) (debug "context default project is: ~a" (default-project)))))))
