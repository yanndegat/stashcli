(import :gerbil/gambit
        :std/sugar
        :std/format
        :std/iter
        :std/ref
        :std/srfi/13
        :std/net/request
        :url-string/url-string
        :std/text/json )

(export #t)

(def (stash-url ctx path)
  (format "~a/rest~a" (~ ctx 'url) path))

(def (default-http-headers ctx)
  (let-hash ctx
    [["User-Agent" :: "Mozilla/5.0 (compatible; gerbil/1.0)"]
     ["Connection" :: "close"]
     ["Accept" :: "*/*"]
     ["Accept-Encoding" :: "gzip, deflate, identity"]
     ["Content-type" :: "application/json"]
     ["Authorization" :: (format "Bearer ~a" .token) ]]))

(def (json-http-headers ctx)
  (let-hash ctx
    [["User-Agent" :: "Mozilla/5.0 (compatible; gerbil/1.0)"]
     ["Connection" :: "close"]
     ["Accept" :: "application/json"]
     ["Accept-Encoding" :: "gzip, deflate, identity"]
     ["Content-type" :: "application/json"]
     ["Authorization" :: (format "Bearer ~a" .token) ]]))

(def (request-success? req)
  (and (>= (request-status req) 200) (<= (request-status req) 299)))

(def (projects/repo ctx project repo)
  (def url (stash-url ctx (format "/api/1.0/projects/~a/repos/~a"
                              project repo)))
  (def req (http-get url headers: (default-http-headers ctx)))
  (let ((body (request-json req)))
    (unless (request-success? req) (error (json-object->string body)))
    body))

(def (projects/repos ctx project limit: (limit 1000))
  (def url (stash-url ctx (format "/api/1.0/projects/~a/repos?limit=~a" project limit)))
  (def req (http-get url headers: (default-http-headers ctx)))
  (def body (request-json req))
  (unless (request-success? req) (error (json-object->string body)))
  (~ body 'values))

(def (projects/repos/branches ctx project repo)
  (def url (stash-url ctx (format
                       "/api/1.0/projects/~a/repos/~a/branches"
                       project repo)))
  (def req (http-get url headers: (default-http-headers ctx)))
  (def body (request-json req))
  (unless (request-success? req) (error (json-object->string body)))
  (~ body 'values))

(def (projects/repos/pull-requests ctx project repo direction state)
  (def url (stash-url ctx (format
                       "/api/1.0/projects/~a/repos/~a/pull-requests?direction=~a&state=~a"
                       project repo direction state)))
  (def req (http-get url headers: (default-http-headers ctx)))
  (def body (request-json req))
  (unless (request-success? req) (error (json-object->string body)))
  (~ body 'values))

(def (projects/repos/pull-requests/create
      ctx project repo from-branch to-branch title desc reviewers)
  (def url (stash-url ctx (format
                           "/api/1.0/projects/~a/repos/~a/pull-requests?create"
                           project repo)))

  (def data (hash (fromRef (hash (id from-branch)
                                 (repository (hash (project (hash (key project)))
                                                   (slug (format "~a" repo))))))
                  (toRef (hash (id to-branch)
                               (repository (hash (project (hash (key project)))
                                                 (slug (format "~a" repo))))))
                  (title title)
                  (description (or desc ""))
                  (reviewers reviewers)))

  (def req (http-post url
                      headers: (default-http-headers ctx)
                      data: (json-object->string data)))
  (def body (request-json req))
  (unless (request-success? req) (error (~ body 'errors 0 'message)))
  body)

(def (projects/repos/pull-requests/diff ctx project repo id)
  (def url (stash-url ctx (format
                       "/api/1.0/projects/~a/repos/~a/pull-requests/~a/diff"
                       project repo id)))
  (def req (http-get url headers: (default-http-headers ctx)))
  (def body (request-json req))
  (unless (request-success? req) (error (~ body 'errors 0 'message)))
  body)

(def (projects/repos/pull-request ctx project repo id)
  (def url (stash-url ctx (format
                           "/api/1.0/projects/~a/repos/~a/pull-requests/~a"
                           project repo id)))
  (def req (http-get url headers: (default-http-headers ctx)))
  (let ((body (request-json req)))
    (def body (request-json req))
    (unless (request-success? req) (error (~ body 'errors 0 'message)))
    body))

(def (projects/repos/pull-requests/delete ctx project repo id version: (version 0))
     (def url (stash-url ctx (format
                              "/api/1.0/projects/~a/repos/~a/pull-requests/~a"
                              project repo id)))
     (def req (http-delete url
                           headers: (json-http-headers ctx)
                           data: (json-object->string (hash (version version)))))
     (unless (request-success? req) (error (~ (request-json req) 'errors 0 'message)))
     #t)

(def (projects/repos/pull-requests/approve ctx project repo id version: (version 0))
  (def url (stash-url ctx (format
                           "/api/1.0/projects/~a/repos/~a/pull-requests/~a/approve"
                           project repo id)))
  (def req (http-post url
                        headers: (json-http-headers ctx)
                        data: (json-object->string (hash (version version)))))
  (unless (request-success? req) (error (~ (request-json req) 'errors 0 'message)))
  #t)

(def (projects/repos/pull-requests/unapprove ctx project repo id version: (version 0))
  (def url (stash-url ctx (format
                           "/api/1.0/projects/~a/repos/~a/pull-requests/~a/approve"
                           project repo id)))
  (def req (http-delete url
                        headers: (json-http-headers ctx)
                        data: (json-object->string (hash (version version)))))
  (unless (request-success? req) (error (~ (request-json req) 'errors 0 'message)))
  #t)

(def (projects/repos/pull-requests/decline ctx project repo id version: (version 0))
  (def url (stash-url ctx (format
                           "/api/1.0/projects/~a/repos/~a/pull-requests/~a/decline"
                           project repo id)))
  (def req (http-post url
                        headers: (json-http-headers ctx)
                        data: (json-object->string (hash (version version)))))
  (unless (request-success? req) (error (~ (request-json req) 'errors 0 'message)))
  #t)

(def (projects/repos/pull-requests/merge ctx project repo id version: (version 0))
  (def url (stash-url ctx (format
                           "/api/1.0/projects/~a/repos/~a/pull-requests/~a/merge"
                           project repo id)))
  (def req (http-post url
                        headers: (json-http-headers ctx)
                        data: (json-object->string (hash (version version)))))
  (unless (request-success? req) (error (~ (request-json req) 'errors 0 'message)))
  #t)

(def (inbox/pull-requests ctx role)
  (def url (stash-url ctx (format "/api/1.0/inbox/pull-requests?role=~a&limit=100" role)))
  (def req (http-get url headers: (default-http-headers ctx)))
  (def body (request-json req))
  (unless (request-success? req) (error (json-object->string body)))
  (~ body 'values))

(def (default-reviewers/projects/conditions ctx project)
  (def url (stash-url ctx (format "/default-reviewers/1.0/projects/~a/conditions" project)))
  (def req (http-get url headers: (default-http-headers ctx)))
  (def body (request-json req))
  (unless (request-success? req) (error (json-object->string body)))
  body)

(def (build-status/commits ctx id)
  (def url (stash-url ctx (format "/build-status/1.0/commits/~a" id)))
  (def req (http-get url headers: (default-http-headers ctx)))
  (def body (request-json req))
  (unless (request-success? req) (error (json-object->string body)))
  body)
