#!/usr/local/bin/gxi
;; -*- Gerbil -*-

(import :std/make)

(def static? (make-parameter #f))

;; the library module build specification
(def lib-build-spec
  '("utils.ss" "project.ss" "repository.ss" "inbox.ss"))

(def (bin-build-spec)
  (if (static?)
    '((static-exe: "stashcli" "-cc-options" "-Bstatic" "-ld-options" "-static -lyaml -lssl -lz"))
    '((exe: "stashcli"))))

;; the source directory anchor
(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def libsrcdir
  (path-normalize (path-directory "src/stash/")))

;; the main function of the script
(def (main . args)
  (match args
    (["lib"]
     ;; this action builds the library modules -- with static compilation artifacts
     (make srcdir: libsrcdir
           bindir: libsrcdir
           optimize: #t
           debug: 'src             ; enable debugger introspection for library modules
           static: (static?)              ; generate static compilation artifacts; required!
           prefix: "stash"
           lib-build-spec))

    (["bin"]
     ;; this action builds the static executables -- no debug introspection
     (make srcdir: srcdir
           bindir: srcdir
           optimize: #t
           debug: #f               ; no debug bloat for executables
           static: (static?)              ; generate static compilation artifacts; required!
           (bin-build-spec)))

    (["static"]
     (static? #t)
     (main "lib")
     (main "bin"))

    ;; this is the default action, builds libraries and executables
    ([]
     (main "lib")
     (main "bin"))))
