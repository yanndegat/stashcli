#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("stash/utils"
    "stash/api"
    "stash/context"
    "stash/inbox"
    "stash/project"
    "stash/repository"
    "stash/pullrequest"
    (exe: "stash/stashcli")))
