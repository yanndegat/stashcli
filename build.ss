#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("stash/inbox"
    "stash/project"
    "stash/utils"
    "stash/repository"
    "stash/pullrequest"
    (exe: "stash/stashcli")))
