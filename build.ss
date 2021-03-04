#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("stash/utils" "stash/inbox" "stash/project" "stash/repository"
    (exe: "stash/stashcli")))
