.PHONY: linux-static linux-static-docker deps build clean

##
# stashcli
#
# @file
# @version 0.1

GERBIL_PATH ?= $(PWD)/.gerbil
all: build

deps:
	$(GERBIL_HOME)/bin/gxpkg install github.com/yanndegat/colorstring
	$(GERBIL_HOME)/bin/gxpkg install github.com/yanndegat/gerbil-url-string

build: deps
	GERBIL_PATH=$(GERBIL_PATH) ./build.ss

linux-static: build
	GERBIL_PATH=$(GERBIL_PATH) $(GERBIL_HOME)/bin/gxc -o stashcli-static \
    -cc-options "-Bstatic" \
    -static \
    -ld-options "-static -lpthread -L/usr/lib64 -lssl -ldl -lyaml -lz" \
    -exe stash/stashcli.ss

linux-static-docker:
	docker run --net host \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src \
	yanndegat/gerbil:static make -C /src linux-static

clean:
	rm -Rf $(GERBIL_PATH) stashcli-static
