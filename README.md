# stashcli - stash/bitbucket cli

This is a side project to discover and play with gerbil scheme.

stashcli is a linux static binary to interact with a stash/bitbucket api.

IMPORTANT: 
- The feature set is very minimalist.
- There's absolutely not intent to maintain this cli.

# build instructions

Use a docker gerbil environment to build the binary from sources

``` sh
git clone https://github.com/yanndegat/gerbil-docker
(cd gerbil-docker; docker build --build-arg GERBIL_VERSION=master -t yanndegat/gerbil:static .)

git clone https://github.com/yanndegat/stashcli
(cd stashcli; make linux-static)
ldd stashcli/stashcli-static
        not a dynamic executable
```


# use stashcli

## setup a ~/.stashrc.yaml

```yaml
---
token: "MDM...."
url: "https://stash.mycomp.net"
default-project: myproject
```

## use the cli

```sh
stashcli help
Usage: stashcli [option ...] <command> command-arg ...

Options:
 -c --config <config>             stash config file [default: ~/.stashrc.yaml]
 -r --remote <remote>             git remote [default: #f]
 -n                               disable coloured output
 -d                               debug mode

Commands:
 project:                         project interactions
 pr:                              pullrequest interactions
 inbox:                           inbox interactions
 repository:                      repository interactions
 help:                            display usage help
```
