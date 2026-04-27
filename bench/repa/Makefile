ROOT := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))/../..
PROJECT := $(ROOT)/bench/repa/cabal.project

.PHONY: build run clean

build:
	cd $(ROOT) && cabal build --project-file=$(PROJECT) exe:repa-bench

run:
	cd $(ROOT)/bench/repa && ./run.sh

clean:
	cd $(ROOT) && cabal clean --project-file=$(PROJECT)
