all: build run

build:
	elm make src/Main.elm

opt:
	elm make src/Main.elm --optimize

run:
	firefox ${PWD}/index.html

.PHONY: build run
