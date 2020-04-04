all: build run

build:
	elm make src/Main.elm --output=main.js

opt:
	elm make src/Main.elm --optimize --output=main.js

run:
	firefox ${PWD}/index.html

package:
	zip -r package.zip assets/* kaios/* index.html main.js manifest.webapp

.PHONY: build run
