all: build run

build:
	elm make src/Main.elm --output=main.js

opt:
	elm make src/Main.elm --optimize --output=main.js

run:
	firefox ${PWD}/index.html

package:
	rm -f package.zip
	zip -r package.zip assets/* kaios/* index.html main.js manifest.webapp

.PHONY: build run
