all:
	@elm make --debug src/Main.elm --output dist/bundle.js

.PHONY: all
