all:
	@elm make --optimize src/Main.elm --output dist/bundle.js

watch:
	@elm make --debug src/Main.elm --output dist/bundle.js

.PHONY: all watch
