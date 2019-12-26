help:
	@cat Makefile

build:
	elm make Main.elm --output=index.html

open:
	open index.html

b: build
o: open
