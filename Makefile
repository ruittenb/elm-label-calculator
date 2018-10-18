
install:
	elm install elm-lang/core

run:
	elm-make Main.elm --output index.js
	open index.html

