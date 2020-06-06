.PHONY: all say_hello generate clean

all: compile move

compile:
	elm make src/Main.elm --output=main.js

move:
	cp main.js ../rwm-backend/resources/web/js/

run: compile
	elm reactor

productionMove: production move

production: compileOptimize uglify

compileOptimize:
	elm make src/Main.elm --output main.js --optimize

uglify:
	uglifyjs main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=main.js && uglifyjs main.js --mangle --output=main.js