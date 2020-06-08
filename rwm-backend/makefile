.PHONY: all say_hello generate clean

all:
	$(warning You should not run everything at once)

build:
	stack build

run: getWebResources build
	stack run