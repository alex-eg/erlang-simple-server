MODULES=log server client
FILES=$(addsuffix .erl,$(MODULES))

all: clean compile

clean:
	rm -f ./ebin/*.beam

compile: $(FILES)
	erlc -o ./ebin $(FILES)

.PHONY: clean
