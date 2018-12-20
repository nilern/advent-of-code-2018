.PHONY: marbles
marbles: $(shell find src -name '*.mlb' -or -name '*.sig' -or -name '*.sml')
	mkdir -p target
	mlton -output target/marbles src/marbles.mlb

.PHONY: clean
clean:
	rm -r target
