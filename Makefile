SOURCES = \
Settings.ml \
Hirschberg.ml \
Dict.ml \
Author_I.ml \
Author.ml \
Parser.ml \
Compare_I.ml \
Compare.ml \
Main.ml




all: $(SOURCES)
	corebuild Main.native

check: $(SOURCES) Main.native
	@chmod u+x ../check_width
	@../check_width Settings.ml; \
        ../check_width Hirschberg.ml; \
	../check_width Dict.ml; \
	../check_width Author_I.ml; \
	../check_width Author.ml; \
	../check_width Parser.ml; \
	../check_width Compare.ml; \
	../check_width Compare_I.ml; \
	../check_width Main.ml

clean:
	rm -rf _build Main.native
