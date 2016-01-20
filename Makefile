PACKAGENAME=cameron
COLLECTS=cameron
MANUAL=cameron/scribble/cameron.scrbl

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf manual 2>/dev/null

setup:
	raco setup --tidy $(COLLECTS)

link:
	raco pkg install --link -n $(PACKAGENAME) $$(pwd)

unlink:
	raco pkg remove $(PACKAGENAME)

#Run all tests.
test:
	raco test -x .

# Compile documentation
manual: $(MANUAL)
	raco scribble \
		--html \
		--dest manual \
		--dest-name index \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		$(MANUAL)


