PACKAGENAME=cameron
COLLECTS=cameron
MANUAL=cameron/scribble/cameron.scrbl

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf manual 2>/dev/null

setup:
	raco setup --tidy $(COLLECTS)

install-link:
	raco pkg install --link -n $(PACKAGENAME) $$(pwd)

uninstall:
	raco pkg remove --force $(PACKAGENAME)

# if any part of cameron is changed, run this to keep dependent pkgs in sync
refresh:
	raco setup --avoid-main

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


