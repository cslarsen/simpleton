TARGETS := ex1

scm:
	@cat ex1.st | chibi-scheme trans.scm

all: $(TARGETS)

%: %.st
	@python trans2.py $<

clean:
	rm -f $(TARGETS) a.out
