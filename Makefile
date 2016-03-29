TARGETS := ex1

scm:
	@cat ex1.st | chibi-scheme trans.scm

all: $(TARGETS)

%: %.st
	@python trans2.py $<

comp2: comp2.scm
	cat $< | chibi-scheme $<

clean:
	rm -f $(TARGETS) a.out
