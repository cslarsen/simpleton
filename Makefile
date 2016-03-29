TARGETS := ex1

default: all

scm:
	@cat ex1.st | chibi-scheme trans.scm

all: $(TARGETS)

%: %.st
	./compile.sh $<

comp2: comp2.scm
	cat $< | chibi-scheme $<

clean:
	rm -f $(TARGETS) a.out
