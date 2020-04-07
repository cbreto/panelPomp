REXE = R --no-init-file --no-site-file
ROUTSAVES := $(shell ls *.Rout.save)

.PHONY: default

default: $(ROUTSAVES) clean

%.Rout.save: %.R
	$(REXE) < $*.R > $*.Rout.save 2>&1

%.Rout: %.R
	$(REXE) < $*.R > $*.Rout 2>&1

clean:
	$(RM) *.c *.o *.so 

fresh: clean
	$(RM) *.png
