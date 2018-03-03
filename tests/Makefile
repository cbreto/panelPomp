R_ = R --no-init-file --no-site-file
R_CMD = $(R_) CMD

ROUTSAVE := $(shell ls *.Rout.save)
ROUT := $(shell ls *.Rout)

.PHONY: default
default:
	for f in $(ROUT); do (make $$f); done

.PHONY: new
new:
	for f in $(ROUTSAVE); do (make $$f); done

%.Rout: %.R
	$(R_CMD) BATCH --no-restore --no-save --no-timing $*.R $*.Rout

%.Rout.save: %.R
	$(R_CMD) BATCH --no-restore --no-save --no-timing $*.R $*.Rout.save
