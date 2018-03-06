R_ = R --no-init-file --no-site-file
R_CMD = $(R_) CMD

RS := $(basename $(shell ls *.R 2>&1))
ROUTSAVES := $(shell ls *.Rout.save 2>&1)
ROUTS := $(shell ls *.Rout 2>&1)

.PHONY: default
default:
	for f in $(ROUTS); do (make $$f); done

.PHONY: new
new:
	for f in $(ROUTSAVES); do (make $$f); done

.PHONY: routs
routs:
	for f in $(RS); do (make $$f.Rout); done

.PHONY: routsaves
routsaves:
	for f in $(RS); do (make $$f.Rout.save); done

%.Rout: %.R
	$(R_CMD) BATCH --no-restore --no-save --no-timing $*.R $*.Rout

%.Rout.save: %.R
	$(R_CMD) BATCH --no-restore --no-save --no-timing --quiet $*.R $*.Rout.save
