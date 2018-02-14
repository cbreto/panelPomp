R_ = R --no-init-file --no-site-file
R_CMD = $(R_) CMD

SAVED = $(shell ls *.Rout.save)

default: $(SAVED)

%.Rout.save: %.R
	$(R_CMD) BATCH --no-restore --no-save --no-timing $*.R $*.Rout.save
