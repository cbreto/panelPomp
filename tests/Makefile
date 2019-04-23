R_ = R --no-init-file --no-site-file
R_CMD_BATCH = $(R_) CMD BATCH --no-restore --no-save

ROUTSAVES := $(shell ls *.Rout.save)

.PHONY: default
default:
	for f in $(ROUTSAVES); do (make $$f); done

%.Rout.save: %.R
	$(R_CMD_BATCH) --no-timing $*.R $*.Rout.save
