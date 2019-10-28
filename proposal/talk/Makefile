TALK=proposal.ss
RACO=raco

all: preview

compile:
	@${RACO} make -v ${TALK}

pict: compile
	@${RACO} pict ${TALK}

preview: compile
	@${RACO} slideshow --right-half-screen ${TALK}

show: compile
	@${RACO} slideshow ${TALK}

pdf: compile
	@${RACO} slideshow --condense --pdf ${TALK}
