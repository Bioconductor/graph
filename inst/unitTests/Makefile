TOP=../../..
RUNIT_DIR=$(shell pwd)

all: test

test:
	cd ${TOP}; \
	R CMD INSTALL graph && \
	cd ${RUNIT_DIR}
	R --slave < runalltests.R

