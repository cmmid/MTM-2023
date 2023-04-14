
R = $(strip Rscript $^ $(1) $@)

TARGETS := $(shell find solutions/ -type f -name '*.R')

default: $(subst solutions/,scripts/,$(TARGETS))

scripts/%: solnfilter.R solutions/%
	$(call R)

check:
	diff -r scripts/ solutions/ | more
