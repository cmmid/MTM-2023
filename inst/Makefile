
R = $(strip Rscript $^ $(1) $@)

TARGETS := $(wildcard solutions/*/*.R)

$(info $(TARGETS))

default: $(patsubst solutions/,scripts/,$(TARGETS))

scripts/%: solnfilter.R solutions/%
	$(call R)
