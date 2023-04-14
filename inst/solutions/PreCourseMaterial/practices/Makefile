
default: figures/histogram_pop_comparison.png

.PRECIOUS: %.csv

output/%_synthetic_pop.csv: synthesize.R input/%.json
	Rscript $^ $@

LOCATIONS = peru chile
POPFILES = $(patsubst %,output/%_synthetic_pop.csv,$(LOCATIONS))

output/measurements.rds: analyze.R $(POPFILES)
	Rscript $^ $@

figures/histogram_pop_comparison.png: plot.R $(POPFILES) output/measurements.rds
	Rscript $^ $@