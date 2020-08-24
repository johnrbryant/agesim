## Input data

.PHONY : all
all : data/Lx_west.rda \
      data/mx_west.rda \
      data/Lx_south.rda \
      data/mx_south.rda \
      data/propn_age_fert_booth.rda \
      documentation

data/mx_west.rda : data-raw/mx_west.R
	Rscript $<

data/Lx_west.rda : data-raw/Lx_west.R
	Rscript $<

data/mx_south.rda : data-raw/mx_south.R
	Rscript $<

data/Lx_south.rda : data-raw/Lx_south.R
	Rscript $<

data/propn_age_fert_booth.rda : data-raw/propn_age_fert_booth.R
	Rscript $<


## Documentation

.PHONY : documentation
documentation :
	Rscript -e "devtools::document()"


## Clean up

.PHONY : clean
clean :
	rm -rf data
	rm -rf man
	mkdir -p data
	mkdir -p man
