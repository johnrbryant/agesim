## Input data

.PHONY : all
all : data/Lx_west.rda \
      data/mx_west.rda \
      data/propn_age_fert_booth.rda \
      documentation

data-raw/Lx_west_tmp.rda : data-raw/Lx_west_tmp.R
	Rscript $<

data/mx_west.rda : data-raw/mx_west.R \
                   data-raw/Lx_west_tmp.rda
	Rscript $<

data/Lx_west.rda : data-raw/Lx_west.R \
                   data/mx_west.rda \
                   data-raw/Lx_west_tmp.rda
	Rscript $<
	rm data-raw/Lx_west_tmp.rda

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
