RSCRIPT = Rscript --no-init-file

all: move rmd2md

move:
		cp inst/vign/rgbif_vignette.md vignettes;\
		cp inst/vign/issues_vignette.md vignettes;\
		cp inst/vign/taxonomic_names.md vignettes;\
		cp inst/vign/downloads.md vignettes;\
		cp -r inst/vign/figure/ vignettes/figure/

rmd2md:
		cd vignettes;\
		mv rgbif_vignette.md rgbif_vignette.Rmd;\
		mv issues_vignette.md issues_vignette.Rmd;\
		mv taxonomic_names.md taxonomic_names.Rmd;\
		mv downloads.md downloads.Rmd

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

eg:
	${RSCRIPT} -e "devtools::run_examples()"
