md:
	Rscript -e "rmarkdown::render('README.Rmd', output_file = 'README.md')"

site:
	Rscript -e "rmarkdown::render('README.Rmd', output_file = 'README.md')"
	Rscript -e "pkgdown::build_site()"

check:
	Rscript -e "devtools::check()"

checkfast:
	Rscript -e "devtools::check(build_args = '--no-build-vignettes')"

revdep:
	Rscript -e "revdepcheck::revdep_check()"

test: export NOT_CRAN = "TRUE"
test:
	Rscript -e "devtools::test()"

test_cran: export NOT_CRAN = "FALSE"
test_cran:
	Rscript -e "devtools::test()"
	
doc:
	Rscript -e "devtools::document()"

build:
	Rscript -e "devtools::build()"

buildfast:
	Rscript -e "devtools::build(vignettes = FALSE)"

coverage:
	Rscript -e "covr::codecov()"

style:
	Rscript -e "styler::style_pkg()"

pr: style check site
	echo "If all checks have passed, you are ready to submit PR"
