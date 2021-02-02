md:
	Rscript -e "rmarkdown::render('README.Rmd', output_file = 'README.md')"

site:
	Rscript -e "pkgdown::build_site()"

check:
	Rscript -e "devtools::check()"

checkfast:
	Rscript -e "devtools::check(build_args = '--no-build-vignettes')"

revdep:
	Rscript -e "revdepcheck::revdep_check()"

test:
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

pr: style check md site
	echo "If all checks have passed, you are ready to submit PR"
