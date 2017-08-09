md:
	  r -e "rmarkdown::render('README.Rmd')"

site:
	  r -e "pkgdown::build_site()"

check:
	  r -e "devtools::check()"

test:
	  r -e "devtools::test()"

doc:
	  r -e "devtools::document()"

cov:
	  r -e "covr::codecov()"
