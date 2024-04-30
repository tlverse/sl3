# Rscript -e "devtools::check(env_vars = c(NOT_CRAN = 'false'))" 2>&1 | gawk '{ date=strftime("%Y-%m-%d %H:%M:%S"); print date,$0}' > testresults.txt
Rscript -e "devtools::check(env_vars = c(NOT_CRAN = 'false'))" 2>&1 | ts '[%Y-%m-%d %H:%M:%S]' > testresults.txt
