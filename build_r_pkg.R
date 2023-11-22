list.of.packages <- c("lintr", "covr", "pkgbuild", "roxygen2", "Rcpp", "testthat")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# lintr::use_lintr(type = "tidyverse")

Rcpp::compileAttributes()
roxygen2::roxygenize('.', roclets = c('rd', 'collate', 'namespace'))

l <- lintr::lint_package()
print(l)
stopifnot(length(l)==0)

check_res <- devtools::check()
stopifnot(max(sapply(check_res[c("errors", "warnings", "notes")], length))==0)

# This needs install to work
cov <- covr::package_coverage()
print(cov)
stopifnot(covr::percent_coverage(cov)==100)
#covr::report(cov)
