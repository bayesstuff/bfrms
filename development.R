
devtools::load_all()
devtools::document()

usethis::use_test("contr.bayes")
library("testthat")

usethis::use_readme_rmd()

usethis::use_package("lme4")
usethis::use_package("brms", type = "Depends")

usethis::use_package("MEMSS", type = "Suggests") ## for example data
usethis::use_package("BayesFactos", type = "Suggests") ## for comparisons

#usethis::use_testthat()
#usethis::use_roxygen_md()
#usethis::create_package("../bfrms")
