
devtools::load_all()
devtools::document()
devtools::test()

usethis::use_test("Machines")
library("testthat")

usethis::use_test("make_stancode-basics")

usethis::use_readme_rmd()
usethis::use_travis()

usethis::use_package("lme4")
usethis::use_package("stats")
usethis::use_package("brms", type = "Depends")

usethis::use_package("MEMSS", type = "Suggests") ## for example data
usethis::use_package("BayesFactor", type = "Suggests") ## for comparisons
usethis::use_package("bridgesampling", type = "Suggests") ## for comparisons

#usethis::use_testthat()
#usethis::use_roxygen_md()
#usethis::create_package("../bfrms")
