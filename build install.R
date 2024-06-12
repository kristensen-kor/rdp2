# install.packages("roxygen2")
# install.packages("devtools")

if ("package:rdp2" %in% search()) {
	detach("package:rdp2", unload = TRUE)
}

if ("rdp2" %in% loadedNamespaces()) {
	unloadNamespace("rdp2")
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# roxygen2::roxygenize()
devtools::build(".", ".")
devtools::install(".")

library(rdp2)



# Use usethis::use_roxygen_md() to set up roxygen2 for documenting functions.
# Generate vignettes with usethis::use_vignette().
