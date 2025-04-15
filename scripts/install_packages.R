install_if_missing <- function(pkg) {
  if (!pkg %in% installed.packages()[, "Package"]) {
    install.packages(pkg, dependencies = TRUE, repos = "https://cran.rstudio.com/")
  }
}
