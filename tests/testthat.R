library(testthat)
library(ReUseData)

test_check("ReUseData")
Sys.setenv(cachePath = file.path(tempdir(), "cache"))
