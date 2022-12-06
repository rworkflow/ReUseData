rcp <- recipeLoad("ensembl_liftover")
rcp$species <- "mouse"
rcp$from <- "NCBIM37"
rcp$to <- "GRCm38"
outdir <- file.path(tempdir(), "test_SharedData")
res <- getData(rcp,
               outdir = outdir,
               notes = c("ensembl", "liftover", "NCBIM37", "GRCm38"))

test_that("recipe evaluation works", {
    expect_equal(dirname(res$output), outdir)
    expect_equal(basename(res$output), "NCBIM37_to_GRCm38.chain")
    expect_vector(dir(outdir, pattern = "rcp_"))
    expect_vector(dir(outdir, pattern = "sh"))
    expect_vector(dir(outdir, pattern = "yml"))
    expect_vector(dir(outdir, pattern = "md5"))
    expect_vector(dir(outdir, pattern = "cwl"))
})

test_that("data updating and getter function works", {
    dh <- dataUpdate(dir = outdir,
                     cachePath = "ReUseData")
    expect_s4_class(dh, "dataHub")
    expect_vector(dataNames(dh))
    expect_vector(dataParams(dh))
    expect_vector(dataNotes(dh))
    expect_vector(dataTags(dh))
    expect_vector(dataYml(dh))
    expect_s4_class(dh[1], "dataHub")
    expect_length(dh[1], 1)
}

test_that("data searching works", {
    ds <- dataSearch(cachePath = "ReUseData")
    expect_s4_class(ds, "dataHub")
    expect_equal(dataNames(ds), "NCBIM37_to_GRCm38.chain")
})

test_that("data reuse function works") {
    expect_type(toList(ds), "list")
    expect_type(toList(ds, format = "json"), "character")
    expect_type(toList(ds, format = "yaml"), "character")
})

test_that("meta data works", {
    mt <- meta_data(outdir)
    expect_identical(colnames(mt), c( "yml", "params", "output", "notes", "date", "tag"))
    expect_equal(mt$yml, res$yml)
    expect_equal(mt$output, res$output)
    expect_equal(as.Date(mt$date), Sys.Date())
})

res1 <- getCloudData(rcp,
               outdir = outdir,
               notes = c("ensembl", "liftover", "NCBIM37", "GRCm38"))

test_that("get cloud data works", {
    dh <- dataUpdate(dir = outdir, cloud = TRUE)
    expect_true(any(grepl("web", mcols(dh)$rtype)))
    getCloudData(dh[2], outdir = outdir)
    expect_vector(dir(outdir, pattern = dataNames(dh[2])))  ## cloud data downloaded!
    expect_vector(dir(outdir, pattern = basename(dataYml(dh[2]))))  ## yaml file downloaded!
})
