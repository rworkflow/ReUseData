Rcwl::install_cwltool()

rcp <- recipeLoad("echo_out")
rcp$input <- "Hello World!"
rcp$outfile <- "outfile"
outdir <- file.path(tempdir(), "test_SharedData")
res <- getData(rcp,
               outdir = outdir,
               notes = c("echo", "hello", "world", "txt"))

test_that("recipe evaluation works", {
    expect_equal(normalizePath(dirname(res$output)), normalizePath(outdir))
    expect_equal(basename(res$output), "outfile.txt")
    expect_vector(dir(outdir, pattern = "rcp_"))
    expect_vector(dir(outdir, pattern = "sh"))
    expect_vector(dir(outdir, pattern = "yml"))
    expect_vector(dir(outdir, pattern = "md5"))
    expect_vector(dir(outdir, pattern = "cwl"))
})

test_that("data updating and getter function works", {
    dh <- dataUpdate(dir = outdir, cachePath = "ReUseData")
    expect_s4_class(dh, "dataHub")
    expect_vector(dataNames(dh))
    expect_vector(dataParams(dh))
    expect_vector(dataNotes(dh))
    expect_vector(dataTags(dh))
    expect_vector(dataYml(dh))
    expect_s4_class(dh[1], "dataHub")
    expect_length(dh[1], 1)
})

ds <- dataSearch(cachePath = "ReUseData")
test_that("data searching works", {
    expect_s4_class(ds, "dataHub")
    ## expect_equal(dataNames(ds), "outfile.txt")
    expect_true("outfile.txt" %in% dataNames(ds))
})

test_that("data reuse function works", {
    expect_type(toList(ds), "list")
    expect_type(toList(ds, format = "json"), "character")
    expect_type(toList(ds, format = "yaml"), "character")
})

test_that("meta data works", {
    mt <- meta_data(outdir)
    expect_identical(colnames(mt), c( "yml", "params", "output", "notes", "date"))
    expect_equal(mt$output, res$output)
})

new_outdir <- file.path(tempdir(), "test_gcpData")

test_that("get cloud data works", {
    dh <- dataUpdate(dir = new_outdir, cloud = TRUE)
    expect_true(any(grepl("web", mcols(dh)$rtype)))
    getCloudData(dh[dataNames(dh) == "outfile.txt"], outdir = new_outdir)
    expect_vector(dir(new_outdir))  ## cloud data downloaded!
    expect_true("outfile.txt" %in% dir(new_outdir))
    expect_vector(dir(new_outdir, pattern = basename(dataYml(dh[1]))))  ## yaml file downloaded!
})
