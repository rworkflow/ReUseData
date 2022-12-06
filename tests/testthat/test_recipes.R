script <- '
aa=$1
bb=$2
echo "$aa" > $bb.txt
'
rcp <- recipeMake(shscript = script,
                  paramID = c("aa", "bb"),
                  paramType = c("string", "string"),
                  outputID = "echoout",
                  outputGlob = "*.txt")
inp <- inputs(rcp)
outp <- outputs(rcp)

test_that("recipeMake works", {
    expect_s4_class(rcp, "cwlProcess")
    expect_s4_class(inp, "InputParamList")
    expect_s4_class(outp, "OutputParamList")
    expect
})

rcp$aa <- "Hello World!"
rcp$bb <- "outfile"
res <- getData(rcp,
               outdir = tempdir(),
               notes = c("echo", "txt", "test"),
               showLog = TRUE)
out <- readLines(res$output)
outyml <- readLines(res$yml)

test_that("recipe evaluation works", {
    expect_equal(dirname(res$output), tempdir())
    expect_equal(basename(res$output), paste0(rcp$bb, ".txt"))
    expect_equal(out, "Hello World!")
    expect_match(outyml[1], "aa: ")
    expect_match(outyml[2], "bb: ")
    expect_match(outyml[3], "output:")
    expect_match(outyml[3], res$output)
    expect_equal(outyml[4], "# notes: echo txt test")
    expect_equal(outyml[5], paste0("# date: ", Sys.Date())
})

test_that("recipe updating works", {
    rcphub <- recipeUpdate(cachePath = "ReUseDataRecipe",
                           repos = "rworkflow/ReUseDataRecipe",
                           force = TRUE)
    expect_s4_class(rcphub, "recipeHub")
    expect_vector(recipeNames(rcphub))
    expect_s4_class(rcphub[1], "recipeHub")
    expect_length(rcphub[1], 1)
}

test_that("recipe searching works", {
    rcp <- recipeSearch(c("ensembl", "liftover"))
    expect_s4_class(rcp, "recipeHub")
    expect_equal(recipeNames(rcp), "ensembl_liftover")
})

test_that("recipe loading works") {
    recipeLoad("ensembl_liftover", return=TRUE)
    expect_s4_class(ensembl_liftover, "cwlProcess")
})
