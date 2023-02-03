## Here we document the files that are in "inst/extdata"

################
## echo_out.sh
################

## This is an example shell script file that can be converted into a
## data recipe. Example and evaluable code are included in the
## vignette of "ReUseData_recipe.Rmd".

############################
## gencode_transcripts.sh
############################

## This is an example shell script file that can be converted into a
## data recipe. Example code is included in the vignette of
## "ReUseData_recipe.Rmd". The evaluation of the recipes takes several
## minutes, so that part in vignette is tagged as "eval=FALSE",
## however user can evaluate the code chunk in their own computer.

####################
## meta_gcp.csv
####################

## This is a csv file recording all available data sets that are
## pre-generated and located in the Google Cloud Bucket. This file is
## used when "dataUpdate(cloud=TRUE)". if "remote=FALSE", if reads the
## file under inst/extdata. If "remote=TRUE", it downloads the file
## from GitHub which is supposed to be the most updated version. 

#####################
## dataRecipes/*.R
#####################

## This folder saves all pre-built data recipes in "*.R" scripts. They
## are called by "recipeUpdate()" when users update and sync their
## local cache for data recipes with the package. If "remote=TRUE",
## recipeUpdate() syns with the remote folder from GitHub
## (rworkflow/ReUseDataRecipe) for the most updated data recipes
## available.


