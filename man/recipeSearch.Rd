% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recipeSearch.R
\name{recipeSearch}
\alias{recipeSearch}
\title{search existing data recipes.}
\usage{
recipeSearch(keywords, cachePath = "ReUseDataRecipe")
}
\arguments{
\item{keywords}{character vector of keywords to be matched to the recipe names.}

\item{cachePath}{A character string for the recipe caching path. "ReUseDataRecipe" by default.}
}
\value{
return a recipeHub object.
}
\description{
search existing data recipes.
}
\examples{
recipeSearch("gencode")
recipeSearch(c("STAR", "index"))
}
