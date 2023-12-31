% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dataNorming.R
\name{get_normedScore}
\alias{get_normedScore}
\title{Takes in vector of baseline scores and full list of scores (cross-walking to equivalents should have already happened)
Returns normed scores}
\usage{
get_normedScore(vector_baseline, vector_full)
}
\arguments{
\item{vector_baseline}{vector containing baseline tests (could be generated with get_baselineCohort)}

\item{vector_full}{vector containing all scores for the cognitive test of interest (be sure you've already converted relevant scores using crosswalks if necessary)}
}
\value{
normed_scores vector containing z scores based on baseline norms
}
\description{
Takes in vector of baseline scores and full list of scores (cross-walking to equivalents should have already happened)
Returns normed scores
}
\examples{
get_normedScore(c(300, 300, 290, 270, NA, NA), c(300, 300, 290, 292, 295, 100, NA, 199))
}
