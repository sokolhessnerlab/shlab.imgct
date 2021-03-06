% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_category_responses.R
\name{count_category_responses}
\alias{count_category_responses}
\title{Count Category Responses By Image}
\usage{
count_category_responses(block, category_ids)
}
\arguments{
\item{block}{A dataframe from loading a clean set of responses
to images, or passed in from a parent function.}

\item{category_ids}{A list of the identiers used for categorization in the
given task.}
}
\value{
A dataframe with the block's images and corresponding category
counts.
}
\description{
\code{count_category_responses} uses a combination of long and wide pivots to
provide counted category responses for a given image block.
}
\examples{
count_category_responses(block, c("1", "2", "3", "4", "5"))

}
