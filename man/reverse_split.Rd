% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{reverse_split}
\alias{reverse_split}
\title{A Function To Reverse The Role Of Names And Values In A List.}
\usage{
reverse_split(lst)
}
\arguments{
\item{lst}{list: A named list with values that are vectors.}
}
\value{
list: A list with length equal to the number of distinct values in the
  input list and values from the names of the input list.
}
\description{
Given a list with names `x` and values in a set `y` this function returns a
list with names in `y` and values in `x`.
}
\details{
First the list is unrolled to provide a two long vectors, names are
  repeated, once for each of their values. Then the names are split by the
  values. This turns out to be useful for inverting mappings between one set
  of identifiers and an other.
}
\note{
This function is taken from the \code{Biobase} package
}
\examples{
l1 = list(a=1:4, b=c(2,3), d=c(4,5))
reverse_split(l1)
}
\seealso{
\code{\link{split}}
}
