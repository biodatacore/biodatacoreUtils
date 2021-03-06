% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mzid.R
\name{mzid}
\alias{mzid}
\title{Creates a new mzid object}
\usage{
mzid(mz, rt, n_mz = 6, n_rt = 4, sep = "_", tag = "mzid", source = NA)
}
\arguments{
\item{mz}{scalar numeric: The mz and rt.}

\item{rt}{scalar numeric: The mz and rt.}

\item{n_mz}{scalar integerish: The number of digits after the decimal
point from the mz and the rt to be used in}

\item{n_rt}{scalar integerish: The number of digits after the decimal
point from the mz and the rt to be used in}

\item{sep}{scalar character: character separator in mzid}

\item{tag}{scalar character: tag at start of mzid}

\item{source}{scalar character: data source of the mzids. Pass `NA` to keep
the field, but fill with `NA`, pass `NULL` to create an object without the
fields.}
}
\value{
mzid object
}
\description{
What is an mzid object and why would I want to use it?
}
