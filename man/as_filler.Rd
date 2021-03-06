% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill.R
\name{as_filler}
\alias{as_filler}
\title{Fill does things}
\usage{
as_filler(.f, ...)
}
\arguments{
\item{.f}{A function, formula, or scalar atomic. Must return a scalar atomic.

  If a function, it is used as is.

  If a formula, e.g. ~ .x + 2, it is converted to a function. There are three
  ways to refer to the arguments: For a single argument function, use . For a
  two argument function, use .x and .y For more arguments, use ..1, ..2, ..3
  etc.

  This syntax allows you to create very compact anonymous functions.

  If scalar character or scalar numeric, it is converted to a formula, which
  is then converted to a function as above. The end result is that the value
  is filled in in the selected locations.}

\item{...}{Additional arguments passed on to methods.}
}
\description{
Fill does things
}
