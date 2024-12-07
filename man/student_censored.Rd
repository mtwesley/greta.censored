% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/censored.R
\name{student_censored}
\alias{student_censored}
\title{Student's t Censored Distribution}
\usage{
student_censored(
  df,
  loc,
  scale,
  is_censored,
  censor = "right",
  lower = NULL,
  upper = NULL,
  dim = length(is_censored)
)
}
\arguments{
\item{df}{Degrees of freedom for the Student's t distribution.}

\item{loc}{Location parameter (mean).}

\item{scale}{Scale parameter.}

\item{is_censored}{Logical vector indicating whether each observation is censored.}

\item{censor}{Type of censoring: one of 'right', 'left', 'interval'.}

\item{lower}{Lower bound for interval censoring (optional).}

\item{upper}{Upper bound for interval censoring (optional).}

\item{dim}{Dimension of the data (optional, defaults to length of \code{df}).}
}
\value{
A greta censored Student's t distribution node.
}
\description{
Creates a censored Student's t distribution for use with greta.
}
