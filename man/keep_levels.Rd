% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projection_rules.R
\name{keep_levels}
\alias{keep_levels}
\title{Keep future coverage at certain level}
\usage{
keep_levels(
  d,
  year_from,
  year_to,
  level,
  gender = NULL,
  age_from = NULL,
  age_to = NULL
)
}
\arguments{
\item{d}{a data frame containing at least (year, coverage)}

\item{year_from}{year from}

\item{year_to}{year to}

\item{level}{coverage level}

\item{gender}{1 for both, 2 for males, 3 for females}

\item{age_from}{age_from}

\item{age_to}{age_to}
}
\description{
routine coverage projection rule - keep future coverage at certain level
}
