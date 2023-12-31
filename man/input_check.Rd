% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general.R
\name{input_check}
\alias{input_check}
\title{Sanity check for input}
\usage{
input_check(input, key_cols)
}
\arguments{
\item{input}{input for vacsce
input is a list contains three objects - params, src and proj_rules
params is a list (example below)
    list(country = "<country_code>",
        disease = "<disease_code>",
        proportion_risk = <proportion of at risk population>,
        year_cur = <current_year>, # separates historical data and future projection
        introduction = data.frame(vaccine = c("<vaccine_1>", "<vaccine_2>"),
                                 activity_type = c("<activity_vaccine_1>", "<activity_vaccine_2>"),
                                 year_intro = c(<vaccine_1_introduction_year>, <vaccine_2_introduction_year>)))
src is a list (example below)
    list((historic = <data frame of historical coverage source>, future = <data frame of future coverage source>)}

\item{key_cols}{Key columns in coverage data to maintain}
}
\description{
saninty check for input date
}
