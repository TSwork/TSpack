% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ted_mr_data_prep.R
\name{ted_mr_data_prep}
\alias{ted_mr_data_prep}
\title{Prepares data for multiple response questions}
\usage{
ted_mr_data_prep(.df, .dd, .x, .full = TRUE)
}
\arguments{
\item{.df}{An imported dataframe}

\item{.dd}{A data dictionary}

\item{.x}{A question number}

\item{.full}{Respondents are to be aggregated at the full board level - T or F}
}
\value{
A tibble
}
\description{
\code{ted_mr_data_prep} returns a tibble of a given question,
    prepared for tables or graphs.
}
\details{
This is an NACD function: Should you experience difficulties, see Ted Sikora
}
\examples{
df \%>\% ted_mr_data_prep(dd, Q25, .full = F)

}
