% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ted_mr_data_prep_f.R
\name{ted_mr_data_prep_f}
\alias{ted_mr_data_prep_f}
\title{Prepares data for multiple response questions}
\usage{
ted_mr_data_prep_f(.df, .dd, .x, .fct, .full = TRUE)
}
\arguments{
\item{.df}{An imported dataframe}

\item{.dd}{A data dictionary}

\item{.x}{A question number}

\item{.fct}{A factor or character}

\item{.full}{Respondents are to be aggregated at the full board level - T or F}
}
\value{
A tibble
}
\description{
\code{ted_mr_data_prep_f} returns a tibble of a given question,
    prepared for tables or graphs.
}
\details{
This is an NACD function: Should you experience difficulties, see Ted Sikora
}
\examples{
df \%>\% ted_mr_data_prep_f(dd, Q25, Q9, .full=FALSE)

}
