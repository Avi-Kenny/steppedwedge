% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_data.R
\name{load_data}
\alias{load_data}
\title{Load and format data object}
\usage{
load_data(
  time,
  cluster_id,
  individual_id = NULL,
  treatment,
  outcome,
  time_type = "discrete",
  data
)
}
\arguments{
\item{time}{A character string; the name of the numeric variable representing time. Time can be either discrete or continuous.}

\item{cluster_id}{A character string; the name of the numeric variable identifying the cluster.}

\item{individual_id}{A character string (optional); the name of the numeric variable identifying the individual.}

\item{treatment}{A character string; the name of the binary variable indicating treatment. Accepts either integer (0/1) or Boolean (T/F) values.}

\item{outcome}{A character string; the name of the numeric or binary variable indicating outcome. Accepts either numeric or Boolean (T/F) values.}

\item{time_type}{A character string describing how time is accounted for: 'discrete' (default) or 'continuous'}

\item{data}{A dataframe containing the stepped wedge trial data.}
}
\value{
An object of class \code{sw_data}
}
\description{
Load and format data object
}
\examples{
load_data(time = "period", cluster_id = "id", individual_id = NULL,
treatment = "treatment", outcome = "y_bin",
data = geeCRT::sampleSWCRTLarge)
}
