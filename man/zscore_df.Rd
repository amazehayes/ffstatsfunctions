% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zscore_table.R
\name{zscore_df}
\alias{zscore_df}
\title{Z-Score Table}
\usage{
zscore_df(position, scoring, fwd, pfrplayers)
}
\arguments{
\item{position}{QB,RB,WR,TE}

\item{scoring}{ppr,halfppr,standard,sixpttd}

\item{fwd}{a player database}

\item{pfrplayers}{PFR database}
}
\value{
dataframe
}
\description{
creates z-score table
}
\examples{
zscore_df(position,scoring)

}
