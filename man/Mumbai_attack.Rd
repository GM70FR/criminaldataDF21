% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{Mumbai_attack}
\alias{Mumbai_attack}
\title{Mumbai Attack (2008) (dataset)}
\format{
(bipartite) igraph object
}
\source{
The data were published in Azad, S., & Gupta, A. (2011).
A Quantitative Assessment on 26/11 Mumbai Attack using Social Network Analysis.
Journal of Terrorism Research, 2(2). doi:http://doi.org/10.15664/jtr.187
}
\usage{
data(Mumbai_attack, package = "DF21")
}
\description{
On November 26, 2008, a major terrorist attack,
known as the 26/11 terror attack, was executed in Mumbai (India)
by members of the Lashkar-e-Taiba (LeT).
Ten terrorists equipped with advanced weaponry and three handlers in Pakistan,
were involved in the attack and five major locations in Mumbai were targeted,
causing nearly 260 casualties from ten different countries.
The attack was well planned and handlers from Pakistan
were in communication with these terrorists, during the attack.

The dataset includes the names of the terrorists and their place of
operation (termed \code{location}).
The edges were constructed based on intercepted phone calls (who-called-whom).
}
