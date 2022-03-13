
#' secrecy of a graph
#' 
#' Calculates the graph level secrecy index of a graph
#' 
#' The \code{secrecy} measure is useful for covert networks where its
#' members want to remain undetected to the law enforcement agencies (LEA's), 
#' even 
#' when some of their peers are detected. 
#' The \code{secrecy} measure is defined as the fraction of the network that 
#' remains unexposed if a single member of the network is detected. 
#' Hence, the score runs between 0 (=everybody gets exposed as soon as 1 
#' person is exposed) to 1 (=nobody gets exposed when 1 person is exposed). 
#' In real cases, the boundary values of 0 and 1 will only occur in pathological 
#' networks.
#' 
#' This relies on two things: 1. the probability of an individual becoming 
#' exposed when the LEA conducts a surveillance; 2. the fraction of 
#' the network that is exposed when a member of the network becomes detected 
#' in that surveillance.
#' 
#' There are several ways of computing the network's \code{secrecy}.
#' 
#' \code{S1} assumes that every actor has the same probability of being 
#' detected under a surveillance (with p = 1/number_of_vertices) and 
#' deﬁnes the secrecy individual i ‘contributes’ to the
#' network as the fraction of individuals that remain unexposed when
#' upon monitoring individual i all his links with his neighbors are
#' detected. 
#' 
#' \code{S2} assumes that whenever an individual in
#' the network is being monitored communication between him and
#' one of his neighbors is detected independently with probability p.
#' The case where p = 1 therefore corresponds to measure \code{S1}. 
#' If individual i has di neighbors the 
#' number of neighbors that will be detected is binomially distributed.
#' As with \code{S1}, it is assumed that every actor has the same probability 
#' of being detected under a surveillance (with p = 1/number_of_vertices).
#' 
#' The calculation of \code{S2} requires the user to specify a reasonable 
#' value for \code{p}, which may not be obvious.
#' 
#' \code{S3} no longer assumes that ˛i = 1/n for all i ∈ V (as in \code{S1} 
#' and \code{S2}. 
#' It can be argued that this is a fair assumption when a covert operation 
#' is in its initial phase.
#' However, if an operation passed its initial stage the probability of
#' exposure will vary among network members. 
#' This happens because certain individuals, due to a more central position 
#' in the network, are more likely to be discovered. 
#' In \code{S3} this is captured by the equilibrium distribution of a random 
#' walk on the graph. 
#' This random walk chooses its next vertex at random from the neighbors
#' of the current vertex including itself.
#' 
#' NOTE: measure \code{S3} can break down in certain graphs and is no longer
#' bound to \\[0,1\\]. Discard of \code{S3} in those situations.
#' 
#' This function either returns one of the three secrecy measures (specified 
#' by \code{type}) or a data.frame containing all three. 
#' When \code{type == 0} or \code{type == 2}, a reasonable value for \code{p} 
#' should be specified.
#'
#' @param g graph of class \code{igraph}
#' @param type numeric, which secrecy type needs to be returned: 0, 1, 2, or 3.
#' @param p probability, needed when \code{type == 0} or \code{type == 2}
#' @param digits number of decimals used
#'
#' @return data.frame
#' @export
#' 
#' @references the formulas come from Lindelauf, R., Borm, P., & Hamers, H. (2009).
#' The influence of secrecy on the communication structure of covert networks. 
#' Social Networks, 31(2), 126-137. 
#' 
#' @examples
#' \dontrun{
#' data(Madrid_bombing, package = "DF21")
#' g <- Madrid_bombing[[25]]
#' secrecy(g) # all three measures, p = .25
#' secrecy(g, p = .1) # all three measures, p = .1
#' secrecy(g, type = 1)  # only S1
#' }
secrecy_graph <- function(g, type = 0, p = .25, digits = 3) {
  if (!inherits(g, "igraph")) {
    stop("'g' should be an igraph object")
  }
  
  if (p < 0 || p > 1) {
    stop("'p' needs to be between 0 and 1 (inclusive)")
  }
  
  if (!type %in% 0:3) {
    stop("'type' can only be 0, 1, 2, or 3")
  }
  
  n <- igraph::vcount(g)
  m <- igraph::ecount(g)
  
  if (type == 1) {
    S1 <- (n^2 - n - 2*m)/(n^2)
    secrecy <- data.frame(S1 = round(S1, digits = digits))
  }
  if (type == 2) {
    S2 <- (n^2 - n - 2*p*m)/(n^2)
    secrecy <- data.frame(S2 = round(S2, digits = digits))
  }
  if (type == 3) {
    di <- igraph::degree(g)
    S3 <- ((2*m*(n - 2)) + (n*(n - 1)) - sum(di^2))/((2*m + n)*n)
    secrecy <- data.frame(S3 = round(S3, digits = digits))
  }
  if (type == 0) {
    S1 <- (n^2 - n - 2*m)/(n^2)
    S2 <- (n^2 - n - 2*p*m)/(n^2)
    browser() ###---###---###---###---###---###---###---###---
    di <- igraph::degree(g)
    S3 <- ((2*m*(n - 2)) + (n*(n - 1)) - sum(di^2))/((2*m + n)*n)
    secrecy <- data.frame(S1 = round(S1, digits = digits), 
                          S2 = round(S2, digits = digits), 
                          S3 = round(S3, digits = digits))
  }

  return(secrecy)
}