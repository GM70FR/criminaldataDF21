#' Find the geodesic k-path centrality
#'
#' Geodesic K-path centrality counts the number of vertices that can be reached 
#' through a geodesic path of length less than "k". 
#' 
#' @details 
#' This function counts the number of vertices that a specific vertex can 
#' reach within k steps. 
#' By default, this number is weighted (if the graph has a \code{weight} 
#' edge attribute). 
#' This can be overridden by setting the \code{weights} argument to 
#' \code{NA} (no weight is used) or to a vector with weights (typically 
#' this is a numeric edge attribute).
#' 
#' More detail at 
#' \href{http://www.centiserver.org/?q1=centrality&q2=Geodesic_K-Path_Centrality}{Geodesic K-Path Centrality}
#' @param graph The input graph as igraph object
#' @param mode Character constant, gives whether the shortest paths to or from 
#' the given vertices should be calculated for directed graphs. 
#' If \code{out} then the shortest paths from the vertex, if \code{in} then to 
#' it will be considered. If \code{all}, the default, then the corresponding 
#' undirected graph will be used, ie. not directed paths are searched. 
#' This argument is ignored for undirected graphs.
#' @param vids Numeric vertex sequence, the vertices that should be considered.
#' Default is all vertices. Otherwise, the operation is performed on the 
#' subgraph only containing vertices \code{vids}. 
#' @param weights Possibly a numeric vector giving edge weights. 
#' If this is \code{NULL}, the default, and the graph has a weight edge attribute, 
#' then the attribute is used. If this is \code{NA} then no weights are used 
#' (even if the graph has a weight attribute).
#' @param k The k parameter. The default is 3.
#' @return A numeric vector contaning the centrality scores for the selected vertices.
#' @author Mahdi Jalili \email{m_jalili@@farabi.tums.ac.ir} (adapted for this package)
#' @references Borgatti, Stephen P., and Martin G. Everett. 
#' "A graph-theoretic perspective on centrality." Social networks 28.4 (2006): 466-484.
#' @examples
#' g <- igraph::barabasi.game(100)
#' geokpath_centrality(g)
#' @export
geokpath_centrality <- function (graph, vids = igraph::V(graph),
                                 mode = c("all", "out", "in"),
                                 weights = NULL, k = 3){
  
  if (!igraph::is.igraph(graph)) {
    stop("Not a graph object", call. = FALSE)
  }
  
  induced <- FALSE
  if (!is.numeric(vids)) stop("'vids' must be a numeric vector")
  if (length(unique(vids)) < length(igraph::V(graph))) {
    graph <- igraph::induced_subgraph(graph, vids = vids)
    # igraph hernummert nu de vertices, daarvoor moeten we dus corrigeren
    vids_orig <- sort(vids) # voor de zekerheid, omdat igraph vids in numerieke volgorde toepast
    vids <- igraph::V(graph)
    induced <- TRUE
  } else if (any(!vids %in% igraph::V(graph))) {
    stop("You asked for vertices that are not present in the graph")
  }
  
  # check vertex names
  v <- vids
  if (is.character(v) && "name" %in% igraph::list.vertex.attributes(graph)) {
    v <- as.numeric(match(v, igraph::V(graph)$name))
    if (any(is.na(v))) {
      stop("Invalid vertex names: there are NA's in the names")
    }
    vids <- v
  } else {
    if (is.logical(v)) {
      res <- as.vector(igraph::V(graph))[v]
    }
    else if (is.numeric(v) && any(v < 0)) {
      res <- as.vector(igraph::V(graph))[v]
    }
    else {
      res <- as.numeric(v)
    }
    if (any(is.na(res))) {
      stop("Invalid vertex name(s): there are NA's in the names")
    }
    vids = res
  }
  
  k <- as.integer(k)
  if(k <= 0) stop("The k parameter must be greater than 0.", call. = FALSE)
  res <- integer()
  sp <- igraph::shortest.paths(graph, v = igraph::V(graph), mode = mode[1], 
                       weights = weights)
  for (v in igraph::V(graph)[vids]){
    res <- append(res, length(sp[v, sp[v,] <= k]) - 1);
  }
  if (igraph::is.named(graph)) {
    names(res) <- igraph::V(graph)$name[vids]
  }  
  res
}
