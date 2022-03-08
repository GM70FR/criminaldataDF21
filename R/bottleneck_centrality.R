#' Find the BottleNeck centrality score
#'
#' BottleNeck Centrality for vertex v is defined as:
#' \deqn{BN(v) = \sum_{s\in v} P_{s}(v)}{BN(v) = sum(P(s)(v), s in v)}
#' Let \eqn{T_{s}}{T(s)} be a shortest path tree rooted at node \eqn{s}{s}.
#' \eqn{P_{s}(v) = 1}{P(s)(v) = 1} if more than 1/nth of the shortest paths 
#' from node \eqn{s}{s} to other nodes in \eqn{T_{s}}{T(s)} go through 
#' vertex \eqn{v}{v}, otherwise \eqn{P_{s}(v) = 0}{P(s)(v) = 0}. 
#' @details 
#' For each node \eqn{v}{v} in the graph, construct a tree \eqn{T_{v}}{T(v)} 
#' of shortest paths from that node to all other nodes in the graph. 
#' 
#' For a node \eqn{v}{v}, \eqn{n_{v}}{n(v)} is the number of nodes that are 
#' directly or indirectly connected to node \eqn{v}{v} 
#' (i.e. the tree \eqn{T_{v}}{T(v)} contains these nodes). 
#' 
#' For each tree \eqn{T_{v}}{T(v)}, determine which nodes w are on at least 
#' 1/nth of the shortest paths from v. 
#' These nodes w represent 'bottle necks' of the shortest path tree 
#' \eqn{T_{v}}{T(v)} rooted at node \eqn{v}{v}.
#' 
#' Note that the implementation of this measure in the \code{centiserve} package 
#' is incorrect, so use our function and not theirs.
#' 
#' @param graph The input graph as igraph object
#' @param mode Character constant, gives whether the shortest paths to or from 
#' the given vertices should be calculated for directed graphs. 
#' If \code{out} then the shortest paths from the vertex, if \code{in} then to 
#' it will be considered. If \code{all}, the default, then the corresponding 
#' undirected graph will be used. This argument is ignored for undirected graphs.
#' @param vids Numeric vertex sequence, the vertices that should be considered.
#' Default is all vertices. Otherwise, the operation is performed on the 
#' subgraph only containing vertices \code{vids}.
#' @param n scalar, defaults to 4.
#' @return A numeric vector contaning the centrality scores for the selected vertices.
#' @references Przulj, N., Dennis A. Wigle, and Igor Jurisica. 
#' "Functional topology in a network of protein interactions." Bioinformatics 20.3 (2004): 340-348.
#' @examples
#' g <- igraph::graph(c(1,2,2,3,3,4,4,2))
#' bottleneck_centrality(g)
#' bottleneck_centrality(g, vids = c(1, 2, 4))
#' bottleneck_centrality(g, mode = "out")
#' bottleneck_centrality(g, mode = "in")
#' @export

bottleneck_centrality <- function (graph, mode = c("all", "out", "in"), 
                                   vids = igraph::V(graph),
                                   n = 4
                                   ){
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
  
  if (!igraph::is.igraph(graph)) {
    stop("Not a graph object", call. = FALSE)
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
  
  res <- matrix(0, igraph::vcount(graph), 1)
  # browser() ###############
  rownames(res) <- igraph::V(graph)

  for(v in igraph::V(graph)){
    s <- table(unlist(igraph::get.all.shortest.paths(graph, from = v, 
                                                     to = igraph::V(graph), 
                                                     mode = mode[1], 
                                                     weights = NA)$res))
    v = as.character(v)
    for(i in names(s)[s > (length(s)/n)]){
      i <- as.character(i)
      if(i != v) res[i,1] <- res[i,1] + 1
    }
  }
  res <- res[, 1]
  if (induced) {names(res) <- vids_orig}
  res
}

