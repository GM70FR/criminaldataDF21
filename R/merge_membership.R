
#' Merge community membership
#' 
#' Merge communities together for nicer plotting.
#' 
#' Community detection algorithms, such as \code{link[igraph]{cluster_edge_betweenness}},
#' \code{link[igraph]{cluster_fast_greedy}},
#' \code{link[igraph]{cluster_louvain}},
#' \code{link[igraph]{cluster_leiden}},
#' \code{link[igraph]{cluster_walktrap}}, et cetera,
#' sometimes yield a bunch of small communities, in addition to larger, 
#' meaningful communities.
#' This is always the case when there are isolates, as they might each get 
#' a community of their own.
#' 
#' Then, it can be useful to merge (some of) the small communities together for 
#' more informative plotting. 
#' 
#' The input of this function is the output of one of the algorithms above: 
#' an object of class \code{communities}. 
#' The output is also of class \code{communities} and can thus be fed into 
#' igraph's plot function.
#' 
#' The \code{merges} argument specifies which communities should be merged. 
#' It is a list, where each element contains the numbers of the communities 
#' (as specified in the \code{coms} object) that need to be merged into a new 
#' community. 
#' The list can contain multiple elements, each will become their own new community.
#' See the examples for, well, examples of this.
#' 
#' For clarity, the new communities will be numbered from n + 1, where 
#' 'n' is the number of communities in \code{coms}.
#' 
#' NOTE: this is only useful for plotting!! 
#' The only thing the function does is change the membership of the vertices 
#' into the new membership structure. 
#' However, it leaves the modularity values intact and also does not change the 
#' 'merges' element from the \code{coms} object. 
#' Hence, running \code{\link[igraph]{modularity}} on the output of this function 
#' will return the modularity of the **original** community memberships, not the 
#' new merged ones. 
#' Applying \code{\link[igraph]{sizes}} does work fine on the output of 
#' this function.
#' 
#' @param coms object of class \code{communities} from one of the appropriate 
#' \code{igraph} cluster / communities functions.
#' @param merges list of the merged that should be performed (see 'details' section)
#'
#' @return new object of class \code{communities}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Madrid_bombing_v2, package = "DF21")
#' # original plot, with 12 communities
#' # there are 6 communities of isolates
#' coms <- igraph::cluster_fast_greedy(Madrid_bombing_v2)
#' plot(coms, Madrid_bombing_v2)
#' igraph::sizes(coms)
#' # put all isolate communities together into a new one
#' # it is best to merge into a new object, so the original result is not lost
#' com2 <- merge_membership(coms, merges = list(7:12))
#' plot(com2, Madrid_bombing_v2)
#' igraph::sizes(com2)
#' # as an example, let's join communities 5 and 6 into a new one too
#' com2 <- merge_membership(coms, merges = list(5:6, 7:12))
#' plot(com2, Madrid_bombing_v2)
#' igraph::sizes(com2)
#' }
merge_membership <- function(coms, merges) {
  if (!is.list(merges)) {stop("'merges' should be a list.")}
  if (!inherits(coms, "communities")) {stop("'coms' should be a 'communities' object.")}
  
  # check that every community is merged only once
  unpack <- table(unlist(merges))
  which_double <- which(unpack > 1)
  
  if (length(which_double) > 0) {
    stop("You can not merge communit(y)(ies) ", 
         paste(names(unpack)[which_double], collapse = ", "), 
         " into multiple communities.")
  }

  which_not_exist <- which(!as.integer(names(unpack)) %in% unique(coms$membership))
  if (length(which_not_exist) > 0) {
    stop("Communit(y)(ies) ", paste(names(unpack[which_not_exist]), collapse = ", "), 
         " do(es) not exist in 'coms' and can therefore not be merged.")
  }
  
  all_communities <- max(coms$membership)
  num_new_communities <- length(merges)
  for (com in 1:num_new_communities) {
    # which vertices should be in a new community together
    which_is_com <- which(coms$membership %in% merges[[com]])
    coms$membership[which_is_com] <- all_communities + 1
    all_communities <- all_communities + 1
  }
  
  coms
}

