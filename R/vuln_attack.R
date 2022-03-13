
vuln_attack <- function(g, k) {
    n <- length(igraph::V(g))
    noeud <- igraph::V(g)
    arc <- igraph::E(g)
    dist <- igraph::distances(g)
    dist[dist == Inf] <- 0
    dist[dist > 0] <- 1
    tot <- sum(dist)
    fin <- matrix(ncol = 5, nrow = n, 0)
    mat <- matrix(ncol = 2, nrow = n, 0) #betweenness attack
    mat[, 1] <- 1:n
    bet <- igraph::betweenness(g)
    mat[, 2] <- bet
    matri <- mat[order(mat[, 2]), ]
    g2 <- g
    for (i in 1:n) {
      v = n + 1 - i
      g2 <- igraph::delete_vertices(g2, matri[v, 1])
      dist2 <- igraph::distances(g2)
      dist2[dist2 == Inf] <- 0
      dist2[dist2 > 0] <- 1
      tot2 <- sum(dist2)
      fin[i, 1] <- i / n
      fin[i, 2] <- tot - tot2
      matri[matri[, 1] > matri[v, 1], 1] <-
        matri[matri[, 1] > matri[v, 1], 1] - 1 #bluff
    }
    mat <- matrix(ncol = 2, nrow = n, 0) #degree attack
    mat[, 1] <- 1:n
    deg <- igraph::degree(g)
    mat[, 2] <- deg
    matri <- mat[order(mat[, 2]), ]
    g2 <- g
    for (i in 1:n) {
      v = n + 1 - i
      g2 <- igraph::delete_vertices(g2, matri[v, 1])
      dist2 <- igraph::distances(g2)
      dist2[dist2 == Inf] <- 0
      dist2[dist2 > 0] <- 1
      tot2 <- sum(dist2)
      fin[i, 3] <- tot - tot2
      matri[matri[, 1] > matri[v, 1], 1] <-
        matri[matri[, 1] > matri[v, 1], 1] - 1 #bluff
    }
    g2 <- g #cascading
    npro <- n
    lim <- n - 1
    for (i in 1:lim) {
      mat <- matrix(ncol = 2, nrow = npro, 0)
      mat[, 1] <- 1:npro
      bet <- igraph::betweenness(g2)
      mat[, 2] <- bet
      matri <- mat[order(mat[, 2]), ]
      g2 <- igraph::delete_vertices(g2, matri[npro, 1])
      dist2 <- igraph::distances(g2)
      dist2[dist2 == Inf] <- 0
      dist2[dist2 > 0] <- 1
      tot2 <- sum(dist2)
      fin[i, 4] <- tot - tot2
      npro <- npro - 1
    }
    fin[n, 4] <- tot
    #random
    for (l in 1:k) {
      al <- sample(1:n, n)
      g2 <- g
      for (i in 1:n) {
        g2 <- igraph::delete_vertices(g2, al[i])
        dist2 <- igraph::distances(g2)
        dist2[dist2 == Inf] <- 0
        dist2[dist2 > 0] <- 1
        tot2 <- sum(dist2)
        fin[i, 5] <- fin[i, 5] + (tot - tot2)
        al[al > al[i]] <- al[al > al[i]] - 1 #bluff
      }
    }
    fin[, 2:4] <- fin[, 2:4] / tot
    fin[, 5] <- fin[, 5] / tot / k
    return(fin)
  }
