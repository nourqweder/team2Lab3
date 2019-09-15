# Authors: Henrik Karlsson, Milda Poceviciute

#' @title Dijkstra Algorithm
#' @name dijkstra
#' @param graph A data frame.
#' @param init_node A numeric scalar.
#' @return Numeric vector, with the shortest distance to each node.
#' @description Dijkstra's algorithm is an algorithm for finding the shortest paths between nodes in a graph.
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' @export


# 1.1.2 dijkstra()
# Solution from Wikipedia: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
# Acknowledgement to Simon Jönsson

dijkstra <- function(graph, init_node) {
  stopifnot(is.data.frame(graph),
            is.atomic(init_node) & length(init_node) == 1,
            init_node <= max(graph[1:2]),
            length(graph) == 3)
  
  stopifnot(names(graph) == c("v1", "v2", "w"))
  
  q <- c()
  dist <- c()
  prev <- c()
  
  for (v in unique(c(graph$v1, graph$v2))) {
    dist[v] <- Inf
    prev[v] <- NA
    q[v] <- v
  }
  
  dist[init_node] <- 0
  while (length(q) != 0) {
    u <- q[which.min(dist[q])]
    q <- q[q != u]
    
    for (v in graph$v2[graph$v1 == u]) {
      w <- graph[, 3][graph$v1 == u & graph$v2 == v]
      alt <- dist[u] + w
      
      if (alt < dist[v]) {
        dist[v] <- alt
        prev[v] <- u
      }
    }
  }
  return(dist)
}
