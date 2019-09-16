#' Takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph.
#'
#' It is an algorithm used for finding the shortest path from a single node to a single destination node 
#' by stopping the algorithm onc e the shortest path to the destination node has been determined.
#'
#' @param graph A data frame.
#' @param init_node A number.
#' @return The vector of the shortest path to every other node from the starting node.
#' @examples
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' 
#' @references 
#' \href{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}{Dijkstra}
#' 
#' @export 

dijkstra <- function(graph, init_node)
{
  # Input validation
  stopifnot(is.data.frame(graph),is.numeric(init_node), (colnames(graph) == c("v1","v2","w")), (ncol(graph)==3), 
            (init_node <= max(graph[1:2])),
            (init_node %in% graph$v1),(init_node %in% graph$v1), is.atomic(init_node) & length(init_node) == 1
  )
   #graph<- wiki_graph
   #init_node <- 1
  # Algorithem
  #1 create a list of unique nodes
  node_list <- unique(graph[,1])
  weight <- c()
  #print(weight)
  for (node in node_list)
  {
    #2 Set the distance to to init_node to 0 and set the current_node to init_node
    if (node == init_node)
    {
      weight[node] <- 0
    }
    else
    {
      weight[node] <- Inf   #This is an inifinity number
    }
  }
  #3 Go through all unvisited node and calculate the minimum distance
  while (length(node_list)>0)
  {
    #4 
    current_node <- which(weight == min(weight[node_list]))
    #print(current_node)
    node_list <- node_list[! node_list %in% current_node]
    #print(node_list)
    # Only consider unvisited and the next to be
    for (unvisited in graph[graph$v1 == current_node,]$v2)
    {
      dist <- graph[,3][which(graph[,1]==current_node & graph[,2]==unvisited)] + weight[current_node]
     # print(dist)
      current_dist <- weight[unvisited]
      #print(current_dist)
      ifelse((dist <= current_dist), weight[unvisited] <- dist, weight[unvisited] <- current_dist)
    }
  }
  return(weight)
}