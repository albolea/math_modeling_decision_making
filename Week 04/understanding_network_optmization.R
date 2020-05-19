library(igraph)
`%>%` <- magrittr::`%>%`

seer <- matrix(nc = 4, byrow = TRUE, 
               c(1, 2, 2, 5, 
                 1, 3, 5, 7, 
                 1, 4, 4, 4, 
                 2, 3, 2, 1, 
                 2, 5, 7, 3, 
                 3, 4, 1, 2,
                 3, 5, 4, 4,
                 3, 6, 3, 5,
                 4, 6, 4, 4,
                 5, 6, 1, 1,
                 5, 7, 5, 9,
                 6, 7, 7, 6)) 

g <- add_edges(make_empty_graph(n=7, directed = FALSE),
               t(seer[,1:2]), weight=seer[,3]) %>%
  set_vertex_attr("name", value = c("O", "A", "B", "C", "D", "E", "T"))

g$layout <- matrix(c(0, 25, 50, 40, 100, 90, 150, #x coordinates
                     150, 175, 150, 125, 150, 125, 160), nc=2)  #y coordinates

E(g)$label <- seer[,3]
plot(g)
  
all_shortest_paths(g, "O", "T")$res[[1]]

distances(g, "O", "T")

g_mst <-  mst(g, weights = NULL)
g_mst
plot(g_mst)
