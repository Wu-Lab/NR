library(igraph)



# import community label
Network <- as.matrix(read.table('./Exp4_Citeseer/Data/Network.txt', header = F))
Nodes_Labels <- read.table('./Exp4_Citeseer/Data/Nodes_Labels.txt', header = F)
Nodes_Labels <- c(Nodes_Labels$V1)
g <- graph_from_adjacency_matrix(Network, mode = 'undirected', weighted = T)
  

# largest connected components
Nodes <- which(components(g)$membership==which.max(components(g)$csize))
Network <- Network[Nodes, Nodes]

# change labels 
Nodes_Labels <- Nodes_Labels[Nodes]


# save
write.table(Network, file='./Exp4_Citeseer/Data/Network.csv',
            row.names = F, col.names = F, sep=',')
write.table(Nodes_Labels, file='./Exp4_Citeseer/Data/Nodes_Labels.csv',
            row.names = F, col.names = F, sep=',')






