library(igraph)


g <- read.graph('./Exp4_Polblogs/Data/polblogs.gml', format = 'gml')

n <- length(V(g))
e <- length(E(g))

adj_origi <- matrix(as_adjacency_matrix(g), nrow = n)
label <- V(g)$value

Network <- adj_origi+t(adj_origi)
Network[Network>1] <- 1

Network <- Network-diag(diag(Network))

g <- graph_from_adjacency_matrix(Network,
                                 mode = 'undirected',
                                 weighted = T)
n <- length(V(g))
e <- length(E(g))

kept_index <- which(components(g)$membership == 1)
Network <- Network[kept_index, kept_index]
label <- label[kept_index]



write.table(Network, file='./Exp4_Polblogs/Data/Network.csv',
            row.names = F, col.names = F, sep=',')
write.table(label, file='./Exp4_Polblogs/Data/Nodes_Labels.csv',
            row.names = F, col.names = F, sep=',')


