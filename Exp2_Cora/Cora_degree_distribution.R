library(ggplot2)
library(igraph)
library(MASS)
library(Matrix)
source('./NR/NR_help_func.R')


# adj_origi <-
#   as.matrix(read.table('./Exp3_Cora/Data/Network.txt', header = F))
# true_membership <-
#   read.table('./Exp3_Cora/Data/Nodes_Labels.txt', header = F)
# true_membership <- c(true_membership$V1)
# 
# Network <- adj_origi+t(adj_origi)
# Network[Network>1] <- 1
# 
# Network <- Network-diag(diag(Network))
# 
# g <- graph_from_adjacency_matrix(Network,
#                                  mode = 'undirected',
#                                  weighted = T)
# n <- length(V(g))
# e <- length(E(g))
# 
# kept_index <- which(components(g)$membership == 1)
# Network <- Network[kept_index, kept_index]
# true_membership <- true_membership[kept_index]
# 
# write.table(Network, file='./Exp3_Cora/Data/Network.csv',
#             row.names = F, col.names = F, sep=',')
# write.table(true_membership, file='./Exp3_Cora/Data/Nodes_Labels.csv',
#             row.names = F, col.names = F, sep=',')




adj_origi <- as.matrix(read.csv('./Exp3_Cora/Data/Network.csv', header = F))
true_membership <- read.csv('./Exp3_Cora/Data/Nodes_Labels.csv', header = F)
true_membership <- c(true_membership$V1)
g_origi <- graph_from_adjacency_matrix(adj_origi, mode = 'undirected', weighted = T)

n <- length(V(g_origi))
e <- length(E(g_origi))

diameter(g_origi)
max(degree(g_origi))
mean(degree(g_origi))
transitivity(g_origi)


mem.col <- brewer.pal(n = length(unique(true_membership)), name = "Spectral")
plot(g_origi,
     vertex.color = mem.col[true_membership+1],
     layout = layout_with_drl,
     vertex.label = NA,
     vertex.size = round(log(degree(g_origi))),
     edge.width = 0.8)


plot(degree.distribution(g_origi),
     main = 'Degree distribution of Cora graph',
     xlab = 'degree',
     ylab = 'probability',
     col = '#124E78',
     type = 'l',
     lwd = 2)

# 500*400