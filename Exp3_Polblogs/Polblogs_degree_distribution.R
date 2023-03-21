library(ggplot2)
library(igraph)
library(MASS)
library(Matrix)
source('./NR/NR_help_func.R')


adj_origi <- as.matrix(read.csv('./Exp4_Polblogs/Data/Network.csv', header = F))
true_membership <- read.csv('./Exp4_Polblogs/Data/Nodes_Labels.csv', header = F)
true_membership <- c(true_membership$V1)
g_origi <- graph_from_adjacency_matrix(adj_origi, mode = 'undirected', weighted = T)

n <- length(V(g_origi))
e1 <- length(E(g_origi))

diameter(g_origi)
max(degree(g_origi))
mean(degree(g_origi))
transitivity(g_origi)

plot(degree.distribution(g_origi),
     main = 'Degree distribution of Polblogs graph',
     xlab = 'degree',
     ylab = 'probability',
     col = '#124E78',
     type = 'l',
     lwd = 2)
# 500*400
mem.col <- c('#F2BB05', '#124E78')
plot(g_origi,
     vertex.color = mem.col[true_membership+1],
     layout = layout_with_mds,
     vertex.label = NA,
     vertex.size = round(log(degree(g_origi))),
     edge.width = 0.6)

# 800*800