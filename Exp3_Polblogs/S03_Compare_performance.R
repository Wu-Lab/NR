library(igraph)
library(ggplot2)
library(expm)
library(MASS)
library(RColorBrewer)
source('./NR/NR_help_func.R')



Methods <- c(
  'cluster_fast_greedy',
  'cluster_louvain',
  'cluster_leading_eigen',
  'cluster_walktrap'
)

adj_origi <- as.matrix(read.csv('./Exp4_Polblogs/Data/Network.csv', header = F))
true_membership <- read.csv('./Exp4_Polblogs/Data/Nodes_Labels.csv', header = F)
true_membership <- c(true_membership$V1)
g_origi <- graph_from_adjacency_matrix(adj_origi, mode = 'undirected', weighted = T)

n <- length(V(g_origi))
e1 <- length(E(g_origi))


m <- 1.5

#-------------------------------------------------------------------------------
# apply NR algorithm once
print('Applying NR once................')
out_NR <- NR(adj_origi, m)
out_NR <- (out_NR + t(out_NR)) / 2
out_NR <- out_NR - diag(diag(out_NR))
out_NR <- (out_NR - min(out_NR)) /
  (max(out_NR) - min(out_NR))
g_NR_once <-graph_from_adjacency_matrix(out_NR, mode = 'undirected', weighted = T)

#-------------------------------------------------------------------------------
# apply NR algorithm twice
print('Applying NR twice................')
out_NR_twice <- NR(out_NR, m)
out_NR_twice <- (out_NR_twice + t(out_NR_twice)) / 2
out_NR_twice <- out_NR_twice - diag(diag(out_NR_twice))
out_NR_twice <- (out_NR_twice - min(out_NR_twice)) /
  (max(out_NR_twice) - min(out_NR_twice))

g_NR_twice <- graph_from_adjacency_matrix(out_NR_twice,  mode = 'undirected', weighted = T)

#-------------------------------------------------------------------------------
# apply katz algorithm once
print('Applying katz once................')
out_katz <- katz(adj_origi/eigen(adj_origi)$val[1], 1/m)
out_katz <- (out_katz + t(out_katz)) / 2
out_katz <- out_katz - diag(diag(out_katz))
out_katz <- (out_katz - min(out_katz)) /
  (max(out_katz) - min(out_katz))

g_katz_once <- graph_from_adjacency_matrix(out_katz, mode = 'undirected', weighted = T)

#-------------------------------------------------------------------------------
# apply katz algorithm twice
print('Applying katz twice................')
out_katz_twice <- katz(out_katz/eigen(out_katz)$val[1], 1/m)
out_katz_twice <- (out_katz_twice + t(out_katz_twice)) / 2
out_katz_twice <- out_katz_twice - diag(diag(out_katz_twice))
out_katz_twice <- (out_katz_twice - min(out_katz_twice)) /
  (max(out_katz_twice) - min(out_katz_twice))

g_katz_twice <- graph_from_adjacency_matrix(out_katz_twice, mode = 'undirected', weighted = T)


#-------------------------------------------------------------------------------
# apply LP algorithm once
print('Applying LP once................')
out_LP <- LP(adj_origi/eigen(adj_origi)$val[1], 1/m)
out_LP <- (out_LP + t(out_LP)) / 2
out_LP <- out_LP - diag(diag(out_LP))
out_LP <- (out_LP - min(out_LP)) /
  (max(out_LP) - min(out_LP))

g_LP_once <- graph_from_adjacency_matrix(out_LP, mode = 'undirected', weighted = T)

#-------------------------------------------------------------------------------
# apply LP algorithm twice
print('Applying katz twice................')
out_LP_twice <- LP(out_LP/eigen(out_LP)$val[1], 1/m)
out_LP_twice <- (out_LP_twice + t(out_LP_twice)) / 2
out_LP_twice <- out_LP_twice - diag(diag(out_LP_twice))
out_LP_twice <- (out_LP_twice - min(out_LP_twice)) /
  (max(out_LP_twice) - min(out_LP_twice))

g_LP_twice <- graph_from_adjacency_matrix(out_LP_twice, mode = 'undirected', weighted = T)



#-------------------------------------------------------------------------------
# apply CN algorithm once
print('Applying CN once................')
out_CN <- CN(adj_origi/eigen(adj_origi)$val[1], 1/m)
out_CN <- (out_CN + t(out_CN)) / 2
out_CN <- out_CN - diag(diag(out_CN))
out_CN <- (out_CN - min(out_CN)) /
  (max(out_CN) - min(out_CN))

g_CN_once <- graph_from_adjacency_matrix(out_CN, mode = 'undirected', weighted = T)

#-------------------------------------------------------------------------------
# apply CN algorithm twice
print('Applying CN twice................')
out_CN_twice <- CN(out_CN/eigen(out_CN)$val[1], 1/m)
out_CN_twice <- (out_CN_twice + t(out_CN_twice)) / 2
out_CN_twice <- out_CN_twice - diag(diag(out_CN_twice))
out_CN_twice <- (out_CN_twice - min(out_CN_twice)) /
  (max(out_CN_twice) - min(out_CN_twice))

g_CN_twice <- graph_from_adjacency_matrix(out_CN_twice, mode = 'undirected', weighted = T)



#-------------------------------------------------------------------------------
# apply NE algorithm once
print('Applying NE once................')
out_NE_once <-
  as.matrix(read.csv(
    "./Exp4_Polblogs/Data/Network_NE.csv",
    sep = ',',
    encoding = "UTF-8",
    header = F
  ))

out_NE_once <- (out_NE_once + t(out_NE_once)) / 2
out_NE_once <- out_NE_once - diag(diag(out_NE_once))
out_NE_once <- (out_NE_once - min(out_NE_once)) /
  (max(out_NE_once) - min(out_NE_once))

g_NE_once <- graph_from_adjacency_matrix(out_NE_once, mode = 'undirected', weighted = T)

#-------------------------------------------------------------------------------
# apply NE algorithm twice
print('Applying NE twice................')
out_NE_twice <-
  as.matrix(read.csv(
    "./Exp4_Polblogs/Data/Network_NE2.csv",
    sep = ',',
    encoding = "UTF-8",
    header = F
  ))

out_NE_twice <- (out_NE_twice + t(out_NE_twice)) / 2
out_NE_twice <- out_NE_twice - diag(diag(out_NE_twice))
out_NE_twice <- (out_NE_twice - min(out_NE_twice)) /
  (max(out_NE_twice) - min(out_NE_twice))

g_NE_twice <- graph_from_adjacency_matrix(out_NE_twice, mode = 'undirected', weighted = T)


#-------------------------------------------------------------------------------



#===============================================
#===============================================
col_content <-
  c(
    'g_origi',
    'g_CN_once',
    'g_CN_twice',
    'g_LP_once',
    'g_LP_twice',
    'g_NE_once',
    'g_NE_twice',
    'g_katz_once',
    'g_katz_twice',
    'g_NR_once',
    'g_NR_twice'
  )
print('***********************************************')
print('evaluating the clustering performance.........')
score_NMI <- matrix(0, nrow = length(Methods), ncol = length(col_content))
rownames(score_NMI) <- Methods
colnames(score_NMI) <- col_content

score_ARI <- matrix(0, nrow = length(Methods), ncol = length(col_content))
rownames(score_ARI) <- Methods
colnames(score_ARI) <- col_content



for (i in 1:length(Methods)) {
  
  for (j in 1:length(col_content)) {
    print(paste0('Methods: ', Methods[i], '_', col_content[j]))
    Compare_membership <- get(Methods[i])(get(col_content[j]),
                                          weights = E(get(col_content[j]))$weight)$membership
    score_NMI[i, j] <-
      compare(true_membership, Compare_membership,
              method = 'nmi')
    
    score_ARI[i, j] <-
      compare(true_membership, Compare_membership,
              method = 'adjusted.rand')
  }
}
print('*********************************************************************')




write.csv(score_NMI, './Exp4_Polblogs/Results/score_NMI.csv')
write.csv(score_ARI, './Exp4_Polblogs/Results/score_ARI.csv')


