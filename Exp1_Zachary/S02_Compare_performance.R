library(igraph)
library(ggplot2)
library(expm)
library(MASS)
source('./NR/NR_help_func.R')

#-----------------------------------------------------------------------------------
# NR improves the accuracy of Community Detection of Zachary karate club network.
#-----------------------------------------------------------------------------------
# We evaluate the clustering performance of six community detection methods
# (fast-greedy, Louvain, optimal, leading-eigen, Walktrap, spinglass)
# on the Zachary network before and after applying NR and NE as a preprocessing
# step once and twice respectively.
#-----------------------------------------------------------------------------------

Methods <- c(
  'cluster_fast_greedy',
  'cluster_louvain',
  'cluster_optimal',
  'cluster_leading_eigen',
  'cluster_walktrap'
)

g_origi <- make_graph("Zachary")
true_membership <- c(1,1,1,1,1,1,1,1,2,2,1,1,1,1,2,2,1,
                     1,2,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2)

n <- length(V(g_origi))
e1 <- length(E(g_origi))

#
# diameter(g_origi)
# max(degree(g_origi))
# mean(degree(g_origi))
# transitivity(g_origi)

# plot(degree.distribution(g_origi),
#      main = 'Degree distribution of Zachary graph',
#      xlab = 'degree',
#      ylab = 'probability',
#      col = '#124E78',
#      type = 'l',
#      lwd = 2)
# # 500*400

adj_origi <- matrix(as_adjacency_matrix(g_origi), nrow = n)

m <- 1.5

#-------------------------------------------------------------------------------
# apply NR algorithm once
print('Applying NR once................')
out_NR <- NR(adj_origi, m)
out_NR <- (out_NR + t(out_NR)) / 2
out_NR <- out_NR - diag(diag(out_NR))
out_NR <- out_NR / max(out_NR)
# out_NR <- find_thre(out_NR)
g_NR_once <-graph_from_adjacency_matrix(out_NR, mode = 'undirected', weighted = T)

#-------------------------------------------------------------------------------
# apply NR algorithm twice
print('Applying NR twice................')
out_NR_twice <- NR(out_NR, m)
out_NR_twice <- (out_NR_twice + t(out_NR_twice)) / 2
out_NR_twice <- out_NR_twice - diag(diag(out_NR_twice))
out_NR_twice <- out_NR_twice / max(out_NR_twice) 
# out_NR_twice <- find_thre(out_NR_twice)
g_NR_twice <- graph_from_adjacency_matrix(out_NR_twice,  mode = 'undirected', weighted = T)

#-------------------------------------------------------------------------------
# apply katz algorithm once
print('Applying katz once................')
out_katz <- katz(adj_origi/eigen(adj_origi)$val[1], 1/m)
out_katz <- (out_katz + t(out_katz)) / 2
out_katz <- out_katz - diag(diag(out_katz))
out_katz <- out_katz / max(out_katz)
g_katz_once <- graph_from_adjacency_matrix(out_katz, mode = 'undirected', weighted = T)

#-------------------------------------------------------------------------------
# apply katz algorithm twice
print('Applying katz twice................')
out_katz_twice <- katz(out_katz/eigen(out_katz)$val[1], 1/m)
out_katz_twice <- (out_katz_twice + t(out_katz_twice)) / 2
out_katz_twice <- out_katz_twice - diag(diag(out_katz_twice))
out_katz_twice <- out_katz_twice / max(out_katz_twice)
g_katz_twice <- graph_from_adjacency_matrix(out_katz_twice, mode = 'undirected', weighted = T)


#-------------------------------------------------------------------------------
# apply LP algorithm once
print('Applying LP once................')
out_LP <- LP(adj_origi/eigen(adj_origi)$val[1], 1/m)
out_LP <- (out_LP + t(out_LP)) / 2
out_LP <- out_LP - diag(diag(out_LP))
out_LP <- out_LP / max(out_LP)
g_LP_once <- graph_from_adjacency_matrix(out_LP, mode = 'undirected', weighted = T)

#-------------------------------------------------------------------------------
# apply LP algorithm twice
print('Applying LP twice................')
out_LP_twice <- LP(out_LP/eigen(out_LP)$val[1], 1/m)
out_LP_twice <- (out_LP_twice + t(out_LP_twice)) / 2
out_LP_twice <- out_LP_twice - diag(diag(out_LP_twice))
out_LP_twice <- out_LP_twice /max(out_LP_twice) 
g_LP_twice <- graph_from_adjacency_matrix(out_LP_twice, mode = 'undirected', weighted = T)


#-------------------------------------------------------------------------------
# apply CN algorithm once
print('Applying CN once................')
out_CN <- CN(adj_origi/eigen(adj_origi)$val[1], 1/m)
out_CN <- (out_CN + t(out_CN)) / 2
out_CN <- out_CN - diag(diag(out_CN))
out_CN <- out_CN /max(out_CN)
g_CN_once <- graph_from_adjacency_matrix(out_CN, mode = 'undirected', weighted = T)


#-------------------------------------------------------------------------------
# apply CN algorithm twice
print('Applying CN twice................')
out_CN_twice <- CN(out_CN/eigen(out_CN)$val[1], 1/m)
out_CN_twice <- (out_CN_twice + t(out_CN_twice)) / 2
out_CN_twice <- out_CN_twice - diag(diag(out_CN_twice))
out_CN_twice <- out_CN_twice/max(out_CN_twice) 
g_CN_twice <- graph_from_adjacency_matrix(out_CN_twice, mode = 'undirected', weighted = T)



#-------------------------------------------------------------------------------
# apply NE algorithm once
print('Applying NE once................')
out_NE_once <-
  as.matrix(read.csv(
    "./Exp2_Zachary/Data/zachary_NE.csv",
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
    "./Exp2_Zachary/Data/zachary_NE2.csv",
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
score_ARI <- matrix(0, nrow = length(Methods), ncol = length(col_content))



for (i in 1:length(Methods)) {
    for (j in 1:length(col_content)) {
      print(paste0('Methods: ', Methods[i], col_content[j]))
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






#-------------------------------------------------------------------------------
methods_name <- c('greedy',
                  'louvain',
                  'optimal',
                  'eigen',
                  'walktrap')

data <- data.frame(
  algorithm = rep(methods_name, length(col_content)),
  methods = c(
    rep('raw', length(methods_name)),
    rep('CN', length(methods_name)),
    rep('CN*2', length(methods_name)),
    rep('LP', length(methods_name)),
    rep('LP*2', length(methods_name)),
    rep('NE', length(methods_name)),
    rep('NE*2', length(methods_name)),
    rep('Katz', length(methods_name)),
    rep('Katz*2', length(methods_name)),
    rep('NR', length(methods_name)),
    rep('NR*2', length(methods_name))
  ),
  NMI = as.vector(score_NMI),
  ARI = as.vector(score_ARI)
)

data$algorithm <- factor(data$algorithm, methods_name)


data$methods <-
  factor(
    data$methods,
    c(
      'raw',
      'NR',
      'NR*2',
      'NE',
      'NE*2',
      'LP',
      'LP*2',
      'CN',
      'CN*2',
      'Katz',
      'Katz*2'
    )
  )




#-------------------------------------------------------------------------------
ggplot(data, mapping = aes(x = algorithm, y = NMI, fill = methods)) +
  geom_bar(stat = 'identity',
           position = 'dodge',
           width = 0.5) +
  scale_fill_manual(
    values = c(
      "#BEBEBE",
      "#EE8CE3",
      "#B536A6",
      "#76EEC6",
      "#67C2A3",
      "#F5DEB3",
      "#DEB887",
      "#C5D86D",
      "#788736",
      "#C6E2FF",
      "#87CEFF"
    )
  ) +
  ylab('NMI') + xlab("") + theme_bw() + ylim(0, 1) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(
      size = 13,
      angle = 45,
      hjust = 0.5,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  )

ggplot(data, mapping = aes(x = algorithm, y = ARI, fill = methods)) +
  geom_bar(stat = 'identity',
           position = 'dodge',
           width = 0.5) +
  scale_fill_manual(
    values = c(
      "#BEBEBE",
      "#EE8CE3",
      "#B536A6",
      "#76EEC6",
      "#67C2A3",
      "#F5DEB3",
      "#DEB887",
      "#C5D86D",
      "#788736",
      "#C6E2FF",
      "#87CEFF"
    )
  ) +
  ylab('ARI') + xlab("") + theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(
      size = 13,
      angle = 45,
      hjust = 0.5,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  )


# 1000*350
