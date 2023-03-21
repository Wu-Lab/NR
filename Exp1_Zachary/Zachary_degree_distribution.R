library(ggplot2)
library(igraph)
library(MASS)
library(Matrix)
source('./NR/NR_help_func.R')


g_origi <- make_graph("Zachary")


plot(degree.distribution(g_origi),
     main = 'Degree distribution of Zachary graph',
     xlab = 'degree',
     ylab = 'probability',
     col = '#124E78',
     type = 'l',
     lwd = 2)

# 500*400