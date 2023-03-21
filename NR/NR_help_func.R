



#---------------------------------------------------------------------------------
#                                 help functions
#---------------------------------------------------------------------------------
# plot the membership of a graph
plot.membership <- function(graph, membership, main = "") {
  V(graph)$member <- membership
  mem.col <- c("#00CD00", "#FFB90F", '#00BFFF', '#EE799F')
  V(graph)$color <- mem.col[membership]
  plot(
    graph,
    edge.width = 2,
    vertex.color = V(graph)$color,
    main = main,
    edge.arrow.size = 0.7,
    vertex.label.cex = 0.8,
    vertex.size = 6
  )
}


# change the weighted matrix into the adjacency matrix
change_into_adj <- function(w, thre) {
  n <- dim(w)[1]
  adj_mat <- matrix(0, nrow = n, ncol = n)
  adj_mat[w > thre] <- 1
  return(adj_mat)
}


# find the threshold such that the unweighted graph has no isolated vertices
find_thre_no_iso <- function(W) {
  n <- dim(W)[1]
  thre <- unique(sort(W, decreasing = T))
  for (k in n:length(thre)) {
    adj <- change_into_adj(W, thre[k])
    adj <- (adj + t(adj)) / 2
    rs <- rowSums(adj)
    if (length(rs[rs == 0]) == 0) {
      break
    }
  }
  
  return(adj)
}


# generate the planted l-partition benchmark graphs randomly
generate_plp <- function(n1, l, p_in, p_out) {
  n <- n1 * l
  A <- matrix(0, nrow = n, ncol = n)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      A[i, j] <- sample(c(0, 1), 1, prob = c(1 - p_out, p_out))
    }
  }
  
  n2 <- 1
  for (m in 1:l) {
    for (i in n2:(m * n1)) {
      if (i == n) {
        break
      }
      for (j in (i + 1):(m * n1)) {
        if (i != j) {
          A[i, j] <- sample(c(0, 1), 1, prob = c(1 - p_in, p_in))
        }
      }
    }
    n2 <- n2 + n1
  }
  
  A <- (A + t(A)) / 2
  A[A > 0] <- 1
  A <- A - diag(diag(A))
}


# generate the planted l-partition benchmark graphs randomly
generate_plp_hard <- function(node_label, p_in, p_out) {
  n <- length(node_label)
  A <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in i:n) {
      if (node_label[i]==node_label[j]){
        A[i, j] <- sample(c(0, 1), 1, prob = c(1 - p_in, p_in))
      }
      else{
        A[i, j] <- sample(c(0, 1), 1, prob = c(1 - p_out, p_out))
      }
    }
  }
  
  A <- (A + t(A)) / 2
  A[A > 0] <- 1
  A <- A - diag(diag(A))
}




# compute noisy proportion
compute_error <- function(node_label, Adj) {
  Adj <- Adj - diag(diag(Adj))
  inner_mat <- matrix(0, nrow = dim(Adj)[1], ncol = dim(Adj)[2])
  for (i in 1:dim(Adj)[1]) {
    for (j in i:dim(Adj)[2]) {
      if (node_label[i] == node_label[j]) {
        inner_mat[i, j] <- 1
      }
    }
  }
  inner_mat <- inner_mat + t(inner_mat)
  inner_mat <- inner_mat - diag(diag(inner_mat))
  
  inner_weight <- sum(inner_mat * Adj)
  outer_weight <- sum(Adj) - inner_weight
  
  return(outer_weight / inner_weight)
}


# =====================================================================
#                   Denoising methods
# =====================================================================


# NR algorithm
NR <- function(mat, m) {
  n <- dim(mat)[1]
  p1 <- mat / rowSums(mat)
  p2 <- (m - 1) * p1 %*% solve(diag(m, nrow = n) - p1)
  w_out <- diag(rowSums(mat)) %*% p2
  w_out <- w_out + t(w_out)
  w_out <- sum(mat)*w_out/sum(w_out)
  return(w_out)
  
}


# katz index
katz <- function(A, beta = 0.01) {
  n <- dim(A)[1]
  I <- diag(n)
  r <- eigen(A)$val[1]  # spectral radius
  thre <- 1 / r
  
  if (beta < thre) {
    w_out <- solve(I - A * beta) - I
  }
  else {
    beta <- thre / 2
    w_out <- solve(I - A * beta) - I
  }
  
  w_out <- w_out + t(w_out)
  w_out <- sum(A)*w_out/sum(w_out)
  return(w_out)
}

# Common neighbors
CN <- function(A, beta = 0.01) {
  w_out <- beta ** 2 * A %*% A + beta * A
  return(w_out)
}

# Local path
LP <- function(A, beta = 0.01) {
  w_out <- beta ** 3 * A %*% A %*% A + beta ** 2 * A %*% A + beta * A
  return(w_out)
}
