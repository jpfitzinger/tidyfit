

.select_optimal_k <- function(
    data, distmat, clust_function, eval_fun, k, k.min, k.max, method, transpose
) {
  
  max_feasible_k <- ifelse(transpose, ncol(data) - 1, nrow(data) - 1)
  
  # create a vector of cluster options
  if (is.null(k)) {
    k_vec <- k.min:min(k.max, max_feasible_k)
  } else {
    k_vec <- min(k, max_feasible_k)
  }
  
  .select_optimal_k_silhouette <- function(k.min, k.max) {
    k_vec <- max(2, k.min):k.max
    res <- lapply(k_vec, function(k) {
      cl_results <- clust_function(x = data, k = k)
      sil_width <- cluster::silhouette(cl_results$clustering, distmat)
      score <- mean(sil_width[, 3])
      res <- list(
        score = score, k = k, clustering = cl_results$clustering, result = cl_results$result
      )
    })
    res <- res[[which.max(sapply(res, function(r) r$score))]]
    return(res)
  }
  
  .select_optimal_k_gap <- function(k.min, k.max) {
    if (transpose) {
      x <- stats::cor(data)
    } else {
      x <- data
    }
    cl_results <- cluster::clusGap(x, clust_function, k.max)
    best_k <- cluster::maxSE(cl_results$Tab[, "gap"], cl_results$Tab[, "SE.sim"], method = "globalSEmax")
    best_k <- max(k.min, best_k)
    res <- clust_function(data, best_k)
    res$k <- best_k
    return(res)
  }
  
  if (!is.null(k)) {
    .optimal_k_selector <- purrr::safely(purrr::quietly(clust_function))
    output <- .optimal_k_selector(data, min(k, max_feasible_k))
  } else if (method == "silhouette") {
    .optimal_k_selector <- purrr::safely(purrr::quietly(.select_optimal_k_silhouette))
    output <- .optimal_k_selector(k.min, max_feasible_k)
  } else if (method == "gap") {
    .optimal_k_selector <- purrr::safely(purrr::quietly(.select_optimal_k_gap))
    output <- .optimal_k_selector(k.min, max_feasible_k)
  } else {
    stop("Invalid method for 'method'")
  }
  
  output$result$result$coords <- .get_pca_coords(data, transpose)
  
  # flatten results
  output$result <- append(
    output$result$result, 
    output$result[c("output", "warnings", "messages")]
  )
  
  return(output)
  
}

.get_cluster_dist_mat <- function(data, transpose, metric) {
  if (transpose) {
    x <- stats::dist(stats::cor(data), method = metric)
  } else {
    x <- stats::dist(data, method = metric)
  }
  return(x)
}

.get_pca_coords <- function(data, transpose) {
  if (transpose)
    data <- cor(data)
  pca <- stats::prcomp(data, retx = TRUE)
  coords <- sweep(pca$x[, 1:2], 2, sign(colSums(pca$rotation[,1:2])), "*")
  colnames(coords) <- c("x", "y")
  return(coords)
}

.align_clusters <- function(df) {
  
  # only align fitted models
  fitted_mods <- sapply(df$model_object, function(mod) !is.null(mod$object))
  ngr <- sum(fitted_mods)
  fitted_results <- lapply(df[fitted_mods,]$model_object, function(obj) obj$fitted()$fitted)
  
  if (ngr <= 1) return(df)
  if (length(unique(sapply(fitted_results, length))) > 1) return(df)
  
  # align cluster labels using Stephen's method
  max_k <- max(unlist(fitted_results))
  if (max_k == 1) return(df)
  Q_list <- lapply(fitted_results, function(mat) {
    sapply(1:max_k, function(i) (mat == i) * 1)
  })
  Q_array <- aperm(simplify2array(Q_list), c(3,1,2))
  perm <- .get_stephens(Q_array)
  Q_list_update <- lapply(seq_len(dim(Q_array)[1]), function(i) {
    q_perm <- Q_list[[i]][, perm$permutations[i, ]]
    q_perm
    })
  
  # align coords
  coord_signs <- list(
    c(1, 1), c(1, -1), c(-1, 1), c(-1, -1)
  )
  
  for (i in 1:ngr) {
    repl_mod <- df[fitted_mods,][i, "model_object"]$model_object[[1]]
    repl_mod$fit_info$clusters <- rowSums(sweep(Q_list_update[[i]], 2, 1:ncol(Q_list_update[[i]]), "*"))
    if (i == 1) {
      ref_coords <- repl_mod$fit_info$coords
    } else {
      mod_coords <- repl_mod$fit_info$coords
      scores <- lapply(coord_signs, function(s) mean((ref_coords - sweep(mod_coords, 2, s, "*"))^2))
      sign_opt <- coord_signs[[which.min(scores)]]
      repl_mod$fit_info$coords <- sweep(mod_coords, 2, sign_opt, "*")
    }
  }
  
  return(df)
}

.get_stephens <- function (p, threshold, maxiter) 
{
  n <- dim(p)[2]
  m <- dim(p)[1]
  k <- dim(p)[3]
  burnin <- 0
  K <- k
  st <- 1:k
  perm <- c(numeric(k))
  cost.matrix <- matrix(numeric(k * k), nrow = k, ncol = k)
  s <- 1:n
  up.threshold <- 1 - 10^(-6)
  down.threshold <- 10^(-6)
  for (k in 1:K) {
    for (i in 1:n) {
      up.index <- which(p[, i, k] > up.threshold)
      down.index <- which(p[, i, k] < down.threshold)
      if (length(up.index) > 0) {
        p[up.index, i, k] <- rep(up.threshold, length(up.index))
      }
      if (length(down.index) > 0) {
        p[down.index, i, k] <- rep(down.threshold, length(down.index))
      }
    }
  }
  for (iter in 1:m) {
    p[iter, , ] <- p[iter, , ]/rowSums(p[iter, , ])
  }
  perm <- array(data = NA, dim = c(m, k))
  for (j in 1:k) {
    perm[, j] <- j
  }
  q <- array(data = 0, dim = c(n, k))
  previous <- -99
  criterion <- 99
  if (missing(threshold)) {
    threshold <- 10^(-6)
  }
  if (missing(maxiter)) {
    maxiter <- 100
  }
  t <- 0
  while ((criterion > threshold) && (t < maxiter)) {
    t <- t + 1
    q <- array(data = 0, dim = c(n, k))
    for (j in 1:k) {
      for (iter in 1:m) {
        q[, j] <- q[, j] + p[iter, , perm[iter, j]]
      }
    }
    q <- q/m
    for (iter in 1:m) {
      for (j in 1:k) {
        temp <- p[iter, , ] * (log(p[iter, , ]) - log(q[, 
                                                        j]))
        cost.matrix[j, ] <- colSums(temp)
      }
      matr <- lpSolve::lp.assign(cost.matrix)$solution
      for (i in 1:k) {
        perm[iter, i] <- st[matr[, i] > 0]
      }
      perm[iter, ] <- order(perm[iter, ])
    }
    current <- cost.function <- sum(cost.matrix * matr)
    criterion <- abs(previous - current)
    previous <- current
  }
  status <- paste("Converged (", t, " iterations)", sep = "")
  if (criterion > threshold) {
    status <- "Max iterations exceeded"
  }
  results <- list(perm, t, status)
  names(results) <- c("permutations", "iterations", "status")
  return(results)
}
