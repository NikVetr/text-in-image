#functions for writing text in images

mcprint <- function(...){
  system(sprintf('echo "%s"', paste0(..., collapse="")))
}

hsla2rgba <- function(hsla) {
  # Helper function to convert HSL to RGB
  h2rgb <- function(p, q, t) {
    if(t < 0) t <- t + 1
    if(t > 1) t <- t - 1
    if(t < 1/6) return(p + (q - p) * 6 * t)
    if(t < 1/2) return(q)
    if(t < 2/3) return(p + (q - p) * (2/3 - t) * 6)
    return(p)
  }

  # Convert function
  convert <- function(color) {
    h <- color[1]
    s <- color[2]
    l <- color[3]
    a <- color[4]

    if(s == 0) {
      r <- g <- b <- l # achromatic
    } else {
      q <- if(l < 0.5) l * (1 + s) else l + s - l * s
      p <- 2 * l - q
      r <- h2rgb(p, q, h + 1/3)
      g <- h2rgb(p, q, h)
      b <- h2rgb(p, q, h - 1/3)
    }

    c(r = r, g = g, b = b, a = a)
  }

  # Apply conversion based on input dimension
  if (length(dim(hsla)) == 3) {
    aperm(apply(hsla, c(1,2), function(x) convert(x)), c(2,3,1))
  } else {
    apply(hsla, 1, convert)
  }
}

rgba2hsla <- function(rgba) {
  # Helper function to convert RGB to HSL
  rgb2hsl <- function(r, g, b, a) {
    max <- max(c(r, g, b))
    min <- min(c(r, g, b))
    l <- (max + min) / 2

    if(max == min) {
      h <- s <- 0 # achromatic
    } else {
      d <- max - min
      s <- if(l > 0.5) d / (2 - max - min) else d / (max + min)
      if(max == r) {
        h <- (g - b) / d + (if(g < b) 6 else 0)
      } else if(max == g) {
        h <- (b - r) / d + 2
      } else {
        h <- (r - g) / d + 4
      }
      h <- h / 6
    }

    c(h = h, s = s, l = l, a = a)
  }

  # Apply conversion based on input dimension
  if (length(dim(rgba)) == 3) {
    aperm(apply(rgba, c(1,2), function(x) rgb2hsl(x[1], x[2], x[3], x[4])), c(2,3,1))
  } else {
    apply(rgba, 1, function(color) rgb2hsl(color[1], color[2], color[3], color[4]))
  }
}


scale_img <- function(max_pixels, img){
  max_dim_ind <- which.max(dim(img)[1:2])
  new_img_scale <- max_pixels / dim(img)[max_dim_ind]
  new_dims <- round(dim(img)[1:2] * new_img_scale)
  pixel_map <- list(ris = ceiling(1:new_dims[1] / new_img_scale),
                    cis = ceiling(1:new_dims[2] / new_img_scale))
  pixel_map$ris <- apply(cbind(pixel_map$ris, dim(img)[1]), 1, min)
  pixel_map$cis <- apply(cbind(pixel_map$cis, dim(img)[2]), 1, min)
  new_img <- array(0, dim = c(new_dims, dim(img)[3]))
  for(i in 1:dim(img)[3]){
    new_img[,,i][as.matrix(expand.grid(1:new_dims[1],
                                       1:new_dims[2]))] <- img[,,i][as.matrix(expand.grid(pixel_map$ris, pixel_map$cis))]
  }
  return(new_img)
}

pythag <- function(x) sqrt(sum(x^2))

rotate_grid <- function(grid, theta) {
  theta_rad <- theta * (pi / 180)
  inv_theta_rad <- -theta_rad  # Inverse rotation

  # Dimensions of the original grid
  n <- nrow(grid)
  m <- ncol(grid)

  # Center of the original grid
  center_x <- (m + 1) / 2
  center_y <- (n + 1) / 2

  # Calculate extremes of the original grid's coordinates after rotation
  orig_coords <- expand.grid(x = c(1, m, 1, m), y = c(1, 1, n, n))
  rot_coords_x <- cos(theta_rad) * (orig_coords$x - center_x) - sin(theta_rad) * (orig_coords$y - center_y) + center_x
  rot_coords_y <- sin(theta_rad) * (orig_coords$x - center_x) + cos(theta_rad) * (orig_coords$y - center_y) + center_y
  x_extremes <- range(rot_coords_x)
  y_extremes <- range(rot_coords_y)

  # Dimensions of the new grid
  new_m <- ceiling(x_extremes[2] - x_extremes[1])
  new_n <- ceiling(y_extremes[2] - y_extremes[1])
  new_center_x <- (new_m + 1) / 2
  new_center_y <- (new_n + 1) / 2

  # New grid
  new_grid <- matrix(-2, nrow = new_n, ncol = new_m)

  # Apply inverse rotation to new grid coordinates
  new_coords <- expand.grid(x = 1:new_m, y = 1:new_n)
  x_inv_rotated <- round(cos(inv_theta_rad) * (new_coords$x - new_center_x) - sin(inv_theta_rad) * (new_coords$y - new_center_y) + center_x)
  y_inv_rotated <- round(sin(inv_theta_rad) * (new_coords$x - new_center_x) + cos(inv_theta_rad) * (new_coords$y - new_center_y) + center_y)

  # Valid indices within the original grid
  valid <- x_inv_rotated >= 1 & x_inv_rotated <= m & y_inv_rotated >= 1 & y_inv_rotated <= n

  # Map valid coordinates from original to new grid
  new_grid[cbind(new_coords$y[valid], new_coords$x[valid])] <- grid[cbind(y_inv_rotated[valid], x_inv_rotated[valid])]

  return(new_grid)
}

unrotate_grid <- function(orig_grid, rotated_grid, theta, new_values) {
  # Reverse rotation
  unrotated_grid <- rotate_grid(rotated_grid, -theta)

  # Calculate dimensions and offsets
  orig_n <- nrow(orig_grid)
  orig_m <- ncol(orig_grid)
  unrot_n <- nrow(unrotated_grid)
  unrot_m <- ncol(unrotated_grid)
  offset_row <- floor((unrot_n - orig_n) / 2) + 1
  offset_col <- floor((unrot_m - orig_m) / 2) + 1

  # Trim to match dimensions of original grid
  trimmed_unrotated_grid <- unrotated_grid[offset_row:(offset_row + orig_n - 1), offset_col:(offset_col + orig_m - 1)]

  # Update original grid with new values where original grid has 0s
  update_mask <- orig_grid == 0 & trimmed_unrotated_grid %in% new_values
  orig_grid[update_mask] <- trimmed_unrotated_grid[update_mask]

  return(orig_grid)
}


add_transparent_layer <- function(img, transparent_color = "black"){
  tc_dist <- sqrt(3) / 3
  tprgb <- c(col2rgb(transparent_color))
  pixrgb <- cbind(c(img[,,1]), c(img[,,2]), c(img[,,3]))
  distrgb <- apply(t(t(pixrgb) - tprgb), 1, pythag)
  img <- abind::abind(img, img[,,3], along = 3)
  img[,,4] <- as.numeric(distrgb > tc_dist)
  return(img)
}

sqdists_to_centers <- function(mat, cents){
  if(is.null(nrow(cents))){
    cents <- matrix(cents, 1, length(cents))
  }
  sqdists <- do.call(cbind, lapply(1:nrow(cents), function(ki){
    colSums((t(mat) - cents[ki,])^2)
  }))
  return(sqdists)
}


wkmeans <- function(x, w, k, kmeans.iter.max = NA, n = NA, eps = NA, min_dist_q = NA){

  #hyperparameters
  if(is.na(n)){
    n <- k * 10
  }

  p <- ncol(x)

  if(is.na(kmeans.iter.max)){
    kmeans.iter.max <- k * 5
  }
  

  
  xdist <- c(dist(x))
  min_dist_cents <- quantile(xdist, ifelse(is.na(min_dist_q), 1/k, min_dist_q))
  
  if(is.na(eps)){
    eps <- min(xdist) / 10
  }

  kmeans_out <- parallel::mclapply(1:n, function(kmeans_run){
    
    #initialize centers w/ weighted kmeans++
    kmeans_iter <- 1
    converged <- F
    cents <- matrix(NA, k, p)
    cents[1,] <- x[sample(1:nrow(x), 1, prob = w),]
    for(ki in 2:k){
      sqdists <- sqdists_to_centers(x, cents[1:(ki-1),])
      cls <- apply(sqdists, 1, which.min)
      smallest_sqdists <- sqdists[cbind(1:length(cls), cls)]
      cents[ki,] <- x[sample(1:length(cls), 1, prob = smallest_sqdists * w),]
    }

    #evaluate initial cluster assignments
    sqdists <- sqdists_to_centers(x, cents)
    cls <- apply(sqdists, 1, which.min)

    #run kmeans iteratively
    while(!converged & kmeans_iter < kmeans.iter.max){
      
      #increment iter
      kmeans_iter <- kmeans_iter + 1

      #check that all clusters present and reassign cents if not
      prev_cents <- cents
      if(!all(1:k %in% cls)){
        empty_cls <- (1:k)[!(1:k %in% cls)]
        for(ki in empty_cls){
          sqdists <- sqdists_to_centers(x, cents)
          cls <- apply(sqdists, 1, which.min)
          smallest_sqdists <- sqdists[cbind(1:length(cls), cls)]
          cents[ki,] <- x[sample(1:length(cls), 1, prob = smallest_sqdists * w),]
        }
        sqdists <- sqdists_to_centers(x, cents)
        cls <- apply(sqdists, 1, which.min)
      }

      #find new centers
      cents <- do.call(rbind, lapply(1:k, function(ki){
        clusmems <- which(cls == ki) #members of the cluster
        clusvals <- x[clusmems,]
        cluscounts <- w[clusmems]
        apply(t(clusvals) * rep(cluscounts, each = p), 1, sum) / sum(cluscounts)
      }))
      
      #reassign any clusters that are too close
      close_pairs <- which((as.matrix(dist(cents)) + diag(nrow(cents)) * min_dist_cents * 1.1) < 
                             min_dist_cents, arr.ind = T)
      while(nrow(close_pairs) > 0){
        close_pairs <- unique(do.call(rbind, apply(close_pairs, 1, sort, simplify = F)))
        clus_weights <- sapply(split(w, cls), sum)
        cents[close_pairs[,1],] <- do.call(rbind, apply(close_pairs, 1, function(cis) 
          (cents[cis[1],] * clus_weights[cis[1]] + cents[cis[2],] * clus_weights[cis[2]]) / sum(clus_weights[cis]), 
          simplify = F))
        cents[close_pairs[,2],] <- NA 
        close_pairs <- which((as.matrix(dist(cents)) + diag(nrow(cents)) * min_dist_cents * 1.1) < 
                               min_dist_cents, arr.ind = T)
      }
      
      
      close_pairs_new <- which((as.matrix(dist(cents)) + diag(nrow(cents)) * min_dist_cents * 1.1) < 
                                 min_dist_cents, arr.ind = T)
      if(nrow(close_pairs_new) > 0){
        mcprint(close_pairs)
        mcprint("\n")
        mcprint(close_pairs_new)
      }
      
      #check that no cents are NA, and if they are, assign them to the worst matching pt that is not too close
      missing_centers <- sapply(1:nrow(cents), function(ri) any(is.na(cents[ri,])))
      if(any(missing_centers)){
        missing_inds <- which(missing_centers)
        for(ri in missing_inds){
          sqdists <- sqdists_to_centers(x, cents[-missing_inds,])
          min_sqdists <- apply(sqdists, 1, min)
          min_sqdists[min_sqdists < min_dist_cents] <- 0
          wmin_sqdists <- min_sqdists * w
          cents[ri,] <- x[which.max(wmin_sqdists),]
          missing_inds <- setdiff(missing_inds, ri)
        }
      }
      sqdists <- sqdists_to_centers(x, cents)
      

      #check for convergence
      converged <- all(abs(cents - prev_cents) < eps)

      #evaluate new cluster assignments
      if(!converged){
        cls <- apply(sqdists, 1, which.min)
      }

    }

    #calculate total variance / sum of squares
    total_ss <- sum(sqdists[cbind(1:length(cls), cls)] * w)

    #return value
    list(converged = converged, k = k, kmeans_iter = kmeans_iter, cents = cents, cls = cls, total_ss = total_ss, obs_counts = w)
  
  }, mc.cores = 15)

  #pick best run
  total_sss <- sapply(kmeans_out, function(kmeans_indiv) kmeans_indiv$total_ss)
  optimal_cluster_index <- which.min(total_sss)
  optimal_cluster <- kmeans_out[[optimal_cluster_index]]
  return(optimal_cluster)

}

shift_array <- function(arr, shift) {

  # Get the number of dimensions of the array
  dims <- dim(arr)
  num_dims <- length(dims)

  # Ensure 'shift' is as long as the number of dimensions, padding with zeros if necessary
  shift <- c(shift, rep(0, num_dims - length(shift)))

  # Initialize the shifted array as a copy of the original
  shifted_arr <- arr

  # Iterate through each dimension to apply the shift
  for (i in 1:num_dims) {
    if (shift[i] == 0) {
      next # No shift for this dimension
    } else {
      # Permute dimensions to bring the i-th dimension to the front
      perm <- c(i, setdiff(1:num_dims, i))
      shifted_arr <- aperm(shifted_arr, perm)

      if (shift[i] > 0) {
        shifted_arr[(shift[i]+1):dims[i],,] <- shifted_arr[1:(dims[i]-shift[i]),,]
        shifted_arr[1:shift[i],,] <- shifted_arr[1,,]
      } else {
        shifted_arr[1:(dims[i]+shift[i]),,] <- shifted_arr[(-shift[i]+1):dims[i],,]
        shifted_arr[(dims[i]+shift[i]+1):dims[i],,] <- shifted_arr[dims[i],,]
      }

      shifted_arr <- aperm(shifted_arr, order(perm))

    }
  }

  return(shifted_arr)
}

abs_diff_shifted_array <- function(arr, shift){

  shifted_arr <- shift_array(arr, shift)
  abs(arr - shifted_arr)

}

shift_array_inds <- function(arr, shift) {

  # Get dimensions and total number of elements
  dims <- dim(arr)
  num_elems <- prod(dims)

  # pad shift with 0 if not long enough
  shift <- c(shift, rep(0, length(dims) - length(shift)))

  # Calculate the linear indices of all elements in the array
  indices <- 1:num_elems

  # Convert linear indices to array indices
  array_indices <- arrayInd(indices, dims)

  # Shift the array indices according to 'shift', taking care to not exceed dimensions
  shifted_indices <- sweep(array_indices, 2, shift, "-")

  # Ensure shifted indices are within bounds
  shifted_indices <- apply(shifted_indices, 2, pmax, 1)
  shifted_indices <- sapply(1:ncol(shifted_indices), function(i) pmin(shifted_indices[,i], dims[i]))

  return(array(arr[shifted_indices], dims))

}

abs_diff_shifted_array_inds <- function(arr, shift){

  # Get dimensions and total number of elements
  dims <- dim(arr)
  num_elems <- prod(dims)

  # pad shift with 0 if not long enough
  shift <- c(shift, rep(0, length(dims) - length(shift)))

  # Calculate the linear indices of all elements in the array
  indices <- 1:num_elems

  # Convert linear indices to array indices
  array_indices <- arrayInd(indices, dims)

  # Shift the array indices according to 'shift', taking care to not exceed dimensions
  shifted_indices <- sweep(array_indices, 2, shift, "-")

  # Ensure shifted indices are within bounds
  shifted_indices <- apply(shifted_indices, 2, pmax, 1)
  shifted_indices <- sapply(1:ncol(shifted_indices), function(i) pmin(shifted_indices[,i], dims[i]))

  # take abs diff from original array
  return(array(abs(arr[shifted_indices] - arr[indices]), dims))

}

pixel_local_dist <- function(img, n_min_neighbors = 1){

  #find distances of pixel to all its neighbors
  pix_shifts <- expand.grid(-1:1, -1:1)
  pix_shifts <- pix_shifts[!apply(pix_shifts == 0, 1, all),]
  shifted_diffs <- apply(pix_shifts, 1, abs_diff_shifted_array, arr = img, simplify = F)

  #find sum of these values (square shifted_diffs for euclidean dist vs. manhattan dist)
  sum_diff <- abind::abind(lapply(1:length(shifted_diffs), function(i)
    apply(shifted_diffs[[i]], c(1,2), sum)
  ), along = 3)
  sum_diff_nmin <- apply(sum_diff, c(1,2), function(x) sum(kit::topn(x, n_min_neighbors, hasna = F, index = F)))
  return(list(sum_diff_nmin = sum_diff_nmin, sum_diff = sum_diff))
}


visualize_color_grid <- function(mat, prop = NA) {

  # Save original par settings
  originalPar <- par(no.readonly = TRUE)

  on.exit(par(originalPar)) # Ensure original settings are restored

  #reset transparent indices
  transcol_inds <- apply(mat, 1, function(x) any(x < 0))
  mat[transcol_inds,] <- 1

  # Set par settings for plotting
  par(mar = c(1, 1, 1, 1))

  # Find dimensions for the grid
  n <- nrow(mat)
  gridSide <- ceiling(sqrt(n))

  # Prepare positions for rectangles
  xPos <- rep(1:gridSide, length.out = n)
  yPos <- rep(gridSide:1, each = gridSide)[1:n]

  # Convert the first three columns to RGB colors
  colors <- rgb(mat[,1], mat[,2], mat[,3], maxColorValue = 1)

  # Prepare the plot window
  plot(1, type="n", xlim=c(0, gridSide+1), ylim=c(0, gridSide+1), axes=FALSE, xlab="", ylab="")

  # Draw the rectangles
  rect(xleft = xPos - 0.45, ybottom = yPos - 0.45,
       xright = xPos+ 0.45, ytop = yPos+ 0.45, col = colors, border = NA, xpd = NA)
  segments(x0 = xPos[transcol_inds] - 0.45, y0 = yPos[transcol_inds] - 0.45,
       x1 = xPos[transcol_inds]+ 0.45, y1 = yPos[transcol_inds]+ 0.45, col = "red")
  segments(x1 = xPos[transcol_inds] - 0.45, y0 = yPos[transcol_inds] - 0.45,
           x0 = xPos[transcol_inds]+ 0.45, y1 = yPos[transcol_inds]+ 0.45, col = "red")
  if(all(is.na(prop))){
    text(xPos - 0.4, yPos + 0.3, labels = 1:length(xPos), cex = 0.5, xpd = NA, pos = 4)
  } else {
    text(xPos - 0.5, yPos + 0.3, pos = 4,
         labels = paste0(1:length(xPos), ", ", round(prop*100), "%"), cex = 0.5, xpd = NA)
  }

}

#### preprocessing functions ####

transform_pixels <- function(img, col_res = 0.01, bad_pix = NA,
                             PCA_thresh = 0.95,
                             transparency_thresh = 0.9, use_HSL = F,
                             use_Lab = F, inverse_normal = F, 
                             dr_method = c("PCA", "UMAP", "tSNE", "none")[3]){
  
  eimg <- img
  
  #simplify colorspace
  # eimg <- round(eimg / col_res) * col_res
  
  #trim transparent pixels
  # all_trans <- apply(eimg[,,4] < transparency_thresh, c(1,2), all)
  # e_img <- img[!apply(all_trans, 1, all), !apply(all_trans, 2, all), ]
  e_img <- eimg
  
  #add in extra information to img, corresponding to spatial information
  img_sd <- apply(e_img, 3, sd)[1:3]
  
  #like the pixel locations
  # row_matrix <- matrix(1:dim(e_img)[1], nrow = dim(e_img)[1], ncol = dim(e_img)[2])
  # row_matrix <- (row_matrix - mean(c(row_matrix))) / sd(c(row_matrix)) * min(img_sd[1:3])
  # col_matrix <- t(matrix(1:dim(e_img)[2], nrow = dim(e_img)[2], ncol = dim(e_img)[1]))
  # col_matrix <- (col_matrix - mean(c(col_matrix))) / sd(c(col_matrix)) * min(img_sd[1:3])
  # e_img <- abind::abind(e_img, row_matrix, along = 3)
  # e_img <- abind::abind(e_img, col_matrix, along = 3)
  
  #or the pixel adjacent similarity values
  # pix_dists <- pixel_local_dist(e_img, 3)
  # pix_dists <- (pix_dists - mean(c(pix_dists))) / sd(c(pix_dists)) * min(img_sd[1:3])
  # pix_dists <- pix_dists - min(pix_dists); pix_dists <- pix_dists / max(pix_dists)
  # e_img <- abind::abind(e_img, pix_dists, along = 3)
  
  #retrieve and transform colors
  cols <- do.call(cbind, lapply(apply(e_img, 3, function(slice) t(slice), simplify = F), c))
  transcols <- cols
  transcol_inds <- cols[,4] < transparency_thresh
  
  if(all(is.na(bad_pix))){
    ciscols_orig <- ciscols <- cols[!transcol_inds,-4]
  } else {
    ciscols_orig <- ciscols <- cols[!(transcol_inds | c(bad_pix)),-4]
  }
  #retrieve actual color hex codes for later comparison
  colhex <- rgb(ciscols_orig[,1:3])
  
  if(use_Lab) {
    ciscols[,1:3] <- apply(convertColor(ciscols[,1:3], "sRGB", "Lab"), 2, scale)
  }
  
  if(use_HSL){
    ciscols[,1:3] <- t(rgba2hsla(cbind(ciscols[,1:3], 1)))[,-4]
  }
  
  if(inverse_normal){
    ciscols <- apply(ciscols, 2, function(x) qnorm(rank(x) / (length(x)+2)))
  }
  
  n_sub <- 2E3
  sub_sample_inds <- sample(x = 1:nrow(ciscols), size = min(n_sub, nrow(ciscols)), replace = T)
  colhex <- colhex[sub_sample_inds]
  clustering_input <- NULL
  
  #reduce dimensionality w/ PCA
  if("PCA" %in% dr_method){
    evd <- eigen(cov(ciscols))
    nPCs <- min(sum(cumsum(evd$values) / sum(evd$values) < PCA_thresh) + 1, 3)
    PCscores <- ciscols[sub_sample_inds,] %*% evd$vectors[,1:nPCs, drop = F]
    PCscores <- apply(PCscores, 2, scale)
    PCscores <- round(PCscores / col_res) * col_res
    
    # mPC <- apply(PCscores, 2, mean)
    # vPC <- apply(PCscores, 2, var)
    # PCscores <- t(t(PCscores) - mPC)
    # PCscores <- (PCscores) %*% diag(1/sqrt(vPC))
    
    # plot(PCscores[,1:2], col = adjustcolor(colhex, 0.2), pch = 19)
    clustering_input <- cbind(clustering_input, PCscores)
  }
  
  
  #or with tsne / umap -- resample so that weights reflect weight_pow?
  if("tSNE" %in% dr_method){
    
    ciscols_sub <- ciscols[sub_sample_inds,]
    ciscols_sub <- ciscols_sub_orig <- round(ciscols_sub / col_res) * col_res
    
    ciscols_sub_dt <- data.table::as.data.table(ciscols_sub)
    ciscols_sub_dt <- ciscols_sub_dt[, .N, by = names(ciscols_sub_dt)]
    ciscols_sub <- as.matrix(ciscols_sub_dt[, 1:ncol(ciscols_sub)])
    
    #displace slightly for use with tsne (requires unique entries)
    # ciscols_eps <- abs(rnorm(nrow(ciscols_sub) * ncol(ciscols_sub), mean = 0, sd = min(img_sd)/1E3))
    # ciscols_eps <- matrix(ciscols_eps, ncol = ncol(ciscols_sub))
    # ciscols_sub <- ciscols_sub - ciscols_eps * sign(ciscols_sub - 0.5)
    # ciscols_sub <- unique(ciscols_sub)
    
    tsne_out <- M3C::tsne(t(unique(ciscols_sub)))$data
    tsne_out <- tsne_out[match(apply(ciscols_sub_orig, 1, paste0, collapse = "~"), 
                               apply(ciscols_sub, 1, paste0, collapse = "~")),]
    # plot(tsne_out, col = adjustcolor(colhex, 0.2), pch = 19)
    tsne_out <- apply(tsne_out, 2, scale)
    tsne_out <- round(tsne_out / col_res) * col_res
    clustering_input <- cbind(clustering_input, as.matrix(tsne_out))
  }
  
  
  #or with umap?
  if("UMAP" %in% dr_method){
    ciscols_sub <- ciscols[sub_sample_inds,]
    ciscols_sub <- ciscols_sub_orig <- round(ciscols_sub / col_res) * col_res
    ciscols_sub <- unique(ciscols_sub)
    umap_out <- umap::umap(ciscols_sub)$layout
    umap_out <- umap_out[match(apply(ciscols_sub_orig, 1, paste0, collapse = "~"), 
                               apply(ciscols_sub, 1, paste0, collapse = "~")),]
    umap_out <- apply(umap_out, 2, scale)
    umap_out <- round(umap_out / col_res) * col_res
    # plot(umap_out, col = adjustcolor(rgb(ciscols_sub), 0.2), pch = 19)
    clustering_input <- cbind(clustering_input, umap_out)
    
  }
  
  if("none" %in% dr_method){
    ciscols_sub <- ciscols[sub_sample_inds,1:3]
    ciscols_sub <- round(ciscols_sub / col_res) * col_res
    clustering_input <- cbind(clustering_input, ciscols_sub)
  }
  
  #find kde of these data to subsample points with given weight after thresholding
  if(ncol(clustering_input) > 3){
    clustering_input <- prcomp(clustering_input)$x[,1:3]
    clustering_input <- apply(clustering_input, 2, scale)
    clustering_input <- round(clustering_input / col_res) * col_res
  }
  
  return(list(clustering_input = clustering_input, cols = cols,
              colhex = colhex, transcol_inds = transcol_inds,
              ciscols_orig = ciscols_orig[,1:3]))
}

subsample_pixels <- function(clustering_input, colhex, kde_thresh = 0.95, grid_n = 50, use_kd_for_cluster = F,
                             n_top = 100, weight_pow = 0.5, col_res = 0.02){
  
  #subsampling the estimated grid -- first perform kde
  kdest <- ks::kde(clustering_input, gridsize = grid_n)
  kdest$estimate <- kdest$estimate - min(kdest$estimate) + min(kdest$estimate[kdest$estimate > 0])
  kde_inds <- as.matrix(expand.grid(replicate(ncol(clustering_input), 1:grid_n, simplify = F)))
  kde_dens <- kdest$estimate[kde_inds]
  
  #now truncate at a particular density to capture kde_thresh of the mass
  skde_dens <- sort(kde_dens, decreasing = T)
  dens_thresh <- skde_dens[sum(cumsum(skde_dens) / sum(kde_dens) > kde_thresh)]
  nz_kde_inds <- which(kde_dens > dens_thresh) #non-zero, not New Zealand
  subsamp_probs <- kdest$estimate[nz_kde_inds]
  
  #transform these data to better reflect rarer colors
  subsamp_probs <- subsamp_probs^weight_pow
  
  #compute points to subsample
  subsamp_inds <- sample(nz_kde_inds, size = 4E3, replace = T, prob = subsamp_probs)
  grid_subsamp <- kde_inds[subsamp_inds,]
  subsamp <- do.call(cbind, lapply(1:ncol(clustering_input), function(i) {
    kdest$eval.points[[i]][grid_subsamp[,i]]
  }))
  subsamp <- apply(subsamp, 2, scale)
  subsamp <- round(subsamp / col_res) * col_res
  
  #retrieve color values for sampled points in this grid?
  unique_subsamp <- unique(subsamp)
  unique_clustering_input <- unique(data.frame(clustering_input, colhex))
  unique_clustering_input_colkey <- setNames(object = unique_clustering_input[,ncol(unique_clustering_input)], 
                                             nm = apply(unique_clustering_input[,-ncol(unique_clustering_input)], 1, paste0, collapse = "~"))
  sqdists <- sqdists_to_centers(mat = unique_subsamp, cents = as.matrix(unique_clustering_input[,-ncol(unique_clustering_input)]))
  closest_input <- apply(sqdists, 1, which.min)
  unique_subsamp_cols_key <- setNames(object = unique_clustering_input_colkey[closest_input], 
                                      nm = apply(unique_subsamp, 1, paste0, collapse = "~"))
  subsamp_cols <- unique_subsamp_cols_key[apply(subsamp, 1, paste0, collapse = "~")]
  
  # #compute kde for these data to subsample
  # kdest <- ks::kde(clustering_input, eval.points = clustering_input)
  # kdest$estimate <- kdest$estimate - min(kdest$estimate) + min(kdest$estimate[kdest$estimate > 0])
  # kde_dens <- kdest$estimate
  # 
  # #find probs of points and transform to better represent rarer colors
  # subsamp_probs <- kde_dens^weight_pow #hmm this is quite wrong -- should be uniform to actually sample propto prob
  # 
  # #compute points to subsample
  # subsamp_inds <- sample(1:length(subsamp_probs), size = n_sub, replace = T, prob = subsamp_probs)
  # subsamp <- clustering_input[subsamp_inds,]
  # subsamp_names <- apply(subsamp, 1, paste0, collapse = "_")
  
  #add in the densities as a further variable for clustering to improve separability
  if(use_kd_for_cluster){
    kde_dens_input <- kde_dens[subsamp_inds]^weight_pow
    kde_dens_input[kde_dens_input < median(kde_dens_input)] <- min(kde_dens_input)
    kde_dens_input <- scale(kde_dens_input)
    subsamp <- cbind(subsamp, kde_dens_input)
  }
  
  #do initial kmeans clustering on this to find general locations for weighted kmeans
  n_init_clust <- min(n_top, nrow(unique(subsamp)))
  km_init <- round(kmeans(subsamp, centers = n_init_clust)$centers, 3)
  
  #find weights of these vs the original pixels
  km_init_weights <- table(apply(sqdists_to_centers(subsamp, cents = km_init), 1, which.min))

  plot(subsamp, pch = 19, col = adjustcolor(subsamp_cols, 0.2))
  points(subsamp, pch = 1, col = adjustcolor(1, 0.1))
  
  if(use_kd_for_cluster){
    # points(km_init, pch = 19, col = sapply((exp(km_init[,3]) / exp(max(km_init[,3])))^(1/4),
    #                                        function(wi) adjustcolor(2, wi)))
  } else {
    # points(km_init, pch = 19, col = adjustcolor(2, 0.8))
  }
 
  return(list(km_init = km_init, km_init_weights = km_init_weights, subsamp = subsamp, subsamp_cols = subsamp_cols))
   
}

multi_wkmeans <- function(ks, km_init, km_init_weights, subsamp, offset_k=0, min_dist_q=NA){
  
  #check on whether k range is valud
  valid_ks <- ks[ks <= length(km_init_weights)]
  if(length(valid_ks) == 0){
    return(km_init)
  }
  
  clust_out <- lapply(ks, function(k){
    
    optimal_cluster <- wkmeans(x = km_init,
                               w = km_init_weights,
                               k, n = 12 * k, min_dist_q=min_dist_q)
    
    
    #reproduce original data
    cents <- optimal_cluster$cents
    sqdists <- sqdists_to_centers(subsamp, cents)
    cls <- apply(sqdists, 1, which.min)
    
    #remove null clusters if any were picked
    nc <- setdiff(1:k, as.integer(names(table(cls))))
    if(length(nc) != 0){
      k <- k - length(nc)
      optimal_cluster$k <- k
      cents <- optimal_cluster$cents <- optimal_cluster$cents[-nc,]
      optimal_cluster$cls <- optimal_cluster$cls -
        apply(do.call(rbind, lapply(nc, function(nci) optimal_cluster$cls > nci)), 2, sum)
      optimal_cluster$obs_counts
      sqdists <- sqdists_to_centers(subsamp, optimal_cluster$cents)
      cls <- apply(sqdists, 1, which.min)
    }
    
    #remove empty clusters so index.DB doesn't complain and compute dbi
    pres_cl <- sort(unique(cls))
    dbi_exploded_cl <- (1:length(pres_cl))[match(cls, pres_cl)]
    davies_bouldin_index <- clusterSim::index.DB(subsamp, dbi_exploded_cl)$DB
    
    #return values
    return(list(cluster_inds = cls, centers = cents, dbi = davies_bouldin_index, k = k))
    
  })
  
  #decide on k
  dbi <- sapply(clust_out, function(x) x$dbi)
  if(all(is.na(dbi))){
    k_picked <- which(ks == (n_top - 1))
  } else {
    k_picked <- which.min(dbi) + offset_k
    k_picked <- min(k_picked, length(ks))
  }
  
  return(list(clust_picked = clust_out[[k_picked]], clust_all = clust_out[-k_picked]))
  
}

discretize_image <- function(img, col_res = 0.01, kde_thresh = 0.95,
                             PCA_thresh = 0.95, ncol_range = 3:10, grid_n = 50, use_kd_for_cluster = F,
                             n_top = 100, offset_k = 0, transparency_thresh = 0.9, use_HSL = F,
                             use_Lab = F, inverse_normal = F, weight_pow = 0.5, qdist_eps = 1E-3,
                             dr_method = c("PCA", "UMAP", "tSNE", "none")[3], dist_w = 0.2, min_dist_q=NA){
  
  
  #remove problematic pixels (ie compression artefacts) before clustering
  print("reducing image noise")
  pix_dists_info <- pixel_local_dist(img, 1)
  pix_dists <- pix_dists_info$sum_diff_nmin
  # pheatmap::pheatmap(pix_dists, cluster_rows = F, cluster_cols = F)
  dist_qs <- quantile(pix_dists, 0:100/100)
  bad_qs <- as.numeric(which(dist_qs > qdist_eps)) - 1
  bad_qs <- bad_qs[bad_qs > 50]
  dist_thresh <- dist_qs[min(min(bad_qs) + 1, 100)]
  bad_pix <- pix_dists >= dist_thresh
  
  #hmm, or just set them to their neighboring value that compromises between 
  #being closest to themselves and least variable
  eimg <- img
  bad_pix_inds <- which(bad_pix, arr.ind = T)
  pix_shifts <- expand.grid(-1:1, -1:1)
  pix_shifts <- pix_shifts[!apply(pix_shifts == 0, 1, all),]
  t(bad_pix_inds) + pix_shifts[1,]
  bad_pix_neighbor_inds <- apply(pix_shifts, 1, function(shift) 
    t(t(bad_pix_inds) + unlist(shift)), 
                             simplify = F)
  bad_pix_neighbor_inds <- lapply(bad_pix_neighbor_inds, function(x){
    invalid_inds <- apply(x < 0, 1, any) | x[,1] > nrow(pix_dists) | x[,2] > ncol(pix_dists)
    x[invalid_inds,] <- NA
    x
  })
  bad_pix_neighbor_dists <- do.call(cbind, lapply(bad_pix_neighbor_inds, function(inds) pix_dists[inds]))
  neighbor_pix_dists <- do.call(cbind, lapply(1:nrow(pix_shifts), function(i){
    pix_dists_info$sum_diff[,,i][bad_pix_inds]
  }))
  substitution_score <- exp(log(neighbor_pix_dists) * dist_w + log(bad_pix_neighbor_dists) * (1-dist_w))
  
  best_neighbors <- apply(substitution_score, 1, which.min)
  flattened_bad_pix_neighbor_inds <- do.call(rbind, bad_pix_neighbor_inds)
  best_neighbor_inds <- flattened_bad_pix_neighbor_inds[1:length(best_neighbors) + (best_neighbors-1) * length(best_neighbors),]
  for(i in 1:dim(eimg)[3]){
    eimg[cbind(bad_pix_inds, i)] <- eimg[cbind(best_neighbor_inds, i)]  
  }
  
  #transform pixels according to specified methods
  print("transforming pixels to latent space")
  transformed_pixels <- transform_pixels(eimg, col_res = col_res,
                                         PCA_thresh = PCA_thresh,
                                         transparency_thresh = transparency_thresh, use_HSL = use_HSL,
                                         use_Lab = use_Lab, inverse_normal = inverse_normal, 
                                         dr_method = dr_method, bad_pix = NA)
  
  clustering_input <- transformed_pixels$clustering_input
  colhex <- transformed_pixels$colhex
  ciscols_orig <- transformed_pixels$ciscols_orig
  cols <- transformed_pixels$cols
  transcol_inds <- transformed_pixels$transcol_inds
  
  #subsample pixels to minimize observations to feed into weighted k-means
  print("subsampling pixels")
  subsampled_pixels <- subsample_pixels(clustering_input, colhex, kde_thresh = kde_thresh, grid_n = grid_n, use_kd_for_cluster = use_kd_for_cluster,
                                        n_top = n_top, weight_pow = weight_pow, col_res = col_res)
  km_init <- subsampled_pixels$km_init
  km_init_weights <- subsampled_pixels$km_init_weights
  subsamp <- subsampled_pixels$subsamp
  subsamp_cols <- subsampled_pixels$subsamp_cols
  
  #perform weighted clustering slowly (may need to rewrite own fast version)
  print("performing clustering")
  clust_out <- multi_wkmeans(ks = ncol_range, km_init = km_init, offset_k = 0,
                             km_init_weights = km_init_weights, subsamp = subsamp, min_dist_q=min_dist_q)
  
  #retrieve clustering output
  cents <- clust_out$clust_picked$centers
  points(cents, pch = 19, col = 3, cex = 2)
  points(cents, pch = 1, col = 1, cex = 2)

  #add in extra clusters for poorly matched pixels?
  # sqdists <- sqdists_to_centers(subsamp, cents)
  # min_sqdists <- apply(sqdists, 1, min)
  # poor_match_inds <- which(min_sqdists > quantile(min_sqdists, 0.95))
  # poor_match_vals <- subsamp[poor_match_inds,]
  # points(poor_match_vals, pch = 19, col = adjustcolor(2, 0.1))

  #assign pixels to centers
  sqdists <- sqdists_to_centers(subsamp, cents)
  cluster_inds <- apply(sqdists, 1, which.min)
  cluster_dists <- apply(sqdists, 1, min)

  #find mean of corresponding pixels to recompute centers?
  subsamp_rgb <- t(col2rgb(subsamp_cols))/255
  centers <- do.call(rbind, lapply(split(1:nrow(subsamp_rgb), cluster_inds), function(col_inds)
    apply(subsamp_rgb[col_inds,,drop = FALSE], 2, mean)))
  
  #or mean of nearest eg 10 pixels?
  nearest_n <- 50
  centers <- do.call(rbind, lapply(split(data.frame(inds = 1:nrow(subsamp_rgb), dists = cluster_dists), cluster_inds), 
                                   function(clusdat){
    cluscols <- subsamp_rgb[clusdat$inds,,drop = FALSE]
    apply(cluscols[order(clusdat$dists, decreasing = F)[1:min(nrow(cluscols), nearest_n)],
                   ,drop = FALSE], 2, mean)
  }))
    
  
  
  mean_cols <- apply(cbind(centers, 1), 1, function(x) rgb(x[1], x[2], x[3], x[4]))
  visualize_color_grid(centers, prop = table(cluster_inds) / length(cluster_inds))
  
  #compute pixel-to-cluster assignments
  sqdists <- sqdists_to_centers(ciscols_orig[,1:3], centers)
  cluster_inds <- apply(sqdists, 1, which.min)

  #interleave these colored pixels with transparent ones -- assign transparent color its own cluster
  all_cluster_inds <- rep(length(mean_cols) + 1, nrow(cols))
  all_cluster_inds[!transcol_inds] <- cluster_inds
  all_mean_cols <- c(mean_cols, rgb(0,0,0,0))
  all_centers <- rbind(cbind(centers, 1), c(0,0,0,0))

  return(list(cluster_inds = all_cluster_inds, mean_cols = all_mean_cols,
              centers = all_centers))

}

denoise_img <- function(img){}

preprocess_image <- function(img, ncol_range, use_HSL = F, use_Lab = T, dr_method = "tSNE",
                             n_passes = 2, col_res = 0.01, kde_thresh = 0.95, dist_w = 0.2,
                             PCA_thresh = 0.95, n_top = 100, offset_k = 0, use_kd_for_cluster = F,
                             transparent_color = "white", transparency_thresh = 0.2, weight_pow = 0.5, min_dist_q=NA){
  
  eimg <- img
  
  #add transparent layer if none exists
  if(dim(eimg)[3] == 3){
    eimg <- add_transparent_layer(eimg, transparent_color)
  }
  eimg[,,4] <- eimg[,,4]^(0.5)
  
  # map img to nearest block colors
  img_data <- discretize_image(eimg, ncol_range = ncol_range, offset_k = offset_k, col_res = col_res,
                               kde_thresh = kde_thresh, PCA_thresh = PCA_thresh, n_top = n_top,
                               transparency_thresh = transparency_thresh, use_HSL = use_HSL,
                               use_kd_for_cluster = use_kd_for_cluster, use_Lab = use_Lab, 
                               weight_pow = weight_pow, dr_method = dr_method, min_dist_q=min_dist_q, dist_w=dist_w)
  eimg <- abind::abind(lapply(1:4, function(i)
    matrix(img_data$centers[img_data$cluster_inds,i], nrow(eimg), ncol(eimg), byrow = T)), along = 3)
  
  #get rid of noisy pixels
  if(n_passes > 0){
    clmat <- simplify_image(img_data$cluster_inds, eimg, n_passes = n_passes)
    cluster_inds <- c(t(clmat))
    clusters_left <- sort(unique(cluster_inds))
    
    #adjust in case we lost a color
    mean_cols <- img_data$mean_cols[clusters_left]
    centers <- img_data$centers[clusters_left,]
    ncols <- length(clusters_left)
    for(i in 1:ncols){
      cluster_inds[cluster_inds == clusters_left[i]] <- i
      clmat[which(clmat == clusters_left[i])] <- i
    }
    
    eimg <- abind::abind(lapply(1:4, function(i)
      matrix(centers[cluster_inds,i], nrow(eimg), ncol(eimg), byrow = T)), along = 3)
    
  }
  
  return(eimg)
  
}

clump_colors <- function(img, dthresh = 0.2){
  di <- dim(img)
  start_pix <- as.matrix(expand.grid(1:3, 1:3) + 1)
  disp_mat <- t(as.matrix(expand.grid(-1:1, -1:1)[-5,]))
  for(i in 1:nrow(start_pix)){
    row_inds <- seq(start_pix[i,1], di[1]-1, by = 3)
    col_inds <- seq(start_pix[i,2], di[2]-1, by = 3)
    pix_cents <- as.matrix(expand.grid(row_inds, col_inds))
    #find adjacent values and match colors
    to_assign <- lapply(1:nrow(pix_cents), function(j){
      comp_pix_inds <- pix_cents[j,] + disp_mat
      center_val <- img[pix_cents[j,1], pix_cents[j,2],]
      comp_pix_vals <- do.call(rbind, lapply(1:di[3], function(k) img[,,k][comp_pix_inds]))
      distances <- sqrt(apply((comp_pix_vals - center_val)^2, 2, sum))
      close_vals <- distances < dthresh

      new_val <- apply(cbind(center_val, comp_pix_vals[,close_vals]), 1, mean)
      new_val_inds <- cbind(pix_cents[j,], comp_pix_inds)
      return(list(new_val = new_val, new_val_inds = new_val_inds))
    })
    #hmm, this is way too slow
  }

}


simplify_image <- function(cluster_inds, img, min_vl = 3, n_passes = 2){
  clmat <- matrix(cluster_inds, nrow(img), ncol(img), byrow = T)
  for(pass in 1:(2*n_passes)){
    clmat <- apply(clmat, 1, function(x){
      rlex <- rle(x)
      ok <- which(rlex$lengths >= min_vl)
      not_ok <- setdiff(1:length(rlex$lengths), ok)
      if(length(not_ok) == 0){
        return(x)
      } else {
        where_to_add <- sapply(not_ok, function(i) min(sum(i > ok) + 1, length(ok)))
        ok_vals <- rlex$values[ok]
        ok_lengths <- rlex$lengths[ok]
        not_ok_lengths <- rlex$lengths[not_ok]
        uniq_w2a <- sort(unique(where_to_add))
        n2a <- sapply(uniq_w2a, function(i) sum(not_ok_lengths[where_to_add == i]))
        ok_lengths[uniq_w2a] <- ok_lengths[uniq_w2a] + n2a
        rep(ok_vals, times = ok_lengths)
      }
    })
  }
  return(clmat)
}

get_char_propspace <- function(uc, font, family){
  png(paste0("~/tmp.png"), width = 1000, height = 250, units = "px")
  par(mar = c(0,0,0,0))
  plot(NA, NA, xlim = c(0,4), ylim = c(0,1), frame.plot = F, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  rect(xleft = -5, ybottom = -5, ytop = 5, xright = 5, border = 1, col = 1)
  uccex <- 1 / strheight(uc, units = "user", font = font, family = family) * 0.5
  text(x = 0, y = 0.5, pos = 4, cex = uccex, col = rgb(0,0,1,1), labels = uc)
  boxw <- strwidth(uc, units = "user", cex = uccex, font = font, family = family)
  boxh <- strheight(uc, units = "user", cex = uccex, font = font, family = family)
  rect(xleft = 2, ybottom = 0, ytop = 0 + boxh, xright = 2 + boxw, border = rgb(1,0,0,1), col = rgb(1,0,0,1))
  dev.off()
  ucmg <- png::readPNG("~/tmp.png")
  file.remove("~/tmp.png")
  sum(ucmg[,,3] > 0.9) / sum(ucmg[,,1] > 0.9)
}

fill_hits_matrix <- function(img){

  #get hits matrix
  hits <- img[,,1] + img[,,2] + img[,,3]
  hits_i <- which((abs(hits - 3) > 1E-9) & img[,,4] > 0.2)
  hits[hits_i] <- 1
  hits[-hits_i] <- 0

  #do harder fill
  pxlw <- 1 / ncol(hits)
  pxlh <- 1 / nrow(hits)
  indhits <- which(hits == 1, arr.ind = T)
  indhits <- indhits[order(indhits[,1]),]
  indlocs <- data.frame(x = indhits[,2] / ncol(hits), y = 1 - indhits[,1] / nrow(hits))

  indlocs$tsw <- NA
  indlocs$bi <- NA
  rlepixy <- rle(indlocs$y)
  rlepixy_locs <- cumsum(c(1,rlepixy$lengths))

  for(i in (1:length(rlepixy$values))){
    curr_inds <- ifelse(i==1,1,rlepixy_locs[i]):(rlepixy_locs[i+1]-1)

    v = indlocs[curr_inds,]$x
    vb <- rep(NA, length(v))
    vbi <- rep(NA, length(v))
    for(vi in 1:length(v)){
      if(vi == 1){
        cvl <- 1
        bi <- 1
      } else {
        dv <- (v[vi] - v[vi-1] - pxlw) < 1E-6
        if(dv){
          cvl <- cvl + 1
          if(vi == length(v)){
            vb[(vi):(vi-cvl+1)] <- cvl
            vbi[(vi):(vi-cvl+1)] <- bi
          }
        } else {
          vb[(vi-1):(vi-cvl)] <- cvl
          vbi[(vi-1):(vi-cvl)] <- bi
          cvl <- 1
          bi <- bi + 1
          if(vi == length(v)){
            vb[vi] <- cvl
            vbi[vi] <- bi
          }
        }
      }
    }

    indlocs$tsw[curr_inds] <- vb * pxlw
    indlocs$bi[curr_inds] <- vbi

  }


  #fill in remaining parameters of indlocs
  twoy <- rlepixy$lengths * pxlw #total width on each row 'y'
  indlocs$tw <- rep(twoy, rlepixy$lengths)
  indlocs$li <- rep(seq_along(twoy), rlepixy$lengths)
  uniq_indlocsy <- unique(indlocs$y)

  #get colors for indlocs
  pixcols <- t(sapply(1:nrow(indhits), function(i) img[indhits[i,1], indhits[i,2],]))
  pixcols <- sapply(1:nrow(pixcols), function(i) rgb(pixcols[i,1], pixcols[i,2], pixcols[i,3], pixcols[i,4]))
  indlocs$col <- pixcols

  return(list(hits = hits, indlocs = indlocs, uniq_indlocsy = uniq_indlocsy, pxlw = pxlw))
}
