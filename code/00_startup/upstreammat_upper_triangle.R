upstreammat_upper_triangle <- function (seg, 
                                        vert, 
                                        rivers, 
                                        logical = NULL, 
                                        ID = NULL, 
                                        flowconnected = FALSE, 
                                        net = FALSE, 
                                        stopiferror = TRUE, 
                                        algorithm = NULL) 
{
  if (!inherits(rivers, "rivernetwork")) 
    stop("Argument 'rivers' must be of class 'rivernetwork'.  See help(line2network) for more information.")
  if (is.null(logical)) 
    logical <- rep(T, length(unique))
  len <- length(vert)
  seg <- seg[logical]
  vert <- vert[logical]
  if (is.null(ID)) 
    ID <- 1:len
  ID <- ID[logical]
  
  # sample_vec <- c(-2,NA,2,1,-2,NA)
  # n_rows <- 4
  # n_cols <- 4
  # vert <- 1:4
  
  dists <- matrix(NA, nrow = length(vert), ncol = length(vert))
  
  # get the distances for the upper triangle, and obviously the seg and edge to itself will have a distance of zero
  for (i in 1:nrow(dists)) {
    for (j in 1:ncol(dists)) {
      
      if (j>i){ # if the dimension is under the diagonal, take the opposite
        
        #dists[i,j] <- sample(sample_vec, size = 1)
        
        dists[i, j] <- upstream(startseg = seg[i], endseg = seg[j],
                                startvert = vert[i], endvert = vert[j], rivers = rivers,
                                net = net, flowconnected = flowconnected, stopiferror = stopiferror,
                                algorithm = algorithm)

        
      } else if (j==i){
        dists[i,j] = 0
      }

    }
  }

  # replace the lower triangle with the negative of the upper triangular: the negative transpose
  
  dists[lower.tri(dists)] <- -t(dists)[lower.tri(dists)]
  
  dimnames(dists)[[1]] <- dimnames(dists)[[2]] <- ID
  return(dists)
}
