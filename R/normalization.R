normalization <- function(feature_map){
  feature_map <- feature_map / max(feature_map)

  nr <- nrow(feature_map)
  nc <- ncol(feature_map)

  local_max <- matrix(FALSE, nr, nc)
  for(i in 1:nr){
    for(j in 1:nc){
      if(i == 1) mr <- 0:1 else if(i == nr) mr <- -1:0 else mr <- -1:1
      if(j == 1) mc <- 0:1 else if(j == nc) mc <- -1:0 else mc <- -1:1

      local_max[i, j] <- all (feature_map[i, j] >= feature_map[i+mr, j+mc])
    }
  }

  local_max <- feature_map[local_max]
  local_max <- local_max[local_max != 1] # remove absolute maximum
  if(length(local_max) > 0) {
    mean_local_max <- mean(local_max)
  } else{
    mean_local_max <- 0
    warning("local maxima not present")
  }

  feature_map <- feature_map * (1-mean_local_max)^2
  return(feature_map)
}
