#' @param center pyramids of the centers
#' @param surround pyramids of the surrounds. If \code{NULL}, center-surround differences in one modality are outputed.
#' @param c_scale which pyramid etages of the centers to take
#' @param d_scale which pyramid etages of the surrounds to take (s = c + d)

feature_maps <- function(center, surround=NULL, c_scale = c(2, 3, 4), d_scale = c(3, 4)){
  if(is.null(surround)) surround <- center

  maps <- list()

  for(c in c_scale){
    for(d in d_scale){
      maps[[paste(c, c+d, sep = "_")]] <- center_surround(center[[as.character(c)]], surround[[as.character(c+d)]])
    }
  }

  return(maps)
}
