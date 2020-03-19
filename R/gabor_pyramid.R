gabor_pyramid <- function(image, octaves=8, orientations=4){
  out <- list()

  gabor <- OpenImageR::GaborFeatureExtract$new()

  gb_im <- list()

  for(i in 1:octaves) {
    out[[i]] <- gabor$gabor_feature_extraction(image = image, scales = 1, orientations = orientations,
                                               gabor_rows = 39, gabor_columns = 39,
                                               downsample_gabor = TRUE, downsample_rows = 2^i, downsample_cols = 2^i,
                                               vectorize_magnitude = FALSE)
  }

  return(out)
}
