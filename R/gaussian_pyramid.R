#' Gaussian pyramid
#'
#' @description Function that iteratively downscales an image with gaussian blur before downscaling
#'
#' @param image
#' @param octaves
#' @param gaussian_blur
#' @param gauss_sigma
#' @param gauss_range
#' @export
gaussian_pyramid <- function(image, octaves=8, gaussian_blur = TRUE, gauss_sigma = 1, gauss_range = 3){
  out <- list()

  out[[1]] <- image

  for(i in 2:(octaves+1)) out[[i]] <- OpenImageR::down_sample_image(out[[i-1]], 2, gaussian_blur, gauss_sigma, gauss_range)

  names(out) <- sprintf("%s", 0:octaves)
  return(out)
}
