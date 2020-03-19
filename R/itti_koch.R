#' Itti and Koch saliency map
#'
#' @description Calculates saliency map of an image
#'
#' @param image Object of class \code{cimg} or a path to an image.
#'
itti_koch <- function(image){
  if(is.character(image)) image <- try(imager::load.image(image))
  if(is(image, "try-error")) stop("could not import image; make sure the file is of recognized format and the path is correct.")
  if(!imager::is.cimg(image)) stop("image must by class cimg")

  channels <- imager::channels(image)
  channels <- lapply(channels, as.matrix)
  names(channels) <- c("R", "G", "B")

  intensity <- Reduce("+", channels) / 3
  which_set_zero <- which(intensity < 0.1 * max(intensity), arr.ind = TRUE)

  channels  <- lapply(channels, divide_by, intensity)
  channels  <- lapply(channels, function(ch) {ch[which_set_zero] <- 0; return(ch)})

  intensity <- gaussian_pyramid(intensity, octaves = 8, gaussian_blur = TRUE, gauss_sigma = 1, gauss_range = 3)

  broad_channels <- list()

  broad_channels$R <- channels$R - (channels$G + channels$B) / 2
  broad_channels$G <- channels$G - (channels$R + channels$B) / 2
  broad_channels$B <- channels$B - (channels$R + channels$G) / 2
  broad_channels$Y <- (channels$R + channels$G) / 2 - abs(channels$R - channels$G) / 2 - channels$B
  broad_channels$Y[broad_channels$Y < 0] <- 0

  broad_channels <- lapply(broad_channels, gaussian_pyramid, octaves = 8, gaussian_blur = TRUE, gauss_sigma = 1, gauss_range = 3)


  intensity_contrast <- feature_maps(intensity)

  red_green_oponency   <- feature_maps(diff_pyramids(broad_channels$R, broad_channels$G),
                                       diff_pyramids(broad_channels$G, broad_channels$R))

  blue_yellow_oponency <- feature_maps(diff_pyramids(broad_channels$B, broad_channels$Y),
                                       diff_pyramids(broad_channels$Y, broad_channels$B))



}

diff_pyramids <- function(pyr1, pyr2){
  ll <- length(pyr1)
  if(ll != length(pyr2)) stop("pyramids must have the same number of octaves")

  out <- list()

  for(i in seq_len(ll)) out[[i]] <- pyr1[[i]] - pyr2[[i]]

  names(out) <- as.character(seq_len(ll) - 1)

  return(out)
}
