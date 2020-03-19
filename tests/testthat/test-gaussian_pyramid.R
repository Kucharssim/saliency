test_that("gaussian pyramid works", {
  set.seed(1)
  im <- matrix(runif(32*16), ncol = 16, nrow = 32)

  pyr <- gaussian_pyramid(im, 3)

  expect_equal(pyr[['3']],
               structure(c(0.258451989593978, 0.597415839648714, 0.69093373713832,
                           0.273743254503014, 0.254818416840121, 0.666800066839204, 0.379175437304562,
                           0.0855277463646713), .Dim = c(4L, 2L)))
})
