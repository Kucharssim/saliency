#' Center surround differences
#'
#' @description This function enables computing center-surround differences as defined by Itii & Koch (1998).
#'
#' @param center Center image
#' @param surround Surround image
#' @param abs Logical (default \code{TRUE}). Should absolute values of the differences be reported?
center_surround <- function(center, surround, abs = TRUE){
  if(!(is.matrix(center) && is.matrix(surround))) stop("center and surround have to be matrices!")

  dim_c  <- dim(center)
  dim_s  <- dim(surround)

  if(!all(dim_c > dim_s)) stop("center has to have higher resolution than surround!")

  rows_c <- seq_len(dim_c[1])
  cols_c <- seq_len(dim_c[2])

  rows_s <- seq_len(dim_s[1])
  cols_s <- seq_len(dim_s[2])

  rows_norm_c <- rows_c / dim_c[1]
  cols_norm_c <- cols_c / dim_c[2]

  rows_norm_s <- rows_s / dim_s[1]
  cols_norm_s <- cols_s / dim_s[2]

  size_rows_s <- 1/dim_s[1]
  size_cols_s <- 1/dim_s[2]
  out <- matrix(NA, nrow = dim_c[1], ncol = dim_c[2])

  for(row_s in rows_s){
    which_rows <- rows_norm_c <= rows_norm_s[row_s] & rows_norm_c >= rows_norm_s[row_s] - size_rows_s
    for(col_s in cols_s){
      which_cols <- cols_norm_c <= cols_norm_s[col_s] & cols_norm_c >= cols_norm_s[col_s] - size_cols_s

      out[which_rows, which_cols] <- center[which_rows, which_cols] - surround[row_s, col_s]
    }
  }

  if(abs) out <- abs(out)
  return(out)
}

