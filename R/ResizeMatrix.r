#' ResizeMatrix
#'
#' Resize a matrix in new dimension
#' @param mat.mtx <matrix>: a Matrix to resize
#' @param rowDim.num <integer>: the number of rows in reiszed matrix
#' @param colDim.num <integer>: the number of columns in reiszed matrix
#' @param rowKeep.ndx <integer>: index of one row to keep in resized matrix
#' @param colKeep.ndx <integer>: index of one column to keep in resized matrix
#' @return resized matrix
#' @examples
#' mat.mtx <- matrix(0,11,11)
#' mat.mtx[which(as.logical(1:(11*11)%%2))] <- 1:ceiling((11*11)/2)
#' mat.mtx[2,] <- 100
#' mat.mtx[,7] <- 200
#' mat.mtx
#' ResizeMatrix(mat.mtx=mat.mtx, rowDim.num=7, colDim.num=7, rowKeep.ndx=NULL, colKeep.ndx=NULL)
#' ResizeMatrix(mat.mtx=mat.mtx, rowDim.num=7, colDim.num=7, rowKeep.ndx=2, colKeep.ndx=7)
#' ResizeMatrix(mat.mtx=mat.mtx, rowDim.num=13, colDim.num=13, rowKeep.ndx=NULL, colKeep.ndx=NULL)
#' ResizeMatrix(mat.mtx=mat.mtx, rowDim.num=13, colDim.num=13, rowKeep.ndx=2, colKeep.ndx=7)
ResizeMatrix <- function(mat.mtx=NULL, rowDim.num=NULL ,colDim.num=NULL ,rowKeep.ndx=NULL ,colKeep.ndx=NULL) {
    rows.ndx <- round(seq(0, rowDim.num-1)/(rowDim.num-1) * (nrow(mat.mtx)-1) +1)
    cols.ndx <- round(seq(0, colDim.num-1)/(colDim.num-1) * (ncol(mat.mtx)-1) +1)
    if(!is.null(rowKeep.ndx)){if(is_not_in(rowKeep.ndx,rows.ndx)){rows.ndx[which.min(abs(rows.ndx-rowKeep.ndx))]<-rowKeep.ndx}}
    if(!is.null(colKeep.ndx)){if(is_not_in(colKeep.ndx,cols.ndx)){cols.ndx[which.min(abs(cols.ndx-colKeep.ndx))]<-colKeep.ndx}}
    mat.mtx[rows.ndx, ][ , cols.ndx] %>% return(.)
}
