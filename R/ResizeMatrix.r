#' ResizeMatrix
#'
#' Resize a matrix in new dimension
#' @param matrice.mtx <matrix>: a Matrix to resize
#' @param newDim.num <integer>: the number of rows and cols in resized matrix.
#' @return resized matrix
#' @examples
#' matrice.mtx <- matrix(0,11,11)
#' matrice.mtx[which(as.logical(1:(11*11)%%2))] <- 1:ceiling((11*11)/2)
#' matrice.mtx[2,] <- 100
#' matrice.mtx[,7] <- 200
#' matrice.mtx
#' ResizeMatrix(matrice.mtx=matrice.mtx, newDim.num=c(7,7))
#' ResizeMatrix(matrice.mtx=matrice.mtx, newDim.num=c(13,13))
ResizeMatrix <- function(matrice.mtx, newDim.num=dim(matrice.mtx)){
    # Rescaling
        newCoord.mtx <- as.matrix(expand.grid(seq_len(newDim.num[1]), seq_len(newDim.num[2])))
        rescaleCol.ndx <- StatTK::MinMaxScale(newCoord.mtx[,1],1, dim(matrice.mtx)[1])
        rescaleRow.ndx <- StatTK::MinMaxScale(newCoord.mtx[,2],1, dim(matrice.mtx)[2])
    # Interpolation
        col.ndx <- floor(rescaleCol.ndx)
        row.ndx <- floor(rescaleRow.ndx)
        xGap.num <- rescaleCol.ndx - col.ndx
        yGap.num <- rescaleRow.ndx - row.ndx
        xGap.num[col.ndx == dim(matrice.mtx)[1]] <- 1
        yGap.num[row.ndx == dim(matrice.mtx)[2]] <- 1
        col.ndx[col.ndx == dim(matrice.mtx)[1]] <- dim(matrice.mtx)[1] - 1
        row.ndx[row.ndx == dim(matrice.mtx)[2]] <- dim(matrice.mtx)[2] - 1
    # Output
        resizedMatrice.mtx <- matrix(NA, nrow=newDim.num[1], ncol=newDim.num[2])
        resizedMatrice.mtx[newCoord.mtx] <-
            matrice.mtx[cbind(col.ndx, row.ndx)] * (1 - xGap.num) * (1 - yGap.num) +
            matrice.mtx[cbind(col.ndx+1, row.ndx)] * xGap.num * (1 - yGap.num) +
            matrice.mtx[cbind(col.ndx, row.ndx + 1)] * (1 - xGap.num) * yGap.num +
            matrice.mtx[cbind(col.ndx + 1, row.ndx + 1)] * xGap.num * yGap.num
        return(resizedMatrice.mtx)
}