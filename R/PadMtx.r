#' PadMtx
#'
#' Add a value around a matrix
#' @param mat.mtx <matrix>: Numerical matrix
#' @param padSize.num <numeric>: Number of columns or rows to add (Default 1)
#' @param value.num <numeric>: Value to add (Default 0)
#' @param side.chr <character>: side to pad, must be one or some of 'top','bot','right' or 'left' (Default c('top','bot','right','left') )
#' @return Matrix
#' @examples
PadMtx <- function(mat.mtx=NULL, padSize.num=1, value.num=0, side.chr=c('top','bot','right','left')){
    row.pad = rep(list(rep(value.num,dim(mat.mtx)[2])),padSize.num) %>% do.call(rbind,.)
    if('top' %in% side.chr){
        mat.mtx <- rbind(row.pad,mat.mtx)
    }
    if('bot' %in% side.chr){
        mat.mtx <- rbind(mat.mtx,row.pad)
    }
    col.pad = rep(list(rep(value.num,dim(mat.mtx)[1])),padSize.num) %>% do.call(cbind,.)
    if('left' %in% side.chr){
        mat.mtx <- cbind(col.pad,mat.mtx)
    }
    if('right' %in% side.chr){
        mat.mtx <- cbind(mat.mtx,col.pad)
    }
    return(mat.mtx)
}
