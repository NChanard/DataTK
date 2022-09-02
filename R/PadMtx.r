#' PadMtx
#'
#' Add a value around a matrix
#' @param mat.mtx <matrix>: Numerical matrix
#' @param padSize.num <numeric>: Number of columns or rows to add (Default 1)
#' @param value.num <numeric>: Value to add (Default 0) If Null create mirror of choosen sides.
#' @param side.chr <character>: side to pad, must be one or some of 'top','bot','right' or 'left' (Default c('top','bot','right','left') )
#' @return Matrix
#' @examples
#' mat.mtx = matrix(1:25,5,5)
#' PadMtx(mat.mtx=mat.mtx,  padSize.num=1, value.num=0, side.chr=c('top','bot','right','left') )
#' PadMtx(mat.mtx=mat.mtx,  padSize.num=1, value.num=NULL, side.chr=c('top','bot','right','left') )
#' PadMtx(mat.mtx=mat.mtx,  padSize.num=1, value.num=0, side.chr=c('right','left') )
#' PadMtx(mat.mtx=mat.mtx,  padSize.num=1, value.num=0, side.chr=c('top') )
PadMtx <- function(mat.mtx=NULL, padSize.num=1, value.num=0, side.chr=c('top','bot','right','left')){
    if('top' %in% side.chr){
        if(!is.null(value.num)){
            row.pad = rep(list(rep(value.num,dim(mat.mtx)[2])),padSize.num) %>% do.call(rbind,.)
        }else{
            row.pad = mat.mtx[padSize.num:1,]
        }
        mat.mtx <- rbind(row.pad,mat.mtx)
    }
    if('bot' %in% side.chr){
        if(!is.null(value.num)){
            row.pad = rep(list(rep(value.num,dim(mat.mtx)[2])),padSize.num) %>% do.call(rbind,.)
        }else{
            row.pad = mat.mtx[(nrow(mat.mtx)-padSize.num+1):nrow(mat.mtx),]
        }
        mat.mtx <- rbind(mat.mtx,row.pad)
    }
    if('left' %in% side.chr){
        if(!is.null(value.num)){
            col.pad = rep(list(rep(value.num,dim(mat.mtx)[1])),padSize.num) %>% do.call(cbind,.)
        }else{
            col.pad = mat.mtx[,padSize.num:1]
        }
        mat.mtx <- cbind(col.pad,mat.mtx)
    }
    if('right' %in% side.chr){
        if(!is.null(value.num)){
            col.pad = rep(list(rep(value.num,dim(mat.mtx)[1])),padSize.num) %>% do.call(cbind,.)
        }else{
            col.pad = mat.mtx[,(ncol(mat.mtx)-padSize.num+1):ncol(mat.mtx)]
        }
        mat.mtx <- cbind(mat.mtx,col.pad)
    }
    return(mat.mtx)
}
