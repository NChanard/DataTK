#' MeltSpm
#'
#' Coerce a sparse matrix M in tibble where columns: i is row index, j is column index and x the value M[i,j]
#' @param mat.spm <dgCMatrix or dgCMatrix coercible>: Matrix
#' @return A tibble
#' @examples
#' i=c(1,1,2,2,3,3,4,4,4,4)
#' j=c(1,4,2,5,1,4,2,3,4,5)
#' x=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' mat.spm = Matrix::sparseMatrix(i=i, j=j, x=x, dims=c(5,5))
#' mat.spm
#' meltedMat.tbl <- MeltSpm(mat.spm)
#' meltedMat.tbl[order(meltedMat.tbl$i),]
MeltSpm = function(mat.spm=NULL){
        if(DevToolKit::is_not_in("dgCMatrix",class(mat.spm))){mat.spm=as(mat.spm, "dgCMatrix")}
        dp.num <- mat.spm@p %>% diff
        tibble::tibble(
            i=mat.spm@i+1 %>% as.integer,
            j=mat.spm@Dim %>% magrittr::extract(.,2) %>% seq(1,.) %>% lapply(.,function(j.ndx){dp.num[j.ndx] %>% rep(j.ndx,.) %>% return(.)}) %>% unlist,
            x=mat.spm@x
        ) %>% DevToolKit::add_attributes(list(matrice.attr=attributes(mat.spm)[which(DevToolKit::is_not_in(names(attributes(mat.spm)),c("i","p","Dimnames","x","factors","class")))])) %>% return
}
