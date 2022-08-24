#' ReduceRun
#'
#' apply a function to the values of two RLE. e.g paste, sum, mean
#' @param first.rle <matrix>: first rle
#' @param second.rle <matrix>: second rle
#' @param reduceFun.chr <character>: name of a function to apply e.g paste, sum, mean.
#' @param ... <...>: Other parameter for the reduce function
#' @return reduced Rle
#' @examples
#' first.rle = rle(c("A","A","B"))
#' second.rle = rle(c("A","B","B"))
#' ReduceRun(first.rle=first.rle, second.rle=second.rle, reduceFun.chr="paste", sep="_" )
#' first.rle = S4Vectors::Rle(c(1,2,3))
#' second.rle = S4Vectors::Rle(c(5,5,5))
#' ReduceRun(first.rle=first.rle, second.rle=second.rle, reduceFun.chr="sum")
ReduceRun <- function(first.rle, second.rle, reduceFun.chr="paste",...){
    if (class(first.rle)=="rle"){
        firstLen.num = first.rle$length
        firstVal.vec = first.rle$values
    }else if (class(first.rle)=="Rle") {
        firstLen.num = S4Vectors::runLength(first.rle)
        firstVal.vec = S4Vectors::runValue(first.rle)
    }
    if (class(second.rle)=="rle"){
        secondLen.num = second.rle$length
        secondVal.vec = second.rle$values
    }else if (class(second.rle)=="Rle") {
        secondLen.num = S4Vectors::runLength(second.rle)
        secondVal.vec = S4Vectors::runValue(second.rle)
    }
    newLen.num=NULL
    newVal.vec=NULL
    lens.lst = list(firstLen.num=firstLen.num,secondLen.num=secondLen.num)
    vals.lst = list(firstVal.vec=firstVal.vec,secondVal.vec=secondVal.vec)
    while(!is.na(lens.lst[[1]][1]) && !is.na(lens.lst[[2]][1]) && !is.na(vals.lst[[1]][1]) && !is.na(vals.lst[[2]][1]) ){
        A.len = lens.lst[[which.min(c(lens.lst[[1]][1],lens.lst[[2]][1]))]][1]
        A.val = eval(parse(text=reduceFun.chr))(vals.lst[[1]][1],vals.lst[[2]][1],...)
        newVal.vec = c(newVal.vec,A.val)
        newLen.num=c(newLen.num,A.len)
        lapply(seq_along(lens.lst),function(i){
            if((lens.lst[[i]][1]-A.len)>0){lens.lst[[i]][1]<<-(lens.lst[[i]][1]-A.len)}
            else{
                lens.lst[[i]]<<-lens.lst[[i]][-1]
                vals.lst[[i]]<<-vals.lst[[i]][-1]
            }
            }) %>% invisible()
    }
    return(S4Vectors::Rle(values=newVal.vec,lengths=newLen.num))
}