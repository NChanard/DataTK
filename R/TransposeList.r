#' Turns a nested list "inside-out".
#'
#' TransposeList
#' @description Turns a nested list "inside-out".
#' @param var.nlst <list[list]>: a nested list to transpose.
#' @return  The tranposed nested list.
#' @examples
#' my_lst <- list(
#'     first=list("A1","B1","C1"),
#'     second=list("A2","B2")
#' )
#' TransposeList(my_lst)
TransposeList <- function(var.nlst){
    listName.chr <- names(var.nlst)
    var.nlst %>%
        lapply(length) %>%
        unlist %>%
        max %>%
        seq_len %>%
        lapply(function(newLst.ndx){
            new.lst <- var.nlst %>% 
                lapply(function(ele.lst){
                    ifelse(length(ele.lst) >= newLst.ndx, yes=ele.lst[[newLst.ndx]], no=NA)
                    }) %>%
                unlist %>%
                magrittr::set_names(listName.chr)
            return(new.lst[!is.na(new.lst)])
        })
}
