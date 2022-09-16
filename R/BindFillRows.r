#' Bind data frames after filling missing columns.
#'
#' BindFillRows
#' @description Bind data frames by rows after filling missing columns with NA.
#' @param data.lst_df <data.frames or list[data.frame]>: data frames to bind or list of data.frames. If is a data.frame create a list with arguments `data.lst_df` and `...`, else `...` are ignored.
#' @param ... <data.frames or list[data.frame]>: data frames to bind or list of data.frames.
#' @return The binded data frame 
#' @examples
#' df1 <- data.frame(a = c(1:5), b = c(6:10))
#' df2 <- data.frame(a = c(11:15), b = c(16:20), c = LETTERS[1:5])
#' BindFillRows(df1,df2)
#' BindFillRows(list(df1,df2))

BindFillRows <- function(data.lst_df,...){
    if(is.data.frame(data.lst_df)){
        data.lst_df <- list(data.lst_df,...)
    }
    data.lst_df <- lapply(seq_along(data.lst_df), function(data.ndx){
        data.df <- data.lst_df[[data.ndx]]
        dataNames.chr <- lapply(data.lst_df[-data.ndx], names) %>%
            unlist %>%
            unique
        data.df[setdiff(dataNames.chr, names(data.df))] <- NA
        return(data.df)
    })
    return(do.call(rbind,data.lst_df))
}