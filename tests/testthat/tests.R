df1 <- data.frame(a = c(1:5), b = c(6:10))
df2 <- data.frame(a = c(11:15), b = c(16:20), c = LETTERS[1:5])
BindFillRows(df1,df2)
BindFillRows(list(df1,df2))


i=c(1,1,2,2,3,3,4,4,4,4)
j=c(1,4,2,5,1,4,2,3,4,5)
x=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
mat.spm = Matrix::sparseMatrix(i=i, j=j, x=x, dims=c(5,5))
mat.spm
meltedMat.tbl <- MeltSpm(mat.spm)
meltedMat.tbl[order(meltedMat.tbl$i),]

mat.mtx = matrix(1:25,5,5)
PadMtx(mat.mtx=mat.mtx,  padSize.num=1, value.num=0, side.chr=c('top','bot','right','left') )
PadMtx(mat.mtx=mat.mtx,  padSize.num=1, value.num=NULL, side.chr=c('top','bot','right','left') )
PadMtx(mat.mtx=mat.mtx,  padSize.num=1, value.num=0, side.chr=c('right','left') )
PadMtx(mat.mtx=mat.mtx,  padSize.num=1, value.num=0, side.chr=c('top') )

first.rle = rle(c("A","A","B"))
second.rle = rle(c("A","B","B"))
ReduceRun(first.rle=first.rle, second.rle=second.rle, reduceFun.chr="paste", sep="_" )
first.rle = S4Vectors::Rle(c(1,2,3))
second.rle = S4Vectors::Rle(c(5,5,5))
ReduceRun(first.rle=first.rle, second.rle=second.rle, reduceFun.chr="sum")

matrice.mtx <- matrix(0,11,11)
matrice.mtx[which(as.logical(1:(11*11)%%2))] <- 1:ceiling((11*11)/2)
matrice.mtx[2,] <- 100
matrice.mtx[,7] <- 200
matrice.mtx
ResizeMatrix(matrice.mtx=matrice.mtx, newDim.num=c(7,7))
ResizeMatrix(matrice.mtx=matrice.mtx, newDim.num=c(13,13))

set.seed(123)
mat.spm = as(matrix(floor(runif(7*13,0,2)),7,13), "dgCMatrix")
mat.spm
Rise0(mat.spm=mat.spm, which.ndx=c(1,3,6,10,12))
Rise0(mat.spm=mat.spm, coord.dtf=data.frame(i=c(1,5,3),  j=c(1,2,3) ) )
Rise0(mat.spm=mat.spm)

my_lst <- list(
    first=list("A1","B1","C1"),
    second=list("A2","B2"),
    third=list(NULL,"B3")
)
TransposeList(my_lst)


myString <- "mean(c(2,4,NA), na.rm=TRUE)"
WrapFunction("mean(c(2,4,NA), na.rm=TRUE)")
myResult <- WrapFunction(myString)
myString_2 <- "function(X){mean(X, na.rm=TRUE)}"
WrapFunction(myString_2)
myFunction <- WrapFunction(myString_2)
myFunction(c(1,2,3))
