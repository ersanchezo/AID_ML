library(purrr)
library(parallel)
require('furrr', lib="/stor/home/fri_esanchez")
library(MASS)


fourByFourCTM <- read.csv("K-4x4.csv", stringsAsFactors = FALSE, 
                          colClasses = c("character", "numeric"), header = FALSE)

colnames(fourByFourCTM) <- c("square", "CTM")
rownames(fourByFourCTM) <- fourByFourCTM$square
fourByFourCTM$square    <- NULL

threeByThreeCTM <- read.csv("K-3x3.csv", stringsAsFactors = FALSE, 
                            colClasses = c("character", "numeric"), header = FALSE)

colnames(threeByThreeCTM) <- c("square", "CTM")
rownames(threeByThreeCTM) <- threeByThreeCTM$square
threeByThreeCTM$square    <- NULL

#ind <- function(matDim, blockSize, offset) {
  
    #mcMap(`:`, mc.cores = 24 , seq.int(1, matDim-blockSize+1, by = offset), 
        #seq.int(blockSize, matDim, by = offset))
#}

#myPartition <- function(mat, blockSize, offset) {
    
    #mclapply(cross2(ind(nrow(mat), blockSize, offset), 
                  #ind(ncol(mat), blockSize, offset)), mc.cores = 24, 
           #function(i) mat[i[[1]], i[[2]]])
#}
             
expand.grid.alt <- function(seq1,seq2) {
  cbind(rep.int(seq1, length(seq2)),
        c(t(matrix(rep.int(seq2, length(seq1)), nrow=length(seq2)))))
}
             
fpart<- function(mat,blockSize){
  f<- vector(mode = "list", length = (floor(nrow(mat)/blockSize))^2)
  l<-ind(nrow(mat), blockSize,blockSize)
  temp <- expand.grid.alt(l,l)
  f<-lapply(seq.int(nrow(temp)), mc.cores = 24, function(i){
  mat[.Internal(unlist(temp[i,1],F,F)),.Internal(unlist(temp[i,2],F,F)) ]
  })
  return(f)
}
             



#split_matrix(M,list(1:4,5:8,9:12,13:16),list(1:3,4:6,7:9,10:12))

stringify <- function(smallBlock){
  
    paste0(c(t(smallBlock)), collapse ="")
}

bdm2D <- function(mat, blockSize){
    i= nrow(mat)
    j= ncol(mat)
    submat_i = seq.int(from = 1, to = floor(i/blockSize)*blockSize, by = blockSize) # break-points along X
    submat_j = seq.int(from = 1, to = floor(j/blockSize)*blockSize, by = blockSize)# break-points along Y
    
    brokemat = lapply( X = seq_along(submat_j),
       function(j){
         j1 = submat_j[j] # Along Y, start
         j2 = submat_j[j] + (blockSize-1) # Along Y, end
         
         lapply( X = seq_along(submat_i), function(i){
           i1 = submat_i[i]
           i2 = submat_i[i] + (blockSize-1)
           return(mat[(i1:i2),(j1:j2)])
           
         })
         
       })
    parts<-vector(mode = "list", length = (floor(nrow(mat)/blockSize))^2)
    parts<-unlist(brokemat,FALSE,FALSE)
  
    #parts <- fpart(mat, blockSize)
    flatsquares<-vector(mode = "list", length = (floor(nrow(mat)/blockSize))^2)
    flatSquares <- unlist(lapply( parts, stringify),FALSE,FALSE)
    
    squaresTally <- as.data.frame(table(flatSquares))
    
    rownames(squaresTally) <- squaresTally$flatSquares
    
    squaresTally$flatSquares <- NULL
    
    if(blockSize == 4) {
        bdm <- (sum(fourByFourCTM[rownames(squaresTally), ]) 
                + sum(log2(squaresTally$Freq)))
    } else {
        bdm <- (sum(threeByThreeCTM[rownames(squaresTally), ]) 
                + sum(log2(squaresTally$Freq)))
    }
  
    return(bdm)
}