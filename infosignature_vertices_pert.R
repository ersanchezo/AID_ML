require('acss', lib="/stor/home/fri_esanchez")
require('igraph', lib="/stor/home/fri_esanchez")
require('Matrix', lib="/stor/home/fri_esanchez")
require('bigmemory', lib="/stor/home/fri_esanchez")
source("BDM2D.R")

get_info_signature <- function(original_graph, block_size) {
    original_graph<- igraph::simplify(original_graph)
    original_matrix   <- as.matrix(as_adjacency_matrix(original_graph, names = FALSE, sparse = TRUE))
    original_bdm      <- bdm2D(original_matrix, block_size)
 
    vertex_deletions_df <- as_data_frame(original_graph, what = "vertices")
    deletion_columns  <- c("bdm_value", "information_loss")
    
    vertex_deletions_df[, deletion_columns] <- NA
    
    
    delbdm<- function(vert){
        deleted_vertex_graph  <- delete_vertices(original_graph, vert)
        deleted_vertex_matrix <-as.matrix(as_adjacency_matrix(deleted_vertex_graph, sparse = TRUE, names = FALSE))
        deleted_vertex_bdm    <- bdm2D(deleted_vertex_matrix, block_size)
    }
 
    vertex_deletions_df$bdm_value<-mclapply(vertex_deletions_df[,1], delbdm, mc.cores = 4)
    
    #vertex_deletions_df <- vertex_deletions_df[vertex_deletions_df$information_loss > 0, ]
    #vertex_deletions_df <- vertex_deletions_df[order(-vertex_deletions_df$information_loss), ]
    vertex_deletions_df$information_loss<- as.numeric(original_bdm) - as.numeric(vertex_deletions_df$bdm_value)

    return(vertex_deletions_df)
    #return(original_bdm)
    
}




###################################
############ TEST CASE ############
###################################


###### INFORMATION SIGNATURE ######

# is <- get_info_signature(make_graph("Frucht"),4,4)
# plot(log(is$information_loss)+80, xlab = "sorted by max info value", ylab = "log info values (+80)", col = "red")
# lines(log(is$information_loss)+80, col = "red")


######## SEE CUTTING PLACES #######

# diffs <- c()
# for(i in 1:nrow(is)) {
#      if(i != nrow(is)) {
#          diffs <- c(diffs, is$information_loss[i]-is$information_loss[i+1])
#      }
# }
# plot(diffs, xlab = "edges sorted by max info value", ylab = "sequential info differences", col = "blue")
# lines(diffs, col = "blue")
# curve(log2(2)*x^0, col = "purple", add = TRUE)