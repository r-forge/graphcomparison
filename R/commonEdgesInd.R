commonEdgesInd <-
function(edgesList){
 inter <- unique(unlist(edgesList))
for(i in 1:length(edgesList)) {
   inter <- intersect(inter, edgesList[[i]])
   if(length(inter)==0)
   {
    result <- -1
    return (result)
   }
}
result<-inter
return (result)
}

