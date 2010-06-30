\name{commonEdges}
\Rdversion{1.1}
\alias{commonEdges}

\title{
function to color commonEdges of the given graphes
}
\description{
The function receives as parameters a list of graphs and a color for the common edges. 
}
\usage{
commonEdges(graphList, commonColor)
}

\arguments{
  \item{graphList}{
   The list of graphs given as c(g1, g2, ...)
}
  \item{commonColor}{
   The color of common edges for i.e. "red"
}
}
\details{
The function receives as parameters a list of graphs and a color for the common edges. The output is one graph with the edges of both graphs.
}
\value{
One graph with edges of both graphs, commonEdges will be colored with the given color 
}
\references{
no ref
}
\author{
Khadija El Amrani
}
\note{
no notes
}



\seealso{
ccc
}
\examples{
data(graph1)
data(graph2)
commonEdges(c(graph1, graph2), "red")

## The function is currently defined as
#commonEdges<-function(graph1, graph2,color1,color2)
commonEdges<-function(graphList, commonColor)
{

  require(Rgraphviz)
  
  edges.list<-list()
  
  for(i in 1:length(graphList))
  {
   edges.list[[i]]<-edgeNames(graphList[[i]])
  }

   commonEdg <- commonEdgesInd(edges.list)
   
   indices<-which(edges.list[[1]] %in% commonEdg)
   outGraph<-graphList[[1]]

    for(i in 2:length(edges.list))
    {
       edges <- edges.list[[i]][which(!edges.list[[i]] %in% edges.list[[1]])]
       
        List<-strsplit(edges, "~")
        Mat<-as(outGraph, "matrix")
        if(length(edges)!=0)
         {
          for(j in 1:length(edges))
           {
              Mat[List[[j]][1], List[[j]][2]]=1
              Mat[List[[j]][2], List[[j]][1]]=1
           }
         }
           
    }

   

       outGraph<-as(Mat,"graphNEL")


    edges.outGr<-edgeNames(outGraph)

    edges.outGr[indices]

    col.vec<-rep("black", length(edges.outGr))
    names(col.vec)<-edges.outGr
    col.vec[edges.list[[1]][indices]] <- commonColor

    #index2 <-which(edgeNames(gr) %in% edges)
    #col.vec[index2] <- color2

    names(col.vec)<-edges.outGr

    edgeRenderInfo(outGraph)=list(col=col.vec)
    y=layoutGraph(outGraph, attrs=list(node=list(shape="ellipse",
    fixedsize=FALSE)))
    renderGraph(y)
     x=1


}

}

\keyword{classif}
\keyword{device}
