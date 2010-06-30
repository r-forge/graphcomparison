\name{commonEdgesInd}
\Rdversion{1.1}
\alias{commonEdgesInd}

\title{
commonEdges returns the indices of common edges 
}
\description{
This function is called in the function of commonEdges
}
\usage{
commonEdgesInd(edgesList)
}

\arguments{
  \item{edgesList}{
Intern use
}
}
\details{
Not necessary
}
\value{
indices of coomon edges 
}
\references{
nn
}
\author{
Khadija El Amrani
}
\note{
no further notes
}


\examples{
commonEdges(EdgesList)

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
}

\keyword{classif}
\keyword{device}