\name{GraphComparison-package}
\Rdversion{1.1}
\alias{GraphComparison-package}
\alias{GraphComparison}
\docType{package}
\title{
 GRAPHical COMPARISON of Graphs with the same node-set

}
\description{
The graphical comparison of graphs with less than 60 nodes can be
helpful to demonstrate the similarities and differences between these
graphs. This project aims to support functions, which enable the visual
comparison of graphs. 
}
\details{
\tabular{ll}{
Package: \tab GraphComparison\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-05-19\cr
License: \tab  GPL version 2\cr
LazyLoad: \tab yes\cr
}
commonEdges(graphList, commonColor)
}
\author{
Khadija El Amrani

Maintainer:  <ben4508@ibe.med.uni-muenchen.de>
Khadija El Amrani
}
\references{
nn
}

\keyword{ package }

\examples{
data(graph1)
data(graph2)
commonEdges(c(graph1, graph2), "red")
}
