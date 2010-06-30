\name{graph1}
\Rdversion{1.1}
\alias{graph1}
\docType{data}
\title{
This is an example graph
}
\description{
This graph has 6 nodes
}
\usage{data(graph1)}
\format{
  The format is:
Formal class 'graphNEL' [package "graph"] with 6 slots
  ..@ nodes     : chr [1:6] "1" "2" "3" "4" ...
  ..@ edgeL     :List of 6
  .. ..$ 1:List of 1
  .. .. ..$ edges: int 2
  .. ..$ 2:List of 1
  .. .. ..$ edges: int [1:2] 1 4
  .. ..$ 3:List of 1
  .. .. ..$ edges: int(0) 
  .. ..$ 4:List of 1
  .. .. ..$ edges: int [1:2] 2 5
  .. ..$ 5:List of 1
  .. .. ..$ edges: int 4
  .. ..$ 6:List of 1
  .. .. ..$ edges: int(0) 
  ..@ edgeData  :Formal class 'attrData' [package "graph"] with 2 slots
  .. .. ..@ data    : list()
  .. .. ..@ defaults: list()
  ..@ nodeData  :Formal class 'attrData' [package "graph"] with 2 slots
  .. .. ..@ data    : list()
  .. .. ..@ defaults: list()
  ..@ renderInfo:Formal class 'renderInfo' [package "graph"] with 4 slots
  .. .. ..@ nodes: list()
  .. .. ..@ edges: list()
  .. .. ..@ graph: list()
  .. .. ..@ pars : list()
  ..@ graphData :List of 1
  .. ..$ edgemode: chr "undirected"
}
\details{
The graph1 has 6 nodes and is used here as an example
}
\source{
Simple example
}
\references{
Simple example
}
\examples{
data(graph1)
plot(graph1)
}
\keyword{datasets}
