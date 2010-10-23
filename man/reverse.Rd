\name{reverse}
\alias{reverse}
\title{
Get the reverse sequence.
}
\description{
This function could be used to obtain the reverve sequence of the input sequence.
}
\usage{
reverse(dna)
}

\arguments{
  \item{dna}{
A character string of input DNA sequence
}
}

\details{
This function could be used to obtain the reverve sequence of the input sequence.
}
\value{
A string containde the reverse sequence. 
}
\references{
Kress W., Erickson D., Jones F., Swenson N., Perez R., Sanjur O., Bermingham E., Plant DNA barcodes and community phylogeny of a tropical forest dynamics plot in Panama. Proceedings of the National Academy of Sciences of the United States of America. 2009 18621-18626
}
\author{

Jinlong Zhang \email{jinlongzhang01@gmail.com}

}
\note{
None
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also \code{\link{fmatch}}
}
\examples{

## reverse examle ########
dna = "---ATTGCCTTAS--TTTAAAAAACCGTTC-----"
reverse(dna)

}

\keyword{ reverse }
