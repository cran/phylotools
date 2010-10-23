\name{phylotools-package}
\alias{phylotools-package}
\alias{phylotools}
\docType{package}
\title{
Phylogenetic tools for ecologists
}
\description{
This package currently consists of a few functions for handling DNA-barcoding sequences to build supermatrix for further analysis with RAxML etc. Much more functions for conducting phylogenetic analysis would be added in the future, especially for community phylogentic analysis.
}
\details{
\tabular{ll}{
Package: \tab phylotools\cr
Type: \tab Package\cr
Version: \tab 0.0.2\cr
Date: \tab 2010-10-23\cr
License: \tab GLP-2\cr
LazyLoad: \tab yes\cr
}

}
\author{
Jinlong Zhang, Xiangcheng Mi, Nancai Pei

Maintainer: Jinlong Zhang <jinlongzhang01@gmail.com>
}
\references{
Kress W., Erickson D., Jones F., Swenson N., Perez R., Sanjur O., Bermingham E., Plant DNA barcodes and community phylogeny of a tropical forest dynamics plot in Panama. Proceedings of the National Academy of Sciences of the United States of America. 2009 18621-18626
}

\keyword{ package }
\examples{

### Build super matrix
dir <- system.file("extdata", package = "phylotools")
setwd(dir)

## Supermatrix with "rbcla","matk","trnH-psbA"
supermat <- supermat(rbcl = "rbcla.phy", matk = "matK.phy", 
          trn = c("trn1.phy", "trn2.phy","trn3.phy","trn4.phy"))	   
## Save to file
write.mat(supermat, "result.phy")

## Delete file
unlink("result.phy")
}
