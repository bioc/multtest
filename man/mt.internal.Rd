\name{multtest-internal}
\alias{.mt.BLIM}
\alias{.mt.RandSeed}
\alias{.mt.naNUM}
\alias{mt.number2na}
\alias{mt.na2number}
\alias{mt.getmaxB}
\alias{mt.transformL}
\alias{mt.transformX}
\alias{mt.transformV}
\alias{mt.checkothers}
\alias{mt.checkX}
\alias{mt.checkV}
\alias{mt.checkclasslabel}
\title{Internal multtest functions and variables}
\description{
  Internal multtest functions and variables
}
\usage{
.mt.BLIM
.mt.RandSeed
.mt.naNUM
mt.number2na(x,na)
mt.na2number(x,na)
mt.getmaxB(classlabel,test,B, verbose)
mt.transformL(classlabel,test)
mt.transformV(V,classlabel,test,na,nonpara)
mt.transformX(X,classlabel,test,na,nonpara)
mt.checkothers(side="abs",fixed.seed.sampling="y", B=10000,
na=.mt.naNUM, nonpara="n")
mt.checkX(X,classlabel,test)
mt.checkV(V,classlabel,test)
mt.checkclasslabel(classlabel,test)
}
\details{
  These are not to be called directly by the user.
}
\keyword{internal}
