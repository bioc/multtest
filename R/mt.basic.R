mt.rawp2adjp<-function(rawp,
      proc=c("Bonferroni","Holm","Hochberg","SidakSS","SidakSD","BH","BY"))
{

  m<-length(rawp)
  n<-length(proc)
  index<-order(rawp)
  spval<-rawp[index]

  adjp<-matrix(0,m,n+1)
  dimnames(adjp)<-list(NULL,c("rawp",proc))
  adjp[,1]<-spval

  if(is.element("Bonferroni",proc))
  {
    tmp<-m*spval
    tmp[tmp>1]<-1
    adjp[,"Bonferroni"]<-tmp
  }

  if(is.element("Holm",proc))
  {
    tmp<-spval
    tmp[1]<-min(m*spval[1],1)
    for(i in 2:m)
      tmp[i]<-max(tmp[i-1],min((m-i+1)*spval[i],1))
    adjp[,"Holm"]<-tmp
  }

  if(is.element("Hochberg",proc))
  {
    tmp<-spval
    for(i in (m-1):1){
      tmp[i]<-min(tmp[i+1],min((m-i+1)*spval[i],1,na.rm=TRUE),na.rm=TRUE)
      if(is.na(spval[i])) tmp[i]<-NA
    }
    adjp[,"Hochberg"]<-tmp
  }

  if(is.element("SidakSS",proc))
    adjp[,"SidakSS"]<-1-(1-spval)^m

  if(is.element("SidakSD",proc))
  {
    tmp<-spval
    tmp[1]<-1-(1-spval[1])^m
    for(i in 2:m)
      tmp[i]<-max(tmp[i-1],1-(1-spval[i])^(m-i+1))
    adjp[,"SidakSD"]<-tmp
  }

  if(is.element("BH",proc))
  {
    tmp<-spval
    for(i in (m-1):1){
      tmp[i]<-min(tmp[i+1],min((m/i)*spval[i],1,na.rm=TRUE),na.rm=TRUE)
      if(is.na(spval[i])) tmp[i]<-NA
    }
    adjp[,"BH"]<-tmp
  }

  if(is.element("BY",proc))
  {
    tmp<-spval
    a<-sum(1/(1:m))
    tmp[m]<-min(a*spval[m], 1) #noting we need to set tmp[m]
    for(i in (m-1):1){
      tmp[i]<-min(tmp[i+1],min((m*a/i)*spval[i],1,na.rm=TRUE),na.rm=TRUE)
      if(is.na(spval[i])) tmp[i]<-NA
    }
    adjp[,"BY"]<-tmp
  }

  list(adjp=adjp,index=index)
}

###########################################################################

mt.reject<-function(adjp,alpha)
{
  which<-adjp<=alpha[1]
  dimnames(which)<-dimnames(adjp)

  if(is.matrix(adjp))
  {
    r<-matrix(0,length(alpha),ncol(adjp))
    for(i in 1:length(alpha))
      r[i,]<-apply(adjp<=alpha[i],2,sum)
    dimnames(r)<-list(alpha,dimnames(adjp)[[2]])
  }

  if(!is.matrix(adjp))
  {
    r<-rep(0,length(alpha))
    for(i in 1:length(alpha))
      r[i]<-sum(adjp<=alpha[i])
  }

  list(r=r,which=which)
}


###########################################################################

mt.plot<-function(adjp,teststat, plottype="rvsa",logscale=FALSE,
                  alpha=seq(0,1,length=100), proc="",leg=c(0,0),...)
{
  m<-nrow(adjp)
  n<-ncol(adjp)
  a<-length(alpha)

  if(plottype=="rvsa")
  {
    r<-mt.reject(adjp,alpha)$r
    matplot(alpha,r,xlab="Type I error rate",
            ylab="Number of rejected hypotheses", type="l", ...)
    legend(leg[1],leg[2],proc,...)
  }

  if(plottype=="pvsr")
  {
    spval<-apply(adjp,2,sort)
    matplot(1:m,spval,xlab="Number of rejected hypotheses",
            ylab="Sorted adjusted p-values", type="l", ...)
    legend(leg[1],leg[2],proc,...)
  }

  if(plottype=="pvst")
  {
    if(!logscale)
      matplot(teststat,adjp,xlab="Test statistics",
              ylab="Adjusted p-values", type="p", ...)
    if(logscale)
      matplot(teststat,-log(adjp,10),xlab="Test statistics",
              ylab="-log(adjusted p-values,10)", type="p", ...)
    legend(leg[1],leg[2],proc,...)
  }
  if(plottype=="pvsi")
  {
    if(!logscale)
      matplot(1:m,adjp,xlab="index",ylab="Adjusted p-values", type="l", ...)
    if(logscale)
      matplot(1:m,-log(adjp,10),xlab="index",
              ylab="-log(adjusted p-values,10)", type="l", ...)
    legend(leg[1],leg[2],proc,...)
  }
}



