#main user-level function for multiple hypothesis testing

MTP<-function(X,W=NULL,Y=NULL,Z=NULL,Z.incl=NULL,Z.test=NULL,na.rm=TRUE,test="t.twosamp.unequalvar",robust=FALSE,standardize=TRUE,alternative="two.sided",psi0=0,typeone="fwer",k=0,q=0.1,fdr.method="conservative",alpha=0.05,smooth.null=FALSE,nulldist="boot.cs",B=1000,ic.quant.trans=FALSE,MVN.method="mvrnorm",penalty=1e-6,method="ss.maxT",get.cr=FALSE,get.cutoff=FALSE,get.adjp=TRUE,keep.nulldist=TRUE,keep.rawdist=FALSE,seed=NULL,cluster=1,type=NULL,dispatch=NULL,marg.null=NULL,marg.par=NULL,keep.margpar=TRUE,ncp=NULL,perm.mat=NULL,keep.index=FALSE,keep.label=FALSE){
  ##sanity checks / formatting
  #X
  if(missing(X)) stop("Argument X is missing")
  if(inherits(X,"eSet")){ 
    if(is.character(Y)) Y<-pData(X)[,Y]
    if(is.character(Z)){
      if(Z%in%Y){
        Z<-Z[!(Z%in%Y)]
	warning(paste("Outcome Y=",Y,"should not be included in the covariates Z=",Z,". Removing Y from Z",sep=""))
	}
      Z<-pData(X)[,Z]
    }
    X<-exprs(X)
  }
  X<-as.matrix(X)
  dx<-dim(X)
  if(length(dx)==0) stop("dim(X) must have positive length")
  p<-dx[1]
  n<-dx[2]
  #W
  if(!is.null(W)){
    W[W<=0]<-NA
    if(is.vector(W) & length(W)==n) W <- matrix(rep(W,p),nrow=p,ncol=n,byrow=TRUE)
    if(is.vector(W) & length(W)==p) W <- matrix(rep(W,n),nrow=p,ncol=n)
    if(test%in%c("f","f.block","f.twoway","t.cor","z.cor")){
      warning("Weights can not be used with F-tests or tests of correlation parameters, arg W is being ignored.")
      W<-NULL
    }
  }
  #Y
  if(!is.null(Y)){
    if(is.Surv(Y)){
      if(test!="coxph.YvsXZ") stop(paste("Test ",test," does not work with a survival object Y",sep=""))
    }
    else{
      Y<-as.matrix(Y)
      if(ncol(Y)!=1) stop("Argument Y must be a vector")
    }
    if(nrow(Y)!=n) stop("Outcome Y has length ",nrow(Y),", not equal to n=",n)
  }
  if(test=="t.pair") n <- dx[2]/2
  #Z
  if(!is.null(Z)){
    Z<-as.matrix(Z)
    if(nrow(Z)!=n) stop("Covariates in Z have length ",nrow(Z),", not equal to n=",n,"\n")
    #Z.incl tells which columns of Z to include in model
    if(is.null(Z.incl)) Z.incl<-(1:ncol(Z))
    if(length(Z.incl)>ncol(Z)) stop("Number of columns in Z.incl ",length(Z.incl)," exceeds ncol(Z)=",ncol(Z))
    if(is.logical(Z.incl)) Z.incl<-(1:ncol(Z))[Z.incl]
    if(is.character(Z.incl) & length(Z.incl)!=sum(Z.incl%in%colnames(Z))) stop(paste("Z.incl=",Z.incl," names columns not in Z",sep=""))
    Za<-Z[,Z.incl]
    #Z.test tells which column of Z to test for an association
    if(test=="lm.XvsZ"){
      if(is.null(Z.test)){
        warning(paste("Z.test not specified, testing for association with variable in first column of Z:",colnames(Z)[1],sep=""))
	Z.test<-1
      }
      if(is.logical(Z.test)) Z.test<-(1:ncol(Z))[Z.test]
      if(is.character(Z.test) & !(Z.test%in%colnames(Z))) stop(paste("Z.test=",Z.test," names a column not in Z",sep=""))
      if(is.numeric(Z.test) & !(Z.test%in%(1:ncol(Z)))) stop("Value of Z.test must be >0 and <",ncol(Z))
      if(Z.test%in%Z.incl){
        Z.incl<-Z.incl[!(Z.incl%in%Z.test)]
	Za<-Z[,Z.incl]
      }
      Za<-cbind(Z[,Z.test],Za)
    }
    Z<-Za
    rm(Za)
  }
  #test
  TESTS<-c("t.onesamp","t.twosamp.equalvar","t.twosamp.unequalvar","t.pair","f","f.block","f.twoway","lm.XvsZ","lm.YvsXZ","coxph.YvsXZ","t.cor","z.cor")
  test<-TESTS[pmatch(test,TESTS)]
  if(is.na(test)) stop(paste("Invalid test, try one of ",TESTS,sep=""))
  #robust + see below with choice of nulldist
  if(test=="coxph.YvsXZ" & robust==TRUE)
    warning("No robust version of coxph.YvsXZ, proceding with usual version")
  #temp until fix
  if((test=="t.onesamp" | test=="t.pair") & robust==TRUE)
    stop("Robust test statistics currently not available for one-sample or two-sample paired test statistics.")
  #alternative
  ALTS<-c("two.sided","less","greater")
  alternative<-ALTS[pmatch(alternative,ALTS)]
  if(is.na(alternative)) stop(paste("Invalid alternative, try one of ",ALTS,sep=""))
  #null values
  if(length(psi0)>1) stop(paste("In current implementation, all hypotheses must have the same null value. Number of null values: ",length(psi0),">1",sep=""))
  #Error rate
  ERROR<-c("fwer","gfwer","tppfp","fdr")
  typeone<-ERROR[pmatch(typeone,ERROR)]
  if(is.na(typeone)) stop(paste("Invalid typeone, try one of ",ERROR,sep=""))
  if(any(alpha<0) | any(alpha>1)) stop("Nominal level alpha must be between 0 and 1")
  nalpha<-length(alpha)
  reject<-
    if(nalpha) array(dim=c(p,nalpha),dimnames=list(rownames(X),paste("alpha=",alpha,sep="")))
    if(test=="z.cor" | test=="t.cor") matrix(nrow=0,ncol=0) # deprecated for correlations, rownames now represent p choose 2 edges - too weird and clunky in current state for output.
    else matrix(nrow=0,ncol=0)
  if(typeone=="gfwer"){
    if(get.cr==TRUE) warning("Confidence regions not currently implemented for gFWER")
    if(get.cutoff==TRUE) warning("Cut-offs not currently implemented for gFWER")
    get.cr<-get.cutoff<-FALSE
    if(k<0) stop("Number of false positives can not be negative")
    if(k>=p) stop(paste("Number of false positives must be less than number of tests=",p,sep=""))
    if(length(k)>1){
      k<-k[1]
      warning("can only compute gfwer(k) adjp for one value of k at a time (using first value), try fwer2gfwer() function for multiple k")
    }
  }
  if(typeone=="tppfp"){
    if(get.cr==TRUE) warning("Confidence regions not currently implemented for TPPFP")
    if(get.cutoff==TRUE) warning("Cut-offs not currently implemented for TPPFP")
    get.cr<-get.cutoff<-FALSE
    if(q<0) stop("Proportion of false positives, q, can not be negative")
    if(q>1) stop("Proportion of false positives, q, must be less than 1")
    if(length(q)>1){
      q<-q[1]
      warning("Can only compute tppfp adjp for one value of q at a time (using first value), try fwer2tppfp() function for multiple q")
    }
  }
  if(typeone=="fdr"){
    if(!nalpha) stop("Must specify a nominal level alpha for control of FDR")
    if(get.cr==TRUE) warning("Confidence regions not currently implemented for FDR")
    if(get.cutoff==TRUE) warning("Cut-offs not currently implemented for FDR")
    get.cr<-get.cutoff<-FALSE
  }		
  #null distribution
  NULLS<-c("boot","boot.cs","boot.ctr","boot.qt","ic","perm")
  nulldist<-NULLS[pmatch(nulldist,NULLS)]
  if(is.na(nulldist)) stop(paste("Invalid nulldist, try one of ",NULLS,sep=""))
  if(nulldist=="boot"){
    nulldist <- "boot.cs"
    warning("nulldist='boot' is deprecated and now corresponds to 'boot.cs'. Proceeding with default center and scaled null distribution.")
  }
  if(nulldist!="perm" & test=="f.block") stop("f.block test only available with permutation null distribution. Try test=f.twoway")
  if((nulldist=="perm" | nulldist=="ic") & keep.rawdist==TRUE) stop("Test statistics distribution estimation using keep.rawdist=TRUE is only available with a bootstrap-based null distribution")
  if(nulldist=="boot.qt" & robust==TRUE) stop("Quantile transform method requires parametric marginal nulldist.  Set robust=FALSE")
  if(nulldist=="boot.qt" & standardize==FALSE) stop("Quantile transform method requires standardized test statistics.  Set standardize=TRUE")
  if(nulldist=="ic" & robust==TRUE) stop("Influence curve null distributions available only for (parametric) t-statistics.  Set robust=FALSE")
  if(nulldist=="ic" & standardize==FALSE) stop("Influence curve null distributions available only for (standardized) t-statistics.  Set standardize=TRUE")
  if(nulldist=="ic" & (test=="f" | test=="f.twoway" | test=="f.block" | test=="coxph.YvsXZ")) stop("Influence curve null distributions available only for tests of mean, regression and correlation parameters. Cox PH also not yet implemented.")
  if(nulldist!="ic" & (test=="t.cor" | test=="z.cor")) stop("Tests of correlation parameters currently only implemented for influence curve null distributions")
  if((test!="t.cor" & test!="z.cor") & keep.index) warning("Matrix of indices only returned for tests of correlation parameters")
  ### specifically for sampling null test statistics with IC nulldist
  MVNS <- c("mvrnorm","Cholesky")
  MVN.method <- MVNS[pmatch(MVN.method,MVNS)]
  if(is.na(MVN.method)) stop("Invalid sampling method for IC-based MVN null test statistics.  Try either 'mvrnorm' or 'Cholesky'")
  #methods
  METHODS<-c("ss.maxT","ss.minP","sd.maxT","sd.minP")
  method<-METHODS[pmatch(method,METHODS)]
  if(is.na(method)) stop(paste("Invalid method, try one of ",METHODS,sep=""))
  #estimate and conf.reg
  ftest<-FALSE
  if(test=="f" | test=="f.block"){
    ftest<-TRUE
    if(get.cr) stop("Confidence intervals not available for F tests, try get.cr=FALSE")
    if(!is.null(W)) warning("Weighted F tests not yet implemented, proceding with unweighted version")
  }

  
  #permutation null distribution - self contained in this if statement
  if(nulldist=="perm"){
    if(method=="ss.minP" | method=="ss.maxT") stop("Only step-down procedures are currently available with permutation nulldist")
    if(smooth.null) warning("Kernal density p-values not available with permutation nulldist")
    if(get.cr) warning("Confidence regions not available with permutation nulldist")
    if(get.cutoff) warning("Cut-offs not available with permutation nulldist")
    #if(keep.nulldist) warning("keep.nulldist not available with permutation nulldist")
    ptest<-switch(test,
                  t.onesamp=stop("One sample t-test not available with permutation nulldist"),
                  t.twosamp.equalvar=ifelse(robust,"wilcoxon","t.equalvar"),
                  t.twosamp.unequalvar="t",
                  t.pair="pairt",
                  f="f",
                  f.block="blockf",
                  f.twoway=stop("f.twoway not available with permutation nulldist"),
                  lm.XvsZ=stop("lm.XvsZ not available with permutation nulldist"),
                  lm.YvsXZ=stop("lm.YvsXZ not available with permutation nulldist"),
                  coxph.YvsXZ=stop("coxph.YvsXZ not available with permutation nulldist"),
                  t.cor=stop("t.cor not available with permutation nulldist"),
                  z.cor=stop("z.cor not available with permutation nulldist")
                  )
    pside<-switch(alternative,two.sided="abs",less="lower",greater="upper")
    pnonpara<-
      if(robust)"y"
      else "n"
    if(any(is.na(Y))){
      bad<-is.na(Y)
      Y<-Y[!bad]
      X<-X[,!bad]
      warning("No NAs allowed in Y, these observations have been removed.")
    }
    presult<-switch(method,
                    sd.maxT=mt.maxT(X,classlabel=Y,test=ptest,side=pside,B=B,nonpara=pnonpara),
                    sd.minP=mt.minP(X,classlabel=Y,test=ptest,side=pside,B=B,nonpara=pnonpara)
                    )
    if(typeone=="fwer" & nalpha){
      for(a in 1:nalpha) reject[,a]<-(presult$adjp<=alpha[a])
    }
    if(typeone=="gfwer"){
      presult$adjp<-fwer2gfwer(presult$adjp,k)
      if(nalpha){
        for(a in 1:nalpha) reject[,a]<-(presult$adjp<=alpha[a])
      }
      if(!get.adjp)
        presult$adjp<-vector("numeric",0)
    }
    if(typeone=="tppfp"){
      presult$adjp<-fwer2tppfp(presult$adjp,q)
      if(nalpha){
        for(a in 1:nalpha) reject[,a]<-(presult$adjp<=alpha[a])
      }
      if(!get.adjp)
        presult$adjp<-vector("numeric",0)
    }
    if(typeone=="fdr"){
      temp<-fwer2fdr(presult$adjp,fdr.method,alpha)
      reject<-temp$reject
      if(!get.adjp) presult$adjp<-vector("numeric",0)
      else presult$adjp<-temp$adjp
      rm(temp)
    }			
    #output results
    orig<-order(presult$index)
    if(keep.label) label <- as.numeric(Y)
    else label <- vector("numeric",0)
    out<-new("MTP",statistic=presult$teststat[orig],estimate=vector("numeric",0),sampsize=n,rawp=presult$rawp[orig],adjp=presult$adjp[orig],conf.reg=array(dim=c(0,0,0)),cutoff=matrix(nrow=0,ncol=0),reject=as.matrix(reject[orig,]),rawdist=matrix(nrow=0,ncol=0),nulldist=matrix(nrow=0,ncol=0),nulldist.type="perm",marg.null=vector("character",0),marg.par=matrix(nrow=0,ncol=0),label=label,index=matrix(nrow=0,ncol=0),call=match.call(),seed=vector("integer",0))
  }
  
  else{ # This should apply to all other MTP calls using the bootstrap and IC nulldists.
    if(nulldist=="boot.qt"){ # get parameter vals for quantile transform.
      # Get parameter values for the quantile transformed nulldist
      if(!is.null(marg.par)){
        if(is.matrix(marg.par)) marg.par <- marg.par
        if(is.vector(marg.par)) marg.par <- matrix(rep(marg.par,p),nrow=p,ncol=length(marg.par),byrow=TRUE)
        }
      if(is.null(ncp)) ncp = 0
      if(!is.null(perm.mat)){ 
        if(dim(X)[1]!=dim(perm.mat)[1]) stop("perm.mat must same number of rows as X.")
        }
    
      nstats <- c("t.twosamp.unequalvar","z.cor","lm.XvsZ","lm.YvsXZ","coxph.lmYvsXZ")
      tstats <- c("t.onesamp","t.twosamp.equalvar","t.pair","t.cor")
      fstats <- c("f","f.block","f.twoway")
      
      # If default , set values of marg.null to pass on.
      if(is.null(marg.null)){
	  if(any(nstats == test)) marg.null="normal"
	  if(any(tstats == test)) marg.null="t"
	  if(any(fstats == test)) marg.null="f"
        }
      else{ # Check to see that user-supplied entries make sense.  
        MARGS <- c("normal","t","f","perm")
        marg.null <- MARGS[pmatch(marg.null,MARGS)]
        if(is.na(marg.null)) stop("Invalid marginal null distribution. Try one of: normal, t, f, or perm")
        if(any(tstats==test) & marg.null == "f") stop("Choice of test stat and marginal nulldist do not match")
        if(any(fstats==test) & (marg.null == "normal" | marg.null=="t")) stop("Choice of test stat and marginal nulldist do not match")
        if(marg.null=="perm" & is.null(perm.mat)) stop("Must supply a matrix of permutation test statistics if marg.null='perm'")
        if(marg.null=="f" & ncp < 0) stop("Cannot have negative noncentrality parameter with F distribution.")
      }
    
      # If default (=NULL), set values of marg.par. Return as m by 1 or 2 matrix.
      if(is.null(marg.par)){
		marg.par <- switch(test,
                          t.onesamp = n-1,
                          t.twosamp.equalvar = n-2,
                          t.twosamp.unequalvar = c(0,1),
                          t.pair = floor(n/2-1),
                          f = c(length(is.finite(unique(Y)))-1,dim(X)[2]- length(is.finite(unique(Y))) ),
                          f.twoway = {
                            c(length(is.finite(unique(Y)))-1, dim(X)[2]-(length(is.finite(unique(Y)))*length(gregexpr('12', paste(Y, collapse=""))[[1]]))-2)
                            },
                          lm.XvsZ = c(0,1),
                          lm.YvsXZ = c(0,1),
                          coxph.YvsXZ = c(0,1),
                          t.cor = n-2,
                          z.cor = c(0,1)
                          )
      marg.par <- matrix(rep(marg.par,dim(X)[1]),nrow=dim(X)[1],ncol=length(marg.par),byrow=TRUE)
              }
     else{ # Check that user-supplied values of marg.par make sense (marg.par != NULL)
       if((marg.null=="t" | marg.null=="f") & any(marg.par[,1]==0)) stop("Cannot have zero df with t or F distributions. Check marg.par settings")
       if(marg.null=="t" & dim(marg.par)[2]>1) stop("Too many parameters for t distribution.  marg.par should have length 1.")
       if((marg.null=="f" | marg.null=="normal") & dim(marg.par)[2]!=2) stop("Incorrect number of parameters defining marginal null distribution.  marg.par should have length 2.")
     }
    }

    ##making a closure for the particular test
    theta0<-0
    tau0<-1
    stat.closure<-switch(test,
                         t.onesamp=meanX(psi0,na.rm,standardize,alternative,robust),
                         t.twosamp.equalvar=diffmeanX(Y,psi0,var.equal=TRUE,na.rm,standardize,alternative,robust),
                         t.twosamp.unequalvar=diffmeanX(Y,psi0,var.equal=FALSE,na.rm,standardize,alternative,robust),
                         t.pair={
                           uY<-sort(unique(Y))
                           if(length(uY)!=2) stop("Must have two class labels for this test")
                           if(trunc(ncol(X)/2)!=ncol(X)/2) stop("Must have an even number of samples for this test")
                           X<-X[,Y==uY[2]]-X[,Y==uY[1]]
                           Y<-NULL
                           n<-dim(X)[2]
                           meanX(psi0,na.rm,standardize,alternative,robust)
                         },
                         f={
                           theta0<-1
                           tau0<-2/(length(unique(Y))-1)
                           FX(Y,na.rm,robust)
                         },
                         f.twoway={
                           theta0<-1
                           tau0 <- 2/((length(unique(Y))*length(gregexpr('12', paste(Y, collapse=""))[[1]]))-1)
                           twowayFX(Y,na.rm,robust)
                         },
                         lm.XvsZ=lmX(Z,n,psi0,na.rm,standardize,alternative,robust),
                         lm.YvsXZ=lmY(Y,Z,n,psi0,na.rm,standardize,alternative,robust),
                         coxph.YvsXZ=coxY(Y,Z,psi0,na.rm,standardize,alternative),
                         t.cor=NULL,
                         z.cor=NULL)
    ##computing observed test statistics
    if(test=="t.cor" | test=="z.cor") obs<-corr.Tn(X,test=test,alternative=alternative,use="pairwise")
    else obs<-get.Tn(X,stat.closure,W)
    ##or computing influence curves
    if(nulldist=="ic"){
      rawdistn <- matrix(nrow=0,ncol=0)
      nulldistn<-switch(test,
                        t.onesamp=corr.null(X,W,Y,Z,test="t.onesamp",alternative,use="pairwise",B,MVN.method,penalty,ic.quant.trans,marg.null,marg.par,perm.mat),
                        t.pair=corr.null(X,W,Y,Z,test="t.pair",alternative,use="pairwise",B,MVN.method,penalty,ic.quant.trans,marg.null,marg.par,perm.mat),
                        t.twosamp.equalvar=corr.null(X,W,Y,Z,test="t.twosamp.equalvar",alternative,use="pairwise",B,MVN.method,penalty,ic.quant.trans,marg.null,marg.par,perm.mat),
                        t.twosamp.unequalvar=corr.null(X,W,Y,Z,test="t.twosamp.unequalvar",alternative,use="pairwise",B,MVN.method,penalty,ic.quant.trans,marg.null,marg.par,perm.mat),
                        lm.XvsZ=corr.null(X,W,Y,Z,test="lm.XvsZ",alternative,use="pairwise",B,MVN.method,penalty,ic.quant.trans,marg.null,marg.par,perm.mat),
                        lm.YvsXZ=corr.null(X,W,Y,Z,test="lm.YvsXZ",alternative,use="pairwise",B,MVN.method,penalty,ic.quant.trans,marg.null,marg.par,perm.mat),
                        t.cor=corr.null(X,W,Y,Z,test="t.cor",alternative,use="pairwise",B,MVN.method,penalty,ic.quant.trans,marg.null,marg.par,perm.mat),
                        z.cor=corr.null(X,W,Y,Z,test="z.cor",alternative,use="pairwise",B,MVN.method,penalty,ic.quant.trans,marg.null,marg.par,perm.mat)
                        )
    }

    ## Cluster Checking
    if ((!is.numeric(cluster))&(!inherits(cluster,c("MPIcluster", "PVMcluster", "SOCKcluster"))))
       stop("Cluster argument must be integer or cluster object")
    ## Create cluster if cluster > 1 and load required packages on nodes
    if(is.numeric(cluster)){
      if(cluster>1){
    ## Check installation of packages
      have_snow <- qRequire("snow")
      if(!have_snow) stop("The package snow is required to use a cluster. Either snow is not installed or it is not in the standard library location.")
      if (is.null(type))
         stop("Must specify type argument to use a cluster. Alternatively, provide a cluster object as the argument to cluster.")
      if (type=="SOCK")
         stop("Create desired cluster and specify cluster object as the argument to cluster directly.")
      if ((type!="PVM")&(type!="MPI"))
         stop("Type must be MPI or PVM")
      else if (type=="MPI"){
         have_rmpi <- qRequire("Rmpi")
         if(!have_rmpi) stop("The package Rmpi is required for the specified type. Either Rmpi is not installed or it is not in the standard library location.")
      }
      else if (type=="PVM"){
         have_rpvm <- qRequire("rpvm")
         if(!have_rpvm) stop("The package rpvm is required for the specified type. Either rpvm is not installed or it is not in the standard library location.")
      }
      cluster <- makeCluster(cluster, type)
      clusterEvalQ(cluster, {library(Biobase); library(multtest)})
      if (is.null(dispatch)) dispatch=0.05
      }
    }
    else if(inherits(cluster,c("MPIcluster", "PVMcluster", "SOCKcluster"))){
      clusterEvalQ(cluster, {library(Biobase); library(multtest)})
      if (is.null(dispatch)) dispatch=0.05
    }

    ##computing the nonparametric bootstrap (null) distribution
    if(nulldist=="boot.cs" | nulldist=="boot.ctr" | nulldist=="boot.qt"){
      nulldistn<-boot.null(X,Y,stat.closure,W,B,test,nulldist,theta0,tau0,marg.null,marg.par,ncp,perm.mat,alternative,seed,cluster,dispatch,keep.nulldist,keep.rawdist)
     if(inherits(cluster,c("MPIcluster", "PVMcluster", "SOCKcluster")))  stopCluster(cluster)
    rawdistn <- nulldistn$rawboot
    nulldistn <- nulldistn$muboot
    }

    
    ##performing multiple testing
    #rawp values
    rawp<-apply((obs[1,]/obs[2,])<=nulldistn,1,mean)
    if(smooth.null & (min(rawp,na.rm=TRUE)==0)){
      zeros<-(rawp==0)
      if(sum(zeros)==1){
        den<-density(nulldistn[zeros,],to=max(obs[1,zeros]/obs[2,zeros],nulldist[zeros,],na.rm=TRUE),na.rm=TRUE)
	rawp[zeros]<-sum(den$y[den$x>=(obs[1,zeros]/obs[2,zeros])])/sum(den$y)
      }
      else{
        den<-apply(nulldistn[zeros,],1,density,to=max(obs[1,zeros]/obs[2,zeros],nulldistn[zeros,],na.rm=TRUE),na.rm=TRUE)
	newp<-NULL
	stats<-obs[1,zeros]/obs[2,zeros]
	for(i in 1:length(den)){
          newp[i]<-sum(den[[i]]$y[den[[i]]$x>=stats[i]])/sum(den[[i]]$y)
	}
        rawp[zeros]<-newp		
      }
      rawp[rawp<0]<-0
    }
    #c, cr, adjp values
    pind<-ifelse(typeone!="fwer",TRUE,get.adjp)
    if(method=="ss.maxT") out<-ss.maxT(nulldistn,obs,alternative,get.cutoff,get.cr,pind,alpha)
    if(method=="ss.minP") out<-ss.minP(nulldistn,obs,rawp,alternative,get.cutoff,get.cr,pind,alpha)
    if(method=="sd.maxT") out<-sd.maxT(nulldistn,obs,alternative,get.cutoff,get.cr,pind,alpha)
    if(method=="sd.minP") out<-sd.minP(nulldistn,obs,rawp,alternative,get.cutoff,get.cr,pind,alpha)
    if(typeone=="fwer" & nalpha & (test!="t.cor" & test !="z.cor")){
      for(a in 1:nalpha) reject[,a]<-(out$adjp<=alpha[a])
    }
    #augmentation procedures
    if(typeone=="gfwer"){
      out$adjp<-as.numeric(fwer2gfwer(out$adjp,k))
      out$c<-matrix(nrow=0,ncol=0)
      out$cr<-array(dim=c(0,0,0))
      if(nalpha){
        for(a in 1:nalpha) reject[,a]<-(out$adjp<=alpha[a])
      }
      if(!get.adjp) out$adjp<-vector("numeric",0)
    }
    if(typeone=="tppfp"){
      out$adjp<-as.numeric(fwer2tppfp(out$adjp,q))
      out$c<-matrix(nrow=0,ncol=0)
      out$cr<-array(dim=c(0,0,0))
      if(nalpha){
        for(a in 1:nalpha) reject[,a]<-(out$adjp<=alpha[a])
      }
      if(!get.adjp) out$adjp<-vector("numeric",0)
    }
    if(typeone=="fdr"){
      out$c<-matrix(nrow=0,ncol=0)
      out$cr<-array(dim=c(0,0,0))
      temp<-fwer2fdr(out$adjp,fdr.method,alpha)
      reject<-temp$reject
      if(!get.adjp) out$adjp<-vector("numeric",0)
      else out$adjp<-temp$adjp
      rm(temp)
    }
    #output results
    if(!keep.nulldist) nulldistn<-matrix(nrow=0,ncol=0)
    if(!keep.rawdist) rawdist<-matrix(nrow=0,ncol=0)
    if(nulldist!="boot.qt"){  
      marg.null <- vector("character")
      marg.par <- matrix(nrow=0,ncol=0)
    }
    if(!keep.label) label <- vector("numeric",0)
    if(!keep.index) index <- matrix(nrow=0,ncol=0)
    if(test!="z.cor" & test !="t.cor") index <- matrix(nrow=0,ncol=0)
    if(keep.index & (test!="z.cor" | test !="t.cor")){
      index <- t(combn(p,2))
      colnames(index) <- c("Var1","Var2")
    }
    names(out$adjp)<-names(rawp)
    estimates <- obs[3,]*obs[1,]
    if(ftest) estimates <- vector("numeric",0)
    if(test=="t.onesamp" | test=="t.pair") estimates <- obs[3,]*obs[1,]/sqrt(n)
    out<-new("MTP",statistic=(obs[3,]*obs[1,]/obs[2,]),
      estimate=estimates,
      sampsize=n,rawp=rawp,adjp=out$adjp,conf.reg=out$cr,cutoff=out$c,reject=reject,
      rawdist=rawdistn,nulldist=nulldistn,nulldist.type=nulldist,
      marg.null=marg.null,marg.par=marg.par,label=label,index=index,
      call=match.call(),seed=as.integer(seed))
  }
  return(out)
}

#funtions to compute cutoffs and adjusted pvals

ss.maxT<-function(null,obs,alternative,get.cutoff,get.cr,get.adjp,alpha=0.05){
  p<-dim(null)[1]
  B<-dim(null)[2]
  nalpha<-length(alpha)
  mT<-apply(null,2,max)
  getc<-matrix(nrow=0,ncol=0)
  getcr<-array(dim=c(0,0,0))
  getp<-vector(mode="numeric")
  if(get.cutoff | get.cr){
    getc<-array(dim=c(p,nalpha),dimnames=list(dimnames(null)[[1]],paste("alpha=",alpha,sep="")))
    if(get.cr) getcr<-array(dim=c(p,2,nalpha),dimnames=list(dimnames(null)[[1]],c("LB","UB"),paste("alpha=",alpha,sep="")))
    for(a in 1:nalpha){
      getc[,a]<-rep(quantile(mT,pr=(1-alpha[a])),p)
      if(get.cr) getcr[,,a]<-cbind(ifelse(rep(alternative=="less",p),rep(-Inf,p),obs[3,]*obs[1,]-getc[,a]*obs[2,]),ifelse(rep(alternative=="greater",p),rep(Inf,p),obs[3,]*obs[1,]+getc[,a]*obs[2,]))
    }
  }
  if(get.adjp) getp<-apply((obs[1,]/obs[2,])<=matrix(mT,nrow=p,ncol=B,byrow=TRUE),1,mean)
  if(!get.cutoff) getc<-matrix(nrow=0,ncol=0)
  list(c=getc,cr=getcr,adjp=getp)
}

ss.minP<-function(null,obs,rawp,alternative,get.cutoff,get.cr,get.adjp,alpha=0.05){
  p<-dim(null)[1]
  B<-dim(null)[2]
  nalpha<-length(alpha)
  getc<-matrix(nrow=0,ncol=0)
  getcr<-array(dim=c(0,0,0))
  getp<-vector(mode="numeric")
  R<-apply(null,1,rank)
  if(get.cutoff | get.cr){
    getc<-array(dim=c(p,nalpha),dimnames=list(dimnames(null)[[1]],paste("alpha=",alpha,sep="")))
    if(get.cr) getcr<-array(dim=c(p,2,nalpha),dimnames=list(dimnames(null)[[1]],c("LB","UB"),paste("alpha=",alpha,sep="")))
    for(a in 1:nalpha){
      q<-quantile(apply(R,1,max),1-alpha[a])
      for(j in 1:p){
        getc[j,a]<-min(c(null[j,R[,j]>=q],max(null[j,])))
      }
      if(get.cr) getcr[,,a]<-cbind(ifelse(rep(alternative=="less",p),rep(-Inf,p),obs[3,]*obs[1,]-getc[,a]*obs[2,]),ifelse(rep(alternative=="greater",p),rep(Inf,p),obs[3,]*obs[1,]+getc[,a]*obs[2,]))
    }
  }
  if(get.adjp){
    R<-matrix(apply((B+1-R)/B,1,min),nrow=p,ncol=B,byrow=TRUE)
    getp<-apply(rawp>=R,1,mean)
  }
  if(!get.cutoff) getc<-matrix(nrow=0,ncol=0)
  list(c=getc,cr=getcr,adjp=getp)
}

sd.maxT<-function(null,obs,alternative,get.cutoff,get.cr,get.adjp,alpha=0.05){
  p<-dim(null)[1]
  B<-dim(null)[2]
  nalpha<-length(alpha)
  ord<-rev(order(obs[1,]/obs[2,]))
  mT<-null[ord[p],]
  getc<-matrix(nrow=0,ncol=0)
  getcr<-array(dim=c(0,0,0))
  getp<-vector(mode="numeric")
  if(get.cutoff | get.cr){
    getc<-array(dim=c(p,nalpha),dimnames=list(dimnames(null)[[1]],paste("alpha=",alpha,sep="")))
    for(a in 1:nalpha) getc[ord[p],a]<-quantile(mT,pr=1-alpha[a])
  }
  if(get.adjp) getp[ord[p]]<-mean((obs[1,]/obs[2,])[ord[p]]<=mT)
  for(j in (p-1):1){
    mT<-pmax(mT,null[ord[j],])
    if(get.adjp) getp[ord[j]]<-mean((obs[1,ord[j]]/obs[2,ord[j]])<=mT)
    if(get.cutoff | get.cr){
      for(a in 1:nalpha) getc[ord[j],a]<-quantile(mT,pr=(1-alpha[a]))
    }
  }
  c.ind<-rep(TRUE,nalpha)
  for(j in 2:p){
    if(get.adjp) getp[ord[j]]<-max(getp[ord[j]],getp[ord[j-1]])
    if(get.cutoff | get.cr){
      for(a in 1:nalpha){
        if(c.ind[a]){
          if((obs[1,]/obs[2,])[ord[j-1]]<=getc[ord[j-1],a]){
            getc[ord[j:p],a]<-Inf
            c.ind[a]<-FALSE
          }
	}
      }
    }
  }
  if(get.cr){
    getcr<-array(dim=c(p,2,nalpha),dimnames=list(dimnames(null)[[1]],c("LB","UB"),paste("alpha=",alpha,sep="")))
    for(a in 1:nalpha){
      getcr[,,a]<-cbind(ifelse(rep(alternative=="less",p),rep(-Inf,p),obs[3,]*obs[1,]-getc[,a]*obs[2,]),ifelse(rep(alternative=="greater",p),rep(Inf,p),obs[3,]*obs[1,]+getc[,a]*obs[2,]))
    }
  }
  if(!get.cutoff) getc<-matrix(nrow=0,ncol=0)
  list(c=getc,cr=getcr,adjp=getp)
}

sd.minP<-function(null,obs,rawp,alternative,get.cutoff,get.cr,get.adjp,alpha=0.05){
  p<-dim(null)[1]
  B<-dim(null)[2]
  nalpha<-length(alpha)
  ord<-order(rawp)
  R<-apply(null,1,rank) #B x p
  mR<-R[,ord[p]]
  getc<-matrix(nrow=0,ncol=0)
  getcr<-array(dim=c(0,0,0))
  getp<-vector(mode="numeric")
  if(get.cutoff | get.cr){
    getc<-array(dim=c(p,nalpha),dimnames=list(dimnames(null)[[1]],paste("alpha=",alpha,sep="")))
    for(a in 1:nalpha){
      q<-quantile(mR,pr=1-alpha[a])
      getc[ord[p],a]<-min(c(null[ord[p],R[,ord[p]]>=q],max(null[ord[p],])))
    }
  }
  if(get.adjp){
    mP<-(B+1-mR)/B
    getp[ord[p]]<-mean(rawp[ord[p]]>=mP)
  }
  for(j in (p-1):1){
    mR<-pmax(mR,R[,ord[j]])
    if(get.adjp){
      mP<-(B+1-mR)/B
      getp[ord[j]]<-mean(rawp[ord[j]]>=mP)
    }
    if(get.cutoff | get.cr){
      for(a in 1:nalpha){
        q<-quantile(mR,pr=1-alpha[a])
	getc[ord[j],a]<-min(c(null[ord[j],R[,ord[j]]>=q],max(null[ord[j],])))
      }
    }
  }
  c.ind<-rep(TRUE,nalpha)
  for(j in 2:p){
    if(get.adjp) getp[ord[j]]<-max(getp[ord[j]],getp[ord[j-1]])
    if(get.cutoff | get.cr){
      for(a in 1:nalpha){
        if(c.ind[a]){
          if((obs[1,]/obs[2,])[ord[j-1]]<=getc[ord[j-1],a]){
            getc[ord[j:p],a]<-Inf
            c.ind[a]<-FALSE
          }
	}
      }
    }
  }
  if(get.cr){
    getcr<-array(dim=c(p,2,nalpha),dimnames=list(dimnames(null)[[1]],c("LB","UB"),paste("alpha=",alpha,sep="")))
    for(a in 1:nalpha){
      getcr[,,a]<-cbind(ifelse(rep(alternative=="less",p),rep(-Inf,p),obs[3,]*obs[1,]-getc[,a]*obs[2,]),ifelse(rep(alternative=="greater",p),rep(Inf,p),obs[3,]*obs[1,]+getc[,a]*obs[2,]))
    }
  }
  if(!get.cutoff) getc<-matrix(nrow=0,ncol=0)
  list(c=getc,cr=getcr,adjp=getp)
}

#functions to convert FWER adjp to AMTP (gFWER, TPPFP) adjp:
fwer2gfwer<-function(adjp,k=0){
  ord<-order(adjp)
  m<-length(adjp)
  if(any(k>=m)) stop(paste("number of rejections k=",k," must be less than number of hypotheses=",m,sep=""))
  newp<-NULL
  for(j in k) newp<-rbind(newp,c(rep(0,j),adjp[ord[1:(m-j)]]))
  rownames(newp)<-k
  colnames(newp)<-ord
  newp<-matrix(newp[,order(ord)],ncol=m,byrow=FALSE)
  return(t(newp))
}

fwer2tppfp<-function(adjp,q=0.05){
  ord<-order(adjp)
  m<-length(adjp)
  newp<-NULL
  if(any(q>1)|any(q<0)) stop(paste("proportion of false positives q=",q," must be in [0,1]",sep=""))
  for(l in q) newp<-rbind(newp,adjp[ord][ceiling((1:m)*(1-l))])
  rownames(newp)<-q
  colnames(newp)<-names(ord)
  newp<-matrix(newp[,order(ord)],ncol=m,byrow=FALSE)
  return(t(newp))
}

#function to compute rejection indicator for FDR methods
fwer2fdr<-function(adjp,method="both",alpha=0.05){
  get.cons<-function(adjp,alpha,ord,M,nalpha){
    newp<-NULL
    for(m in 1:M){
      #try ceiling/floor
      k<-if(m%%2) 0:((m-1)/2) else 0:(m/2)
      f<-2*adjp[ord][m-k]
      u<-2*(k+1)/m
      l<-2*k/m
      if(sum(f<=u)){
        ind<-min(which(f<=u))
	newp[ord[m]]<-
	if(f[ind]>=l[ind]) f[ind]
	else l[ind]
      }
      else newp[ord[m]]<-1
    }
    newp[newp>1]<-1
    a<-alpha/2
    rejections<-matrix(nrow=M,ncol=nalpha)
    for(i in 1:nalpha) rejections[,i]<-(fwer2tppfp(adjp,a[i])<=a[i])
    return(list(reject=rejections,adjp=newp))
  }
  get.restr<-function(adjp,alpha,ord,M,nalpha){
    newp<-NULL
    ginv<-function(x) 1-(1-x)^2
    for(m in 1:M){
      k<-m:1
      f<-adjp[ord][k]
      u<-1-(k-1)/m
      l<-1-k/m
      if(sum(f<=u)){
        ind<-min(which(f<=u))
	newp[ord[m]]<-
          if(f[ind]>=l[ind]) ginv(f[ind])
          else ginv(l[ind])
      }
      else
	newp[ord[m]]<-1
    }
    newp[newp>1]<-1
    a<-1-sqrt(1-alpha)
    rejections<-matrix(nrow=M,ncol=nalpha)
    for(i in 1:nalpha) rejections[,i]<-(fwer2tppfp(adjp,a[i])<=a[i])
    return(list(reject=rejections,adjp=newp))
  }
  ord<-order(adjp)
  nalpha<-length(alpha)
  M<-length(adjp)
  if(method=="both"){
    rejections<-array(dim=c(M,nalpha,2),dimnames=list(NULL,paste("alpha=",alpha,sep=""),c("conservative","restricted")))
    newp<-matrix(nrow=M,ncol=2,dimnames=list(NULL,c("conservative","restricted")))
    temp<-get.cons(adjp,alpha,ord,M,nalpha)
    rejections[,,"conservative"]<-temp$reject
    newp[,"conservative"]<-temp$adjp
    temp<-get.restr(adjp,alpha,ord,M,nalpha)
    rejections[,,"restricted"]<-temp$reject
    newp[,"restricted"]<-temp$adjp
    rm(temp)
  }
  else{
    rejections<-matrix(nrow=M,ncol=nalpha,dimnames=list(NULL,paste("alpha=",alpha,sep="")))
    newp<-NULL
    if(method=="conservative") temp<-get.cons(adjp,alpha,ord,M,nalpha)
    else temp<-get.restr(adjp,alpha,ord,M,nalpha)
    rejections<-temp$reject
    newp<-temp$adjp
    rm(temp)
  }
  return(list(reject=rejections,adjp=newp))
}
