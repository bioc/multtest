setClass("MTP",representation(statistic="numeric",
                              estimate="numeric",
                              sampsize="numeric",
                              rawp="numeric",
                              adjp="numeric",
                              conf.reg="array",
                              cutoff="matrix",
                              reject="matrix",
                              nulldist="matrix",
                              call="call",
                              seed="integer"),
         prototype=list(statistic=vector("numeric",0),
         estimate=vector("numeric",0),
         sampsize=vector("numeric",0),
         rawp=vector("numeric",0),
         adjp=vector("numeric",0),
         conf.reg=array(),
         cutoff=matrix(nr=0,nc=0),
         reject=matrix(nr=0,nc=0),
         nulldist=matrix(nr=0,nc=0),
         call=NULL,
         seed=vector("integer",0)))

setMethod("print","MTP",
          function(x,...){
              call.list<-as.list(x@call)
              cat("\n")
              writeLines(strwrap("Multiple Testing Procedure",prefix="\t"))
              cat("\n")
              cat(paste("Object of class: ",class(x)))
              cat("\n")
              cat(paste("sample size =",x@sampsize,"\n"))
              cat(paste("number of hypotheses =",length(x@statistic),"\n"))
              cat("\n")
              cat(paste("test statistics =",ifelse(is.null(call.list$test),"t.twosamp.unequalvar",call.list$test),"\n"))
              cat(paste("type I error rate =",ifelse(is.null(call.list$typeone),"fwer",call.list$typeone),"\n"))
              nominal<-eval(call.list$alpha)
              if(is.null(eval(call.list$alpha)))
                  nominal<-0.05
              cat("nominal level alpha = ")
              cat(nominal,"\n")
              cat(paste("multiple testing procedure =",ifelse(is.null(call.list$method),"ss.maxT",call.list$method),"\n"))
              cat("\n")
              cat("Call: ")
              print(x@call)
              cat("\n")
              cat("Slots: \n")
              snames<-slotNames(x)
              n<-length(snames)
              out<-matrix(nr=n,nc=4)
              dimnames(out)<-list(snames,c("Class","Mode","Length","Dimension"))
              for(s in snames)
                  out[s,]<-c(class(slot(x,s)),mode(slot(x,s)),length(slot(x,s)),paste(dim(slot(x,s)),collapse=","))
              out<-data.frame(out)
              print(out)
              invisible(x)
          })

setMethod("plot","MTP",
          function(x,y="missing",which=1:4,caption=c("Rejections vs. Error Rate",
                                           "Ordered Adjusted p-values","Adjusted p-values vs. Statistics",
                                           "Unordered Adjusted p-values","Estimates & Confidence Regions",
                                           "Test Statistics & Cut-offs"),sub.caption = deparse(x@call,width.cutoff=500),
                   ask = prod(par("mfcol"))<length(which)&&dev.interactive(),
                   logscale=FALSE,top=10,...){
              call.list<-as.list(x@call)
              if(!inherits(x,"MTP"))
                  stop("Use only with 'MTP' objects")
              if(is.null(which))
                  which<-1:6
              if(length(x@adjp)==0 & any(which))
                  stop("Plot methods require adjusted p-values")
              if(length(x@conf.reg)==0 & any(which==5))
                  stop("plot method 5 requires confidence regions")
              if(length(x@cutoff)==0 & any(which==6))
                  stop("plot method 6 requires cut-offs")
              if(!is.numeric(which) || any(which<1) || any(which>6))
                  stop("which must be in 1:6")
              show<-rep(FALSE,6)
              show[which]<-TRUE
  			m<-length(x@adjp)
              if(top>m)
                  stop("number of top hypotheses to plot exceeds total number of hypotheses")
              ord<-order(x@adjp)
              if(any(show[2:4]) & logscale){
                  pv<-(-log(x@adjp,10))
                  pvlab<-"-log (base 10) Adjusted p-values"
              }
              else{
                  pv<-x@adjp
                  pvlab<-"Adjusted p-values"
              }
              one.fig<-prod(par("mfcol"))==1
              if(ask){
                  op<-par(ask=TRUE)
                  on.exit(par(op))
              }
              if(show[1]){
                  nominal<-seq(0,1,by=0.05)
                  r<-mt.reject(x@adjp,nominal)$r
                  matplot(nominal,r,xlab="Type I error rate",
                          ylab="Number of rejected hypotheses",
                          type="l",...)
                  if(one.fig)
                      title(sub=sub.caption,cex.sub=0.5,...)
                  mtext(caption[1],3,0.25)
              }
              if(show[2]){
                  spval<-sort(pv)
                  matplot(1:m,spval,xlab="Number of rejected hypotheses",
                          ylab=paste("Sorted",pvlab,sep=" "),type="l",...)
                  if(one.fig)
                      title(sub=sub.caption,cex.sub=0.5,...)
                  mtext(caption[2],3,0.25)
              }
              if(show[3]){
                  symb<-ifelse(length(pv)<100,"o",".")
                  matplot(x@statistic,pv,xlab="Test statistics",
                          ylab=pvlab,type="p",pch=symb,...)
                  if(one.fig)
                      title(sub=sub.caption,cex.sub=0.5,...)
                  mtext(caption[3],3,0.25)
              }
              if(show[4]){
                  matplot(1:m,pv,xlab="Index",ylab=pvlab,type = "l", ...)
                  if(one.fig)
                      title(sub=sub.caption,cex.sub=0.5,...)
                  mtext(caption[4],3,0.25)
              }
              if(show[5]){
                  if(is.null(call.list$test))
                      call.list$test<-"t.twosamp.unequalvar"
                  if(call.list$test=="f" | call.list$test=="f.block")
                      stop("Plot 5 requires confidence intervals, which are not available with F tests")
                  if(top>length(x@adjp))
                      top<-length(x@adjp)
                  topp<-order(x@adjp)[1:top]
                  plot(c(1,top),range(c(x@estimate[topp],x@conf.reg[topp,,])),type="n",main=paste("Top",top,"Hypotheses",sep=" "),xlab="Hypotheses",ylab="Estimates")
                  points(1:top,x@estimate[topp],pch="o")
                  nominal<-eval(call.list$alpha)
                  if(is.null(nominal))
                      nominal<-0.05
                  for(a in 1:length(nominal)){
                      text(1:top,x@conf.reg[topp,1,a],nominal[a])
                      text(1:top,x@conf.reg[topp,2,a],nominal[a])
                  }
                  if(one.fig)
                      title(sub=sub.caption,cex.sub=0.5,...)
                  mtext(caption[5],3,0.25)
              }
              if(show[6]){
                  if(top>length(x@adjp))
                      top<-length(x@adjp)
                  topp<-order(get.index(adjp=x@adjp,rawp=x@rawp,stat=x@statistic))[1:top]
                  plot(c(1,top),range(c(x@cutoff[topp,],x@statistic[topp])),type="n",main=paste("Top",top,"Hypotheses",sep=" "),xlab="Hypotheses",ylab="Test Statistics")
                  points(1:top,x@statistic[topp],pch="o")
                  nominal<-eval(call.list$alpha)
                  if(is.null(nominal))
                      nominal<-0.05
                  for(a in 1:length(nominal))
                      text(1:top,x@cutoff[topp,a],nominal[a])
                  if(one.fig)
                      title(sub=sub.caption,cex.sub=0.5,...)
                  mtext(caption[6],3,0.25)
              }
              if(!one.fig && par("oma")[3]>=1)
                  mtext(sub.caption,outer=TRUE,cex=0.8)
              invisible()
          })

setMethod("summary","MTP",
          function(object,...){
              call.list<-as.list(object@call)
              cat(paste("MTP: ",ifelse(is.null(call.list$method),"ss.maxT",call.list$method),"\n"))
              err<-ifelse(is.null(call.list$typeone),"fwer",call.list$typeone)
              if(err=="gfwer")
                  err<-paste(err," (k=",ifelse(is.null(call.list$k),0,call.list$k),")",sep="")
              if(err=="tppfp")
				err<-paste(err," (q=",ifelse(is.null(call.list$q),0.1,call.list$q),")",sep="")
              if(err=="fdr")
                  err<-paste(err," (",ifelse(is.null(call.list$fdr.method),"conservative",call.list$method),")",sep="")
			cat(paste("Type I error rate: ",err,"\n\n"))
              nominal<-eval(call.list$alpha)
              if(is.null(nominal))
                  nominal<-0.05
              out1<-data.frame(Level=nominal,Rejections=apply(object@reject,2,sum),row.names=NULL)
              print(out1)
              cat("\n")
              out2<-get.index(object@adjp,object@rawp,abs(object@statistic))
              out3<-rn<-NULL
              if(!is.null(object@adjp)){
                  out3<-rbind(out3,summary(object@adjp))
                  rn<-c(rn,"adjp")
              }
              if(!is.null(object@rawp)){
                  out3<-rbind(out3,summary(object@rawp))
                  rn<-c(rn,"rawp")
              }
              if(!is.null(object@statistic)){
                  out3<-rbind(out3,summary(object@statistic))
				rn<-c(rn,"statistic")
              }
              if(!is.null(object@estimate)){
                  out3<-rbind(out3,summary(object@estimate))
                  rn<-c(rn,"estimate")
              }
			rownames(out3)<-rn
              print(out3)
              invisible(list(rejections=out1,index=out2,summaries=out3))
          })

setMethod("[","MTP",
          function(x,i,j=NULL,...,drop=FALSE){
			if(missing(i))
                            i<-TRUE
			newx<-x
			slot(newx,"statistic")<-x@statistic[i]
			slot(newx,"estimate")<-x@estimate[i]
			slot(newx,"rawp")<-x@rawp[i]
			if(sum(length(x@adjp)))
                            slot(newx,"adjp")<-x@adjp[i]
			d<-dim(x@conf.reg)
			dn<-dimnames(x@conf.reg)
			if(sum(d))
                            slot(newx,"conf.reg")<-array(x@conf.reg[i,,],dim=c(ifelse(i[1]==TRUE & !is.numeric(i),d[1],length(i)),d[-1]),dimnames=list(dn[[1]][i],dn[[2]],dn[[3]]))
			d<-dim(x@cutoff)
			dn<-dimnames(x@cutoff)
			if(sum(d))
                            slot(newx,"cutoff")<-matrix(x@cutoff[i,],nr=ifelse(i[1]==TRUE & !is.numeric(i),d[1],length(i)),nc=d[-1],dimnames=list(dn[[1]][i],dn[[2]]))
			d<-dim(x@reject)
			dn<-dimnames(x@reject)
			if(sum(d))
                            slot(newx,"reject")<-matrix(x@reject[i,],nr=ifelse(i[1]==TRUE & !is.numeric(i),d[1],length(i)),nc=d[-1],dimnames=list(dn[[1]][i],dn[[2]]))
			if(sum(dim(x@nulldist)))
                            slot(newx,"nulldist")<-x@nulldist[i,]
			invisible(newx)
                    })

setMethod("as.list","MTP",
          function(x,...){
              snames<-slotNames(x)
				n<-length(snames)
              lobj<-list()
              for(i in 1:n)
                  lobj[[i]]<-slot(x,snames[i])
              names(lobj)<-snames
              invisible(lobj)
          })



.First.lib<-function(libname,pkgname,where){
    library.dynam("multtest",pkgname,libname)
}

.Last.lib<-function(libpath){
    dyn.unload(
               file.path(libpath,
                         "libs",
                         paste("multtest",
                               .Platform$"dynlib.ext",
                               sep = ""
                               )
                         )
               )
}

#apply function with a weight matrix/vector
#written copying apply, except that X must
# be a matrix and MARGIN must be 1 or 2.
# W is NULL, matrix or vector.

wapply<-function(X,MARGIN,FUN,W=NULL,...){
	if(is.null(W))
		return(apply(X,MARGIN,FUN,...))
	else{
		if(length(MARGIN)!=1)
			stop("length(MARGIN) should be 1")
		if(!(MARGIN==1 || MARGIN==2))
			stop("MARGIN must be 1 or 2")
		FUN<-match.fun(FUN)
	    	X<-as.matrix(X)
		dx<-dim(X)
		if(length(dx)!=2)
        		stop("X must be a matrix")
    		dn<-dimnames(X)
		if(!(is.vector(W) | is.matrix(W)))
			stop("W must be a vector or matrix")
		if(is.vector(W)){
			if(MARGIN==1 & length(W)!=dx[2])
				stop("length(W) not equal to ",dx[2])
			if(MARGIN==2 & length(W)!=dx[1])
				stop("length(W) not equal to ",dx[1])
		}
		if(is.matrix(W) & sum(dx!=dim(W))>0)
			stop("X and W must have the same dimension(s)")
	    	d.call<-dx[-MARGIN]
	    	d.ans<-dx[MARGIN]
	    	dn.call<-dn[-MARGIN]
	    	dn.ans<-dn[MARGIN]
	    	if(is.na(d.ans) || !(d.ans>0))
			stop("dim(X)[",MARGIN,"] is not a positive number")
	    	if(MARGIN==1){
			X<-t(X)
			if(is.matrix(W))
				W<-t(W)
		}
	    	ans<-vector("list",d.ans)
       		if(length(dn.call))
          		dimnames(X)<-c(dn.call,list(NULL))
        	for(i in 1:d.ans){
			if(is.vector(W))
				ans[[i]]<-FUN(X[,i],W,...)
			else
				ans[[i]]<-FUN(X[,i],W[,i],...)
    		}
    		ans.list<-is.recursive(ans[[1]])
    		l.ans<-length(ans[[1]])
    		ans.names<-names(ans[[1]])
    		if(!ans.list)
        		ans.list<-any(unlist(lapply(ans,length))!=l.ans)
    		if(!ans.list && length(ans.names)){
        		all.same<-sapply(ans,function(x) identical(names(x),ans.names))
        		if(!all(all.same))
            			ans.names<-NULL
    		}
	    	len.a<-if(ans.list) d.ans
    			else length(ans<-unlist(ans,recursive=FALSE))
    		if(len.a==d.ans){
	        	names(ans)<-if(length(dn.ans[[1]])) dn.ans[[1]]
        		return(ans)
    		}
    		if(len.a>0 && len.a%%d.ans==0)
	        	return(array(ans,c(len.a%/%d.ans,d.ans),
				if(is.null(dn.ans)){
            				if(!is.null(ans.names))
						list(ans.names,NULL)
				}
				else c(list(ans.names),dn.ans)))
    		return(ans)
	}
}

#function to make a vector for ordering the results by
# adjp, then rawp, then abs(stat)
get.index<-function(adjp,rawp,stat){
	adj<-!is.null(adjp)
	raw<-!is.null(rawp)
	sta<-!is.null(stat)
	if(adj)
		p<-length(adjp)
	else{
		if(raw)
			p<-length(rawp)
		else
			stop("Must have at least one argument")
	}
	if(!sta)
		stat<-rep(1,p)
	if(!raw)
		rawp<-rep(1,p)
	if(!adj)
		adjp<-rep(1,p)
	if((length(adjp)!=length(rawp)) | (length(adjp)!=length(stat)))
		stop("adjp, rawp, and stat must all be the same length")
	index<-rank(adjp)
	d1<-duplicated(index)
	u1<-u2<-NULL
	if(sum(d1)){
		u1<-unique(index[d1])
		for(u in u1){
			sub<-index==u
			i2<-rank(rawp[sub])
			index[sub]<-index[sub]+i2-mean(i2)
			d2<-duplicated(index[sub])
			if(sum(d2))
				u2<-unique(index[sub][d2])
				for(uu in u2){
					sub2<-index==uu
					i3<-length(stat[sub2])-rank(abs(stat[sub2]))+1
					index[sub2]<-index[sub2]+i3-mean(i3)
				}
		}
	}
	if(sum(duplicated(index)))
		warning("indices are not unique")
	if(sum(index)!=sum(1:length(index)))
		warning("indices are not based on true ranks")
	order(index)
}



