jomo.glm.MCMCchainC <-
  function(formula, data, beta.start=NULL, l1cov.start=NULL, l1cov.prior=NULL, betaY.start=NULL, varY.start=NULL, nburn=1000, start.imp=NULL, start.imp.sub=NULL, output=1, out.iter=10, family="binomial") {
    cat("This function is beta software. Use carefully and please report any bug to the package mantainer\n")
    if (family != "gaussian" & family != "binomial")
      cat("ERROR: choose either family binomial or gaussian\n")
    if (family == "gaussian") {
      jomo.lm.MCMCchain(formula, data, start.imp = start.imp,
                        start.imp.sub = start.imp.sub, beta.start = beta.start,
                        l1cov.start = l1cov.start, l1cov.prior = l1cov.prior,
                        betaY.start = betaY.start, varY.start = varY.start,
                        nburn = nburn, output = output, out.iter = out.iter)
    }
    if (family == "binomial") {
      stopifnot(is.data.frame(data))
      stopifnot(any(grepl("~",deparse(formula))))
      fit.cr<-glm(formula,data=data, na.action = na.omit, family=binomial)
      if (is.null(betaY.start)) betaY.start<-coef(fit.cr)
      if (is.null(varY.start)) varY.start<-1
      varY.prior<-1
      colnamysub<-all.vars(formula[[2]])
      Ysub<-get(colnamysub,pos=data)
      Ycov<-data.frame(mget(all.vars(formula[[3]]), envir =as.environment(data)))
      terms.sub<-attr(terms(formula), "term.labels")
      split.terms<-strsplit(terms.sub,":")
      length.sub<-length(terms.sub)
      order.sub<-attr(terms(formula), "order")
      submod<-matrix(1,4,sum(order.sub))
      Y.con<-NULL
      Y.cat<-NULL
      Y.numcat<-NULL
      for (j in 1:ncol(Ycov)) {
        if (is.numeric(Ycov[,j])) {
          if (is.null(Y.con)) Y.con<-data.frame(Ycov[,j,drop=FALSE])
          else Y.con<-data.frame(Y.con,Ycov[,j,drop=FALSE])
        }
        if (is.factor(Ycov[,j])) {
          if (is.null(Y.cat)) Y.cat<-data.frame(Ycov[,j,drop=FALSE])
          else Y.cat<-data.frame(Y.cat,Ycov[,j,drop=FALSE])
          Y.numcat<-cbind(Y.numcat,nlevels(Ycov[,j]))
        }
      }
      h<-1
      for ( j in 1:length.sub) {
        for ( k in 1:order.sub[j]) {
          current.term<-split.terms[[j]][k]
          current.term<-sub(".*I\\(","",current.term)
          current.term<-sub("\\)","",current.term)
          if (grepl("\\^",current.term)) {
            submod[3,h]<-as.integer(sub(".*\\^","",current.term))
            current.term<-sub("\\^.*","",current.term)
          } else {
            submod[3,h]<-1
          }
          if (length(which(colnames(Y.cat)==current.term))!=0) {
            submod[1,h]<-which(colnames(Y.cat)==current.term)
            submod[2,h]<-2
            submod[4,h]<-Y.numcat[submod[1,h]]-1
          } else if (length(which(colnames(Y.con)==current.term))!=0) {
            submod[1,h]<-which(colnames(Y.con)==current.term)
            submod[2,h]<-1
          }
          h<-h+1
        }
      }
      Y.auxiliary<-data.frame(data[,-c(which(colnames(data)%in%colnames(Y.con)),which(colnames(data)%in%colnames(Y.cat)),which(colnames(data)==colnamysub)), drop=FALSE])
      Y.aux.con<-NULL
      Y.aux.cat<-NULL
      Y.aux.numcat<-NULL
      if (ncol(Y.auxiliary)>0) {
        for (j in 1:ncol(Y.auxiliary)) {
          if (is.numeric(Y.auxiliary[,j])) {
            if (is.null(Y.aux.con)) Y.aux.con<-data.frame(Y.auxiliary[,j,drop=FALSE])
            else Y.aux.con<-data.frame(Y.aux.con,Y.auxiliary[,j,drop=FALSE])
          }
          if (is.factor(Y.auxiliary[,j])) {
            if (is.null(Y.aux.cat)) Y.aux.cat<-data.frame(Y.auxiliary[,j,drop=FALSE])
            else Y.aux.cat<-data.frame(Y.aux.cat,Y.auxiliary[,j,drop=FALSE])
            Y.aux.numcat<-cbind(Y.aux.numcat,nlevels(Y.auxiliary[,j]))
          }
        }
      }
      X=matrix(1,max(nrow(Y.cat),nrow(Y.con)),1)
      if (is.null(beta.start)) beta.start=matrix(0,ncol(X),(max(as.numeric(!is.null(Y.con)),ncol(Y.con))+max(0,(sum(Y.numcat)-length(Y.numcat)))+max(as.numeric(!is.null(Y.aux.con)),ncol(Y.aux.con))+max(0,(sum(Y.aux.numcat)-length(Y.aux.numcat)))))

      if (is.null(l1cov.start)) {
        l1cov.start=diag(1,ncol(beta.start))
      }
      if (is.null(l1cov.prior)) l1cov.prior=diag(1,ncol(l1cov.start))
      ncolYcon<-rep(NA,4)
      ncolYcon[1]=max(as.numeric(!is.null(Y.con)),ncol(Y.con))+max(as.numeric(!is.null(Y.aux.con)),ncol(Y.aux.con))
      ncolYcon[2]=max(as.numeric(!is.null(Y.con)),ncol(Y.con))
      ncolYcon[3]=ncolYcon[1]+max(0,(sum(Y.numcat)-length(Y.numcat)))
      ncolYcon[4]=max(0,ncol(Y.cat))
      stopifnot(((!is.null(Y.con))||(!is.null(Y.cat)&!is.null(Y.numcat))))
      Ysub<-as.factor(Ysub)
      previous_levelssub<-levels(Ysub)
      levels(Ysub)<-1:2
      if (!is.null(Y.cat)) {
        isnullcat=0
        previous_levels<-list()
        Y.cat<-data.frame(Y.cat)
        for (i in 1:ncol(Y.cat)) {
          Y.cat[,i]<-factor(Y.cat[,i])
          previous_levels[[i]]<-levels(Y.cat[,i])
          levels(Y.cat[,i])<-1:nlevels(Y.cat[,i])
        }
      } else {
        isnullcat=1
      }
      if (!is.null(Y.aux.cat)) {
        isnullcataux=0
        previous_levelsaux<-list()
        Y.aux.cat<-data.frame(Y.aux.cat)
        for (i in 1:ncol(Y.aux.cat)) {
          Y.aux.cat[,i]<-factor(Y.aux.cat[,i])
          previous_levelsaux[[i]]<-levels(Y.aux.cat[,i])
          levels(Y.aux.cat[,i])<-1:nlevels(Y.aux.cat[,i])
        }
      } else {
        isnullcataux=1
      }

      stopifnot(nrow(beta.start)==ncol(X), ncol(beta.start)==(ncolYcon[1]+max(0,(sum(Y.numcat)-length(Y.numcat)))+max(0,(sum(Y.aux.numcat)-length(Y.aux.numcat)))))
      stopifnot(nrow(l1cov.start)==ncol(l1cov.start),nrow(l1cov.prior)==nrow(l1cov.start),nrow(l1cov.start)==ncol(beta.start))
      stopifnot(nrow(l1cov.prior)==ncol(l1cov.prior))
      betait=matrix(0,nrow(beta.start),ncol(beta.start))
      for (i in 1:nrow(beta.start)) {
        for (j in 1:ncol(beta.start)) betait[i,j]=beta.start[i,j]
      }
      covit=matrix(0,nrow(l1cov.start),ncol(l1cov.start))
      for (i in 1:nrow(l1cov.start)) {
        for (j in 1:ncol(l1cov.start)) covit[i,j]=l1cov.start[i,j]
      }
      if (!is.null(Y.con)) {
        colnamycon<-colnames(Y.con)
        Y.con<-data.matrix(Y.con)
        storage.mode(Y.con) <- "numeric"
      }
      if (isnullcat==0) {
        colnamycat<-colnames(Y.cat)
        Y.cat<-data.matrix(Y.cat)
        storage.mode(Y.cat) <- "numeric"
      }
      if (!is.null(Y.aux.con)) {
        colnamyauxcon<-colnames(Y.aux.con)
        Y.aux.con<-data.matrix(Y.aux.con)
        storage.mode(Y.aux.con) <- "numeric"
      }
      if (isnullcataux==0) {
        colnamyauxcat<-colnames(Y.aux.cat)
        Y.aux.cat<-data.matrix(Y.aux.cat)
        storage.mode(Y.aux.cat) <- "numeric"
      }
      colnamx<-colnames(X)
      X<-data.matrix(X)
      storage.mode(X) <- "numeric"
      Y=cbind(Y.con,Y.aux.con,Y.cat, Y.aux.cat)
      Yi=cbind(Y.con, Y.aux.con, switch(is.null(Y.cat)+1, matrix(0,nrow(Y),(sum(Y.numcat)-length(Y.numcat))), NULL), switch(is.null(Y.aux.cat)+1, matrix(0,nrow(Y.aux.cat),(sum(Y.aux.numcat)-length(Y.aux.numcat))), NULL))
      h=1
      if (isnullcat==0) {
        for (i in 1:length(Y.numcat)) {
          for (j in 1:nrow(Y)) {
            if (is.na(Y.cat[j,i])) {
              Yi[j,(ncolYcon[1]+h):(ncolYcon[1]+h+Y.numcat[i]-2)]=NA
            }
          }
          h=h+Y.numcat[i]-1
        }
      }
      if (isnullcataux==0) {
        for (i in 1:length(Y.aux.numcat)) {
          for (j in 1:nrow(Y)) {
            if (is.na(Y.aux.cat[j,i])) {
              Yi[j,(ncolYcon[1]+h):(ncolYcon[1]+h+Y.aux.numcat[i]-2)]=NA
            }
          }
          h=h+Y.aux.numcat[i]-1
        }
      }
      if (isnullcat==0||isnullcataux==0) {
        Y.cat.tot<-cbind(Y.cat,Y.aux.cat)
        Y.numcat.tot<-c(Y.numcat, Y.aux.numcat)
      } else {
        Y.cat.tot=-999
        Y.numcat.tot=-999
      }
      Ysubimp<-Ysub

      if (output!=1) out.iter=nburn+2
      nimp=1
      imp=matrix(0,nrow(Y)*(nimp+1),ncol(Y)+3)
      imp[1:nrow(Y),1]=Ysub
      imp[1:nrow(Y),2:(1+ncol(Y))]=Y
      imp[1:nrow(X), (ncol(Y)+2)]=c(1:nrow(Y))
      Yimp=Yi
      Yimp2=matrix(Yimp, nrow(Yimp),ncol(Yimp))
      imp[(nrow(X)+1):(2*nrow(X)), (ncol(Y)+2)]=c(1:nrow(Y))
      imp[(nrow(X)+1):(2*nrow(X)), (ncol(Y)+3)]=1
      betapost<- array(0, dim=c(nrow(beta.start),ncol(beta.start),(nburn)))
      betaYpost<- array(0, dim=c(1,length(betaY.start),(nburn)))
      omegapost<- array(0, dim=c(nrow(l1cov.start),ncol(l1cov.start),(nburn)))
      varYpost<-rep(0,(nburn))
      meanobs<-colMeans(Yi,na.rm=TRUE)
      if (!is.null(start.imp)) {
        start.imp<-as.matrix(start.imp)
        if ((nrow(start.imp)!=nrow(Yimp2))||(ncol(Yimp2)!=ncol(start.imp))) {
          cat("start.imp dimensions incorrect. Not using start.imp as starting value for the imputed dataset.\n")
          start.imp=NULL
        } else {
          Yimp2<-start.imp
        }
      }
      if (is.null(start.imp)) {
        for (i in 1:nrow(Yi)) for (j in 1:ncol(Yi)) if (is.na(Yimp[i,j])) Yimp2[i,j]=meanobs[j]
      }
      if (!is.null(start.imp.sub)) {
        start.imp<-as.matrix(start.imp)
        if (!is.vector(start.imp.sub)) {
          cat("start.imp.sub must be a vector. Not using start.imp as starting value for the imputed dataset.\n")
          start.imp.sub=NULL
        } else {
          Ysubimp=start.imp.sub
        }
      }
      if (is.null(start.imp.sub)) {
        for (i in 1:length(Ysubimp)) if (is.na(Ysubimp[i])) Ysubimp[i]=mean(Ysubimp, na.rm = TRUE)
      }
      Ysubcat <- c(Ysub)

      dyn.load("MCMCjomoglmbinB")
      .Call("MCMCjomoglmbinB", Ysub, Ysubimp, Ysubcat, submod, order.sub, Y, Yimp, Yimp2, Y.cat.tot, X,betaY.start,betaYpost, betait,betapost, varY.start, varYpost, covit,omegapost, nburn, varY.prior, l1cov.prior,Y.numcat.tot, ncolYcon,out.iter)
      dyn.unload("MCMCjomoglmbinB")
      #betapost[,,1]=bpost
      #omegapost[,,(1)]=opost
      imp[(nrow(Y)+1):(2*nrow(Y)),1]=Ysubcat
      if (!is.null(Y.con)|!is.null(Y.aux.con)) {
        imp[(nrow(Y)+1):(2*nrow(Y)),2:(1+max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]=Yimp2[,1:(max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]
      }
      if (isnullcat==0|isnullcataux==0) {
        imp[(nrow(Y)+1):(2*nrow(Y)),(ncolYcon[1]+2):(1+ncol(Y))]=Y.cat.tot
      }
      if (output==1) cat("First imputation registered.", "\n")

      betaYpostmean<-apply(betaYpost, c(1,2), mean)
      varYpostmean<-mean(varYpost)
      betapostmean<-apply(betapost, c(1,2), mean)
      omegapostmean<-apply(omegapost, c(1,2), mean)
      if (output==1) {
        cat("The posterior mean of the substantive model fixed effects estimates is:\n")
        print(betaYpostmean)
        cat("The posterior mean of the substantive model residual variance is:\n")
        print(varYpostmean)
        cat("The posterior mean of the fixed effects estimates is:\n")
        print(betapostmean)
        cat("The posterior mean of the level 1 covariance matrix is:\n")
        print(omegapostmean)
      }
      imp<-data.frame(imp)
      imp[,1]<-as.factor(imp[,1])
      levels(imp[,1])<-previous_levelssub
      if (isnullcat==0) {
        for (i in 1:ncol(Y.cat)) {
          imp[,(1+ncolYcon[1]+i)]<-as.factor(imp[,(1+ncolYcon[1]+i)])
          levels(imp[,(1+ncolYcon[1]+i)])<-previous_levels[[i]]
        }
      }
      if (isnullcataux==0) {
        for (i in 1:ncol(Y.aux.cat)) {
          imp[,(1+ncolYcon[3]+i)]<-as.factor(imp[,(1+ncolYcon[3]+i)])
          levels(imp[,(1+ncolYcon[3]+i)])<-previous_levelsaux[[i]]
        }
      }
      if (ncolYcon[1]>0) {
        for (j in 1:(ncolYcon[1])) {
          imp[,j+1]=as.numeric(imp[,j+1])
        }
      }
      if (isnullcat==0) {
        if (is.null(colnamycat)) colnamycat=paste("Ycat", 1:ncol(Y.cat), sep = "")
      } else {
        colnamycat=NULL
        Y.cat=NULL
        Y.numcat=NULL
      }
      if (isnullcataux==0) {
        if (is.null(colnamyauxcat)) colnamyauxcat=paste("Ycat.aux", 1:ncol(Y.aux.cat), sep = "")
      } else {
        colnamyauxcat=NULL
        Y.aux.cat=NULL
        Y.aux.numcat=NULL
      }
      if (!is.null(Y.con)) {
        if (is.null(colnamycon)) colnamycon=paste("Ycon", 1:ncol(Y.con), sep = "")
      } else {
        colnamycon=NULL
      }
      if (!is.null(Y.aux.con)) {
        if (is.null(colnamyauxcon)) colnamyauxcon=paste("Ycon.aux", 1:ncol(Y.aux.con), sep = "")
      } else {
        colnamyauxcon=NULL
      }
      if (is.null(colnamysub)) colnamysub="Ysub"
      colnames(imp)<-c(colnamysub,colnamycon,colnamyauxcon,colnamycat,colnamyauxcat,"id","Imputation")
      if (isnullcat==0) {
        cnycatcomp<-rep(NA,(sum(Y.numcat)-length(Y.numcat)))
        count=0
        for ( j in 1:ncol(Y.cat)) {
          for (k in 1:(Y.numcat[j]-1)) {
            cnycatcomp[count+k]<-paste(colnamycat[j],k,sep=".")
          }
          count=count+Y.numcat[j]-1
        }

      } else {
        cnycatcomp<-NULL
      }
      if (isnullcataux==0) {
        cnyauxcatcomp<-rep(NA,(sum(Y.aux.numcat)-length(Y.aux.numcat)))
        count=0
        for ( j in 1:ncol(Y.aux.cat)) {
          for (k in 1:(Y.aux.numcat[j]-1)) {
            cnyauxcatcomp[count+k]<-paste(colnamyauxcat[j],k,sep=".")
          }
          count=count+Y.aux.numcat[j]-1
        }

      } else {
        cnyauxcatcomp<-NULL
      }
      cnamycomp<-c(colnamycon,colnamyauxcon,cnycatcomp,cnyauxcatcomp)
      dimnames(betapost)[1] <- list(colnamx)
      dimnames(betapost)[2] <- list(cnamycomp)
      dimnames(omegapost)[1] <- list(cnamycomp)
      dimnames(omegapost)[2] <- list(cnamycomp)
      dimnames(Yimp2)[2] <- list(cnamycomp)

      return(list("finimp"=imp,"collectbeta"=betapost,"collectomega"=omegapost, "finimp.latnorm" = Yimp2,  "collectbetaY"=betaYpost, "collectvarY"=varYpost))
    }
  }

jomo.glmC <-
  function(formula, data, beta.start=NULL, l1cov.start=NULL, l1cov.prior=NULL, nburn=1000, nbetween=1000, nimp=5, output=1, out.iter=10, family="binomial") {
    cat("This function is beta software. Use carefully and please report any bug to the package mantainer\n")
    if (nimp<2) {
      nimp=2
      cat("Minimum number of imputations:2. For single imputation using function jomo.lm.MCMCchain\n")
    }
    if (family != "gaussian" & family != "binomial")
      cat("ERROR: choose either family binomial or gaussian\n")
    if (family == "gaussian") {
      imp<-jomo.lmC(formula, data, beta.start = beta.start, l1cov.start = l1cov.start,
                    l1cov.prior = l1cov.prior, nburn = nburn, nbetween = nbetween,
                    nimp = nimp, output = output, out.iter = out.iter)
    }
    if (family == "binomial") {
      stopifnot(is.data.frame(data))
      stopifnot(any(grepl("~",deparse(formula))))
      fit.cr<-glm(formula,data=data, na.action = na.omit, family=binomial)
      betaY.start<-coef(fit.cr)
      varY.start<-1
      varY.prior<-1
      colnamysub<-all.vars(formula[[2]])
      Ysub<-get(colnamysub,pos=data)
      Ycov<-data.frame(mget(all.vars(formula[[3]]), envir =as.environment(data)))
      terms.sub<-attr(terms(formula), "term.labels")
      split.terms<-strsplit(terms.sub,":")
      length.sub<-length(terms.sub)
      order.sub<-attr(terms(formula), "order")
      submod<-matrix(1,4,sum(order.sub))
      Y.con<-NULL
      Y.cat<-NULL
      Y.numcat<-NULL
      for (j in 1:ncol(Ycov)) {
        if (is.numeric(Ycov[,j])) {
          if (is.null(Y.con)) Y.con<-data.frame(Ycov[,j,drop=FALSE])
          else Y.con<-data.frame(Y.con,Ycov[,j,drop=FALSE])
        }
        if (is.factor(Ycov[,j])) {
          if (is.null(Y.cat)) Y.cat<-data.frame(Ycov[,j,drop=FALSE])
          else Y.cat<-data.frame(Y.cat,Ycov[,j,drop=FALSE])
          Y.numcat<-cbind(Y.numcat,nlevels(Ycov[,j]))
        }
      }
      h<-1
      for ( j in 1:length.sub) {
        for ( k in 1:order.sub[j]) {
          current.term<-split.terms[[j]][k]
          current.term<-sub(".*I\\(","",current.term)
          current.term<-sub("\\)","",current.term)
          if (grepl("\\^",current.term)) {
            submod[3,h]<-as.integer(sub(".*\\^","",current.term))
            current.term<-sub("\\^.*","",current.term)
          } else {
            submod[3,h]<-1
          }
          if (length(which(colnames(Y.cat)==current.term))!=0) {
            submod[1,h]<-which(colnames(Y.cat)==current.term)
            submod[2,h]<-2
            submod[4,h]<-Y.numcat[submod[1,h]]-1
          } else if (length(which(colnames(Y.con)==current.term))!=0) {
            submod[1,h]<-which(colnames(Y.con)==current.term)
            submod[2,h]<-1
          }
          h<-h+1

        }
      }
      Y.auxiliary<-data.frame(data[,-c(which(colnames(data)%in%colnames(Y.con)),which(colnames(data)%in%colnames(Y.cat)),which(colnames(data)==colnamysub)), drop=FALSE])
      Y.aux.con<-NULL
      Y.aux.cat<-NULL
      Y.aux.numcat<-NULL
      if (ncol(Y.auxiliary)>0) {
        for (j in 1:ncol(Y.auxiliary)) {
          if (is.numeric(Y.auxiliary[,j])) {
            if (is.null(Y.aux.con)) Y.aux.con<-data.frame(Y.auxiliary[,j,drop=FALSE])
            else Y.aux.con<-data.frame(Y.aux.con,Y.auxiliary[,j,drop=FALSE])
          }
          if (is.factor(Y.auxiliary[,j])) {
            if (is.null(Y.aux.cat)) Y.aux.cat<-data.frame(Y.auxiliary[,j,drop=FALSE])
            else Y.aux.cat<-data.frame(Y.aux.cat,Y.auxiliary[,j,drop=FALSE])
            Y.aux.numcat<-cbind(Y.aux.numcat,nlevels(Y.auxiliary[,j]))
          }
        }
      }
      X=matrix(1,max(nrow(Y.cat),nrow(Y.con)),1)
      if (is.null(beta.start)) beta.start=matrix(0,ncol(X),(max(as.numeric(!is.null(Y.con)),ncol(Y.con))+max(0,(sum(Y.numcat)-length(Y.numcat)))+max(as.numeric(!is.null(Y.aux.con)),ncol(Y.aux.con))+max(0,(sum(Y.aux.numcat)-length(Y.aux.numcat)))))

      if (is.null(l1cov.start)) {
        l1cov.start=diag(1,ncol(beta.start))
      }
      if (is.null(l1cov.prior)) l1cov.prior=diag(1,ncol(l1cov.start))
      ncolYcon<-rep(NA,4)
      ncolYcon[1]=max(as.numeric(!is.null(Y.con)),ncol(Y.con))+max(as.numeric(!is.null(Y.aux.con)),ncol(Y.aux.con))
      ncolYcon[2]=max(as.numeric(!is.null(Y.con)),ncol(Y.con))
      ncolYcon[3]=ncolYcon[1]+max(0,(sum(Y.numcat)-length(Y.numcat)))
      ncolYcon[4]=max(0,ncol(Y.cat))
      stopifnot(((!is.null(Y.con))||(!is.null(Y.cat)&!is.null(Y.numcat))))
      Ysub<-as.factor(Ysub)
      previous_levelssub<-levels(Ysub)
      levels(Ysub)<-1:2
      if (!is.null(Y.cat)) {
        isnullcat=0
        previous_levels<-list()
        Y.cat<-data.frame(Y.cat)
        for (i in 1:ncol(Y.cat)) {
          Y.cat[,i]<-factor(Y.cat[,i])
          previous_levels[[i]]<-levels(Y.cat[,i])
          levels(Y.cat[,i])<-1:nlevels(Y.cat[,i])
        }
      } else {
        isnullcat=1
      }
      if (!is.null(Y.aux.cat)) {
        isnullcataux=0
        previous_levelsaux<-list()
        Y.aux.cat<-data.frame(Y.aux.cat)
        for (i in 1:ncol(Y.aux.cat)) {
          Y.aux.cat[,i]<-factor(Y.aux.cat[,i])
          previous_levelsaux[[i]]<-levels(Y.aux.cat[,i])
          levels(Y.aux.cat[,i])<-1:nlevels(Y.aux.cat[,i])
        }
      } else {
        isnullcataux=1
      }

      stopifnot(nrow(beta.start)==ncol(X), ncol(beta.start)==(ncolYcon[1]+max(0,(sum(Y.numcat)-length(Y.numcat)))+max(0,(sum(Y.aux.numcat)-length(Y.aux.numcat)))))
      stopifnot(nrow(l1cov.start)==ncol(l1cov.start),nrow(l1cov.prior)==nrow(l1cov.start),nrow(l1cov.start)==ncol(beta.start))
      stopifnot(nrow(l1cov.prior)==ncol(l1cov.prior))
      betait=matrix(0,nrow(beta.start),ncol(beta.start))
      for (i in 1:nrow(beta.start)) {
        for (j in 1:ncol(beta.start)) betait[i,j]=beta.start[i,j]
      }
      covit=matrix(0,nrow(l1cov.start),ncol(l1cov.start))
      for (i in 1:nrow(l1cov.start)) {
        for (j in 1:ncol(l1cov.start)) covit[i,j]=l1cov.start[i,j]
      }
      if (!is.null(Y.con)) {
        colnamycon<-colnames(Y.con)
        Y.con<-data.matrix(Y.con)
        storage.mode(Y.con) <- "numeric"
      }
      if (isnullcat==0) {
        colnamycat<-colnames(Y.cat)
        Y.cat<-data.matrix(Y.cat)
        storage.mode(Y.cat) <- "numeric"
      }
      if (!is.null(Y.aux.con)) {
        colnamyauxcon<-colnames(Y.aux.con)
        Y.aux.con<-data.matrix(Y.aux.con)
        storage.mode(Y.aux.con) <- "numeric"
      }
      if (isnullcataux==0) {
        colnamyauxcat<-colnames(Y.aux.cat)
        Y.aux.cat<-data.matrix(Y.aux.cat)
        storage.mode(Y.aux.cat) <- "numeric"
      }
      colnamx<-colnames(X)
      X<-data.matrix(X)
      storage.mode(X) <- "numeric"
      Y=cbind(Y.con,Y.aux.con,Y.cat, Y.aux.cat)
      Yi=cbind(Y.con, Y.aux.con, switch(is.null(Y.cat)+1, matrix(0,nrow(Y),(sum(Y.numcat)-length(Y.numcat))), NULL), switch(is.null(Y.aux.cat)+1, matrix(0,nrow(Y.aux.cat),(sum(Y.aux.numcat)-length(Y.aux.numcat))), NULL))
      h=1
      if (isnullcat==0) {
        for (i in 1:length(Y.numcat)) {
          for (j in 1:nrow(Y)) {
            if (is.na(Y.cat[j,i])) {
              Yi[j,(ncolYcon[1]+h):(ncolYcon[1]+h+Y.numcat[i]-2)]=NA
            }
          }
          h=h+Y.numcat[i]-1
        }
      }
      if (isnullcataux==0) {
        for (i in 1:length(Y.aux.numcat)) {
          for (j in 1:nrow(Y)) {
            if (is.na(Y.aux.cat[j,i])) {
              Yi[j,(ncolYcon[1]+h):(ncolYcon[1]+h+Y.aux.numcat[i]-2)]=NA
            }
          }
          h=h+Y.aux.numcat[i]-1
        }
      }
      if (isnullcat==0||isnullcataux==0) {
        Y.cat.tot<-cbind(Y.cat,Y.aux.cat)
        Y.numcat.tot<-c(Y.numcat, Y.aux.numcat)
      } else {
        Y.cat.tot=-999
        Y.numcat.tot=-999
      }
      Ysubimp<-Ysub

      if (output!=1) out.iter=nburn+nbetween
      imp=matrix(0,nrow(Y)*(nimp+1),ncol(Y)+3)
      imp[1:nrow(Y),1]=Ysub
      imp[1:nrow(Y),2:(1+ncol(Y))]=Y
      imp[1:nrow(X), (ncol(Y)+2)]=c(1:nrow(Y))
      Yimp=Yi
      Yimp2=matrix(Yimp, nrow(Yimp),ncol(Yimp))
      imp[(nrow(X)+1):(2*nrow(X)), (ncol(Y)+2)]=c(1:nrow(Y))
      imp[(nrow(X)+1):(2*nrow(X)), (ncol(Y)+3)]=1
      betapost<- array(0, dim=c(nrow(beta.start),ncol(beta.start),(nimp-1)))
      betaYpost<- array(0, dim=c(1,length(betaY.start),(nimp-1)))
      bpost<-matrix(0,nrow(beta.start),ncol(beta.start))
      bYpost<-matrix(0,1,length(betaY.start))
      omegapost<- array(0, dim=c(nrow(l1cov.start),ncol(l1cov.start),(nimp-1)))
      opost<-matrix(0,nrow(l1cov.start),ncol(l1cov.start))
      varYpost<-rep(0,(nimp-1))
      vYpost<-matrix(0,1,1)
      meanobs<-colMeans(Yi,na.rm=TRUE)
      for (i in 1:nrow(Yi)) for (j in 1:ncol(Yi)) if (is.na(Yimp[i,j])) Yimp2[i,j]=meanobs[j]
      for (i in 1:length(Ysubimp)) if (is.na(Ysubimp[i])) Ysubimp[i]=mean(Ysubimp, na.rm = TRUE)
      Ysubcat <- c(Ysub)

      dyn.load("jomoglmbinB")
      .Call("jomoglmbinB", Ysub, Ysubimp, Ysubcat, submod, order.sub, Y, Yimp, Yimp2, Y.cat.tot, X, betaY.start, bYpost, betait,bpost, varY.start, vYpost, covit,opost, nburn, varY.prior, l1cov.prior,Y.numcat.tot, ncolYcon,out.iter)
      dyn.unload("jomoglmbinB")
      #betapost[,,1]=bpost
      #omegapost[,,(1)]=opost
      bpost<-matrix(0,nrow(beta.start),ncol(beta.start))
      bYpost<-matrix(0,1,length(betaY.start))
      opost<-matrix(0,nrow(l1cov.start),ncol(l1cov.start))
      vYpost<-matrix(0,1,1)
      imp[(nrow(Y)+1):(2*nrow(Y)),1]=Ysubcat
      if (!is.null(Y.con)|!is.null(Y.aux.con)) {
        imp[(nrow(Y)+1):(2*nrow(Y)),2:(1+max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]=Yimp2[,1:(max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]
      }
      if (isnullcat==0|isnullcataux==0) {
        imp[(nrow(Y)+1):(2*nrow(Y)),(ncolYcon[1]+2):(1+ncol(Y))]=Y.cat.tot
      }
      if (output==1) cat("First imputation registered.", "\n")
      for (i in 2:nimp) {
        #Yimp2=matrix(0, nrow(Yimp),ncol(Yimp))
        imp[(i*nrow(X)+1):((i+1)*nrow(X)), (ncol(Y)+2)]=c(1:nrow(Y))
        imp[(i*nrow(X)+1):((i+1)*nrow(X)), (ncol(Y)+3)]=i
        dyn.load("jomoglmbinB")
        .Call("jomoglmbinB", Ysub, Ysubimp, Ysubcat, submod, order.sub, Y, Yimp, Yimp2, Y.cat.tot, X, betaY.start, bYpost, betait,bpost, varY.start, vYpost, covit,opost, nbetween, varY.prior, l1cov.prior,Y.numcat.tot, ncolYcon,out.iter)
        dyn.unload("jomoglmbinB")
        betapost[,,(i-1)]=bpost
        betaYpost[,,(i-1)]=bYpost
        omegapost[,,(i-1)]=opost
        varYpost[i-1]=vYpost
        bpost<-matrix(0,nrow(beta.start),ncol(beta.start))
        bYpost<-matrix(0,1,length(betaY.start))
        opost<-matrix(0,nrow(l1cov.start),ncol(l1cov.start))
        vYpost<-matrix(0,1,1)
        imp[(i*nrow(X)+1):((i+1)*nrow(X)),1]=Ysubcat
        if (!is.null(Y.con)|!is.null(Y.aux.con)) {
          imp[(i*nrow(X)+1):((i+1)*nrow(X)),2:(1+max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]=Yimp2[,1:(max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]
        }
        if (isnullcat==0|isnullcataux==0) {
          imp[(i*nrow(X)+1):((i+1)*nrow(X)),(ncolYcon[1]+2):(1+ncol(Y))]=Y.cat.tot
        }
        if (output==1) cat("Imputation number ", i, "registered", "\n")
      }

      betaYpostmean<-apply(betaYpost, c(1,2), mean)
      varYpostmean<-mean(varYpost)
      betapostmean<-apply(betapost, c(1,2), mean)
      omegapostmean<-apply(omegapost, c(1,2), mean)
      if (output==1) {
        cat("The posterior mean of the substantive model fixed effects estimates is:\n")
        print(betaYpostmean)
        cat("The posterior mean of the substantive model residual variance is:\n")
        print(varYpostmean)
        cat("The posterior mean of the fixed effects estimates is:\n")
        print(betapostmean)
        cat("The posterior mean of the level 1 covariance matrix is:\n")
        print(omegapostmean)
      }
      imp<-data.frame(imp)
      imp[,1]<-as.factor(imp[,1])
      levels(imp[,1])<-previous_levelssub
      if (isnullcat==0) {
        for (i in 1:ncol(Y.cat)) {
          imp[,(1+ncolYcon[1]+i)]<-as.factor(imp[,(1+ncolYcon[1]+i)])
          levels(imp[,(1+ncolYcon[1]+i)])<-previous_levels[[i]]
        }
      }
      if (isnullcataux==0) {
        for (i in 1:ncol(Y.aux.cat)) {
          imp[,(1+ncolYcon[3]+i)]<-as.factor(imp[,(1+ncolYcon[3]+i)])
          levels(imp[,(1+ncolYcon[3]+i)])<-previous_levelsaux[[i]]
        }
      }
      if (ncolYcon[1]>0) {
        for (j in 1:(ncolYcon[1])) {
          imp[,j+1]=as.numeric(imp[,j+1])
        }
      }
      if (isnullcat==0) {
        if (is.null(colnamycat)) colnamycat=paste("Ycat", 1:ncol(Y.cat), sep = "")
      } else {
        colnamycat=NULL
        Y.cat=NULL
        Y.numcat=NULL
      }
      if (isnullcataux==0) {
        if (is.null(colnamyauxcat)) colnamyauxcat=paste("Ycat.aux", 1:ncol(Y.aux.cat), sep = "")
      } else {
        colnamyauxcat=NULL
        Y.aux.cat=NULL
        Y.aux.numcat=NULL
      }
      if (!is.null(Y.con)) {
        if (is.null(colnamycon)) colnamycon=paste("Ycon", 1:ncol(Y.con), sep = "")
      } else {
        colnamycon=NULL
      }
      if (!is.null(Y.aux.con)) {
        if (is.null(colnamyauxcon)) colnamyauxcon=paste("Ycon.aux", 1:ncol(Y.aux.con), sep = "")
      } else {
        colnamyauxcon=NULL
      }
      if (is.null(colnamysub)) colnamysub="Ysub"
      colnames(imp)<-c(colnamysub,colnamycon,colnamyauxcon,colnamycat,colnamyauxcat,"id","Imputation")
    }
    return(imp)
  }

jomo.lmC <-
  function(formula, data, beta.start=NULL, l1cov.start=NULL, l1cov.prior=NULL, nburn=1000, nbetween=1000, nimp=5, output=1, out.iter=10) {
    cat("This function is beta software. Use carefully and please report any bug to the package mantainer\n")
    if (nimp<2) {
      nimp=2
      cat("Minimum number of imputations:2. For single imputation using function jomo.lm.MCMCchain\n")
    }
    stopifnot(is.data.frame(data))
    stopifnot(any(grepl("~",deparse(formula))))
    fit.cr<-lm(formula,data=data, na.action = na.omit)
    betaY.start<-coef(fit.cr)
    varY.start<-(summary(fit.cr)$sigma)^2
    varY.prior<-(summary(fit.cr)$sigma)^2
    colnamysub<-all.vars(formula[[2]])
    Ysub<-get(colnamysub,pos=data)
    Ycov<-data.frame(mget(all.vars(formula[[3]]), envir =as.environment(data)))
    terms.sub<-attr(terms(formula), "term.labels")
    split.terms<-strsplit(terms.sub,":")
    length.sub<-length(terms.sub)
    order.sub<-attr(terms(formula), "order")
    submod<-matrix(1,4,sum(order.sub))
    Y.con<-NULL
    Y.cat<-NULL
    Y.numcat<-NULL
    for (j in 1:ncol(Ycov)) {
      if (is.numeric(Ycov[,j])) {
        if (is.null(Y.con)) Y.con<-data.frame(Ycov[,j,drop=FALSE])
        else Y.con<-data.frame(Y.con,Ycov[,j,drop=FALSE])
      }
      if (is.factor(Ycov[,j])) {
        if (is.null(Y.cat)) Y.cat<-data.frame(Ycov[,j,drop=FALSE])
        else Y.cat<-data.frame(Y.cat,Ycov[,j,drop=FALSE])
        Y.numcat<-cbind(Y.numcat,nlevels(Ycov[,j]))
      }
    }
    h<-1
    for ( j in 1:length.sub) {
      for ( k in 1:order.sub[j]) {
        current.term<-split.terms[[j]][k]
        current.term<-sub(".*I\\(","",current.term)
        current.term<-sub("\\)","",current.term)
        if (grepl("\\^",current.term)) {
          submod[3,h]<-as.integer(sub(".*\\^","",current.term))
          current.term<-sub("\\^.*","",current.term)
        } else {
          submod[3,h]<-1
        }
        if (length(which(colnames(Y.cat)==current.term))!=0) {
          submod[1,h]<-which(colnames(Y.cat)==current.term)
          submod[2,h]<-2
          submod[4,h]<-Y.numcat[submod[1,h]]-1
        } else if (length(which(colnames(Y.con)==current.term))!=0) {
          submod[1,h]<-which(colnames(Y.con)==current.term)
          submod[2,h]<-1
        }
        h<-h+1
      }

    }
    Y.auxiliary<-data.frame(data[,-c(which(colnames(data)%in%colnames(Y.con)),which(colnames(data)%in%colnames(Y.cat)),which(colnames(data)==colnamysub)), drop=FALSE])
    Y.aux.con<-NULL
    Y.aux.cat<-NULL
    Y.aux.numcat<-NULL
    if (ncol(Y.auxiliary)>0) {
      for (j in 1:ncol(Y.auxiliary)) {
        if (is.numeric(Y.auxiliary[,j])) {
          if (is.null(Y.aux.con)) Y.aux.con<-data.frame(Y.auxiliary[,j,drop=FALSE])
          else Y.aux.con<-data.frame(Y.aux.con,Y.auxiliary[,j,drop=FALSE])
        }
        if (is.factor(Y.auxiliary[,j])) {
          if (is.null(Y.aux.cat)) Y.aux.cat<-data.frame(Y.auxiliary[,j,drop=FALSE])
          else Y.aux.cat<-data.frame(Y.aux.cat,Y.auxiliary[,j,drop=FALSE])
          Y.aux.numcat<-cbind(Y.aux.numcat,nlevels(Y.auxiliary[,j]))
        }
      }
    }
    X=matrix(1,max(nrow(Y.cat),nrow(Y.con)),1)
    if (is.null(beta.start)) beta.start=matrix(0,ncol(X),(max(as.numeric(!is.null(Y.con)),ncol(Y.con))+max(0,(sum(Y.numcat)-length(Y.numcat)))+max(as.numeric(!is.null(Y.aux.con)),ncol(Y.aux.con))+max(0,(sum(Y.aux.numcat)-length(Y.aux.numcat)))))

    if (is.null(l1cov.start)) {
      l1cov.start=diag(1,ncol(beta.start))
    }
    if (is.null(l1cov.prior)) l1cov.prior=diag(1,ncol(l1cov.start))
    ncolYcon<-rep(NA,4)
    ncolYcon[1]=max(as.numeric(!is.null(Y.con)),ncol(Y.con))+max(as.numeric(!is.null(Y.aux.con)),ncol(Y.aux.con))
    ncolYcon[2]=max(as.numeric(!is.null(Y.con)),ncol(Y.con))
    ncolYcon[3]=ncolYcon[1]+max(0,(sum(Y.numcat)-length(Y.numcat)))
    ncolYcon[4]=max(0,ncol(Y.cat))
    stopifnot(((!is.null(Y.con))||(!is.null(Y.cat)&!is.null(Y.numcat))))
    if (!is.null(Y.cat)) {
      isnullcat=0
      previous_levels<-list()
      Y.cat<-data.frame(Y.cat)
      for (i in 1:ncol(Y.cat)) {
        Y.cat[,i]<-factor(Y.cat[,i])
        previous_levels[[i]]<-levels(Y.cat[,i])
        levels(Y.cat[,i])<-1:nlevels(Y.cat[,i])
      }
    } else {
      isnullcat=1
    }
    if (!is.null(Y.aux.cat)) {
      isnullcataux=0
      previous_levelsaux<-list()
      Y.aux.cat<-data.frame(Y.aux.cat)
      for (i in 1:ncol(Y.aux.cat)) {
        Y.aux.cat[,i]<-factor(Y.aux.cat[,i])
        previous_levelsaux[[i]]<-levels(Y.aux.cat[,i])
        levels(Y.aux.cat[,i])<-1:nlevels(Y.aux.cat[,i])
      }
    } else {
      isnullcataux=1
    }

    stopifnot(nrow(beta.start)==ncol(X), ncol(beta.start)==(ncolYcon[1]+max(0,(sum(Y.numcat)-length(Y.numcat)))+max(0,(sum(Y.aux.numcat)-length(Y.aux.numcat)))))
    stopifnot(nrow(l1cov.start)==ncol(l1cov.start),nrow(l1cov.prior)==nrow(l1cov.start),nrow(l1cov.start)==ncol(beta.start))
    stopifnot(nrow(l1cov.prior)==ncol(l1cov.prior))
    betait=matrix(0,nrow(beta.start),ncol(beta.start))
    for (i in 1:nrow(beta.start)) {
      for (j in 1:ncol(beta.start)) betait[i,j]=beta.start[i,j]
    }
    covit=matrix(0,nrow(l1cov.start),ncol(l1cov.start))
    for (i in 1:nrow(l1cov.start)) {
      for (j in 1:ncol(l1cov.start)) covit[i,j]=l1cov.start[i,j]
    }
    if (!is.null(Y.con)) {
      colnamycon<-colnames(Y.con)
      Y.con<-data.matrix(Y.con)
      storage.mode(Y.con) <- "numeric"
    }
    if (isnullcat==0) {
      colnamycat<-colnames(Y.cat)
      Y.cat<-data.matrix(Y.cat)
      storage.mode(Y.cat) <- "numeric"
    }
    if (!is.null(Y.aux.con)) {
      colnamyauxcon<-colnames(Y.aux.con)
      Y.aux.con<-data.matrix(Y.aux.con)
      storage.mode(Y.aux.con) <- "numeric"
    }
    if (isnullcataux==0) {
      colnamyauxcat<-colnames(Y.aux.cat)
      Y.aux.cat<-data.matrix(Y.aux.cat)
      storage.mode(Y.aux.cat) <- "numeric"
    }
    colnamx<-colnames(X)
    X<-data.matrix(X)
    storage.mode(X) <- "numeric"
    Y=cbind(Y.con,Y.aux.con,Y.cat, Y.aux.cat)
    Yi=cbind(Y.con, Y.aux.con, switch(is.null(Y.cat)+1, matrix(0,nrow(Y),(sum(Y.numcat)-length(Y.numcat))), NULL), switch(is.null(Y.aux.cat)+1, matrix(0,nrow(Y.aux.cat),(sum(Y.aux.numcat)-length(Y.aux.numcat))), NULL))
    h=1
    if (isnullcat==0) {
      for (i in 1:length(Y.numcat)) {
        for (j in 1:nrow(Y)) {
          if (is.na(Y.cat[j,i])) {
            Yi[j,(ncolYcon[1]+h):(ncolYcon[1]+h+Y.numcat[i]-2)]=NA
          }
        }
        h=h+Y.numcat[i]-1
      }
    }
    if (isnullcataux==0) {
      for (i in 1:length(Y.aux.numcat)) {
        for (j in 1:nrow(Y)) {
          if (is.na(Y.aux.cat[j,i])) {
            Yi[j,(ncolYcon[1]+h):(ncolYcon[1]+h+Y.aux.numcat[i]-2)]=NA
          }
        }
        h=h+Y.aux.numcat[i]-1
      }
    }
    if (isnullcat==0||isnullcataux==0) {
      Y.cat.tot<-cbind(Y.cat,Y.aux.cat)
      Y.numcat.tot<-c(Y.numcat, Y.aux.numcat)
    } else {
      Y.cat.tot=-999
      Y.numcat.tot=-999
    }
    Ysubimp<-Ysub

    if (output!=1) out.iter=nburn+nbetween
    imp=matrix(0,nrow(Y)*(nimp+1),ncol(Y)+3)
    imp[1:nrow(Y),1]=Ysub
    imp[1:nrow(Y),2:(1+ncol(Y))]=Y
    imp[1:nrow(X), (ncol(Y)+2)]=c(1:nrow(Y))
    Yimp=Yi
    Yimp2=matrix(Yimp, nrow(Yimp),ncol(Yimp))
    imp[(nrow(X)+1):(2*nrow(X)), (ncol(Y)+2)]=c(1:nrow(Y))
    imp[(nrow(X)+1):(2*nrow(X)), (ncol(Y)+3)]=1
    betapost<- array(0, dim=c(nrow(beta.start),ncol(beta.start),(nimp-1)))
    betaYpost<- array(0, dim=c(1,length(betaY.start),(nimp-1)))
    bpost<-matrix(0,nrow(beta.start),ncol(beta.start))
    bYpost<-matrix(0,1,length(betaY.start))
    omegapost<- array(0, dim=c(nrow(l1cov.start),ncol(l1cov.start),(nimp-1)))
    opost<-matrix(0,nrow(l1cov.start),ncol(l1cov.start))
    varYpost<-rep(0,(nimp-1))
    vYpost<-matrix(0,1,1)
    meanobs<-colMeans(Yi,na.rm=TRUE)
    for (i in 1:nrow(Yi)) for (j in 1:ncol(Yi)) if (is.na(Yimp[i,j])) Yimp2[i,j]=meanobs[j]
    for (i in 1:length(Ysubimp)) if (is.na(Ysubimp[i])) Ysubimp[i]=mean(Ysubimp, na.rm = TRUE)

    dyn.load("jomolmB")
    .Call("jomolmB", Ysub, Ysubimp, submod, order.sub, Y, Yimp, Yimp2, Y.cat.tot, X, betaY.start, bYpost, betait,bpost, varY.start, vYpost, covit,opost, nburn, varY.prior, l1cov.prior,Y.numcat.tot, ncolYcon,out.iter)
    dyn.unload("jomolmB")
    #betapost[,,1]=bpost
    #omegapost[,,(1)]=opost
    bpost<-matrix(0,nrow(beta.start),ncol(beta.start))
    bYpost<-matrix(0,1,length(betaY.start))
    opost<-matrix(0,nrow(l1cov.start),ncol(l1cov.start))
    vYpost<-matrix(0,1,1)
    imp[(nrow(Y)+1):(2*nrow(Y)),1]=Ysubimp
    if (!is.null(Y.con)|!is.null(Y.aux.con)) {
      imp[(nrow(Y)+1):(2*nrow(Y)),2:(1+max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]=Yimp2[,1:(max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]
    }
    if (isnullcat==0|isnullcataux==0) {
      imp[(nrow(Y)+1):(2*nrow(Y)),(ncolYcon[1]+2):(1+ncol(Y))]=Y.cat.tot
    }
    if (output==1) cat("First imputation registered.", "\n")
    for (i in 2:nimp) {
      #Yimp2=matrix(0, nrow(Yimp),ncol(Yimp))
      imp[(i*nrow(X)+1):((i+1)*nrow(X)), (ncol(Y)+2)]=c(1:nrow(Y))
      imp[(i*nrow(X)+1):((i+1)*nrow(X)), (ncol(Y)+3)]=i
      dyn.load("jomolmB")
      .Call("jomolmB", Ysub, Ysubimp, submod, order.sub, Y, Yimp, Yimp2, Y.cat.tot, X, betaY.start, bYpost, betait,bpost, varY.start, vYpost, covit,opost, nbetween, varY.prior, l1cov.prior,Y.numcat.tot, ncolYcon,out.iter)
      dyn.unload("jomolmB")
      betapost[,,(i-1)]=bpost
      betaYpost[,,(i-1)]=bYpost
      omegapost[,,(i-1)]=opost
      varYpost[i-1]=vYpost
      bpost<-matrix(0,nrow(beta.start),ncol(beta.start))
      bYpost<-matrix(0,1,length(betaY.start))
      opost<-matrix(0,nrow(l1cov.start),ncol(l1cov.start))
      vYpost<-matrix(0,1,1)
      imp[(i*nrow(X)+1):((i+1)*nrow(X)),1]=Ysubimp
      if (!is.null(Y.con)|!is.null(Y.aux.con)) {
        imp[(i*nrow(X)+1):((i+1)*nrow(X)),2:(1+max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]=Yimp2[,1:(max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]
      }
      if (isnullcat==0||isnullcataux==0) {
        imp[(i*nrow(X)+1):((i+1)*nrow(X)),(ncolYcon[1]+2):(1+ncol(Y))]=Y.cat.tot
      }
      if (output==1) cat("Imputation number ", i, "registered", "\n")
    }

    betaYpostmean<-apply(betaYpost, c(1,2), mean)
    varYpostmean<-mean(varYpost)
    betapostmean<-apply(betapost, c(1,2), mean)
    omegapostmean<-apply(omegapost, c(1,2), mean)
    if (output==1) {
      cat("The posterior mean of the substantive model fixed effects estimates is:\n")
      print(betaYpostmean)
      cat("The posterior mean of the substantive model residual variance is:\n")
      print(varYpostmean)
      cat("The posterior mean of the fixed effects estimates is:\n")
      print(betapostmean)
      cat("The posterior mean of the level 1 covariance matrix is:\n")
      print(omegapostmean)
    }
    imp<-data.frame(imp)
    if (isnullcat==0) {
      for (i in 1:ncol(Y.cat)) {
        imp[,(1+ncolYcon[1]+i)]<-as.factor(imp[,(1+ncolYcon[1]+i)])
        levels(imp[,(1+ncolYcon[1]+i)])<-previous_levels[[i]]
      }
    }
    if (isnullcataux==0) {
      for (i in 1:ncol(Y.aux.cat)) {
        imp[,(1+ncolYcon[1]+length(Y.numcat)+i)]<-as.factor(imp[,(1+ncolYcon[1]+length(Y.numcat)+i)])
        levels(imp[,(1+ncolYcon[1]+length(Y.numcat)+i)])<-previous_levelsaux[[i]]
      }
    }
    if (ncolYcon[1]>0) {
      for (j in 1:(ncolYcon[1])) {
        imp[,j+1]=as.numeric(imp[,j+1])
      }
    }
    if (isnullcat==0) {
      if (is.null(colnamycat)) colnamycat=paste("Ycat", 1:ncol(Y.cat), sep = "")
    } else {
      colnamycat=NULL
      Y.cat=NULL
      Y.numcat=NULL
    }
    if (isnullcataux==0) {
      if (is.null(colnamyauxcat)) colnamyauxcat=paste("Ycat.aux", 1:ncol(Y.aux.cat), sep = "")
    } else {
      colnamyauxcat=NULL
      Y.aux.cat=NULL
      Y.aux.numcat=NULL
    }
    if (!is.null(Y.con)) {
      if (is.null(colnamycon)) colnamycon=paste("Ycon", 1:ncol(Y.con), sep = "")
    } else {
      colnamycon=NULL
    }
    if (!is.null(Y.aux.con)) {
      if (is.null(colnamyauxcon)) colnamyauxcon=paste("Ycon.aux", 1:ncol(Y.aux.con), sep = "")
    } else {
      colnamyauxcon=NULL
    }
    if (is.null(colnamysub)) colnamysub="Ysub"
    colnames(imp)<-c(colnamysub,colnamycon,colnamyauxcon,colnamycat,colnamyauxcat,"id","Imputation")
    return(imp)
  }

jomo.lm.MCMCchainC <-
  function(formula, data, beta.start=NULL, l1cov.start=NULL, l1cov.prior=NULL, betaY.start=NULL, varY.start=NULL, nburn=1000, start.imp=NULL, start.imp.sub=NULL, output=1, out.iter=10) {
    cat("This function is beta software. Use carefully and please report any bug to the package mantainer\n")
    stopifnot(is.data.frame(data))
    stopifnot(any(grepl("~",deparse(formula))))
    fit.cr<-lm(formula,data=data, na.action = na.omit)
    if (is.null(betaY.start)) betaY.start<-coef(fit.cr)
    if (is.null(varY.start)) varY.start<-(summary(fit.cr)$sigma)^2
    varY.prior<-(summary(fit.cr)$sigma)^2
    colnamysub<-all.vars(formula[[2]])
    Ysub<-get(colnamysub,pos=data)
    Ycov<-data.frame(mget(all.vars(formula[[3]]), envir =as.environment(data)))
    terms.sub<-attr(terms(formula), "term.labels")
    split.terms<-strsplit(terms.sub,":")
    length.sub<-length(terms.sub)
    order.sub<-attr(terms(formula), "order")
    submod<-matrix(1,4,sum(order.sub))
    Y.con<-NULL
    Y.cat<-NULL
    Y.numcat<-NULL
    for (j in 1:ncol(Ycov)) {
      if (is.numeric(Ycov[,j])) {
        if (is.null(Y.con)) Y.con<-data.frame(Ycov[,j,drop=FALSE])
        else Y.con<-data.frame(Y.con,Ycov[,j,drop=FALSE])
      }
      if (is.factor(Ycov[,j])) {
        if (is.null(Y.cat)) Y.cat<-data.frame(Ycov[,j,drop=FALSE])
        else Y.cat<-data.frame(Y.cat,Ycov[,j,drop=FALSE])
        Y.numcat<-cbind(Y.numcat,nlevels(Ycov[,j]))
      }
    }
    h<-1
    for ( j in 1:length.sub) {
      for ( k in 1:order.sub[j]) {
        current.term<-split.terms[[j]][k]
        current.term<-sub(".*I\\(","",current.term)
        current.term<-sub("\\)","",current.term)
        if (grepl("\\^",current.term)) {
          submod[3,h]<-as.integer(sub(".*\\^","",current.term))
          current.term<-sub("\\^.*","",current.term)
        } else {
          submod[3,h]<-1
        }
        if (length(which(colnames(Y.cat)==current.term))!=0) {
          submod[1,h]<-which(colnames(Y.cat)==current.term)
          submod[2,h]<-2
          submod[4,h]<-Y.numcat[submod[1,h]]-1
        } else if (length(which(colnames(Y.con)==current.term))!=0) {
          submod[1,h]<-which(colnames(Y.con)==current.term)
          submod[2,h]<-1
        }
        h<-h+1
      }
    }
    Y.auxiliary<-data.frame(data[,-c(which(colnames(data)%in%colnames(Y.con)),which(colnames(data)%in%colnames(Y.cat)),which(colnames(data)==colnamysub)), drop=FALSE])
    Y.aux.con<-NULL
    Y.aux.cat<-NULL
    Y.aux.numcat<-NULL
    if (ncol(Y.auxiliary)>0) {
      for (j in 1:ncol(Y.auxiliary)) {
        if (is.numeric(Y.auxiliary[,j])) {
          if (is.null(Y.aux.con)) Y.aux.con<-data.frame(Y.auxiliary[,j,drop=FALSE])
          else Y.aux.con<-data.frame(Y.aux.con,Y.auxiliary[,j,drop=FALSE])
        }
        if (is.factor(Y.auxiliary[,j])) {
          if (is.null(Y.aux.cat)) Y.aux.cat<-data.frame(Y.auxiliary[,j,drop=FALSE])
          else Y.aux.cat<-data.frame(Y.aux.cat,Y.auxiliary[,j,drop=FALSE])
          Y.aux.numcat<-cbind(Y.aux.numcat,nlevels(Y.auxiliary[,j]))
        }
      }
    }
    X=matrix(1,max(nrow(Y.cat),nrow(Y.con)),1)
    if (is.null(beta.start)) beta.start=matrix(0,ncol(X),(max(as.numeric(!is.null(Y.con)),ncol(Y.con))+max(0,(sum(Y.numcat)-length(Y.numcat)))+max(as.numeric(!is.null(Y.aux.con)),ncol(Y.aux.con))+max(0,(sum(Y.aux.numcat)-length(Y.aux.numcat)))))

    if (is.null(l1cov.start)) {
      l1cov.start=diag(1,ncol(beta.start))
    }
    if (is.null(l1cov.prior)) l1cov.prior=diag(1,ncol(l1cov.start))
    ncolYcon<-rep(NA,4)
    ncolYcon[1]=max(as.numeric(!is.null(Y.con)),ncol(Y.con))+max(as.numeric(!is.null(Y.aux.con)),ncol(Y.aux.con))
    ncolYcon[2]=max(as.numeric(!is.null(Y.con)),ncol(Y.con))
    ncolYcon[3]=ncolYcon[1]+max(0,(sum(Y.numcat)-length(Y.numcat)))
    ncolYcon[4]=max(0,ncol(Y.cat))
    stopifnot(((!is.null(Y.con))||(!is.null(Y.cat)&!is.null(Y.numcat))))
    if (!is.null(Y.cat)) {
      isnullcat=0
      previous_levels<-list()
      Y.cat<-data.frame(Y.cat)
      for (i in 1:ncol(Y.cat)) {
        Y.cat[,i]<-factor(Y.cat[,i])
        previous_levels[[i]]<-levels(Y.cat[,i])
        levels(Y.cat[,i])<-1:nlevels(Y.cat[,i])
      }
    } else {
      isnullcat=1
    }
    if (!is.null(Y.aux.cat)) {
      isnullcataux=0
      previous_levelsaux<-list()
      Y.aux.cat<-data.frame(Y.aux.cat)
      for (i in 1:ncol(Y.aux.cat)) {
        Y.aux.cat[,i]<-factor(Y.aux.cat[,i])
        previous_levelsaux[[i]]<-levels(Y.aux.cat[,i])
        levels(Y.aux.cat[,i])<-1:nlevels(Y.aux.cat[,i])
      }
    } else {
      isnullcataux=1
    }

    stopifnot(nrow(beta.start)==ncol(X), ncol(beta.start)==(ncolYcon[1]+max(0,(sum(Y.numcat)-length(Y.numcat)))+max(0,(sum(Y.aux.numcat)-length(Y.aux.numcat)))))
    stopifnot(nrow(l1cov.start)==ncol(l1cov.start),nrow(l1cov.prior)==nrow(l1cov.start),nrow(l1cov.start)==ncol(beta.start))
    stopifnot(nrow(l1cov.prior)==ncol(l1cov.prior))
    betait=matrix(0,nrow(beta.start),ncol(beta.start))
    for (i in 1:nrow(beta.start)) {
      for (j in 1:ncol(beta.start)) betait[i,j]=beta.start[i,j]
    }
    covit=matrix(0,nrow(l1cov.start),ncol(l1cov.start))
    for (i in 1:nrow(l1cov.start)) {
      for (j in 1:ncol(l1cov.start)) covit[i,j]=l1cov.start[i,j]
    }
    if (!is.null(Y.con)) {
      colnamycon<-colnames(Y.con)
      Y.con<-data.matrix(Y.con)
      storage.mode(Y.con) <- "numeric"
    }
    if (isnullcat==0) {
      colnamycat<-colnames(Y.cat)
      Y.cat<-data.matrix(Y.cat)
      storage.mode(Y.cat) <- "numeric"
    }
    if (!is.null(Y.aux.con)) {
      colnamyauxcon<-colnames(Y.aux.con)
      Y.aux.con<-data.matrix(Y.aux.con)
      storage.mode(Y.aux.con) <- "numeric"
    }
    if (isnullcataux==0) {
      colnamyauxcat<-colnames(Y.aux.cat)
      Y.aux.cat<-data.matrix(Y.aux.cat)
      storage.mode(Y.aux.cat) <- "numeric"
    }
    colnamx<-colnames(X)
    X<-data.matrix(X)
    storage.mode(X) <- "numeric"
    Y=cbind(Y.con,Y.aux.con,Y.cat, Y.aux.cat)
    Yi=cbind(Y.con, Y.aux.con, switch(is.null(Y.cat)+1, matrix(0,nrow(Y),(sum(Y.numcat)-length(Y.numcat))), NULL), switch(is.null(Y.aux.cat)+1, matrix(0,nrow(Y.aux.cat),(sum(Y.aux.numcat)-length(Y.aux.numcat))), NULL))
    h=1
    if (isnullcat==0) {
      for (i in 1:length(Y.numcat)) {
        for (j in 1:nrow(Y)) {
          if (is.na(Y.cat[j,i])) {
            Yi[j,(ncolYcon[1]+h):(ncolYcon[1]+h+Y.numcat[i]-2)]=NA
          }
        }
        h=h+Y.numcat[i]-1
      }
    }
    if (isnullcataux==0) {
      for (i in 1:length(Y.aux.numcat)) {
        for (j in 1:nrow(Y)) {
          if (is.na(Y.aux.cat[j,i])) {
            Yi[j,(ncolYcon[1]+h):(ncolYcon[1]+h+Y.aux.numcat[i]-2)]=NA
          }
        }
        h=h+Y.aux.numcat[i]-1
      }
    }
    if (isnullcat==0||isnullcataux==0) {
      Y.cat.tot<-cbind(Y.cat,Y.aux.cat)
      Y.numcat.tot<-c(Y.numcat, Y.aux.numcat)
    } else {
      Y.cat.tot=-999
      Y.numcat.tot=-999
    }
    Ysubimp<-Ysub

    if (output!=1) out.iter=nburn+2
    nimp=1
    imp=matrix(0,nrow(Y)*(nimp+1),ncol(Y)+3)
    imp[1:nrow(Y),1]=Ysub
    imp[1:nrow(Y),2:(1+ncol(Y))]=Y
    imp[1:nrow(X), (ncol(Y)+2)]=c(1:nrow(Y))
    Yimp=Yi
    Yimp2=matrix(Yimp, nrow(Yimp),ncol(Yimp))
    imp[(nrow(X)+1):(2*nrow(X)), (ncol(Y)+2)]=c(1:nrow(Y))
    imp[(nrow(X)+1):(2*nrow(X)), (ncol(Y)+3)]=1
    betapost<- array(0, dim=c(nrow(beta.start),ncol(beta.start),(nburn)))
    betaYpost<- array(0, dim=c(1,length(betaY.start),(nburn)))
    omegapost<- array(0, dim=c(nrow(l1cov.start),ncol(l1cov.start),(nburn)))
    varYpost<-rep(0,(nburn))
    meanobs<-colMeans(Yi,na.rm=TRUE)
    if (!is.null(start.imp)) {
      start.imp<-as.matrix(start.imp)
      if ((nrow(start.imp)!=nrow(Yimp2))||(ncol(Yimp2)!=ncol(start.imp))) {
        cat("start.imp dimensions incorrect. Not using start.imp as starting value for the imputed dataset.\n")
        start.imp=NULL
      } else {
        Yimp2<-start.imp
      }
    }
    if (is.null(start.imp)) {
      for (i in 1:nrow(Yi)) for (j in 1:ncol(Yi)) if (is.na(Yimp[i,j])) Yimp2[i,j]=meanobs[j]
    }
    if (!is.null(start.imp.sub)) {
      start.imp<-as.matrix(start.imp)
      if (!is.vector(start.imp.sub)) {
        cat("start.imp.sub must be a vector. Not using start.imp as starting value for the imputed dataset.\n")
        start.imp.sub=NULL
      } else {
        Ysubimp=start.imp.sub
      }
    }
    if (is.null(start.imp.sub)) {
      for (i in 1:length(Ysubimp)) if (is.na(Ysubimp[i])) Ysubimp[i]=mean(Ysubimp, na.rm = TRUE)
    }
    dyn.load("MCMCjomolmB")
    .Call("MCMCjomolmB", Ysub, Ysubimp, submod, order.sub, Y, Yimp, Yimp2, Y.cat.tot, X,betaY.start,betaYpost, betait,betapost, varY.start, varYpost, covit,omegapost, nburn, varY.prior, l1cov.prior,Y.numcat.tot, ncolYcon,out.iter)
    dyn.unload("MCMCjomolmB")
    #betapost[,,1]=bpost
    #omegapost[,,(1)]=opost
    imp[(nrow(Y)+1):(2*nrow(Y)),1]=Ysubimp
    if (!is.null(Y.con)|!is.null(Y.aux.con)) {
      imp[(nrow(Y)+1):(2*nrow(Y)),2:(1+max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]=Yimp2[,1:(max(0,ncol(Y.con))+max(0,ncol(Y.aux.con)))]
    }
    if (isnullcat==0|isnullcataux==0) {
      imp[(nrow(Y)+1):(2*nrow(Y)),(ncolYcon[1]+2):(1+ncol(Y))]=Y.cat.tot
    }
    if (output==1) cat("First imputation registered.", "\n")

    betaYpostmean<-apply(betaYpost, c(1,2), mean)
    varYpostmean<-mean(varYpost)
    betapostmean<-apply(betapost, c(1,2), mean)
    omegapostmean<-apply(omegapost, c(1,2), mean)
    if (output==1) {
      cat("The posterior mean of the substantive model fixed effects estimates is:\n")
      print(betaYpostmean)
      cat("The posterior mean of the substantive model residual variance is:\n")
      print(varYpostmean)
      cat("The posterior mean of the fixed effects estimates is:\n")
      print(betapostmean)
      cat("The posterior mean of the level 1 covariance matrix is:\n")
      print(omegapostmean)
    }
    imp<-data.frame(imp)
    if (isnullcat==0) {
      for (i in 1:ncol(Y.cat)) {
        imp[,(1+ncolYcon[1]+i)]<-as.factor(imp[,(1+ncolYcon[1]+i)])
        levels(imp[,(1+ncolYcon[1]+i)])<-previous_levels[[i]]
      }
    }
    if (isnullcataux==0) {
      for (i in 1:ncol(Y.aux.cat)) {
        imp[,(1+ncolYcon[3]+i)]<-as.factor(imp[,(1+ncolYcon[3]+i)])
        levels(imp[,(1+ncolYcon[3]+i)])<-previous_levelsaux[[i]]
      }
    }
    if (ncolYcon[1]>0) {
      for (j in 1:(ncolYcon[1])) {
        imp[,j+1]=as.numeric(imp[,j+1])
      }
    }
    if (isnullcat==0) {
      if (is.null(colnamycat)) colnamycat=paste("Ycat", 1:ncol(Y.cat), sep = "")
    } else {
      colnamycat=NULL
      Y.cat=NULL
      Y.numcat=NULL
    }
    if (isnullcataux==0) {
      if (is.null(colnamyauxcat)) colnamyauxcat=paste("Ycat.aux", 1:ncol(Y.aux.cat), sep = "")
    } else {
      colnamyauxcat=NULL
      Y.aux.cat=NULL
      Y.aux.numcat=NULL
    }
    if (!is.null(Y.con)) {
      if (is.null(colnamycon)) colnamycon=paste("Ycon", 1:ncol(Y.con), sep = "")
    } else {
      colnamycon=NULL
    }
    if (!is.null(Y.aux.con)) {
      if (is.null(colnamyauxcon)) colnamyauxcon=paste("Ycon.aux", 1:ncol(Y.aux.con), sep = "")
    } else {
      colnamyauxcon=NULL
    }
    if (is.null(colnamysub)) colnamysub="Ysub"
    colnames(imp)<-c(colnamysub,colnamycon,colnamyauxcon,colnamycat,colnamyauxcat,"id","Imputation")
    if (isnullcat==0) {
      cnycatcomp<-rep(NA,(sum(Y.numcat)-length(Y.numcat)))
      count=0
      for ( j in 1:ncol(Y.cat)) {
        for (k in 1:(Y.numcat[j]-1)) {
          cnycatcomp[count+k]<-paste(colnamycat[j],k,sep=".")
        }
        count=count+Y.numcat[j]-1
      }

    } else {
      cnycatcomp<-NULL
    }
    if (isnullcataux==0) {
      cnyauxcatcomp<-rep(NA,(sum(Y.aux.numcat)-length(Y.aux.numcat)))
      count=0
      for ( j in 1:ncol(Y.aux.cat)) {
        for (k in 1:(Y.aux.numcat[j]-1)) {
          cnyauxcatcomp[count+k]<-paste(colnamyauxcat[j],k,sep=".")
        }
        count=count+Y.aux.numcat[j]-1
      }

    } else {
      cnyauxcatcomp<-NULL
    }
    cnamycomp<-c(colnamycon,colnamyauxcon,cnycatcomp,cnyauxcatcomp)
    dimnames(betapost)[1] <- list(colnamx)
    dimnames(betapost)[2] <- list(cnamycomp)
    dimnames(omegapost)[1] <- list(cnamycomp)
    dimnames(omegapost)[2] <- list(cnamycomp)
    dimnames(Yimp2)[2] <- list(cnamycomp)

    return(list("finimp"=imp,"collectbeta"=betapost,"collectomega"=omegapost, "finimp.latnorm" = Yimp2,  "collectbetaY"=betaYpost, "collectvarY"=varYpost))
  }
