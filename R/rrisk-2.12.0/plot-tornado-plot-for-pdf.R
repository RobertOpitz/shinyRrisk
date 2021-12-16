#' @description A function that creates a Tornado plot.
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session. 
#'
#' @name plotTornado
#' @aliases plotTornado
#' @title Function that draws a Tornado plot
#' @usage plotTornado(rriskModel,pdfGraph=FALSE)
#' @param rriskModel is an instance of the class \code{modelClass}
#' @param pdfGraph decides whether the plot is as PDF data to be saved
#' @keywords graphs
#' @export
#' @examples
#' \donttest{rriskModel <- init.Model1()
#' rriskModel<-run(rriskModel)
#' plotTornado(rriskModel,pdfGraph=FALSE)  }

# IS ALREADY IMPLEMENTED IN MODUL ANALYSIS (WHY ARE THE OTHER PLOTS STILL HERE)
plotTornado <- function(rriskModel, pdfGraph = FALSE) { # replication in der Funktion?
  cat("plotTornado for pdf\n")
  
  # Problem: tornado-charts für geschichtete mcrv items...
  
  #-----------------------------------------------------------------------------
  # define output
  #-----------------------------------------------------------------------------
  pdffilenames <- c()
  on.exit(return(pdffilenames))
  
  #-----------------------------------------------------------------------------
  # define help variables and functions
  #-----------------------------------------------------------------------------
  zscore <- function(x) (x-mean(x))/sd(x)
  mycol <- rriskModel@settings@mycol
  
  #-----------------------------------------------------------------------------
  # identify available mcrv items, available OF items and available uncert (mcrv and 'u', 'uv') items
  #-----------------------------------------------------------------------------
  OF.items<-c()
  mcrv.items<-c()
  unc.items<-c()
  mcrv.col<-c()
  mcrv.border<-c()
  
  #-----------------------------------------------------------------------------
  # get OF, mcrv, u and uv items
  #-----------------------------------------------------------------------------
  if (length(rriskModel@items@items) > 0) {
    
    for (i in 1:length(rriskModel@items@items)) {
      
      #-------------------------------------------------------------------------
      # get OF items
      #-------------------------------------------------------------------------
      if (rriskModel@items@items[[i]]@rolecode == "OF") {
        OF.items <- c(OF.items,rriskModel@items@items[[i]]@name)
      } # end if(rriskModel@items@items[[i]]@rolecode=="OF"){
      
      #-------------------------------------------------------------------------
      # get mcrv items of type "u" oder "uv"
      #-------------------------------------------------------------------------
      if(rriskModel@items@items[[i]]@typecode=="mcrv" & rriskModel@items@items[[i]]@stratum==""){
        if(!is.null(rriskModel@items@items[[i]]@data) & rriskModel@items@items[[i]]@fullc!=""){
          # collect mcrv items
          mcrv.items<-c(mcrv.items,rriskModel@items@items[[i]]@name)
          mcrv.col<-c(mcrv.col,"white")
          mcrv.border<-c(mcrv.border,rriskModel@settings@mycol)
          
          if(rriskModel@items@items[[i]]@rolecode=="u" | rriskModel@items@items[[i]]@rolecode=="uv"){
            # collect uncertainty mcrv items
            unc.items<-c(unc.items,rriskModel@items@items[[i]]@name)
            mcrv.col[length(mcrv.col)]<-rriskModel@settings@mycol
            mcrv.border[length(mcrv.border)]<-"white"
          } # end  if(rriskModel@items@items[[i]]@rolecode=="u" | rriskModel@items@items[[i]]@rolecode=="uv"){
        } else {
          cat(paste("Not evaluated mcrv item '", rriskModel@items@items[[i]]@name,"' will be ignored.\n",sep=""))
        } # end !is.null(rriskModel@items@items[[i]]@data) & rriskModel@items@items[[i]]@fullc!=""
      } else if (rriskModel@items@items[[i]]@typecode=="mcrv" & rriskModel@items@items[[i]]@stratum!="") {
        cat(paste("Statified mcrv item '", rriskModel@items@items[[i]]@name,"' will be ignored.\n",sep=""))
      }# end  if(rriskModel@items@items[[i]]@typecode=="mcrv"){
    } # end for
    
    if(length(OF.items)==0){
      stop("Tornado plot cannot be created, there is no 'OF' item defined in the model!",call.=FALSE)
    } # end  if(length(OF.items)==0){
    
    if(length(mcrv.items)==0){
      stop("Regression tree plot cannot be created, there is no uncertainty mcrv item(s) defined in the model!",call.=FALSE)
    }
    
    #if(length(mcrv.items)==0){
    #  stop("Regression tree plot cannot be created, there is no uncertainty mcrv item(s) defined in the model!",call.=FALSE)
    #}
    
  } else {
    stop("Tornado plot cannot be created, the list of model items is empty!",call.=FALSE)
  } # end  if(length(rriskModel@items@items)>0){
  
  #-----------------------------------------------------------------------------
  # evaluate data for tornado charts
  #-----------------------------------------------------------------------------
  if (length(rriskModel@output@fullout.1d)==0 | length(rriskModel@output@relaxout.1d)==0){
    stop("Tornado plot cannot be created, the list of 1d full and/or relax model simulation results is empty!",call.=FALSE)
  } else {
    #--------------------------------------------------------------------------
    # get data of 1d simulation for full model
    #--------------------------------------------------------------------------
    fullout.1d<-rriskModel@output@fullout.1d
    fullout.1d<-fullout.1d[c(mcrv.items,OF.items)]
    fullout.1d<-as.data.frame(fullout.1d)
    
    #--------------------------------------------------------------------------
    # get data of 1d simulation for relaxed model
    #--------------------------------------------------------------------------
    relaxout.1d<-rriskModel@output@relaxout.1d
    relaxout.1d<-relaxout.1d[c(mcrv.items,OF.items)]
    relaxout.1d<-as.data.frame(relaxout.1d)
    
    #--------------------------------------------------------------------------
    # transform 1d simulation data accoring to model settings
    #--------------------------------------------------------------------------
    if(rriskModel@settings@trans=="rank"){
      fullout.1d<-apply(fullout.1d,2,rank)
      relaxout.1d<-apply(relaxout.1d,2,rank)
      tornado.xlab0 <- "using rank transformated data"
    } else if(rriskModel@settings@trans=="z-score"){
      fullout.1d<-apply(fullout.1d,2,zscore)
      relaxout.1d<-apply(relaxout.1d,2,zscore)
      tornado.xlab0 <- "using z-transformed data"
    } else if(rriskModel@settings@trans=="identity"){
      tornado.xlab0 <- "using original data"
    } # end if(rriskModel@settings@trans=="rank"){
    
    #--------------------------------------------------------------------------
    # Create tornado plot for each 'OF' item
    #--------------------------------------------------------------------------
    for(i in 1:length(OF.items)){
      OF <- OF.items[i]
      
      OF.index<-which(colnames(fullout.1d)==OF)
      OF.full<-fullout.1d[,OF.index]
      #data.full<-fullout.1d[,-c(OF.index)]
      data.full<-as.data.frame(fullout.1d[,-c(OF.index)])
      
      OF.index<-which(colnames(fullout.1d)==OF)
      OF.relax<-relaxout.1d[,OF.index]
      #data.relax<-relaxout.1d[,-c(OF.index)]
      data.relax<-as.data.frame(relaxout.1d[,-c(OF.index)])
      
      effect.full <- rep(NA,length(mcrv.items))
      effect.relax <- rep(NA,length(mcrv.items))
      
      #-------------------------------------------------------------------------
      # model for sensitivity analysis, choices by settings: "correlation" (default), "regression"
      #-------------------------------------------------------------------------
      if(rriskModel@settings@sens == "correlation"){
        for(i in 1:length(mcrv.items)){
          effect.full[i] <- suppressWarnings(cor(data.full[,i],OF.full))
          effect.relax[i]<- suppressWarnings(cor(data.relax[,i],OF.relax))
        } # end for
        tornado.xlab <- paste("Correlation coefficients",tornado.xlab0)
      } else if(rriskModel@settings@sens == "regression"){
        effect.full <- lm(OF.full ~ -1 + data.full)$coeff
        effect.relax <- lm(OF.relax ~ -1 + data.relax)$coeff
        tornado.xlab <- paste("Regression coefficients",tornado.xlab0)
      } # end if(rriskModel@settings@sens == "correlation"){
      
      # sic to redefine colnames here
      names(effect.full) <- mcrv.items
      names(effect.relax) <- mcrv.items
      
      #effect.full2 <- effect.full^2
      #effect.relax2 <- effect.relax^2
      #max.effect.full2 <- max(effect.full2)
      #max.effect.relax2 <- max(effect.relax2)
      #r.h <- 1+ 1*length(effect.full)
      xlim <- c(min(effect.full,effect.relax,na.rm=TRUE),max(effect.full,effect.relax,na.rm=TRUE))
      if(xlim[1]==xlim[2] & xlim[1]>0){
        xlim<-c(0,xlim[2])
      } else if(xlim[1]==xlim[2] & xlim[2]<0){
        xlim<-c(xlim[2],0)
      }
      
      #-------------------------------------------------------------------------
      # create tornado plots
      #-------------------------------------------------------------------------
      graphRef<-gsub(x=rriskModel@name@name," ",replacement="")
      graphRef <- paste("\\ref{fig:", graphRef, "_tornado_", OF, "}", sep = "")
      
      if (pdfGraph == FALSE) {
        
        # EDIT(1)
        # X11(width=10,height=10)
        
        mainText=paste("\n Tornado chart of the outcome item '",OF,"'\n",graphRef, sep="")
      } else {
        mainText = "Tornado chart"
        pdffilenames.temp<-paste(gsub(x=rriskModel@name@name," ",replacement=""),"_tornado_",OF,sep="")
        pdffilenames<-c(pdffilenames,pdffilenames.temp)
        pdf(file=paste(pdffilenames.temp,".pdf",sep=""),width=10,height=10)
        mainText=paste("\n Tornado chart of the outcome item '",OF,"'",sep="")
      } # end if(pdfGraph==FALSE){
      
      par(mfrow=c(1,2),oma=c(2,1,ifelse(pdfGraph, 5, 5),1))
      suppressWarnings(barplot(effect.full[length(effect.full):1], horiz=TRUE, main="Full model",
                               col=rev(mcrv.col),xlim=xlim,border=rev(mcrv.border),xlab="",xaxt="s",las=1,lwd=2))
      suppressWarnings(abline(v=0))
      
      suppressWarnings(barplot(effect.relax[length(effect.relax):1], horiz=TRUE, main="Relaxed model",
                               col=rev(mcrv.col),xlim=xlim,border=rev(mcrv.border),xlab="",xaxt="s",las=1,lwd=2))
      suppressWarnings(abline(v=0))
      suppressWarnings(mtext(tornado.xlab,side=1,outer=TRUE))
      suppressWarnings(mtext(mainText,side=3,outer=TRUE,cex=1.4))
      
      #-------------------------------------------------------------------------
      # close graph devices
      #-------------------------------------------------------------------------
      if(pdfGraph==TRUE) dev.off()
    } # end for schleife
  } # end   if(length(rriskModel@output@fullout.1d)==0 | length(rriskModel@output@relaxout.1d)==0){
} # end of function plotTornado()