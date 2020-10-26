plotSpectrum2<-function(x,z,yLowerBound=0,yUpperBound=0,ti="",labs=0,bc,labScale=1.6,merge=F){
  #expects a standard full or folded mutation spectrum (uses length to discriminate) as a string of values
  if(yUpperBound==0){
    yUpperBound=(max(x)+(max(x)/50))
  }
  if(yLowerBound==0){
    yLowerBound=(min(x)-(min(x)/50))
  }
  yBounds=c(yLowerBound,yUpperBound)
  #type<-c("blue","black","red","grey","green","salmon")
  alpha<-1
  nA<-fastRgb(c(bc[1,],alpha))
  nC<-fastRgb(c(bc[2,],alpha))
  nG<-fastRgb(c(bc[3,],alpha))
  nT<-fastRgb(c(bc[4,],alpha))
  if(merge==T){
    type<-c(colMerge(from=bc[2,],to=bc[1,]),
      colMerge(from=bc[2,],to=bc[3,]),
      colMerge(from=bc[2,],to=bc[4,]),
      colMerge(from=bc[4,],to=bc[1,]),
      colMerge(from=bc[4,],to=bc[2,]),
      colMerge(from=bc[4,],to=bc[3,])
    )
  }
  if(merge==F) {
    type<-c(nA,nC,nG,nT)
  }
  typeText<-c(expression(bold(A)),
    expression(bold(C)),
    expression(bold(G)),
    expression(bold(T))
  )
  plot(c(1:length(x)),x,type="n",ylim=yBounds,xlim=c(0,length(x+1)),xaxt='n',xlab="",ylab="",main=ti)
  lines(c(1:16),(rep(yUpperBound,16)),col=nC,lwd=3)
  lines(c(17:32),(rep(yUpperBound,16)),col=nG,lwd=3)
  lines(c(33:48),(rep(yUpperBound,16)),col=nT,lwd=3)
  lines(c(49:64),(rep(yUpperBound,16)),col=nA,lwd=3)
  lines(c(65:80),(rep(yUpperBound,16)),col=nG,lwd=3)
  lines(c(81:96),(rep(yUpperBound,16)),col=nT,lwd=3)
  lines(c(97:112),(rep(yUpperBound,16)),col=nA,lwd=3)
  lines(c(113:128),(rep(yUpperBound,16)),col=nC,lwd=3)
  lines(c(129:144),(rep(yUpperBound,16)),col=nT,lwd=3)
  lines(c(145:160),(rep(yUpperBound,16)),col=nA,lwd=3)
  lines(c(161:176),(rep(yUpperBound,16)),col=nC,lwd=3)
  lines(c(177:192),(rep(yUpperBound,16)),col=nG,lwd=3)
  axis(side = 1, at = c(48.5,96.5,144.5,192.5),labels = F)
  for(sb in c(1:4)){
    rng<-c(1:48)+((sb*48)-48)
    mid<-rng[1]+24.5
    points(rng,x[rng],type="h",lwd=3,col=type[sb],lend=1)
    arrows(rng, as.numeric(x[rng])- as.numeric(z[1,][1:16]), rng, as.numeric(x[rng])+ as.numeric(z[1,][1:16]), length=0.05, angle=90, code=0,col=type[sb])
    if (labs!=0){
      mtext(typeText[sb],side=3,at=mid,col=type[sb],cex=labScale,line=labs,font=2)
    }
  }
}

