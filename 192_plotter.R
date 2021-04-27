#craig anderson 210315
#scripts to plot lovely mutation spectra in 96 or 192 channel format. Header is important to ensure everything is ordered correctly.
#run as 
#par(mar=c(1,5,2,2)) ; plotSpectrum192(first_le[1,],bc=baseCol,labs = 1,yUpperBound = 0.1, yLowerBound = 0,labScale = 1,ylab="mu/Mb",RefHeight=-1.1,AltHeight=-2.15)
#RefHeight and AltHeight vary by aspect ratio, hence the options to help centre them in the colour boxes. Use labs=0 if you want to remove the colour boxes and labels.

fastRgb<-function(x){
  if(length(x)<4){
    x<-c(x,1)
  }
  rgb(x[1],x[2],x[3],x[4])
}

baseCol<-matrix(0,4,3)
minorc<-.3
baseCol[1,]=c(1,0.4,0.4)
baseCol[2,]=c(0.4,0.4,1)
baseCol[3,]=c(1,0.7,0.4)
baseCol[4,]=c(0.4,0.7,1)
bc<-baseCol

tnt_order_96<- c("ACA_A","ACC_A","ACG_A","ACT_A","CCA_A","CCC_A","CCG_A","CCT_A","GCA_A","GCC_A","GCG_A","GCT_A","TCA_A","TCC_A","TCG_A","TCT_A","ACA_G","ACC_G","ACG_G","ACT_G","CCA_G","CCC_G","CCG_G","CCT_G","GCA_G","GCC_G","GCG_G","GCT_G","TCA_G","TCC_G","TCG_G","TCT_G","ACA_T","ACC_T","ACG_T","ACT_T","CCA_T","CCC_T","CCG_T","CCT_T","GCA_T","GCC_T","GCG_T","GCT_T","TCA_T","TCC_T","TCG_T","TCT_T","ATA_A","ATC_A","ATG_A","ATT_A","CTA_A","CTC_A","CTG_A","CTT_A","GTA_A","GTC_A","GTG_A","GTT_A","TTA_A","TTC_A","TTG_A","TTT_A","ATA_C","ATC_C","ATG_C","ATT_C","CTA_C","CTC_C","CTG_C","CTT_C","GTA_C","GTC_C","GTG_C","GTT_C","TTA_C","TTC_C","TTG_C","TTT_C","ATA_G","ATC_G","ATG_G","ATT_G","CTA_G","CTC_G","CTG_G","CTT_G","GTA_G","GTC_G","GTG_G","GTT_G","TTA_G","TTC_G","TTG_G","TTT_G")

plotSpectrum96<-function(x,z,yLowerBound=0,yUpperBound=0,ti="",labs=0,bc,labScale=1.6,xlab="",ylab="",RefHeight=-1,AltHeight=-2){
  if ((tnt_order_96 %in% names(x) == "TRUE")[1]) { 
    x <- x[tnt_order_96]
  }
  yBounds=c(yLowerBound,yUpperBound)
  nC<-fastRgb(c(bc[2,],1))
  nT<-fastRgb(c(bc[4,],1))
  nC1<-fastRgb(c(bc[2,],0.75))
  nT1<-fastRgb(c(bc[4,],0.75))
  nC2<-fastRgb(c(bc[2,],0.5))
  nT2<-fastRgb(c(bc[4,],0.5))
  nC3<-fastRgb(c(bc[2,],0.25))
  nT3<-fastRgb(c(bc[4,],0.25))
  type<-c(nC1,nC2,nC3,nT1,nT2,nT3)
  type2<-c(nC,nC,nC,nT,nT,nT)
  typeText<-c(expression(bold(A)),
    expression(bold(G)),
    expression(bold(T)),
    expression(bold(A)),
    expression(bold(C)),
    expression(bold(G))
  )
  botText<-c(expression(bold(C)),
    expression(bold(T))
  )
  text_counter <- 0
  pos_counter <- 24

  plot(c(0:length(x)),c(0,x),type="n",ylim=c(yBounds[1]-(yBounds[2]/4),yBounds[2]),xlim=c(0,192),xaxt='n',yaxt='n', yaxs="i", xaxs="i",xlab="",ylab="",main=ti,bty ="n",frame.plot=FALSE)
  muClass <- c(rep(c(1,4),each=3))
  for (sb in 1:6) {
    rng<-c(1:16)+((sb*16)-16)
    rng2<- c(rng[1]-1,rng[1]-1,rep(rng[1:16],each=2))
    x2<-x[rng]
    x3<-c(0,rep(x2,each=2),0)
    mid<-rng[1]+7
    polygon(rng2,x3,col=type2[sb],border=NA)
    if (missing(z)) {
     }
    else{
     z <- z[tnt_order_192]
     arrows(rng-0.5, as.numeric(x[rng])- as.numeric(z[1,][1:16]), rng-0.5, as.numeric(x[rng])+ as.numeric(z[1,][1:16]), length=0.05, angle=90, code=0,col=type[sb])
    }
    if (labs==1){
    polygon(c(rng[1]-1,rng[1:16],rev(c(rng[1]-1,rng[1:16]))),c(rep(yBounds[1]-(yBounds[2]/4),17),(rep(yBounds[1]-(yBounds[2]/100),17))),col=type[sb],border = F)
    polygon(c(rng[1]-1,rng[1:16],rev(c(rng[1]-1,rng[1:16]))),c(rep(yBounds[1]-(yBounds[2]*2),17),(rep(yBounds[1]-(yBounds[2]/8),17))),col=type2[muClass[sb]],border = F)
    mtext(typeText[sb],side=1,at=mid,col="black",cex=labScale,line=AltHeight,font=2)
    if (sb == 2 || sb == 5 || sb == 8 || sb == 11) {
     text_counter <- text_counter+1
     mtext(botText[text_counter],side=1,at=pos_counter,col="black",cex=labScale,line=RefHeight,font=2)
     pos_counter <- pos_counter + 48
      }
    }
}
  axis(2, at=c(yBounds), labels=c(yBounds), lwd.ticks=1,las = 1,tick = TRUE)
  #axis(1, at=c(0:96), labels=NA,lwd.ticks=0)
  if (xlab != "") {
  mtext(xlab, side=1, line=-1.5, outer=TRUE,font=-1,cex=1.5)
  }
  if (ylab != "") {
  mtext(ylab, side=2, line=-2, outer=TRUE,font=1,cex=1.5)
  }
  if (labs==1) {
  mtext("Alt",side=1,at=(12*8)+7,col="black",cex=labScale,line=AltHeight,font=2)
  mtext("Ref",side=1,at=(12*8)+8,col="black",cex=labScale,line=RefHeight,font=2)
  }
}



tnt_order_192<- c("AAA_C", "AAC_C", "AAG_C", "AAT_C", "CAA_C", "CAC_C", "CAG_C", "CAT_C", "GAA_C", "GAC_C", "GAG_C", "GAT_C", "TAA_C", "TAC_C", "TAG_C", "TAT_C", "AAA_G", "AAC_G", "AAG_G", "AAT_G", "CAA_G", "CAC_G", "CAG_G", "CAT_G", "GAA_G", "GAC_G", "GAG_G", "GAT_G", "TAA_G", "TAC_G", "TAG_G", "TAT_G", "AAA_T", "AAC_T", "AAG_T", "AAT_T", "CAA_T", "CAC_T", "CAG_T", "CAT_T", "GAA_T", "GAC_T", "GAG_T", "GAT_T", "TAA_T", "TAC_T", "TAG_T", "TAT_T", "ACA_A", "ACC_A", "ACG_A", "ACT_A", "CCA_A", "CCC_A", "CCG_A", "CCT_A", "GCA_A", "GCC_A", "GCG_A", "GCT_A", "TCA_A", "TCC_A", "TCG_A", "TCT_A", "ACA_G", "ACC_G", "ACG_G", "ACT_G", "CCA_G", "CCC_G", "CCG_G", "CCT_G", "GCA_G", "GCC_G", "GCG_G", "GCT_G", "TCA_G", "TCC_G", "TCG_G", "TCT_G", "ACA_T", "ACC_T", "ACG_T", "ACT_T", "CCA_T", "CCC_T", "CCG_T", "CCT_T", "GCA_T", "GCC_T", "GCG_T", "GCT_T", "TCA_T", "TCC_T", "TCG_T", "TCT_T", "AGA_A", "AGC_A", "AGG_A", "AGT_A", "CGA_A", "CGC_A", "CGG_A", "CGT_A", "GGA_A", "GGC_A", "GGG_A", "GGT_A", "TGA_A", "TGC_A", "TGG_A", "TGT_A", "AGA_C", "AGC_C", "AGG_C", "AGT_C", "CGA_C", "CGC_C", "CGG_C", "CGT_C", "GGA_C", "GGC_C", "GGG_C", "GGT_C", "TGA_C", "TGC_C", "TGG_C", "TGT_C", "AGA_T", "AGC_T", "AGG_T", "AGT_T", "CGA_T", "CGC_T", "CGG_T", "CGT_T", "GGA_T", "GGC_T", "GGG_T", "GGT_T", "TGA_T", "TGC_T", "TGG_T", "TGT_T", "ATA_A", "ATC_A", "ATG_A", "ATT_A", "CTA_A", "CTC_A", "CTG_A", "CTT_A", "GTA_A", "GTC_A", "GTG_A", "GTT_A", "TTA_A", "TTC_A", "TTG_A", "TTT_A", "ATA_C", "ATC_C", "ATG_C", "ATT_C", "CTA_C", "CTC_C", "CTG_C", "CTT_C", "GTA_C", "GTC_C", "GTG_C", "GTT_C", "TTA_C", "TTC_C", "TTG_C", "TTT_C", "ATA_G", "ATC_G", "ATG_G", "ATT_G", "CTA_G", "CTC_G", "CTG_G", "CTT_G", "GTA_G", "GTC_G", "GTG_G", "GTT_G", "TTA_G", "TTC_G", "TTG_G", "TTT_G")


##new colourful version 210211
fastRgb<-function(x){
  if(length(x)<4){
    x<-c(x,1)
  }
  rgb(x[1],x[2],x[3],x[4])
}

baseCol<-matrix(0,4,3)
minorc<-.3
baseCol[1,]=c(1,0.4,0.4)
baseCol[2,]=c(0.4,0.4,1)
baseCol[3,]=c(1,0.7,0.4)
baseCol[4,]=c(0.4,0.7,1)
bc<-baseCol



plotSpectrum192<-function(x,z,yLowerBound=0,yUpperBound=0,ti="",labs=0,bc,labScale=1.6,xlab="",ylab="",RefHeight=-1,AltHeight=-2){
  if ((tnt_order_192 %in% names(x) == "TRUE")[1]) { 
    x <- x[tnt_order_192]
  }
  yBounds=c(yLowerBound,yUpperBound)
  nA<-fastRgb(c(bc[1,],1))
  nC<-fastRgb(c(bc[2,],1))
  nG<-fastRgb(c(bc[3,],1))
  nT<-fastRgb(c(bc[4,],1))
  nA1<-fastRgb(c(bc[1,],0.75))
  nC1<-fastRgb(c(bc[2,],0.75))
  nG1<-fastRgb(c(bc[3,],0.75))
  nT1<-fastRgb(c(bc[4,],0.75))
  nA2<-fastRgb(c(bc[1,],0.5))
  nC2<-fastRgb(c(bc[2,],0.5))
  nG2<-fastRgb(c(bc[3,],0.5))
  nT2<-fastRgb(c(bc[4,],0.5))
  nA3<-fastRgb(c(bc[1,],0.25))
  nC3<-fastRgb(c(bc[2,],0.25))
  nG3<-fastRgb(c(bc[3,],0.25))
  nT3<-fastRgb(c(bc[4,],0.25))
  type<-c(nA1,nA2,nA3,nC1,nC2,nC3,nG1,nG2,nG3,nT1,nT2,nT3)
  type2<-c(nA,nA,nA,nC,nC,nC,nG,nG,nG,nT,nT,nT)
  typeText<-c(expression(bold(C)),
    expression(bold(G)),
    expression(bold(T)),
    expression(bold(A)),
    expression(bold(G)),
    expression(bold(T)),
    expression(bold(A)),
    expression(bold(C)),
    expression(bold(T)),
    expression(bold(A)),
    expression(bold(C)),
    expression(bold(G))
  )
  botText<-c(expression(bold(A)),
    expression(bold(C)),
    expression(bold(G)),
    expression(bold(T))
  )
  text_counter <- 0
  pos_counter <- 24
  plot(c(0:length(x)),c(0,x),type="n",ylim=c(yBounds[1]-(yBounds[2]/4),yBounds[2]),xlim=c(0,192),xaxt='n',yaxt='n', yaxs="i", xaxs="i",xlab="",ylab="",main=ti,bty ="n",frame.plot=FALSE)
  muClass <- c(rep(c(1,4,7,10),each=3))
  for (sb in 1:12) {
    rng<-c(1:16)+((sb*16)-16)
    rng2<- c(rng[1]-1,rng[1]-1,rep(rng[1:16],each=2))
    x2<-x[rng]
    x3<-c(0,rep(x2,each=2),0)
    mid<-rng[1]+7
    polygon(rng2,x3,col=type2[sb],border=NA)
    if (missing(z)) {
     }
    else{
     z <- z[tnt_order_192]
     arrows(rng-0.5, as.numeric(x[rng])- as.numeric(z[1,][1:16]), rng-0.5, as.numeric(x[rng])+ as.numeric(z[1,][1:16]), length=0.05, angle=90, code=0,col=type[sb])
    }
    if (labs==1){
    polygon(c(rng[1]-1,rng[1:16],rev(c(rng[1]-1,rng[1:16]))),c(rep(yBounds[1]-(yBounds[2]/4),17),(rep(yBounds[1]-(yBounds[2]/100),17))),col=type[sb],border = F)
    polygon(c(rng[1]-1,rng[1:16],rev(c(rng[1]-1,rng[1:16]))),c(rep(yBounds[1]-(yBounds[2]*2),17),(rep(yBounds[1]-(yBounds[2]/8),17))),col=type2[muClass[sb]],border = F)
    mtext(typeText[sb],side=1,at=mid,col="black",cex=labScale,line=AltHeight,font=2)
    if (sb == 2 || sb == 5 || sb == 8 || sb == 11) {
     text_counter <- text_counter+1
     mtext(botText[text_counter],side=1,at=pos_counter,col="black",cex=labScale,line=RefHeight,font=2)
     pos_counter <- pos_counter + 48
      }
    }
}
  axis(2, at=c(yBounds), labels=c(yBounds), lwd.ticks=1,las = 1,tick = TRUE)
  #axis(1, at=c(0:192), labels=NA,lwd.ticks=0)
  if (xlab != "") {
  mtext(xlab, side=1, line=-1.5, outer=TRUE,font=-1,cex=1.5)
  }
  if (ylab != "") {
  mtext(ylab, side=2, line=-2, outer=TRUE,font=1,cex=1.5)
  }
  if (labs==1) {
  mtext("Alt",side=1,at=(12*16)+7,col="black",cex=labScale,line=AltHeight,font=2)
  mtext("Ref",side=1,at=(12*16)+8,col="black",cex=labScale,line=RefHeight,font=2)
  }
}
