release.analysis<-function(data.set,label.set=NULL,new.lots=NULL,
                           analysis.level="Tier 3",where="screen"){

  # Extract the relavent pieces from the data frame
	n<-length(data.set)
	lot.data<-as.numeric(as.vector(data.set[6:n]))  # Convert the text to numbers
	if(is.null(label.set))
		lot.numb=NULL
	else
		lot.numb<-as.vector(label.set[6:n])
	if(!data.set[4]=="") 
		low.spec<-as.numeric(as.vector(data.set[4]))
	else 
		low.spec<-NULL
	if(!data.set[5]=="") 
		high.spec<-as.numeric(as.vector(data.set[5]))
	else 
		high.spec<-NULL
		
	units<-as.vector(data.set[3])
	data.name<-as.vector(data.set[1])

  # Run the capability analysis using long term Ï and format the output for plotting
	proc.ppk<-cap.analysis(lot.data,LSL=low.spec,USL=high.spec)
	if(is.null(proc.ppk$Pp))
	  Pp.value<-"Pp not calculated"
	else 
	  Pp.value<-paste("Pp= ",round(proc.ppk$Pp,3),sep="")
	
  Ppk.value<-paste("Ppk= ",round(proc.ppk$Pp.k,3),sep="")
	
  if(is.null(proc.ppk$Pp.l))
	  Ppl.value<-"Ppl not calculated"
	else
	  Ppl.value<-paste("Ppl= ",round(proc.ppk$Pp.l,3),sep="")
	
  if(is.null(proc.ppk$Pp.u))
	  Ppu.value<-"Ppu not calculated"
	else
	  Ppu.value<-paste("Ppu= ",round(proc.ppk$Pp.u,3),sep="")
	
  if (is.null(proc.ppk$exp.USL))
    exp.over<-"Exp>USL= N/A"
  else
    exp.over<-paste("Exp>USL= ",round(proc.ppk$exp.USL,3),"%",sep="")
	
  if (is.null(proc.ppk$exp.LSL))
    exp.under<-"Exp<LSL= N/A"
  else
    exp.under<-paste("Exp<LSL= ",round(proc.ppk$exp.LSL,3),"%",sep="")
  
  obs.over<-paste("Obs>USL= ",round(proc.ppk$obs.USL,3),"%",sep="")
  obs.under<-paste("Obs<LSL= ",round(proc.ppk$obs.LSL,3),"%",sep="")
  
  # Get the object for Shewart I/MR charts
  data.spc<-i.mr.data(lot.data,data.name,lot.numb,units,plot=FALSE)
  
  # Run the capability analysis using control chart Ï and format the output for plotting
	proc.cpk<-cap.analysis(lot.data,LSL=low.spec,USL=high.spec,sigma=data.spc$sd)
	
  if(is.null(proc.cpk$Pp))
	  Cp.value<-"Cp not calculated"
	else 
	  Cp.value<-paste("Cp= ",round(proc.cpk$Pp,3),sep="")
	
  Cpk.value<-paste("Cpk= ",round(proc.cpk$Pp.k,3),sep="")
	
  if(is.null(proc.cpk$Pp.l))
	  Cpl.value<-"Cpl not calculated"
	else
	  Cpl.value<-paste("Cpl= ",round(proc.cpk$Pp.l,3),sep="")
	
  if(is.null(proc.cpk$Pp.u))
	  Cpu.value<-"Cpu not calculated"
	else
	  Cpu.value<-paste("Cpu= ",round(proc.cpk$Pp.u,3),sep="")
	
	if (is.null(proc.cpk$exp.USL))
	  cexp.over<-"Exp>USL= N/A"
	else
	  cexp.over<-paste("Exp>USL= ",round(proc.cpk$exp.USL,3),"%",sep="")
	
	if (is.null(proc.cpk$exp.LSL))
	  cexp.under<-"Exp<LSL= N/A"
	else
	  cexp.under<-paste("Exp<LSL= ",round(proc.cpk$exp.LSL,3),"%",sep="")
  
	cobs.over<-paste("Obs>USL= ",round(proc.cpk$obs.USL,3),"%",sep="")
	cobs.under<-paste("Obs<LSL= ",round(proc.cpk$obs.LSL,3),"%",sep="")
  
  # Generate margin text for Cpk
#   I.margin <- paste(Cp.value,Cpk.value,Cpu.value,Cpl.value,sep="\n")
	I.margin <- paste(Cp.value,Cpk.value,sep="\n")
  MR.margin <- paste(cobs.over,cobs.under,cexp.over,cexp.under,sep="\n")
  cpk.margin <- paste(I.margin,MR.margin,sep="\n")
  
  # Get the object for CUSUM charts
  data.cusum<-cusum.data(lot.data,data.name,lot.numb,plot=FALSE)
  
  old.par<-par(no.readonly=TRUE)
  
  switch (where,
          "screen" = windows(width=7,height=9),
          "pdf" = {
            filename <- paste(data.name,".pdf",sep="")
            pdf(file=filename,width=7,height=9,paper="letter",
                title=paste("Analaysis of ",data.name,sep=""))
          },
          "ps" = {
            filename <- paste(data.name,".ps",sep="")
            postscript(file=filename,width=7,height=9,paper="letter",
                title=paste("Analaysis of ",data.name,sep=""),horizontal=FALSE)
          },
          "emf" = {
            filename <- paste(data.name,".emf",sep="")
            win.metafile(filename=filename,width=7,height=9)
          }
          )
  
  layout(matrix(c(1,1,2,2,5,3,5,4),4,2,byrow=TRUE))
  
	par(oma=c(0.5,0.5,0.5,3))
	
  par(mar=c(4.1,4.1,4.1,2.1))  
  
  plot.i.chart(data.spc,new.lots,nel.1.test=TRUE)
	  
  par(mar=c(4.1,4.1,4.1,12))
  plot.mr.chart(data.spc,new.lots,nel.1.test=TRUE)
	usr <- par()$usr
	text.at <- (usr[4]+usr[3])/2
	mtext(cpk.margin,side=4,line=6,at=text.at,las=2,cex=0.8)
  
  run.chart(lot.data,LSL=low.spec,USL=high.spec,
	          data.name=data.name,lot.num=lot.numb,units=units)
	plot.cusum(data.cusum)
    
	par(mar=c(10,2.1,4.1,2.1))
	norm.prob(lot.data,data.name=data.name,norm.test=TRUE)
  lft<-par()$usr[1]
  rt<-par()$usr[2]
	mtext(Pp.value,side=1,line=5,at=lft,adj=0,cex=0.8)
	mtext(Ppk.value,side=1,line=6,at=lft,adj=0,cex=0.8)
# 	mtext(Ppu.value,side=1,line=7,at=lft,adj=0,cex=0.8)
# 	mtext(Ppl.value,side=1,line=8,at=lft,adj=0,cex=0.8)
	mtext(obs.over,side=1,line=5,at=rt,adj=1,cex=0.8)
  mtext(obs.under,side=1,line=6,at=rt,adj=1,cex=0.8)  
  mtext(exp.over,side=1,line=7,at=rt,adj=1,cex=0.8)
	mtext(exp.under,side=1,line=8,at=rt,adj=1,cex=0.8)

  switch (where,
          "pdf" = dev.off(),
          "ps" = dev.off(),
          "emf" = dev.off())
  
  par<-old.par  
  
}
