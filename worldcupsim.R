do.world.cup=function(){
	toInstall <- c("png", "devtools", "MASS", "RCurl","XML","plotrix","countrycode")
	lapply(toInstall, library, character.only = TRUE)
	# Some helper functions, lineFinder and makeTable from dsparks
	lineFinder <- function(string, vector){ (1:length(vector))[regexpr(string, vector) != -1] }
	makeTable=function(vector, toregex){  do.call(rbind, strsplit(vector, toregex))  }
	
	plotscore=function(currscore,currx,curry,currwin){
		if(length(currscore)==2){currscore2=currscore[1:2]}else{currscore2=currscore[3:4]}
		points(currx+c(-0.8,-0.2),rep(curry-0.25,2),col="gold",pch=15,cex=3.5)
		text(currx-0.5,curry-0.25,paste(currscore2,collapse="  "),cex=1.5)
		if(length(currscore)>2){
			newtext=countrycode(as.character(currwin),origin="country.name",destination="genc3c")
			newtext[is.na(newtext)]=substr(toupper(as.character(currwin)[is.na(newtext)]),1,3)
			text(currx-0.5,curry-1,paste (newtext,"win",paste(currscore[1:2],collapse="-"),"on penalties"),cex=0.75)
		}	
	}
	
	playmatch=function(team1,team2,maxgoals=12,penalties=F,animated=F){
		
		matchheader=function(team1,team2){
			text(9.5+0.6,7,as.character(team1),cex=4,col="red")
			text(9.5+0.6,5,"Vs.",cex=4,col="red")
			text(9.5+0.6,3,as.character(team2),cex=4,col="red")
			flagstoget=sapply(1:2,function(x){which(stats$Team==c(team1,team2)[x])})
			boxParameter <- 45
			flagycoords=c(8.25,1.75)
			for(j in 1:2){
				currflag=flagpngs[[flagstoget[j]]]
				Dims=flagDimensions[flagstoget[j], ]
				rasterImage(currflag,  # Plot each flag with these boundaries:
				9.5+0.5-Dims[2]/boxParameter, flagycoords[j]-Dims[1]/boxParameter,
				9.5+0.5+Dims[2]/boxParameter, flagycoords[j]+Dims[1]/boxParameter)
				rect(9.5+0.5-Dims[2]/boxParameter, flagycoords[j]-Dims[1]/boxParameter,
				9.5+0.5+Dims[2]/boxParameter, flagycoords[j]+Dims[1]/boxParameter)
			}
		}
		
		matchfooter=function(team1){
			text(9.5+0.6,7,as.character(team1),cex=4,col="red")
			text(9.5+0.6,5,"WINS!",cex=4,col="red")
			flagstoget=which(stats$Team==c(team1))
			boxParameter <- 45
			flagycoords=c(3)
			for(j in 1){
				currflag=flagpngs[[flagstoget[j]]]
				Dims=flagDimensions[flagstoget[j], ]
				rasterImage(currflag,  # Plot each flag with these boundaries:
				9.5+0.5-Dims[2]/boxParameter, flagycoords[j]-Dims[1]/boxParameter,
				9.5+0.5+Dims[2]/boxParameter, flagycoords[j]+Dims[1]/boxParameter)
				rect(9.5+0.5-Dims[2]/boxParameter, flagycoords[j]-Dims[1]/boxParameter,
				9.5+0.5+Dims[2]/boxParameter, flagycoords[j]+Dims[1]/boxParameter)
			}
		}
		
		plotscore2=function(team1,team2){
			newtext=countrycode(c(team1,team2),origin="country.name",destination="genc3c")
			newtext[is.na(newtext)]=substr(toupper(c(team1,team2)[is.na(newtext)]),1,3)
			text(10,12.25,paste(newtext,collapse="           "),cex=1.5)
			flagstoget=sapply(1:2,function(x){which(stats$Team==c(team1,team2)[x])})
			boxParameter <- 60
			flagxcoords=c(7.5,12.5)
			flagycoords=12.25
			for(j in 1:2){
				currflag=flagpngs[[flagstoget[j]]]
				Dims=flagDimensions[flagstoget[j], ]
				rasterImage(currflag,  # Plot each flag with these boundaries:
				flagxcoords[j]-Dims[2]/boxParameter, flagycoords-Dims[1]/boxParameter,
				flagxcoords[j]+Dims[2]/boxParameter, flagycoords+Dims[1]/boxParameter)
				rect(flagxcoords[j]-Dims[2]/boxParameter, flagycoords-Dims[1]/boxParameter,
				flagxcoords[j]+Dims[2]/boxParameter, flagycoords+Dims[1]/boxParameter)
			}
			
		}
		
		drawpitch=function(){
			rect(0,-2,20,12,col="darkgreen")
			for(i in seq(0,18,4)){
				rect(i,-2,i+2,12,col="forestgreen",border=NA)
			}
			rect(0+0.1,-2+0.1,20-0.1,12-0.1,border="white",lwd=4)
			segments(10,-2+0.1,10,12-0.1,lwd=4,col="white")
			draw.circle(10,5,1.5,nv=100,border="white",col=NA,lty=1,lwd=4)
			lines(c(0+0.1,0+0.1+1,0+0.1+1,0+0.1),c(7,7,3,3),col="white",lwd=4)
			lines(c(0+0.1,0+0.1+2,0+0.1+2,0+0.1),c(8,8,2,2),col="white",lwd=4)
			
			lines(c(20-0.1,20-0.1-1,20-0.1-1,20-0.1),c(7,7,3,3),col="white",lwd=4)
			lines(c(20-0.1,20-0.1-2,20-0.1-2,20-0.1),c(8,8,2,2),col="white",lwd=4)
			points(10,5,col="white",pch=16,cex=2)
			
			
		}
		
		team1stats=stats[stats$Team==team1,]
		team2stats=stats[stats$Team==team2,]
		
		wratio1=as.numeric(as.character(team1stats$W))/as.numeric(as.character(team1stats$Pld))
		wratio2=as.numeric(as.character(team2stats$W))/as.numeric(as.character(team2stats$Pld))	
		wratio3=2-wratio2
		
		#choose a random number of goal attempts
		ngoals=sample(1:maxgoals,1,prob=(((maxgoals-(1:maxgoals))+1)/maxgoals)^3)
		score=c(0,0)
		
		if(animated==T){
			plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
			
			drawpitch()
			matchheader(team1,team2)
			Sys.sleep(3)
			drawpitch()
			plotscore(score,10.5,12.5,NA)
			plotscore2(team1,team2)
			Sys.sleep(2)
		}
		
		if(ngoals>0){
			for(x in 1:ngoals){
				unum=runif(1,0,2)
				if(animated==T){
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					diff1=c(abs(unum-wratio1),abs(unum-wratio3))
					text(10,5,paste(c(team1,team2)[diff1==min(diff1)],"shoots!"),offset=0,cex=4,col="red")
					Sys.sleep(2)
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
				}
				
				if(unum<wratio1){
					score[1]=score[1]+1
					if(animated==T){
						for(i in 1:3){
							text(10,5,paste(team1,"scores!"),offset=0,cex=4,col="red")
							Sys.sleep(0.5)
							text(10,5,paste(team1,"scores!"),offset=0,cex=4,col="gold")
							
						}
					}
					
					}else if(unum>wratio3){
					score[2]=score[2]+1
					if(animated==T){
						for(i in 1:3){
							text(10,5,paste(team2,"scores!"),offset=0,cex=4,col="red")
							Sys.sleep(0.5)
							text(10,5,paste(team2,"scores!"),offset=0,cex=4,col="gold")
						}
					}
					
					} else if (animated==T){
					text(10,5,paste(c(team1,team2)[diff1==min(diff1)],"misses!"),offset=0,cex=4,col="red")	
					
				}
				if(animated==T){
					Sys.sleep(2)
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					Sys.sleep(runif(1,3,5))
				}
			}
			} else if (ngoals==0&animated==T){
			for(x in 1:runif(1,1,3)){
				drawpitch()
				plotscore(score,10.5,12.5,NA)
				plotscore2(team1,team2)
				unum=runif(1,0,2)
				diff1=c(abs(unum-wratio1),abs(unum-wratio3))
				text(10,5,paste(c(team1,team2)[diff1==min(diff1)],"shoots!"),offset=0,cex=4,col="red")
				Sys.sleep(2)
				drawpitch()
				plotscore(score,10.5,12.5,NA)
				plotscore2(team1,team2)
				text(10,5,paste(c(team1,team2)[diff1==min(diff1)],"misses!"),offset=0,cex=4,col="red")	
				Sys.sleep(2)
				
				drawpitch()
				plotscore(score,10.5,12.5,NA)
				plotscore2(team1,team2)
				Sys.sleep(runif(1,3,5))
			}
			
		}
		if(penalties==F|(score[1]!=score[2]&penalties==T)){
			if(animated==T){
				drawpitch()
				if(score[1]>score[2]){
					
					matchfooter(team1)
					} else {
					matchfooter(team2)
					
				}
				Sys.sleep(2)
			}	
			return(score)
			} else if(score[1]==score[2]&penalties==T){
			#do penalties
			if(animated==T){
				drawpitch()
				plotscore(score,10.5,12.5,NA)
				plotscore2(team1,team2)
				text(10,5,"Penalty shooutout!",offset=0,cex=4,col="red")
				Sys.sleep(3)
				drawpitch()
				plotscore(score,10.5,12.5,NA)
				plotscore2(team1,team2)
			}
			fulltime=score
			kicks1=rbinom(5,1,wratio1)
			kicks2=rbinom(5,1,wratio2)
			if(animated==F){
				score[1]=score[1]+sum(kicks1)
				score[2]=score[2]+sum(kicks2)
				} else {
				
				for(x in 1:5){
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					text(10,5,paste(team1,"shoots!"),offset=0,cex=4,col="red")
					Sys.sleep(2)
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					if(kicks1[x]==1){
						text(10,5,paste(team1,"scores!"),offset=0,cex=4,col="red")	
						score[1]=score[1]+1
						} else {
						text(10,5,paste(team1,"misses!"),offset=0,cex=4,col="red")	
					}
					Sys.sleep(1)
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					Sys.sleep(1)
					text(10,5,paste(team2,"shoots!"),offset=0,cex=4,col="red")
					Sys.sleep(2)
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					if(kicks2[x]==1){
						text(10,5,paste(team2,"scores!"),offset=0,cex=4,col="red")	
						score[2]=score[2]+1
						} else {
						text(10,5,paste(team2,"misses!"),offset=0,cex=4,col="red")	
					}
					Sys.sleep(1)
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					Sys.sleep(1)
				}
			}
			
			while(score[1]==score[2]){
				kicks1=rbinom(1,1,wratio1)
				kicks2=rbinom(1,1,wratio2)
				if(animated==F){
					score[1]=score[1]+sum(kicks1)
					score[2]=score[2]+sum(kicks2)
					} else {
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					text(10,5,paste(team1,"shoots!"),offset=0,cex=4,col="red")
					Sys.sleep(3)
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					if(kicks1==1){
						text(10,5,paste(team1,"scores!"),offset=0,cex=4,col="red")	
						score[1]=score[1]+1
						} else {
						text(10,5,paste(team1,"misses!"),offset=0,cex=4,col="red")	
					}
					Sys.sleep(1)
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					Sys.sleep(1)
					text(10,5,paste(team2,"shoots!"),offset=0,cex=4,col="red")
					Sys.sleep(3)
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					if(kicks2==1){
						text(10,5,paste(team2,"scores!"),offset=0,cex=4,col="red")	
						score[2]=score[2]+1
						} else {
						text(10,5,paste(team2,"misses!"),offset=0,cex=4,col="red")	
					}
					Sys.sleep(1)
					drawpitch()
					plotscore(score,10.5,12.5,NA)
					plotscore2(team1,team2)
					Sys.sleep(1)
					
				}
			}
			if(animated==T){
				drawpitch()
				if(score[1]>score[2]){
					
					matchfooter(team1)
					} else {
					matchfooter(team2)
					
				}
				Sys.sleep(2)
			}
			return(c(score,fulltime))
		}
	}
	
	importHTML <- readLines("https://en.wikipedia.org/wiki/National_team_appearances_in_the_FIFA_World_Cup")
	stats=readHTMLTable(doc=importHTML,header=T,which=6)#get the stats table
	
	names(stats)=gsub("\n","",names(stats))#for convenience
	#find the appropriate section to get flags from
	flagstart=lineFinder("<h2><span class=\"mw-headline\" id=\"Overall_team_records\">",importHTML )
	flagend=lineFinder("<dl><dt>Breakdown of successor teams</dt></dl>",importHTML )
	flagend=max(flagend)
	importHTML=importHTML[flagstart:flagend]
	flaglines=importHTML[lineFinder("flagicon", importHTML)]
	
	#extract URLs from HTML
	pngURLs <- makeTable(makeTable(flaglines, "src=\"//")[, 2], "\" width=\"")[, 1]
	pngURLs <- paste0("https://", pngURLs)
	
	#download the flags
	flagpngs=lapply(pngURLs,function(x){
		readPNG(getURLContent(x))
	})
	
	
	
	# The dimensions of each item are equal to the pixel dimensions of the .PNG
	flagDimensions <- t(sapply(flagpngs, function(ll){
		dim(ll)[1:2]
	}))
	
	#qualifiers
	#draw 32 teams from pool - higher ranked teams have a greater probability
	stats$Rank=as.numeric(as.character(stats$Rank))
	stats$Team=gsub("\\[[^\\]]*\\]", "",stats$Team)
	selectprobs=(((max(stats$Rank)-stats$Rank)+1)^3)
	selectprobs=selectprobs/max(selectprobs)
	selectteams=as.character(sample(stats$Team,replace=F,32,prob=stats$selectprobs))
	groupvec=.bincode(x=c(1:32),breaks=seq(1,32+4,4), include.lowest = F,right=F)
	groups=lapply(unique(groupvec),function(x){
		selectteams[groupvec==x]
	})
	names(groups)=LETTERS[1:8]
	fixtures=lapply(groups,function(x){
		t(combn(x,2))
	})
	
	groupscores=lapply(fixtures,function(x){
		t(sapply(1:nrow(x),function(y){
			playmatch(x[y,1],x[y,2])
		}))
	})
	
	groupscores2=lapply(1:length(fixtures),function(x){
		result1=matrix(0,ncol=5,nrow=length(groups[[x]]))
		colnames(result1)=c("W","D","L","GD","Score")
		row.names(result1)=groups[[x]]
		for(y in 1:nrow(fixtures[[x]])){
			currfix=fixtures[[x]][y,]
			currscore=groupscores[[x]][y,]
			if(currscore[1]>currscore[2]){
				result1[row.names(result1)==currfix[1],"W"]=result1[row.names(result1)==currfix[1],"W"]+1
				result1[row.names(result1)==currfix[2],"L"]=result1[row.names(result1)==currfix[2],"L"]+1
				
				} else if (currscore[2]>currscore[1]){
				result1[row.names(result1)==currfix[1],"L"]=result1[row.names(result1)==currfix[1],"L"]+1
				result1[row.names(result1)==currfix[2],"W"]=result1[row.names(result1)==currfix[2],"W"]+1
				} else {
				result1[row.names(result1)==currfix[1],"D"]=result1[row.names(result1)==currfix[1],"D"]+1
				result1[row.names(result1)==currfix[2],"D"]=result1[row.names(result1)==currfix[2],"D"]+1
			}
			result1[row.names(result1)==currfix[1],"GD"]=currscore[1]-currscore[2]
			result1[row.names(result1)==currfix[2],"GD"]=currscore[2]-currscore[1]
		}
		result1[,5]=result1[,1:3]%*%c(3,1,0)	
		rank=rank(result1[,5]+(result1[,4]/100),ties.method="min")
		result1=result1[order(rank,decreasing=T),]	
	})
	
	options(warn=-1)
	
	dev.new(width=20, height=10)
	par(xpd=T)
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	
	for(i in 1:3){
		text(9.5,4,"The World Cup begins!",cex=4,col="black")
		line <- Sys.sleep(0.5)
		text(9.5,4,"The World Cup begins!",cex=4,col="darkred")
		line <- Sys.sleep(0.5)
	}
	Sys.sleep(0.5)
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	ycoords=rep(c(6,0),each=4)
	xcoords=rep(seq(1,21,6),2)-0.75
	boxParameter <- 125
	text(9.5,11,"Group Stage",cex=2.5)
	for(i in 1:8){
		colnames(groupscores2[[i]])[5]="Pts"
		textycoords=rev(ycoords[i]+0.25+seq(0,1.5,0.5))
		flagxcoords=xcoords[i]-1.5
		rect(xcoords[i]-2.1,ycoords[i]+2.1,xcoords[i]+2.5,ycoords[i]+0.5+2.1,border=NA,col="gray88")
		segments(xcoords[i]-2.1,ycoords[i]+2.1,xcoords[i]+2.5,ycoords[i]+2.1,lwd=2.5,col="gray75")
		
		addtable2plot(xcoords[i],ycoords[i],groupscores2[[i]],bty="n",display.rownames=F,hlines=TRUE,
		vlines=F,lwd=0.01,box.col="gray88",xpad=0.3)
		
		
		newtext=countrycode(row.names(groupscores2[[i]]),origin="country.name",destination="genc3c")
		newtext[is.na(newtext)]=substr(toupper(row.names(groupscores2[[i]])[is.na(newtext)]),1,3)
		text(xcoords[i],textycoords,newtext,pos=2)
		text(xcoords[i]-1.2,ycoords[i]+3,paste("Group",LETTERS[i]),cex=1.5)
		text(flagxcoords+0.15,ycoords[i]+2.35,"Country")
		flagstoget=sapply(1:4,function(x){which(stats$Team==row.names(groupscores2[[i]])[x])})
		for(j in 1:4){
			currflag=flagpngs[[flagstoget[j]]]
			Dims=flagDimensions[flagstoget[j], ]
			rasterImage(currflag,  # Plot each flag with these boundaries:
			flagxcoords-Dims[2]/boxParameter, textycoords[j]-Dims[1]/boxParameter,
			flagxcoords+Dims[2]/boxParameter, textycoords[j]+Dims[1]/boxParameter)
			rect(flagxcoords-Dims[2]/boxParameter, textycoords[j]-Dims[1]/boxParameter,
			flagxcoords+Dims[2]/boxParameter, textycoords[j]+Dims[1]/boxParameter)
		}
		Sys.sleep(0.2)
	}
	
	
	Sys.sleep(5)
	
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	for(i in 1:3){
		text(9.5,4,"Knockout round",cex=4,col="black")
		line <- Sys.sleep(0.5)
		text(9.5,4,"Knockout round",cex=4,col="darkred")
		line <- Sys.sleep(0.5)
	}
	
	
	groupwinners=sapply(groupscores2,function(x){
		row.names(x)[1]
	})
	
	grouprunnersup=sapply(groupscores2,function(x){
		row.names(x)[2]
	})
	
	
	fixtures2=data.frame(team1=groupwinners,team2=sample(grouprunnersup,replace=F))
	
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	
	roundof16=lapply(1:nrow(fixtures2),function(x){
		playmatch(fixtures2[x,1],fixtures2[x,2],penalties=T)
	})
	
	roundof16result=sapply(1:nrow(fixtures2),function (x) {
		ifelse(roundof16[[x]][1]>roundof16[[x]][2],as.character(fixtures2[x,1]),as.character(fixtures2[x,2]))
	})
	
	plot16stage1=function(){
		ycoords=seq(11,-3,-1.91)+0.55
		xcoords=c(-1,1.5)
		boxParameter <- 80
		text(median(xcoords)-0.5,12.3,"Round of 16",cex=1.1)
		for(i in 1:nrow(fixtures2)){
			flagycoords=ycoords[i]-0.5
			flagxcoords=xcoords+c(-0.51,-0.51)
			rect(flagxcoords[1]-0.75,flagycoords-0.7,flagxcoords[2]+0.75,ycoords[i]+0.4,border=NA,col="gray88")
			segments(flagxcoords[1]-0.75,flagycoords-0.7,flagxcoords[2]+0.75,flagycoords-0.7,lwd=2.5,col="gray75")
			newtext=countrycode(as.character(t(fixtures2[i,])),origin="country.name",destination="genc3c")
			newtext[is.na(newtext)]=substr(toupper(as.character(t(fixtures2[i,]))[is.na(newtext)]),1,3)
			text(xcoords,ycoords[i],newtext,pos=2)
			flagstoget=sapply(1:2,function(x){which(stats$Team==as.character(t(fixtures2[i,]))[x])})
			
			for(j in 1:2){
				currflag=flagpngs[[flagstoget[j]]]
				Dims=flagDimensions[flagstoget[j], ]
				rasterImage(currflag,  # Plot each flag with these boundaries:
				flagxcoords[j]-Dims[2]/boxParameter, flagycoords-Dims[1]/boxParameter,
				flagxcoords[j]+Dims[2]/boxParameter, flagycoords+Dims[1]/boxParameter)
				rect(flagxcoords[j]-Dims[2]/boxParameter, flagycoords-Dims[1]/boxParameter,
				flagxcoords[j]+Dims[2]/boxParameter, flagycoords+Dims[1]/boxParameter)
			}
		}
	}
	
	
	
	plot16stage1()
	Sys.sleep(1)
	for(i in seq(1,8,2)){
		ycoords=seq(11,-3,-1.91)+0.55
		xcoords=c(-1,1.5)
		plotscore(roundof16[[i]],median(xcoords),ycoords[i],roundof16result[i])
		Sys.sleep(0.5)
		plotscore(roundof16[[i+1]],median(xcoords),ycoords[i+1],roundof16result[i+1])
		Sys.sleep(1)	
	}
	
	Sys.sleep(4)
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	for(i in 1:3){
		text(9.5,4,"Quarter-finals",cex=4,col="black")
		line <- Sys.sleep(0.5)
		text(9.5,4,"Quarter-finals",cex=4,col="darkred")
		line <- Sys.sleep(0.5)
	}
	
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	plot16stage1()
	plot16stage2=function(){
		for(i in seq(1,8,2)){
			ycoords=seq(11,-3,-1.91)+0.55
			xcoords=c(-1,1.5)
			plotscore(roundof16[[i]],median(xcoords),ycoords[i],roundof16result[i])
			plotscore(roundof16[[i+1]],median(xcoords),ycoords[i+1],roundof16result[i+1])	
			linex=c(c(xcoords[2]+0.25,xcoords[2]+1),rev(c(xcoords[2]+0.25,xcoords[2]+1)))
			liney=c(rep(ycoords[c(i)]-0.4,2),rep(ycoords[c(i+1)]-0.4,2))
			liney2=median(ycoords[c(i,i+1)])-0.4
			lines(linex,liney,lwd=2.5,col="darkgreen")
			lines(c(linex[2],linex[2]+1),rep(liney2,2),lwd=2.5,col="darkgreen")
		}
	}
	plot16stage2()
	
	fixtures3=data.frame(team1=roundof16result[seq(1,8,2)],team2=roundof16result[seq(2,8,2)])
	
	
	plotquarter=function(){
		ycoords=seq(11,-3,-1.91)+1
		ycoords=sapply(seq(1,8,2),function(x){median(ycoords[c(x,x+1)])-0.4})
		xcoords=c(-1,1.5)+5.75
		boxParameter <- 80
		text(median(xcoords)-0.5,12.3,"Quarter finals",cex=1.1)
		for(i in 1:nrow(fixtures3)){
			flagycoords=ycoords[i]-0.5
			flagxcoords=xcoords+c(-0.51,-0.51)
			rect(flagxcoords[1]-0.75,flagycoords-0.7,flagxcoords[2]+0.75,ycoords[i]+0.4,border=NA,col="gray88")
			segments(flagxcoords[1]-0.75,flagycoords-0.7,flagxcoords[2]+0.75,flagycoords-0.7,lwd=2.5,col="gray75")
			newtext=countrycode(as.character(t(fixtures3[i,])),origin="country.name",destination="genc3c")
			newtext[is.na(newtext)]=substr(toupper(as.character(t(fixtures3[i,]))[is.na(newtext)]),1,3)
			text(xcoords,ycoords[i],newtext,pos=2)
			flagstoget=sapply(1:2,function(x){which(stats$Team==as.character(t(fixtures3[i,]))[x])})
			
			for(j in 1:2){
				currflag=flagpngs[[flagstoget[j]]]
				Dims=flagDimensions[flagstoget[j], ]
				rasterImage(currflag,  # Plot each flag with these boundaries:
				flagxcoords[j]-Dims[2]/boxParameter, flagycoords-Dims[1]/boxParameter,
				flagxcoords[j]+Dims[2]/boxParameter, flagycoords+Dims[1]/boxParameter)
				rect(flagxcoords[j]-Dims[2]/boxParameter, flagycoords-Dims[1]/boxParameter,
				flagxcoords[j]+Dims[2]/boxParameter, flagycoords+Dims[1]/boxParameter)
			}
		}
	}
	
	quarterfinals=lapply(1:nrow(fixtures3),function(x){
		playmatch(fixtures3[x,1],fixtures3[x,2],penalties=T)
	})
	
	quarterresults=sapply(1:nrow(fixtures3),function (x) {
		ifelse(quarterfinals[[x]][1]>quarterfinals[[x]][2],as.character(fixtures3[x,1]),as.character(fixtures3[x,2]))
	})
	
	plotquarter()
	Sys.sleep(1)
	for(i in seq(1,4,2)){
		ycoords=seq(11,-3,-1.91)+1
		ycoords=sapply(seq(1,8,2),function(x){median(ycoords[c(x,x+1)])-0.4})
		xcoords=c(-1,1.5)+5.75
		plotscore(quarterfinals[[i]],median(xcoords),ycoords[i],quarterresults[i])
		Sys.sleep(0.5)
		plotscore(quarterfinals[[i+1]],median(xcoords),ycoords[i+1],quarterresults[i+1])
		Sys.sleep(1)	
	}
	
	plotquarter2=function(){
		for(i in seq(1,4,2)){
			ycoords=seq(11,-3,-1.91)+1
			ycoords=sapply(seq(1,8,2),function(x){median(ycoords[c(x,x+1)])-0.4})
			xcoords=c(-1,1.5)+5.75
			plotscore(quarterfinals[[i]],median(xcoords),ycoords[i],quarterresults[i])
			plotscore(quarterfinals[[i+1]],median(xcoords),ycoords[i+1],quarterresults[i+1])	
			linex=c(c(xcoords[2]+0.25,xcoords[2]+1),rev(c(xcoords[2]+0.25,xcoords[2]+1)))
			liney=c(rep(ycoords[c(i)]-0.4,2),rep(ycoords[c(i+1)]-0.4,2))
			liney2=median(ycoords[c(i,i+1)])-0.4
			lines(linex,liney,lwd=2.5,col="darkgreen")
			lines(c(linex[2],linex[2]+1),rep(liney2,2),lwd=2.5,col="darkgreen")
		}
	}
	
	Sys.sleep(1)
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	for(i in 1:3){
		text(9.5,4,"Semi-finals",cex=4,col="black")
		line <- Sys.sleep(0.5)
		text(9.5,4,"Semi-finals",cex=4,col="darkred")
		line <- Sys.sleep(0.5)
	}
	
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	plot16stage1()
	plot16stage2()
	plotquarter()
	plotquarter2()
	
	fixtures4=data.frame(team1=quarterresults[seq(1,4,2)],team2=quarterresults[seq(2,4,2)])
	
	plotsemi=function(){
		ycoords=seq(11,-3,-1.91)+1
		ycoords=sapply(seq(1,8,2),function(x){median(ycoords[c(x,x+1)])-0.4})
		ycoords=sapply(seq(1,4,2),function(x){median(ycoords[c(x,x+1)])-0.4})+0.5
		xcoords=c(-1,1.5)+5.75+5.75
		boxParameter <- 80
		text(median(xcoords)-0.5,12.3,"Semi finals",cex=1.1)
		for(i in 1:nrow(fixtures4)){
			flagycoords=ycoords[i]-0.5
			flagxcoords=xcoords+c(-0.51,-0.51)
			rect(flagxcoords[1]-0.75,flagycoords-0.7,flagxcoords[2]+0.75,ycoords[i]+0.4,border=NA,col="gray88")
			segments(flagxcoords[1]-0.75,flagycoords-0.7,flagxcoords[2]+0.75,flagycoords-0.7,lwd=2.5,col="gray75")
			newtext=countrycode(as.character(t(fixtures4[i,])),origin="country.name",destination="genc3c")
			newtext[is.na(newtext)]=substr(toupper(as.character(t(fixtures4[i,]))[is.na(newtext)]),1,3)
			text(xcoords,ycoords[i],newtext,pos=2)
			flagstoget=sapply(1:2,function(x){which(stats$Team==as.character(t(fixtures4[i,]))[x])})
			
			for(j in 1:2){
				currflag=flagpngs[[flagstoget[j]]]
				Dims=flagDimensions[flagstoget[j], ]
				rasterImage(currflag,  # Plot each flag with these boundaries:
				flagxcoords[j]-Dims[2]/boxParameter, flagycoords-Dims[1]/boxParameter,
				flagxcoords[j]+Dims[2]/boxParameter, flagycoords+Dims[1]/boxParameter)
				rect(flagxcoords[j]-Dims[2]/boxParameter, flagycoords-Dims[1]/boxParameter,
				flagxcoords[j]+Dims[2]/boxParameter, flagycoords+Dims[1]/boxParameter)
			}
		}
	}
	plotsemi()
	
	
	plotsemi2=function(k){
		ycoords=seq(11,-3,-1.91)+1
		ycoords=sapply(seq(1,8,2),function(x){median(ycoords[c(x,x+1)])-0.4})
		ycoords=sapply(seq(1,4,2),function(x){median(ycoords[c(x,x+1)])-0.4})+0.5
		xcoords=c(-1,1.5)+5.75+5.75
		plotscore(semifinals[[k]],median(xcoords),ycoords[k],semiresults[k])
	}
	
	Sys.sleep(2)
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	
	semifinals=as.list(rep(NA,2))
	semiresults=rep(NA,2)
	semiresults2=rep(NA,2)
	for (i in 1:nrow(fixtures4)){
		semifinals[[i]]=playmatch(as.character(fixtures4[i,1]),as.character(fixtures4[i,2]),penalties=T,animated=T)
		plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
		plot16stage1()
		plot16stage2()
		plotquarter()
		plotquarter2()
		plotsemi()	
		semiresults[i]=ifelse(semifinals[[i]][1]>semifinals[[i]][2],as.character(fixtures4[i,1]),as.character(fixtures4[i,2]))
		semiresults2[i]=ifelse(semifinals[[i]][1]>semifinals[[i]][2],as.character(fixtures4[i,2]),as.character(fixtures4[i,1]))
		for(k in 1:i){
			plotsemi2(k)
		}
		Sys.sleep(3)
	}
	Sys.sleep(1)
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	for(i in 1:3){
		text(9.5,4,"FINAL",cex=4,col="black")
		line <- Sys.sleep(0.5)
		text(9.5,4,"FINAL",cex=4,col="darkred")
		line <- Sys.sleep(0.5)
	}
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	
	plotsemi3=function(){
		
		ycoords=seq(11,-3,-1.91)+1
		ycoords=sapply(seq(1,8,2),function(x){median(ycoords[c(x,x+1)])-0.4})
		ycoords=sapply(seq(1,4,2),function(x){median(ycoords[c(x,x+1)])-0.4})+0.5
		xcoords=c(-1,1.5)+5.75+5.75
		for(i in 1:2){
			plotscore(semifinals[[i]],median(xcoords),ycoords[i],semiresults[i])
			linex=c(c(xcoords[2]+0.25,xcoords[2]+1),rev(c(xcoords[2]+0.25,xcoords[2]+1)))
			liney=c(rep(ycoords[c(i)]-0.4,2),rep(ycoords[c(i+1)]-0.4,2))
			liney2=median(ycoords[c(i,i+1)])-0.4
			lines(linex,liney,lwd=2.5,col="darkgreen")
			lines(c(linex[2],linex[2]+1),rep(liney2,2),lwd=2.5,col="darkgreen")
			
		}
		
	}
	
	plot16stage1()
	plot16stage2()
	plotquarter()
	plotquarter2()
	plotsemi()
	plotsemi3()
	
	fixtures5=rbind(semiresults,semiresults2)
	
	plotfinals=function(){
		ycoords=seq(11,-3,-1.91)+1
		ycoords=sapply(seq(1,8,2),function(x){median(ycoords[c(x,x+1)])-0.4})
		ycoords=sapply(seq(1,4,2),function(x){median(ycoords[c(x,x+1)])-0.4})+0.5
		ycoords=sapply(seq(1,2,2),function(x){median(ycoords[c(x,x+1)])-0.4})+0.5
		xcoords=c(-1,1.5)+5.75+5.75+5.75
		ycoords=c(ycoords,2)
		boxParameter <- 80
		text(median(xcoords)-0.5,12.3,"Finals",cex=1.1)
		for(i in 1:2){
			flagycoords=ycoords[i]-0.5
			flagxcoords=xcoords+c(-0.51,-0.51)
			rect(flagxcoords[1]-0.75,flagycoords-0.7,flagxcoords[2]+0.75,ycoords[i]+0.4,border=NA,col="gray88")
			segments(flagxcoords[1]-0.75,flagycoords-0.7,flagxcoords[2]+0.75,flagycoords-0.7,lwd=2.5,col="gray75")
			newtext=countrycode(as.character(t(fixtures5[i,])),origin="country.name",destination="genc3c")
			newtext[is.na(newtext)]=substr(toupper(as.character(t(fixtures5[i,]))[is.na(newtext)]),1,3)
			text(xcoords,ycoords[i],newtext,pos=2)
			flagstoget=sapply(1:2,function(x){which(stats$Team==as.character(t(fixtures5[i,]))[x])})
			for(j in 1:2){
				currflag=flagpngs[[flagstoget[j]]]
				Dims=flagDimensions[flagstoget[j], ]
				rasterImage(currflag,  # Plot each flag with these boundaries:
				flagxcoords[j]-Dims[2]/boxParameter, flagycoords-Dims[1]/boxParameter,
				flagxcoords[j]+Dims[2]/boxParameter, flagycoords+Dims[1]/boxParameter)
				rect(flagxcoords[j]-Dims[2]/boxParameter, flagycoords-Dims[1]/boxParameter,
				flagxcoords[j]+Dims[2]/boxParameter, flagycoords+Dims[1]/boxParameter)
			}
		}
		
		
	}
	
	plotfinal2=function(k){
		ycoords=seq(11,-3,-1.91)+1
		ycoords=sapply(seq(1,8,2),function(x){median(ycoords[c(x,x+1)])-0.4})
		ycoords=sapply(seq(1,4,2),function(x){median(ycoords[c(x,x+1)])-0.4})+0.5
		ycoords=sapply(seq(1,2,2),function(x){median(ycoords[c(x,x+1)])-0.4})+0.5
		xcoords=c(-1,1.5)+5.75+5.75+5.75
		ycoords=c(ycoords,2)
		plotscore(finals[[k]],median(xcoords),ycoords[k],finalresult[k])
	}
	
	plotfinals()
	finalresult=rep(NA,2)
	finals=as.list(rep(NA,2))
	Sys.sleep(3)
	finals[[2]]=playmatch(fixtures5[2,1],fixtures5[2,2],penalties=T)
	finalresult[2]=ifelse(finals[[2]][1]>finals[[2]][2],as.character(fixtures5[2,1]),as.character(fixtures5[2,2]))
	plotfinal2(2)
	Sys.sleep(3)
	finals[[1]]=playmatch(fixtures5[1,1],fixtures5[1,2],penalties=T,animate=T)
	finalresult[1]=ifelse(finals[[1]][1]>finals[[1]][2],as.character(fixtures5[1,1]),as.character(fixtures5[1,2]))
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	
	plot16stage1()
	plot16stage2()
	plotquarter()
	plotquarter2()
	plotsemi()
	plotsemi3()
	plotfinals()
	plotfinal2(1)
	plotfinal2(2)
	Sys.sleep(4)
	
	plot(0,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n",ylim=c(0,10),xlim=c(0,20))
	second=ifelse(finals[[1]][1]>finals[[1]][2],as.character(fixtures5[1,2]),as.character(fixtures5[1,1]))
	fourth=ifelse(finals[[2]][1]>finals[[2]][2],as.character(fixtures5[2,2]),as.character(fixtures5[2,1]))
	
	
	text(9.5,10,paste("The world cup is over!"),cex=4,col="black")
	text(9.5,7,paste("Final results"),cex=4,col="red")
	text(6,5,paste("1st",finalresult[1]),cex=3,col="black",pos=4)
	text(6,3,paste("2nd",second),cex=3,col="black",pos=4)
	text(6,1,paste("3rd",finalresult[2]),cex=3,col="black",pos=4)
	text(6,-1,paste("4th",fourth),cex=3,col="black",pos=4)
	allresult=c(finalresult[1],second,finalresult[2],fourth)
	flagxcoords=5.5
	flagycoords=c(5,3,1,-1)
	flagstoget=sapply(1:4,function(x){which(stats$Team==allresult[x])})
	boxParameter <- 50
	
	for(j in 1:4){
		currflag=flagpngs[[flagstoget[j]]]
		Dims=flagDimensions[flagstoget[j], ]
		rasterImage(currflag,  # Plot each flag with these boundaries:
		flagxcoords-Dims[2]/boxParameter, flagycoords[j]-Dims[1]/boxParameter,
		flagxcoords+Dims[2]/boxParameter, flagycoords[j]+Dims[1]/boxParameter)
		rect(flagxcoords-Dims[2]/boxParameter, flagycoords[j]-Dims[1]/boxParameter,
		flagxcoords+Dims[2]/boxParameter, flagycoords[j]+Dims[1]/boxParameter)
		
	}
	
	for(x in 1:10){
		Sys.sleep(0.5)
		text(9.5,10,paste("The world cup is over!"),cex=4,col="red")
		Sys.sleep(0.5)
		text(9.5,10,paste("The world cup is over!"),cex=4,col="gold")
	}
	
}









