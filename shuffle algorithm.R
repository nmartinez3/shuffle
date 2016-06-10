#LETS GET READY TO SHUUFFLLEEEE
shuffle<-function(data,cooldown=5,loops=10,rownames=TRUE,sd.mult=3.5){
      if((nrow(data)-cooldown)<2) stop("nrow(data) must be greater than cooldown by at least 2",call.=FALSE)
      if(nrow(data)>100){
            cat("WARNING: number of rows in data frame exceeds 100.
Function output will likely be longer than your screen.
The purpose of this function is to demonstrate a shuffle
algorithm visually rather than actually do something
useful, and nrow(data)>100 makes it hard to visualize.
(A simple workaround would be to convert your data frame
into a local data frame and run the function again.)")
            message("To convert to local data frame, type 'loc'")
            message("Continue anyways? (y/n)")
            line<-readline()
            if(line=="n"|line=="N") stop(simpleError(sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "))))
            if(line=="loc"|line=="LOC"){
                  if(!("dplyr" %in% installed.packages()[,"Package"])){
                        message("Attempting to install package 'dplyr'...")
                        install.packages("dplyr")
                  }
                  require(dplyr)
                  data<-tbl_df(data)
            }
            if(line!="n"&line!="N"&line!="y"&line!="Y"&line!="loc"&line!="LOC")
                  stop("Only acceptable inputs are 'y','n', and 'loc'.",call.=FALSE)
      }
      data$.shuffle.<-0
      data<-data[,c(ncol(data),1:(ncol(data)-1))]
      out<-data[1:cooldown,]
      for(i in 2:ncol(out)){
            if(is.factor(out[[i]])) out[1:cooldown,i]<-levels(out[[i]])[1]
            if(is.character(out[[i]])) out[1:cooldown,i]<-"empty"
            if(is.numeric(out[[i]])) out[1:cooldown,i]<-0
            if(is.integer(out[[i]])) out[1:cooldown,i]<-0
      }
      rownames(out)<-1:cooldown
      playing<-out[1,]
      shufList<-list(playing=playing,out=out,data=data)
      x.old<-0
      line<-readline("Press [enter] to begin")
      for(i in 1:cooldown){
            x<-sample(1:nrow(shufList$data),1)
            if(x==x.old){
                  if(x<nrow(shufList$data)){
                        x<-(x+1)
                  }else{
                        x<-(x-1)
                  }
            }
            shufList$out<-shufList$out[c(cooldown,1:(cooldown-1)),]
            shufList$out[1,]<-shufList$data[x,]
            if(rownames==TRUE) rownames(shufList$out)[1]<-rownames(shufList$data[x,])
            shufList$out$.shuffle.[1]<-shufList$out$.shuffle.[1]+1
            shufList$playing<-shufList$out[1,]
            shufList$data<-shufList$data[-x,]
            print(shufList)
            x.old<-x
            if(i==loops) break
            if(i<cooldown) line<-readline("Press [enter] to continue, [esc] to exit")
      }
      if(loops>cooldown){
            line<-readline("Press [enter] to continue, [esc] to exit")
            for(i in (cooldown+1):loops){
                  count<-shufList$data$.shuffle.
                  if(i==cooldown+1){
                        x<-sample(1:nrow(shufList$data),1)
                  }else{
                        x<-sample(1:nrow(shufList$data),1,prob=(1/pnorm(count,mean(count),sd(count)*sd.mult)))
                  }
                  if(x==x.old){
                        if(x<nrow(shufList$data)){
                              x<-(x+1)
                        }else{
                              x<-(x-1)
                        }
                  }
                  shufList$data[nrow(shufList$data)+1,]<-shufList$out[cooldown,]
                  if(rownames==TRUE) rownames(shufList$data)[nrow(shufList$data)]<-rownames(shufList$out[cooldown,])
                  shufList$out<-shufList$out[c(cooldown,1:(cooldown-1)),]
                  shufList$out[1,]<-shufList$data[x,]
                  if(rownames==TRUE) rownames(shufList$out)[1]<-rownames(shufList$data[x,])
                  shufList$out$.shuffle.[1]<-shufList$out$.shuffle.[1]+1
                  shufList$playing<-shufList$out[1,]
                  shufList$data<-shufList$data[-x,]
                  print(shufList)
                  x.old<-x
                  if(i<loops) line<-readline("Press [enter] to continue, [esc] to exit")
            }     
      }
}