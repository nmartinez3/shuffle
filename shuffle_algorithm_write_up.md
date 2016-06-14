# shuffle algorithm walkthrough/explanation
nathan martinez  
June 13, 2016  

## Shuffle Algorithm in R for Shuffling Rows in a Data Frame

####Introduction
I was bored one day and getting annoyed with Spotify's shuffle algorithm (for doing things like playing the same song twice in a row in a playlist of over 20 songs, etc.), so I decided I would make a shuffle algorithm in R that shuffled rows in a data frame in the way I thought Spotify should be shuffling songs in a playlist. This was strictly for fun; this program has no real utility besides demonstrating a shuffle algorithm that is more advanced than Spotify's and, in my opinion, better.

How the algorithm works:

* Select a row from the data frame at random, where rows with a higher play count are less likely to be chosen and rows with a lower play count are more likely to be chosen. This row is being 'played'.
* Move that row from the main data frame to a cooldown data frame where it will stay for a certain number of 'plays' before getting moved back into the main data frame. This is so that a song cannot be played twice within X number of plays.
* Add 1 to that row's play count so that it is slightly less likely to be selected when it is returned to the main data frame.
* Repeat the above steps with the added condition that the next row to be selected cannot be the one immediately after the previously selected row. This is to ensure that the shuffle algorithm will never select two rows in order, thus keeping things as shuffled as possible.

####Shuffle Function
This is the function I made to shuffle rows in a data :

```r
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
                  x<-sample(1:nrow(shufList$data),1,prob=(1/pnorm(count,mean(count),sd(count)*sd.mult)))
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
```

There's a lot going on here, so let's break it down chunk by chunk. First, we have a number of `if` conditions that will stop the program if certain error-inducing conditions are present:

```r
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
```

The `if((nrow(data)-cooldown)<2)` statement will end the shuffle program if the number of rows in the data frame minus the number of rows to be in the cooldown data set is less than 2. This is because the pool of songs available to be selected by the function will be 1 or less, and with 1 song available at a time, the function will no longer be shuffling and will just be selecting songs in an order (with less than 1 song available, e.g. cooldown>=nrow(data), then the function will just have an error). The `if(nrow(data)>100)` statement will give the user a warning that the data frame they are shuffling is too long to be seen on the screen and ask them how they would like to proceed. This is problematic because the purpose of this function is just a simple proof of concept that is best observed visually, and with a data frame that has greater than 100 rows, it will not be possible to see everything on one screen (thus being harder to see what the function is doing). The following lines of code after the `if(nrow(data)>100)` statement read the user's input and either end the function, convert the data frame to a local data frame (to have fewer rows displayed), or proceed anyways. If the user opts to have their data frame converted to a local data frame, the shuffle function will install `dplyr` if necessary and then use the function ` tbl_df` to convert the data frame to a local data frame.

One cool thing to note in this section is the following line of code:

```r
if(line=="n"|line=="N") stop(simpleError(sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "))))
```

This line controls what will happen if the user has a data frame greater than 100 rows and decides not to continue with the function's execution. All this line does is stop the function with no messages, whether they be error messages or otherwise. I couldn't find a way to quietly exit a function without printing some kind of message, but after doing some quick google searches, I found this nifty line of code that's really just a fancy way of ending the function without printing any messages.

The next chunk of the function does some necessary prepwork to get everything ready for the actual shuffling:

```r
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
```

First, a new column called `.shuffle.` is added to the data frame to be shuffled. Then it's moved to be the first column in the data frame. This column will keep track of how many times a row has been 'played', and it has been moved to be the first column for better visibility. After that, a new data frame called `out` is created by taking the first few rows of the main data frame. This will be the cooldown data frame where rows will remain for X number of 'plays' before being able to be selected by the shuffle function again. The reason I made the `out` data frame by selecting rows from the main data frame is so that the cooldown data frame will have the same dimensions as the main data frame (including the `.shuffle.` column) and the same classes in each of its columns. The `out` data frame is then cleared by turning each of the columns into either 0's, the word 'empty', or the bottom level factor of that column's factor levels. This is done so that the cooldown data frame starts out empty and is then filled up with songs.

The `playing` data frame is then created, which is just a 1-row data frame with the row that is currently selected by the shuffle function. Finally, the `data`, `out`, and `playing` data frames are all moved into a list called `shufList` to make printing each data frame a one line command rather than a three line command. The `line<-readline("Press [enter] to begin")` is just a trick to have the program pause its execution until the user presses enter. This pause happens after each iteration of the shuffle algorithm.

Now we're finally ready to start shuffling! Here's the next chunk of the shuffle function that actually starts the shuffling process:

```r
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
```

This for loop does the first loops of the shuffle function, from the first loop up until the loop that will fill up the `out` data frame (the `out` data frame is the list of rows that are on cooldown and cannot be played again for a certain number of loops (specified by the user under the argument `cooldown`)). For each loop, the row to be selected is the variable `x`, which is determined by the command `x<-sample(1:nrow(shufList$data),1)`. This just selects one of the rows from the main data frame at random. The position of the selected row is then compared to the position of the previously selected row (marked as `x.old`), and if the two values are the same, `x` is changed to be either `x+1` or `x-1` depending on whether or not `x` is equal to the number of rows in the `data` data frame. This is done to prevent the shuffle function from selecting rows in order, which would violate the rules of the algorithm.

After selecting the row to be shuffled, the rows in the `out` data frame are all moved down by one to make room for the incoming row. Then the selected row is moved to the first row of `out`. The following if statement, `if(rownames==TRUE) rownames(shufList$out)[1]<-rownames(shufList$data[x,])`, will also copy over the name of the selected row from the `data` data frame into the `out` data frame. The `rownames` variable is a TRUE/FALSE argument to the shuffle function, since data frames with non-unique row names will make this step of the function fail and should be shuffled with `rownames=FALSE` so as to avoid this step. Then, the selected row's `.shuffle.` count is increased by one, and the row is copied over to the `playing` data frame.

Finally, the selected row is removed from the `data` data frame, `shufList` is printed to show the user what row just got shuffled, and the variable `x.old` is updated. If the current loop is equal to the function's argument `loops`, which is specified by the user, then the function stops execution and ends there. Otherwise, it asks the user to press enter to continue and then moves on to the next loop, repeating this section of code for loops `1:cooldown`.

Once it has done its last loop and the number of loops specified is greater than the `cooldown` variable, the function moves on to the following chunk of code where it continues shuffling rows but starts adding rows back into the `data` data frame from the `out` data frame (since they've finished their cooldown):

```r
if(loops>cooldown){
      line<-readline("Press [enter] to continue, [esc] to exit")
      for(i in (cooldown+1):loops){
            count<-shufList$data$.shuffle.
            x<-sample(1:nrow(shufList$data),1,prob=(1/pnorm(count,mean(count),sd(count)*sd.mult)))
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
```
