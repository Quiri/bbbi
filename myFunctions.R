library(RODBC)
library(ggplot2)
library(mFilter) 
library(Cairo) # more beautiful ggplots
library(sqldf) # Use SQL for data frame manipulation
library(rJava)
library(xlsx)
library(matlab) # get some matlab funtions
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(plm)
library(zoo) # Useful for time series and panel data
#library(RMongo)
library(RJSONIO) 
library(wesanderson) # Colors for ggplot
#library(magnittr)  # %>% Operator
library(XML)

#rm(list = ls(all = TRUE)); gc(); gc() # Cleaning workspace


options(bitmap = 'cairo')
options("scipen"=100, "digits"=4) # dont print e+ notation for big numbers (fb_ids)
options(stringsAsFactors = FALSE)
odbcCloseAll()


`%ni%`<- Negate(`%in%`)

fromdatedim <- function (dim) { #datedim to date
  if (is.character(dim)){}
  
    if(dim != 0 && !is.na(dim)){
      dimnum <- as.numeric(as.character(dim))
      as.Date(paste0(dimnum %/% 10000, "-", dimnum %%10000%/%100, "-", dimnum %% 100))
  }
} 

vm <- function(df) { #view more
  View(df)
  answer <- readline("Do you want to see more? Press Enter, else write n ")
  i <- 1;
  while (answer != "n"){
    i <- i + 1000
    View(df[i:(i+999),])
    answer <- readline("More? Press Enter, else write n ")
  }
}


datefrom <- function(today = Sys.Date(), interval = 6, asdate = T){ #diff month, get the first of a month interval month ago
  today <- as.Date(today)
  this.month <- as.numeric(format(today, format = "%m")) 
  this.year <- as.numeric(format(today, format = "%Y"))
  last.month <- (this.month+(12-interval%%12))%%12
  if(last.month == 0){last.month<-12}
  if (this.month-interval<1){
    last.year <- this.year - (abs(this.month-interval)%/%12+1)
  } else {
    last.year <- this.year
  }
  date <- as.Date(paste0(last.year,"-",sprintf("%02d",last.month),"-01"))
  datedim <- format(date, "%Y%m%d")
  if (asdate) {
    return(date)
  } else {
    return(datedim)
  }
}


todatedim <- function(date) { #convert date to datedim
  return(format(date, "%Y%m%d"))
}


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

write.txt <- function(text, file) {
  fileConn<-file(file)
  writeLines(text, fileConn)
  close(fileConn)
}



df2jira <- function(res) {
  res <- data.frame(lapply(res, as.character), stringsAsFactors=FALSE)
  # Heading
  cat("|")
  cat(paste0("|", names(res)))
  cat("|| \n")
  for (i in 1:nrow(res)) {
    cat(paste0("|", res[i,]))
    cat("| \n")
  }
}

allstring <- function(df) {
  data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



























