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

anno_cnt <- c('DE', 'DK', 'HU', 'EMEA_ENG', 'FR', 'NCSA_ENG', 'PL', 'RU')
tso_cnt <- c('DE','BR', 'CN', 'CZ', 'EMEA_ES', 'FR', 'GR', 'IT', 'MENA', 'NCSA_ES', 'NL', 'PL', 'RO', 'RU', 'UK', 'US')



hadoop <- function () { #connect to hadoop
  try(detach(package:sqldf), silent = T)
  library(RJDBC);
  hive_jars  <- list.files("M:/jar", pattern="jar$", full.names=T);
  # define class name of RJDBC.
  hive_driver <- JDBC("org.apache.hive.jdbc.HiveDriver", c(hive_jars));
  # create an connection:
  hive_conn <- dbConnect(hive_driver, "jdbc:hive2://pdc-gimpli-dwh2-dn02.ubisoft.onbe:10001/default");
  # execute a sql query.
  return(hive_conn)
}

getbb <- function () { # Get all bb users for AO
  hadoop <- hadoop()
  query <- "Select * from sandbox_gimpli.bb_accounts"
  data <- dbGetQuery(hadoop, query);
  data$user_id <- as.integer(data$user_id)
  data$games_id <- as.factor(data$games_id)
  return(data)
}

getaoshop <- 
  function () { # Get all bb users for AO
    hadoop <- hadoop()
    query <- "Select * from sandbox_gimpli.dim_shop_items"
    data <- dbGetQuery(hadoop, query);
    return(data)
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

# ao_ru, ao_de, ao_fr, emea_en
newsletter <- function(cnt, server, active = 0, optin = T){ 
  
  if (active == 0) {
  query <- sprintf("SELECT user_id, external_user_id as uplay, userName, email, preferredLanguage
            FROM %s_gimpli_warehouse_user.dim_users
            right Join (SELECT user_id, itemType as news FROM %s_gimpli_warehouse.fct_newsletter_status a 
                        WHERE time = (Select max(time) 
                                      FROM %s_gimpli_warehouse.fct_newsletter_status b 
                                      WHERE a.user_id = b.user_id)) t2 
                  USING(user_id)
          where news = 'subscribed'", cnt, cnt, cnt)
  } else {
  query <- sprintf("SELECT user_id, external_user_id as uplay, userName, email, preferredLanguage, max_time
            FROM %s_gimpli_warehouse_user.dim_users
            right Join (SELECT user_id, itemType as news FROM %s_gimpli_warehouse.fct_newsletter_status a 
                        WHERE time = (Select max(time) 
                                      FROM %s_gimpli_warehouse.fct_newsletter_status b 
                                      WHERE a.user_id = b.user_id)) t2 
                  USING(user_id)
            join %s_gimpli_warehouse.agg_login_times using(user_id)
          where news = 'subscribed'
          and max_date < %s", cnt, cnt, cnt, cnt, active)
  }
  
  conn <- odbcConnect(server)
  data <- sqlQuery(conn, query)
  odbcCloseAll()
  return(data)
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


pgo_cheat <- c('2554', '188', '123', '9', '165', '13', '16', '27', '3800', '3825', '2568', '24', '14', '160', '161', '100305', '2058', '3805', '3816', '3822', '19279', '6356', '15846', '26400', '26403', '19101', '19064', '19061', '19184', '19065', '19205', '19199', '19070', '19147', '19193', '19055', '19079', '19185')


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

socialnetwork <- function(id) {
  # Uplay has letters
  # Facebook has 9, 10 or 15 digits
  # Google has 21 digits
  # 2014-05-12
  
  ifelse(grepl("[[:alpha:]]", id), "gm_ubilogin_User",
    ifelse(nchar(id)<19, "gm_user_Facebook",
      "gm_user_Google"
    )
  )
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
















































##############################################################################
#                        Calendar Heatmap                                    #
#                                by                                          #
#                         Paul Bleicher                                      #
# an R version of a graphic from:                                            #
# http://stat-computing.org/dataexpo/2009/posters/wicklin-allison.pdf        #
#  requires lattice, chron, grid packages                                    #
############################################################################## 

## calendarHeat: An R function to display time-series data as a calendar heatmap 
## Copyright 2009 Humedica. All rights reserved.

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You can find a copy of the GNU General Public License, Version 2 at:
## http://www.gnu.org/licenses/gpl-2.0.html

calendarHeat <- function(dates, 
                         values, 
                         ncolors=99, 
                         color="r2g", 
                         varname="Values",
                         date.form = "%Y-%m-%d", ...) {
  require(lattice)
  require(grid)
  require(chron)
  if (class(dates) == "character" | class(dates) == "factor" ) {
    dates <- strptime(dates, date.form)
  }
  caldat <- data.frame(value = values, dates = dates)
  min.date <- as.Date(paste(format(min(dates), "%Y"),
                            "-1-1",sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"),
                            "-12-31", sep = ""))
  dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
  
  # Merge moves data by one day, avoid
  caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
  dates <- as.Date(dates) 
  caldat$value[match(dates, caldat$date.seq)] <- values
  
  caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
  caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
  caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
  caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
  yrs <- as.character(unique(caldat$yr))
  d.loc <- as.numeric()                        
  for (m in min(yrs):max(yrs)) {
    d.subset <- which(caldat$yr == m)  
    sub.seq <- seq(1,length(d.subset))
    d.loc <- c(d.loc, sub.seq)
  }  
  caldat <- cbind(caldat, seq=d.loc)
  
  #color styles
  r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
  r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
  w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   #white to blue
  
  assign("col.sty", get(color))
  calendar.pal <- colorRampPalette((col.sty), space = "Lab")
  def.theme <- lattice.getOption("default.theme")
  cal.theme <-
    function() {  
      theme <-
        list(
          strip.background = list(col = "transparent"),
          strip.border = list(col = "transparent"),
          axis.line = list(col="transparent"),
          par.strip.text=list(cex=0.8))
    }
  lattice.options(default.theme = cal.theme)
  yrs <- (unique(caldat$yr))
  nyr <- length(yrs)
  print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
                              as.table=TRUE,
                              aspect=.12,
                              layout = c(1, nyr%%7),
                              between = list(x=0, y=c(1,1)),
                              strip=TRUE,
                              main = paste("Calendar Heat Map of ", varname, sep = ""),
                              scales = list(
                                x = list(
                                  at= c(seq(2.9, 52, by=4.42)),
                                  labels = month.abb,
                                  alternating = c(1, rep(0, (nyr-1))),
                                  tck=0,
                                  cex = 0.7),
                                y=list(
                                  at = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                             "Friday", "Saturday"),
                                  alternating = 1,
                                  cex = 0.6,
                                  tck=0)),
                              xlim =c(0.4, 54.6),
                              ylim=c(6.6,-0.6),
                              cuts= ncolors - 1,
                              col.regions = (calendar.pal(ncolors)),
                              xlab="" ,
                              ylab="",
                              colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
                              subscripts=TRUE
  ) )
  panel.locs <- trellis.currentLayout()
  for (row in 1:nrow(panel.locs)) {
    for (column in 1:ncol(panel.locs))  {
      if (panel.locs[row, column] > 0)
      {
        trellis.focus("panel", row = row, column = column,
                      highlight = FALSE)
        xyetc <- trellis.panelArgs()
        subs <- caldat[xyetc$subscripts,]
        dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
        y.start <- dates.fsubs$dotw[1]
        y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
        dates.len <- nrow(dates.fsubs)
        adj.start <- dates.fsubs$woty[1]
        
        for (k in 0:6) {
          if (k < y.start) {
            x.start <- adj.start + 0.5
          } else {
            x.start <- adj.start - 0.5
          }
          if (k > y.end) {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
          } else {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
          }
          grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
                     default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        if (adj.start <  2) {
          grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
                     default.units = "native", gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(x.finis, x.finis), 
                     y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
          if (dates.fsubs$dotw[dates.len] != 6) {
            grid.lines(x = c(x.finis + 1, x.finis + 1), 
                       y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                       gp=gpar(col = "grey", lwd = 1))
          }
          grid.lines(x = c(x.finis, x.finis), 
                     y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
        }
        for (n in 1:51) {
          grid.lines(x = c(n + 1.5, n + 1.5), 
                     y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        x.start <- adj.start - 0.5
        
        if (y.start > 0) {
          grid.lines(x = c(x.start, x.start + 1),
                     y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start + 1, x.start + 1),
                     y = c(y.start - 0.5 , -0.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start, x.start),
                     y = c(y.start - 0.5, 6.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          if (y.end < 6  ) {
            grid.lines(x = c(x.start + 1, x.finis + 1),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          }
        } else {
          grid.lines(x = c(x.start, x.start),
                     y = c( - 0.5, 6.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
        }
        
        if (y.start == 0 ) {
          if (y.end < 6  ) {
            grid.lines(x = c(x.start, x.finis + 1),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          }
        }
        for (j in 1:12)  {
          last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
          x.last.m <- dates.fsubs$woty[last.month] + 0.5
          y.last.m <- dates.fsubs$dotw[last.month] + 0.5
          grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
                     default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          if ((y.last.m) < 6) {
            grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          }
        }
      }
    }
    trellis.unfocus()
  } 
  lattice.options(default.theme = def.theme)
}

## Example of use: Plot financial data
## This code is not run.
if(FALSE) {
  
  #create faux data; skip this to use data from a file or stock data
  #ndays <- 1500   #set number of days
  #dates <- as.POSIXlt(seq(Sys.Date()- ndays, Sys.Date() - 1, by="days"))
  #vals <- runif(ndays, -100, 100)
  
  #stock data:
  stock <- "MSFT"
  start.date <- "2006-01-12"
  end.date <- Sys.Date()
  quote <- paste("http://ichart.finance.yahoo.com/table.csv?s=",
                 stock,
                 "&a=", substr(start.date,6,7),
                 "&b=", substr(start.date, 9, 10),
                 "&c=", substr(start.date, 1,4), 
                 "&d=", substr(end.date,6,7),
                 "&e=", substr(end.date, 9, 10),
                 "&f=", substr(end.date, 1,4),
                 "&g=d&ignore=.csv", sep="")             
  stock.data <- read.csv(quote, as.is=TRUE)
  
  # Plot as calendar heatmap
  calendarHeat(stock.data$Date, stock.data$Adj.Close, varname="MSFT Adjusted Close")
}





