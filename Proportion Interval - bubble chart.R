library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(stringr)


Cal.pro <- function(dt,OPvar,var,year,startnum,startnum1){
  dt[which((dt[[OPvar]] == var) & (car_p_sum[['year']] == year)),] %>%
    mutate(total = sum(sale)) %>% #Calculate total sale volume as denominator
    filter(RETAIL >= startnum & RETAIL < startnum1) %>%
    mutate(need1 = sum(sale)) %>% #Calculate sale volume in certain interval as numerator
    mutate(REprop = (need1/total)*100) %>% #Calculate the proportion
    mutate(interval = paste(startnum,startnum1,sep='-'))%>% #Paste interval name stored as plot y-value later
    mutate(year = year)%>%
    select(interval,REprop,year)%>%
    sample_n(1)
}

setwd("D:\\codeset\\Interval Bubble Chart\\Sample Graph") #Set path

#SAMPLE DATA
car_p_sum <- data.frame(FBRAND = paste(sample(letters, 1000, replace= TRUE),'BRAND',sep='-'),
                        RETAIL = runif(1000, min=10, max=100),
                        sale = runif(1000,min=1,max=1000),
                        year =  sample(c('2017','2018','2019'), 1000, replace=TRUE))



#Choose output variable here
varsum <- data.frame(table(car_p_sum$FBRAND))[,1]
var <- varsum[1]
######RETAIL#####
for (var in varsum){
  #Change title here
  title <- paste(var,'Retail Proportion',sep='-')

  #Select max/min price to identify the range and interval, here I choose every 5 as a interval
  RETAIL <- car_p_sum[which(car_p_sum[['FBRAND']] == var),][['RETAIL']]
  max <- ceiling(max(RETAIL)/5)
  min <- floor(min(RETAIL)/5)    
  startnum <- min * 5
  endnum <- max*5
  d <- data.frame()

  #Loop to calculating each proportion in certain interval
  while (startnum <= endnum) {
    startnum1 <- startnum + 5
    c <- data.frame()
    a1 <- Cal.pro(car_p_sum,'FBRAND',var,'2017',startnum,startnum1)
    
    if (nrow(a1) != 0){
      c <- rbind(c,a1)
    }
    
    a2 <- Cal.pro(car_p_sum,'FBRAND',var,'2018',startnum,startnum1)
    
    if (nrow(a2) != 0){
      c <- rbind(c,a2)
    }
    
    a3 <- Cal.pro(car_p_sum,'FBRAND',var,'2019',startnum,startnum1)
    
    if (nrow(a1) != 0){
      c <- rbind(c,a3)
    }
    
    d <- bind_rows(d,c)
    print(paste(var,startnum,startnum1,sep='-'))
    startnum <- startnum + 5
  }
  
  
  #Deal with some null situation to fit for banch output
  if (length(d) == 0){
    next
  }

    d[which(d[,2] < 1),2] <- 0 #We don't need proportion smaller than 1 showing on graph
    d[is.na(d)]<-0.0 
    nulllst <- c()
    for (nullnum in (1:nrow(d))){
      if(d[nullnum,2] == 0){
        nulllst <- c(nulllst,nullnum)
      }
    }
    if (is.null(nulllst)){
      d <- d
    }else{
      d <- d[-nulllst,]
    }
  
    #Creat intermediate value of interval as Y-coordinate value
    intervalst <- c()
    for (a in d$interval){
      num1 <- as.numeric(word(a, 1, sep = fixed('-')))
      num2 <- as.numeric(word(a,2,sep = fixed('-')))
      interval <- (num1 + num2) / 2
      intervalst <- c(intervalst,interval)
    }

    graph <- data.frame(interval = intervalst, prop = d$REprop, intername = d$interval, year = d$year)
  
    #Reorder  
    graph$interval <- as.numeric(graph$interval)
    graph$intername <- reorder(graph$intername,graph$interval)
    
    graph$prop[which(graph$prop == 0)] <- NA
    
    p <-  ggplot(graph,aes(x = year, y = intername, color = intername))+
      geom_point(aes(size=prop * 1.2),shape=19,alpha = 0.7)+
      geom_text(aes(label=ifelse(is.na(prop), "", paste(round(prop,2),"%",sep=""))),size=4, color = 'black',na.rm= T)+
      scale_size_identity()
    
    p <- p + labs(y='Price Range') #Specify the Y lab
    
    p <- p + ggtitle(title)
    
    p <- p + theme_economist() + theme(axis.title.x=element_blank(),
                                       legend.position="none",
                                       axis.title.y=element_text(size=14,margin = margin(t = 0, r = 20, b = 0, l = 0)),
                                       plot.title = element_text(size=20,face="bold"))
    filename <- gsub("[[:punct:]]", "", title)
    filename <- paste(filename,".png",sep="")
    print(title)
    png(filename = filename,width = 1000,height = 1300,res=120)
    print(p)
    dev.off()
}


