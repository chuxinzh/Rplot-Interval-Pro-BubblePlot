R Plot created by Chuxin
# Building Bubble Chart of Sale Volume Proportion in different Retail Price interval
By counting the proportion of sale volume in different Retail Price interval, we could learn the overall trend and market distribution of the commodity. 

Sample Plot:

![image](https://github.com/czhang02-su/Image-set/blob/master/aBRANDRetail%20Proportion.png)

## What can this plot do?

* 将离散的售价以区间进行分割归类 - Spilt and group discrete price to fixed interval scale
* 计算这些区间内的销量占总销量的比例 - Counting the proporation of sale volume in the interval to its total sale
* 可根据年份变化进行分组 Able to group by year to show the change by time
* 可根据特定变量（如品牌）进行批量输出- Able to batch output grouping by certain valuable, such as by `Brand`

## Sample Data

```R
  car_p_sum <- data.frame(FBRAND = paste(sample(letters, 1000, replace= TRUE),'BRAND',sep='-'),
                        RETAIL = runif(1000, min=10, max=100),
                        sale = runif(1000,min=1,max=1000),
                        year =  sample(c('2017','2018','2019'), 1000, replace=TRUE))

```
![image](https://github.com/czhang02-su/Image-set/blob/master/sample%20data.jpg)
## Description
### Preparing Data for Ploting

Creating function to calculate the data
```R
  Cal.pro <- function(dt,OPvar,var,year,startnum,startnum1){
    dt[which((dt[[OPvar]] == var) & (car_p_sum[['year']] == year)),] %>%
      mutate(total = sum(TRIMSALES)) %>% #Calculate total sale volume as denominator
      filter(RETAIL >= startnum & RETAIL < startnum1) %>%
      mutate(need1 = sum(TRIMSALES)) %>% #Calculate sale volume in certain interval as numerator
      mutate(REprop = (need1/total)*100) %>% #Calculate the proportion
      mutate(interval = paste(startnum,startnum1,sep='-'))%>% #Paste interval name stored as plot y-value later
      mutate(year = year)%>%
      select(interval,REprop,year)%>%
      sample_n(1)
  }
```

Output data in while loop
```R
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
    a3 <- Cal.pro(car_p_sum,'FBRAND',var,'2019.8',startnum,startnum1)
    if (nrow(a1) != 0){
      c <- rbind(c,a3)
    }
    
    d <- bind_rows(d,c)
    print(paste(var,startnum,startnum1,sep='-'))
    startnum <- startnum + 5
  }
```
![image](https://github.com/czhang02-su/Image-set/blob/master/data%20after%20loop.jpg)

Delete null or too small value
```R
d[which(d[,2] < 1),2] <- 0 #We don't need proportion smaller than 1 showing on graph
d[is.na(d)]<-0.0 
nulllst <- c()
#Delete the row that all of columns are null or smaller than 1
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
```

Prepare Final graph data
```R
    #Creat intermediate value of interval as Y-coordinate value
    intervalst <- c()
    for (a in d$interval){
      num1 <- as.numeric(word(a, 1, sep = fixed('-')))
      num2 <- as.numeric(word(a,2,sep = fixed('-')))
      interval <- (num1 + num2) / 2
      intervalst <- c(intervalst,interval)
    }
    #Graph Data
    graph <- data.frame(interval = intervalst, prop = d$REprop, intername = d$interval, year = d$year)
    #Reorder  
    graph$interval <- as.numeric(graph$interval)
    graph$intername <- reorder(graph$intername,graph$interval)
    graph$prop[which(graph$prop == 0)] <- NA
```
![image](https://github.com/czhang02-su/Image-set/blob/master/final%20graph%20data.jpg)

### Draw the plot
```R
    p <-  ggplot(graph,aes(x = year, y = intername, color = intername))+
      geom_point(aes(size=prop * 1.2),shape=19,alpha = 0.7)+
      geom_text(aes(label=ifelse(is.na(prop), "", paste(round(prop,2),"%",sep=""))),size=4, color = 'black',na.rm= T)+
      scale_size_identity()
    p <- p + labs(y='Retail Price') #Specify the Y lab
    p <- p + ggtitle(title)
    #Theme Decoration
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
```
![image](https://github.com/czhang02-su/Image-set/blob/master/aBRANDRetail%20Proportion.png)