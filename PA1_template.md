# Reproducible Research: Peer Assessment 1


```r
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(pander)
panderOptions('table.style', 'rmarkdown')
library(knitr)
library(rmarkdown)
library(magrittr)
```

## Loading and preprocessing the data


```r
unzip("activity.zip")

Activity<-read.csv("activity.csv")

Activity$interval<-formatC(Activity$interval,NULL,4,"d",0)

Activity$date_interval<-as.POSIXct(paste(Activity$date, Activity$interval), format="%Y-%m-%d %H%M")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day.


```r
Activity.daysteps<-Activity%>%filter(!is.na(steps))%>%
group_by(date=lubridate::date(date_interval))%>%
  summarise(daysteps=sum(steps,na.rm=TRUE))
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.


```r
ggplot(Activity.daysteps, aes(x=daysteps))+
  geom_histogram(aes(fill=..count..),binwidth=2500, alpha=.75)+
  scale_fill_gradient("Count", low = "skyblue2", high = "slateblue4")+
  labs(x="# of Steps", y="# of Days", title="Histogram of Days and Steps Taken")+
  theme_solarized_2(light=FALSE)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day.


```r
Activity.daysteps.summary<-Activity.daysteps%>%
  summarise("mean"=mean(daysteps), 
            "median"=median(daysteps)
            )
Activity.daysteps.summary<-as.data.frame(Activity.daysteps.summary)


pander(Activity.daysteps.summary, caption="Summary of daily steps taken.")
```



| mean  | median |
|:-----:|:------:|
| 10766 | 10765  |

Table: Summary of daily steps taken.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 


```r
Activity.Intervalsteps<-Activity%>%group_by(interval)%>%summarise(Intervalsteps=mean(steps, na.rm=TRUE))

ggplot(data=Activity.Intervalsteps, aes(x=as.numeric(interval), y=Intervalsteps))+
  geom_line(size=2, color="steelblue", alpha=.75)+
  labs(x="Interval", y="Steps", title="Time Series of Steps Taken (by Interval)")+
  theme_solarized_2(light=FALSE)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
Activity.Intervalsteps.summary<-Activity.Intervalsteps[Activity.Intervalsteps$Intervalsteps==max(Activity.Intervalsteps$Intervalsteps),]

pander(Activity.Intervalsteps.summary,caption = "Interval for maximum steps.")
```



| interval | Intervalsteps |
|:--------:|:-------------:|
|   0835   |     206.2     |

Table: Interval for maximum steps.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
Activity_NAS<-Activity

nrow(Activity_NAS[is.na(Activity_NAS$steps)==TRUE,])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


*For this, the average of the interval was used and replaced all observations where steps whwere null.* 


```r
Activity_NAS<-merge(Activity_NAS, Activity.Intervalsteps, by=c("interval"="interval"))
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
Activity_NAS[is.na(Activity_NAS$steps)==TRUE,"steps"]<-Activity_NAS[is.na(Activity_NAS$steps)==TRUE,"Intervalsteps"]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

*At the edges of our distribution, the values did not vary greatly, however, they did vary somewhat at and slightly above 10,000.*


```r
Activity_NAS.daysteps<-Activity_NAS%>%group_by(date=lubridate::date(date_interval))%>%
  summarise(daysteps=sum(steps,na.rm=TRUE))

ggplot(Activity_NAS.daysteps, aes(x=daysteps))+
  geom_histogram(aes(fill=..count..),binwidth=2500, alpha=.75)+
  scale_fill_gradient("Count", low = "skyblue2", high = "slateblue4")+
  labs(x="# of Steps", y="# of Days", title="Histogram of Days and Steps Taken")+
  theme_solarized_2(light=FALSE)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
Activity_NAS<-Activity_NAS%>%mutate("wday"=case_when(
                                weekdays(date_interval) == "Saturday" | weekdays(date_interval) == "Sunday" ~ "Weekend",
                                TRUE ~ "Weekday"
                                ))

Activity_NAS.Intervalsteps<-Activity_NAS%>%group_by(interval, wday)%>%summarise(Intervalsteps=mean(steps, na.rm=TRUE))

Activity_NAS.Intervalsteps$wday<-factor(Activity_NAS.Intervalsteps$wday)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
ggplot(Activity_NAS.Intervalsteps, aes(x=as.numeric(interval), y=Intervalsteps, group=wday))+
  geom_line(size=2, color="steelblue", alpha=.75)+
  facet_wrap(~wday, nrow=2)+
  labs(x="Interval", y="Steps", title="Time Series of Steps Taken (by Interval, by weekday/weekend)")+
  theme_solarized_2(light=FALSE)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->




