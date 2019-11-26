#Is there seasonality in the target variable for a given contract date? 
#Is there an effect of the delayed 2019 seeding on soybean prices 
#(see time lag in datasets on Oil Crops Outlook tab: oil crops chart gallery figure 1)?

# source("Code/Tidy Code/active_soybeans_contracts_july_march_may.R")
# source("Code/Tidy Code/tidy_oil_crops_outlook_figure_1.R")

#the graph from figure 1 of the Oil Crops Outlook tab: oil crops chart gallery
#does not download; I think the x-axis is a numeric version of date (4/21/2019 is 43756.0)
#and the y-axis is the Planted/Blooming variables (in %)



library(lubridate)
library(ggplot2)
library(scales)
library(formattable)
library(reshape2)
library(dplyr)


#Analysis code starts here
#seasonality in close variable
close.seasonality.plot <- ggplot(data = futures_df,aes(x=futures_df$date_ymd,y=futures_df$close,color=futures_df$symbol)) +
  
  geom_point() + 
  #source for date_labels: http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/
  scale_x_date(date_labels = "%b, %Y",breaks = date_breaks("3 months")) +
  scale_color_discrete(name="Contract Symbol",labels = c("ZSH2020 - March","ZSK2020 - May","ZSN2020 - July"))+
  #scale_color_discrete: https://stackoverflow.com/questions/35712062/scale-fill-discrete-does-not-change-label-names
  #https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/#rename-legend-labels-and-change-the-order-of-items
  theme(
    plot.title = element_text(color = "black",size=16,face="bold.italic",
                              hjust=.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color="black",size=11, angle = 90, vjust = .5, face = "bold"),
    axis.text.y = element_text(color="black",size=11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = c(0.1285,0.175),
    legend.text = element_text(face="bold"),
    legend.title = element_text(face="bold"),
    legend.box.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.box.background = element_rect(),
    axis.line = element_line(size=.5,linetype="solid",color="black"),
    strip.text = element_text(size=12,face="bold"),
    strip.text.y = element_text(8,face="bold")
  ) +  
  ggtitle("Close Values by Date") +
  xlab("Date") +
  ylab("Close (cents)")
  
close.seasonality.date.plot <- 
  ggplot(data = futures_df,aes(x=futures_df$date_ymd,y=futures_df$close,color=futures_df$symbol)) +
  geom_point() + 
  scale_x_date(date_labels = "%b, %Y",breaks = date_breaks("3 months")) +
  scale_color_discrete(name="Contract Symbol",labels = c("ZSH2020 - March","ZSK2020 - May","ZSN2020 - July"))+
  theme(
    plot.title = element_text(color = "black",size=16,face="bold.italic",
                              hjust=.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color="black",size=11, angle = 90, vjust = .5, face = "bold"),
    axis.text.y = element_text(color="black",size=11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = c(0.1285,0.175),
    legend.text = element_text(face="bold"),
    legend.title = element_text(face="bold"),
    legend.box.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.box.background = element_rect(),
    axis.line = element_line(size=.5,linetype="solid",color="black"),
    strip.text = element_text(size=12,face="bold"),
    strip.text.y = element_text(8,face="bold")
  ) +  
  ggtitle("Close by Date") +
  xlab("Date") +
  ylab("Close (cents)") + 
  geom_vline(xintercept=as.Date("2017-07-01")) +
  geom_vline(xintercept=as.Date("2017-09-30")) +
  geom_vline(xintercept=as.Date("2018-07-01")) +
  geom_vline(xintercept=as.Date("2018-09-30")) +
  geom_vline(xintercept=as.Date("2019-07-01")) +
  geom_vline(xintercept=as.Date("2019-09-30")) +
  annotate("text",x=as.Date("2017-11-20"),y = 890,label="July - September, 2017",angle=90,fontface="bold") +
  annotate("text",x=as.Date("2018-11-20"),y = 890,label="July - September, 2018",angle=90,fontface="bold") +
  annotate("text",x=as.Date("2019-11-20"),y = 890,label="July - September, 2019",angle=90,fontface="bold")

close_data_table_zsn2020 = as.data.frame(cbind(month = as.character(c("01","02","03",
                                                                      "04","05","06",
                                                                      "07","08","09",
                                                                      "10","11","12"))))

close_data_table_zsh2020 = as.data.frame(cbind(month = as.character(c("01","02","03",
                                                                      "04","05","06",
                                                                      "07","08","09",
                                                                      "10","11","12"))))

close_data_table_zsk2020 = as.data.frame(cbind(month = as.character(c("01","02","03",
                                                                      "04","05","06",
                                                                      "07","08","09",
                                                                      "10","11","12"))))

as.month <- function(x){
  if(x < 10){
    as.character(paste0("0",x))
  } else {
    as.character(x)
  }
}

for(i in 1:12){
  close_data_table_zsn2020[i,2] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2016"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSN2020"])
  close_data_table_zsn2020[i,3] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2017"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSN2020"])
  close_data_table_zsn2020[i,4] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2018"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSN2020"])
  close_data_table_zsn2020[i,5] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2019"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSN2020"])
}

colnames(close_data_table_zsn2020) <- c("Month","2016","2017","2018","2019")

for(i in 1:12){
  close_data_table_zsh2020[i,2] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2016"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSH2020"])
  close_data_table_zsh2020[i,3] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2017"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSH2020"])
  close_data_table_zsh2020[i,4] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2018"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSH2020"])
  close_data_table_zsh2020[i,5] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2019"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSH2020"])
}

colnames(close_data_table_zsh2020) <- c("Month","2016","2017","2018","2019")

for(i in 1:12){
  close_data_table_zsk2020[i,2] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2016"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSK2020"])
  close_data_table_zsk2020[i,3] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2017"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSK2020"])
  close_data_table_zsk2020[i,4] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2018"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSK2020"])
  close_data_table_zsk2020[i,5] = median(
    futures_df$close[substr(futures_df[,1],1,4)=="2019"&substr(futures_df[,1],6,7)==as.month(i)&futures_df$symbol=="ZSK2020"])
}


colnames(close_data_table_zsk2020) <- c("Month","2016","2017","2018","2019")

july_median_close_table <-  formattable(close_data_table_zsn2020,align=c("l",rep("c",4)))
march_median_close_table <- formattable(close_data_table_zsh2020,align=c("l",rep("c",4)))
may_median_close_table <- formattable(close_data_table_zsk2020,align=c("l",rep("c",4)))

july_long <- as.data.frame(july_median_close_table)
july_long <- melt(july_long,id.var = "Month")
colnames(july_long) <- c("month","year","median_close")

march_long <- as.data.frame(march_median_close_table)
march_long <- melt(march_long,id.var = "Month")
colnames(march_long) <- c("month","year","median_close")

may_long <- as.data.frame(may_median_close_table)
may_long <- melt(may_long,id.var = "Month")
colnames(may_long) <- c("month","year","median_close")

july_long <- mutate(july_long,
                    month_character = case_when(
                      month == "01" ~ "Jan",
                      month == "02" ~ "Feb",
                      month == "03" ~ "Mar",
                      month == "04" ~ "Apr",
                      month == "05" ~ "May",
                      month == "06" ~ "Jun",
                      month == "07" ~ "Jul",
                      month == "08" ~ "Aug",
                      month == "09" ~ "Sep",
                      month == "10" ~ "Oct",
                      month == "11" ~ "Nov",
                      month == "12" ~ "Dec"))

march_long <- mutate(march_long,
                     month_character = case_when(
                       month == "01" ~ "Jan",
                       month == "02" ~ "Feb",
                       month == "03" ~ "Mar",
                       month == "04" ~ "Apr",
                       month == "05" ~ "May",
                       month == "06" ~ "Jun",
                       month == "07" ~ "Jul",
                       month == "08" ~ "Aug",
                       month == "09" ~ "Sep",
                       month == "10" ~ "Oct",
                       month == "11" ~ "Nov",
                       month == "12" ~ "Dec"))

may_long <- mutate(may_long,
                   month_character = case_when(
                     month == "01" ~ "Jan",
                     month == "02" ~ "Feb",
                     month == "03" ~ "Mar",
                     month == "04" ~ "Apr",
                     month == "05" ~ "May",
                     month == "06" ~ "Jun",
                     month == "07" ~ "Jul",
                     month == "08" ~ "Aug",
                     month == "09" ~ "Sep",
                     month == "10" ~ "Oct",
                     month == "11" ~ "Nov",
                     month == "12" ~ "Dec"))

#graph median close by year and by contract
july.month.plot <- ggplot(july_long,aes(x=month_character,y=median_close,group=year)) +
  
  geom_line(size=1.3,color="blue") +
  
  geom_point() +
  
  geom_text(aes(label=format(round(median_close,0),nsmall=0),
                vjust=rep(c(-0.65,1.5),24)),show.legend = F,color="black") +
  
  scale_y_continuous(breaks=seq(850,1050,100), limits = c(850,1050)) +
  
  scale_x_discrete(limits = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  
  facet_grid(year~.) +
  
  ggtitle("Median Close by Month (ZSN2020 - July)") +
  xlab("Month") +
  ylab("Median Close (Cents)") + 
  
  theme(
    plot.title = element_text(color = "black",size=16,face="bold.italic",
                              hjust=.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color="black",size=11, angle = 90, vjust = .5, face = "bold"),
    axis.text.y = element_text(color="black",size=11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size=.5,linetype="solid",color="black"),
    legend.position = "none",
    strip.text = element_text(size=12,face="bold"),
    strip.text.y = element_text(8,face="bold"),
    panel.border = element_rect(color="black",fill=NA)
  )

march.month.plot <- ggplot(march_long[march_long$year!="2016",],aes(x=month_character,y=median_close,group=year)) +
  
  geom_line(size=1.3,color="blue") +
  
  geom_point() +
  
  geom_text(aes(label=format(round(median_close,0),nsmall=0),
                vjust=rep(c(-1.5,2),18)),show.legend = F,color="black") +
  
  scale_y_continuous(breaks=seq(850,1050,100), limits = c(850,1050)) +
  
  scale_x_discrete(limits = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  
  facet_grid(year~.) +
  
  ggtitle("Median Close by Month (ZSH2020 - March)") +
  xlab("Month") +
  ylab("Median Close (Cents)") + 
  
  theme(
    plot.title = element_text(color = "black",size=16,face="bold.italic",
                              hjust=.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color="black",size=11, angle = 90, vjust = .5, face = "bold"),
    axis.text.y = element_text(color="black",size=11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size=.5,linetype="solid",color="black"),
    legend.position = "none",
    strip.text = element_text(size=12,face="bold"),
    strip.text.y = element_text(8,face="bold"),
    panel.border = element_rect(color="black",fill=NA)
  )

may.month.plot <- ggplot(may_long[may_long$year!="2016",],aes(x=month_character,y=median_close,group=year)) +
  
  geom_line(size=1.3,color="blue") +
  
  geom_point() +
  
  geom_text(aes(label=format(round(median_close,0),nsmall=0),
                vjust=rep(c(-1.5,2),18)),show.legend = F,color="black") +
  
  scale_y_continuous(breaks=seq(850,1050,100), limits = c(850,1050)) +
  
  scale_x_discrete(limits = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  
  facet_grid(year~.) +
  
  ggtitle("Median Close by Month (ZSK2020 - May)") +
  xlab("Month") +
  ylab("Median Close (Cents)") + 
  
  theme(
    plot.title = element_text(color = "black",size=16,face="bold.italic",
                              hjust=.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color="black",size=11, angle = 90, vjust = .5, face = "bold"),
    axis.text.y = element_text(color="black",size=11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size=.5,linetype="solid",color="black"),
    legend.position = "none",
    strip.text = element_text(size=12,face="bold"),
    strip.text.y = element_text(8,face="bold"),
    panel.border = element_rect(color="black",fill=NA)
  )


#plot difference between 2019 data and 5-year avg against difference in close values
#clean and merge data
oil_crops_chart_gallery_fig_1$next_monday = oil_crops_chart_gallery_fig_1$date_ymd + 1
oil_crops_chart_gallery_fig_1$next_monday[6] <- oil_crops_chart_gallery_fig_1$date_ymd[6] + 2

difference_df <- merge(oil_crops_chart_gallery_fig_1,futures_df,all.x=T,
                       by.x="next_monday",
                       by.y="date_ymd")

difference_df$prev_year_next_monday = difference_df$date_ymd - 363

difference_df <- merge(difference_df,futures_df,all.x=T,
                       by.x = c("prev_year_next_monday","symbol"),
                       by.y = c("date_ymd","symbol"))
                       
difference_df$plant_behind = difference_df$planted_five_year_avg - difference_df$planted_2019
difference_df$blooming_behind = difference_df$blooming_five_year_avg - difference_df$blooming_2019
difference_df$setting_pods_behind = difference_df$setting_pods_five_year_avg - difference_df$setting_pods_2019
difference_df$close_difference = difference_df$close.x - difference_df$close.y

#plot data
plant.behind.plot <- ggplot(difference_df[!is.na(difference_df$symbol),],aes(x=plant_behind,y=close_difference,color=symbol)) +
  
  geom_point() +
  
  scale_y_continuous(breaks=seq(-150,50,50), limits = c(-150,50)) +
  
  scale_color_discrete(name="Contract Symbol",labels = c("ZSH2020 - March","ZSK2020 - May","ZSN2020 - July")) +

  ggtitle("Difference in Close Value by\nDifference in Soybeans Planted") +
  xlab("Difference in Soybeans Planted\n(Five-Year Average - 2019 Value)") +
  ylab("Difference in Close Value - Cents\n(2019 Value - 2018 Value)") + 
  
  theme(
    plot.title = element_text(color = "black",size=16,face="bold.italic",
                              hjust=.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color="black",size=11, angle = 90, vjust = .5, face = "bold"),
    axis.text.y = element_text(color="black",size=11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size=.5,linetype="solid",color="black"),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = c(0.868,0.827),
    legend.text = element_text(face="bold"),
    legend.title = element_text(face="bold"),
    legend.box.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    #source for leneged.box.margin: https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
    legend.box.background = element_rect(),
    strip.text = element_text(size=12,face="bold"),
    strip.text.y = element_text(8,face="bold"),
    panel.border = element_rect(color="black",fill=NA)
  )

bloom.behind.plot <- ggplot(difference_df[!is.na(difference_df$symbol),],aes(x=blooming_behind,y=close_difference,color=symbol)) +
  
  geom_point() +
  
  scale_y_continuous(breaks=seq(-50,50,10), limits = c(-50,50)) +
  
  scale_x_continuous(breaks=seq(10,30,5),limits=c(10,30)) +
  
  scale_color_discrete(name="Contract Symbol",labels = c("ZSH2020 - March","ZSK2020 - May","ZSN2020 - July")) +
  
  ggtitle("Difference in Close Value by\n Difference in Soybeans Blooming") +
  xlab("Difference in Soybeans Blooming\n(Five-Year Average - 2019 Value)") +
  ylab("Difference in Close Value - Cents\n(2019 Value - 2018 Value)") + 
  
  theme(
    plot.title = element_text(color = "black",size=16,face="bold.italic",
                              hjust=.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color="black",size=11, angle = 90, vjust = .5, face = "bold"),
    axis.text.y = element_text(color="black",size=11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size=.5,linetype="solid",color="black"),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = c(0.87,0.171),
    legend.text = element_text(face="bold"),
    legend.title = element_text(face="bold"),
    legend.box.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"), 
    legend.box.background = element_rect(),
    strip.text = element_text(size=12,face="bold"),
    strip.text.y = element_text(8,face="bold"),
    panel.border = element_rect(color="black",fill=NA)
  )

pods.behind.plot <- ggplot(difference_df[!is.na(difference_df$symbol),],aes(x=setting_pods_behind,y=close_difference,color=symbol)) +
  
  geom_point() +
  
  scale_y_continuous(breaks=seq(-50,20,10), limits = c(-50,20)) +
  
  scale_x_continuous(breaks=seq(20,28,2),limits=c(20,28)) +
  
  scale_color_discrete(name="Contract Symbol",labels = c("ZSH2020 - March","ZSK2020 - May","ZSN2020 - July")) +
  
  ggtitle("Difference in Close Value by\nDifference in Soybean Pods Setting") +
  xlab("Difference in Soybean Pods Setting\n(Five-Year Average - 2019 Value)") +
  ylab("Difference in Close Value - Cents\n(2019 Value - 2018 Value)") + 
  
  theme(
    plot.title = element_text(color = "black",size=16,face="bold.italic",
                              hjust=.5),
    axis.title.x = element_text(color="black",size=14,face="bold"),
    axis.title.y = element_text(color="black",size=14,face="bold"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(color="black",size=11, angle = 90, vjust = .5, face = "bold"),
    axis.text.y = element_text(color="black",size=11, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size=.5,linetype="solid",color="black"),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = c(0.87,0.827),
    legend.text = element_text(face="bold"),
    legend.title = element_text(face="bold"),
    legend.box.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"), 
    legend.box.background = element_rect(color="black"),
    strip.text = element_text(size=12,face="bold"),
    strip.text.y = element_text(8,face="bold"),
    panel.border = element_rect(color="black",fill=NA)
  )

