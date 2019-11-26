library(dplyr)
library(lubridate)
library(readxl)

oil_crops_chart_gallery_fig_1 <- 
  read_xlsx("../../Raw_Data/xlsx files/oil crops outlook_August19_relevanttables.xlsx",
            sheet = "Oil Crops Chart Gallery Fig 1",skip = 1)

oil_crops_chart_gallery_fig_1 <- as.data.frame(oil_crops_chart_gallery_fig_1[-1,])

colnames(oil_crops_chart_gallery_fig_1) <- c("date_ymd","planted_2019","planted_five_year_avg",
                                             "blooming_2019","blooming_five_year_avg",
                                             "setting_pods_2019","setting_pods_five_year_avg")

oil_crops_chart_gallery_fig_1$date_ymd <- ymd(oil_crops_chart_gallery_fig_1$date_ymd)
