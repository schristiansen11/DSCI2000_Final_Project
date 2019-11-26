library(readxl)
library(lubridate)

#read in futures contracts
ActiveSoybeanContractsforJuly2020_CSV <- read_excel("../../Raw_Data/CSV files/ActiveSoybeanContractsforJuly2020.CSV.xlsx", 
                                                    skip = 3)
ActiveSoybeanContractsforMarch2020_CSV <- read_excel("../../Raw_Data/CSV files/ActiveSoybeanContractsforMarch2020.CSV.xlsx", 
                                                    skip = 3)
ActiveSoybeanContractsforMay2020_CSV <- read_excel("../../Raw_Data/CSV files/ActiveSoybeanContractsforMay2020.CSV.xlsx", 
                                                     skip = 3)



# Clean Data

#assign names to data frames
july_df <- as.data.frame(ActiveSoybeanContractsforJuly2020_CSV)
march_df <- as.data.frame(ActiveSoybeanContractsforMarch2020_CSV)
may_df <- as.data.frame(ActiveSoybeanContractsforMay2020_CSV)

#rename variables to lower-case convention
colnames(july_df)[colnames(july_df)=="Date"] <- "date_ymd"
colnames(july_df)[colnames(july_df)=="Open"] <- "open"
colnames(july_df)[colnames(july_df)=="High"] <- "high"
colnames(july_df)[colnames(july_df)=="Low"] <- "low"
colnames(july_df)[colnames(july_df)=="Close"] <- "close"
july_df$symbol = "ZSN2020"

colnames(march_df)[colnames(march_df)=="Date"] <- "date_ymd"
colnames(march_df)[colnames(march_df)=="Open"] <- "open"
colnames(march_df)[colnames(march_df)=="High"] <- "high"
colnames(march_df)[colnames(march_df)=="Low"] <- "low"
colnames(march_df)[colnames(march_df)=="Close"] <- "close"
march_df$symbol = "ZSH2020"

colnames(may_df)[colnames(may_df)=="Date"] <- "date_ymd"
colnames(may_df)[colnames(may_df)=="Open"] <- "open"
colnames(may_df)[colnames(may_df)=="High"] <- "high"
colnames(may_df)[colnames(may_df)=="Low"] <- "low"
colnames(may_df)[colnames(may_df)=="Close"] <- "close"
may_df$symbol = "ZSK2020"

#create final futures data frame
futures_df = as.data.frame(rbind(july_df,march_df))
futures_df <- as.data.frame(rbind(futures_df,may_df))

futures_df$date_ymd <- ymd(futures_df$date_ymd)


