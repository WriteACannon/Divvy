library(tidyverse)
library(lubricate)

install.packages("lubricate")

#2019 Data

q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
q1_2019 <- select(q1_2019,c(start_time,usertype))
names(q1_2019)[1] <- "started_at"
names(q1_2019)[2] <- "member_casual"


q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q2_2019 <- select(q2_2019,c("X01...Rental.Details.Local.Start.Time","User.Type"))
names(q2_2019)[1] <- "started_at"
names(q2_2019)[2] <- "member_casual"

q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q3_2019 <- select(q3_2019,c(start_time,usertype))
names(q3_2019)[1] <- "started_at"
names(q3_2019)[2] <- "member_casual"

q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q4_2019 <- select(q4_2019,c(start_time,usertype))
names(q4_2019)[1] <- "started_at"
names(q4_2019)[2] <- "member_casual"


divvy2019 <- full_join(q1_2019,q2_2019)
divvy2019 <- full_join(divvy2019,q3_2019)
divvy2019 <- full_join(divvy2019,q4_2019)

divvy2019$rideable_type <- "classic_bike"

divvy2019$member_casual[divvy2019$member_casual == 'Subscriber'] <- 'member'
divvy2019$member_casual[divvy2019$member_casual == 'Customer'] <- 'casual'

divvy2019$date <-  as.Date(divvy2019$started_at, format="%Y-%m-%d")
divvy2019$year <- format(as.Date(divvy2019$date, format="%Y-%m-%d"),"%Y")
divvy2019$month <- format(as.Date(divvy2019$date, format="%Y-%m-%d"),"%m")
divvy2019$monthname <- format(as.Date(divvy2019$date, format="%Y-%m-%d"),"%b")

divvy2019$rides <- 1

divvy2019summary <- divvy2019 %>% group_by(rideable_type, member_casual,year,month,monthname) %>% summarise(rides=sum(rides))

write.csv(divvy2019summary,"divvy2019summary.csv")

#2020 Data

q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")
apr20 <- read.csv("202004-divvy-tripdata.csv")
may20 <- read.csv("202005-divvy-tripdata.csv")
june20 <- read.csv("202006-divvy-tripdata.csv")
july20 <- read.csv("202007-divvy-tripdata.csv")
aug20 <- read.csv("202008-divvy-tripdata.csv")
sept20 <- read.csv("202009-divvy-tripdata.csv")
oct20 <- read.csv("202010-divvy-tripdata.csv")
nov20 <- read.csv("202011-divvy-tripdata.csv")
dec20 <- read.csv("202012-divvy-tripdata.csv")

divvy2020 <- full_join(q1_2020,apr20)
divvy2020 <- full_join(divvy2020,may20)
divvy2020 <- full_join(divvy2020,june20)
divvy2020 <- full_join(divvy2020,july20)
divvy2020 <- full_join(divvy2020,aug20)
divvy2020 <- full_join(divvy2020,sept20)
divvy2020 <- full_join(divvy2020,oct20)
divvy2020 <- full_join(divvy2020,nov20)

divvy2020 <- select(divvy2020,c(started_at,rideable_type,member_casual))
dec20 <- select(dec20,c(started_at,rideable_type,member_casual))

divvy2020 <- full_join(divvy2020,dec20)

divvy2020$date <-  as.Date(divvy2020$started_at, format="%Y-%m-%d")
divvy2020$year <- format(as.Date(divvy2020$date, format="%Y-%m-%d"),"%Y")
divvy2020$month <- format(as.Date(divvy2020$date, format="%Y-%m-%d"),"%m")
divvy2020$monthname <- format(as.Date(divvy2020$date, format="%Y-%m-%d"),"%b")

divvy2020$rides <- 1

divvy2020summary <- divvy2020 %>% group_by(rideable_type, member_casual,year,month,monthname) %>% summarise(rides=sum(rides))

write.csv(divvy2020summary,"divvy2020summary.csv")

#2021 Data

jan21 <- read.csv("202101-divvy-tripdata.csv")
feb21 <- read.csv("202102-divvy-tripdata.csv")
mar21 <- read.csv("202103-divvy-tripdata.csv")
apr21 <- read.csv("202104-divvy-tripdata.csv")
may21 <- read.csv("202105-divvy-tripdata.csv")
june21 <- read.csv("202106-divvy-tripdata.csv")
july21 <- read.csv("202107-divvy-tripdata.csv")
aug21 <- read.csv("202108-divvy-tripdata.csv")
sept21 <- read.csv("202109-divvy-tripdata.csv")
oct21 <- read.csv("202110-divvy-tripdata.csv")
nov21 <- read.csv("202111-divvy-tripdata.csv")
dec21 <- read.csv("202112-divvy-tripdata.csv")

divvy2021 <- full_join(jan21,feb21)
divvy2021 <- full_join(divvy2021,mar21)
divvy2021 <- full_join(divvy2021,apr21)
divvy2021 <- full_join(divvy2021,may21)
divvy2021 <- full_join(divvy2021,june21)
divvy2021 <- full_join(divvy2021,july21)
divvy2021 <- full_join(divvy2021,aug21)
divvy2021 <- full_join(divvy2021,sept21)
divvy2021 <- full_join(divvy2021,oct21)
divvy2021 <- full_join(divvy2021,nov21)
divvy2021 <- full_join(divvy2021,dec21)

divvy2021 <- select(divvy2021,c(started_at,rideable_type,member_casual))

divvy2021$date <-  as.Date(divvy2021$started_at, format="%Y-%m-%d")
divvy2021$year <- format(as.Date(divvy2021$date, format="%Y-%m-%d"),"%Y")
divvy2021$month <- format(as.Date(divvy2021$date, format="%Y-%m-%d"),"%m")
divvy2021$monthname <- format(as.Date(divvy2021$date, format="%Y-%m-%d"),"%b")

divvy2021$rides <- 1

divvy2021summary <- divvy2021 %>% group_by(rideable_type, member_casual,year,month,monthname) %>% summarise(rides=sum(rides))

write.csv(divvy2021summary,"divvy2021summary.csv")

#2022 Data

jan22 <- read.csv("202201-divvy-tripdata.csv")
feb22 <- read.csv("202202-divvy-tripdata.csv")
mar22 <- read.csv("202203-divvy-tripdata.csv")
apr22 <- read.csv("202204-divvy-tripdata.csv")
may22 <- read.csv("202205-divvy-tripdata.csv")
june22 <- read.csv("202206-divvy-tripdata.csv")
july22 <- read.csv("202207-divvy-tripdata.csv")
aug22 <- read.csv("202208-divvy-tripdata.csv")
sept22 <- read.csv("202209-divvy-publictripdata.csv")
oct22 <- read.csv("202210-divvy-tripdata.csv")
nov22 <- read.csv("202211-divvy-tripdata.csv")
dec22 <- read.csv("202212-divvy-tripdata.csv")

divvy2022 <- full_join(jan22,feb22)
divvy2022 <- full_join(divvy2022,mar22)
divvy2022 <- full_join(divvy2022,apr22)
divvy2022 <- full_join(divvy2022,may22)
divvy2022 <- full_join(divvy2022,june22)
divvy2022 <- full_join(divvy2022,july22)
divvy2022 <- full_join(divvy2022,aug22)
divvy2022 <- full_join(divvy2022,sept22)
divvy2022 <- full_join(divvy2022,oct22)
divvy2022 <- full_join(divvy2022,nov22)
divvy2022 <- full_join(divvy2022,dec22)

divvy2022 <- select(divvy2022,c(started_at,rideable_type,member_casual))

divvy2022$date <-  as.Date(divvy2022$started_at, format="%Y-%m-%d")
divvy2022$year <- format(as.Date(divvy2022$date, format="%Y-%m-%d"),"%Y")
divvy2022$month <- format(as.Date(divvy2022$date, format="%Y-%m-%d"),"%m")
divvy2022$monthname <- format(as.Date(divvy2022$date, format="%Y-%m-%d"),"%b")

divvy2022$rides <- 1

divvy2022summary <- divvy2022 %>% group_by(rideable_type, member_casual,year,month,monthname) %>% summarise(rides=sum(rides))

write.csv(divvy2022summary,divvy2022summary.csv")

#2023 Data 

jan23 <- read.csv("202301-divvy-tripdata.csv")
feb23 <- read.csv("202302-divvy-tripdata.csv")
mar23 <- read.csv("202303-divvy-tripdata.csv")
apr23 <- read.csv("202304-divvy-tripdata.csv")
may23 <- read.csv("202305-divvy-tripdata.csv")
june23 <- read.csv("202306-divvy-tripdata.csv")
july23 <- read.csv("202307-divvy-tripdata.csv")
aug23 <- read.csv("202308-divvy-tripdata.csv")
sept23 <- read.csv("202309-divvy-tripdata.csv")
oct23 <- read.csv("202310-divvy-tripdata.csv")
nov23 <- read.csv("202311-divvy-tripdata.csv")
dec23 <- read.csv("202312-divvy-tripdata.csv")

divvy2023 <- full_join(jan23,feb23)
divvy2023 <- full_join(divvy2023,mar23)
divvy2023 <- full_join(divvy2023,apr23)
divvy2023 <- full_join(divvy2023,may23)
divvy2023 <- full_join(divvy2023,june23)
divvy2023 <- full_join(divvy2023,july23)
divvy2023 <- full_join(divvy2023,aug23)
divvy2023 <- full_join(divvy2023,sept23)
divvy2023 <- full_join(divvy2023,oct23)
divvy2023 <- full_join(divvy2023,nov23)
divvy2023 <- full_join(divvy2023,dec23)

divvy2023 <- select(divvy2023,c(started_at,rideable_type,member_casual))

divvy2023$date <-  as.Date(divvy2023$started_at, format="%Y-%m-%d")
divvy2023$year <- format(as.Date(divvy2023$date, format="%Y-%m-%d"),"%Y")
divvy2023$month <- format(as.Date(divvy2023$date, format="%Y-%m-%d"),"%m")
divvy2023$monthname <- format(as.Date(divvy2023$date, format="%Y-%m-%d"),"%b")

divvy2023$rides <- 1

divvy2023summary <- divvy2023 %>% group_by(rideable_type, member_casual,year,month,monthname) %>% summarise(rides=sum(rides))

write.csv(divvy2023summary,"divvy2023summary.csv")

#2024 Data

jan24 <- read.csv("202401-divvy-tripdata.csv")
feb24 <- read.csv("202402-divvy-tripdata.csv")
mar24 <- read.csv("202403-divvy-tripdata.csv")
apr24 <- read.csv("202404-divvy-tripdata.csv")
may24 <- read.csv("202405-divvy-tripdata.csv")
june24 <- read.csv("202406-divvy-tripdata.csv")
july24 <- read.csv("202407-divvy-tripdata.csv")
aug24 <- read.csv("202408-divvy-tripdata.csv")
sept24 <- read.csv("202409-divvy-tripdata.csv")
#oct24 <- read.csv("202410-divvy-tripdata.csv")
#nov24 <- read.csv("202411-divvy-tripdata.csv")
#dec24 <- read.csv("202412-divvy-tripdata.csv")

divvy2024 <- full_join(jan24,feb24)
divvy2024 <- full_join(divvy2024,mar24)
divvy2024 <- full_join(divvy2024,apr24)
divvy2024 <- full_join(divvy2024,may24)
divvy2024 <- full_join(divvy2024,june24)
divvy2024 <- full_join(divvy2024,july24)
divvy2024 <- full_join(divvy2024,aug24)
divvy2024 <- full_join(divvy2024,sept24)
#divvy2024 <- full_join(divvy2024,oct24)
#divvy2024 <- full_join(divvy2024,nov24)
#divvy2024 <- full_join(divvy2024,dec24)

divvy2024 <- select(divvy2024,c(started_at,rideable_type,member_casual))

divvy2024$date <-  as.Date(divvy2024$started_at, format="%Y-%m-%d")
divvy2024$year <- format(as.Date(divvy2024$date, format="%Y-%m-%d"),"%Y")
divvy2024$month <- format(as.Date(divvy2024$date, format="%Y-%m-%d"),"%m")
divvy2024$monthname <- format(as.Date(divvy2024$date, format="%Y-%m-%d"),"%b")

divvy2024$rides <- 1

divvy2024summary <- divvy2024 %>% group_by(rideable_type, member_casual,year,month,monthname) %>% summarise(rides=sum(rides))

write.csv(divvy2024summary,"divvy2024summary.csv")


#Open Summary Files
divvy2019summary <- read.csv("divvy2019summary.csv")
divvy2020summary <- read.csv("divvy2020summary.csv")
divvy2021summary <- read.csv("divvy2021summary.csv")
divvy2022summary <- read.csv("divvy2022summary.csv")
divvy2023summary <- read.csv("divvy2023summary.csv")
divvy2024summary <- read.csv("divvy2024summary.csv")

#Full Summary 2019 - 2024
divvy2019_2024summary <- full_join(divvy2019summary,divvy2020summary)
divvy2019_2024summary <- full_join(divvy2019_2024summary,divvy2021summary)
divvy2019_2024summary <- full_join(divvy2019_2024summary,divvy2022summary)
divvy2019_2024summary <- full_join(divvy2019_2024summary,divvy2023summary)
divvy2019_2024summary <- full_join(divvy2019_2024summary,divvy2024summary)

#Print
write.csv(divvy2019_2024summary,"C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/DivvyData/summarydata/divvy2019_2024summary.csv")
