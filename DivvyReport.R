library(tidyverse)
library(extrafont)



divvy2019_2024summary <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/DivvyData/summarydata/divvy2019_2024summary.csv")


divsumshort <- divvy2019_2024summary %>% group_by(month,monthname,year) %>% summarise(rides=sum(rides))
divsumshort$cs <- ave(divsumshort$rides, divsumshort$year, FUN=cumsum)

# This plot shows monthly ridership across years

plot1 <- ggplot(data=divsumshort, aes(y=rides,x=reorder(monthname,month),color=factor(year),group=year))+
  geom_line(linewidth=1)+
  geom_point(size=3)+
  scale_color_manual(values=c("#e5e5e5","#cfcfcf","#B8B8B8","#838380","#404040","#41b6e6"))+
  labs(title="Monthly Divvy Ridership",color='Year:',caption="E-scooter data introduced Sept 2024",x="Month",y="Total Rides",subtitle="2019 - 2024")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::comma)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),
        panel.border = element_blank(), plot.caption = element_text( color="#404040"), legend.key.width = unit(0, 'cm'),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

plot1

#This plot shows cumulative ridership across years

plot2 <- ggplot(data=divsumshort, aes(y=cs,x=reorder(monthname,month),color=factor(year),group=year))+
  geom_line(linewidth=1)+
  geom_point(size=3)+
  scale_color_manual(values=c("#e5e5e5","#cfcfcf","#B8B8B8","#838380","#404040","#41b6e6"))+
  labs(title="Cumulative Divvy Ridership",color='Year:',x="Month",caption="E-scooter data introduced Sept 2024",y="Cumulative Rides",subtitle="2019 - 2024")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),label=scales::comma)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),
        panel.border = element_blank(), plot.caption = element_text( color="#404040"),legend.key.width = unit(0, 'cm'),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

plot2


#This plot compares past months ridership data, broken down by vehicle type

plot3 <- ggplot(data=filter(divvy2019_2024summary %>% group_by(rideable_type),month=="9"), aes(y=rides,x=year,fill=forcats::fct_rev(rideable_type)))+
  geom_col(position="stack")+
  stat_summary(aes(label=scales::comma(after_stat(y))),fun.y='sum',geom='text',position=position_stack(vjust=0.5),size=3.5)+
  geom_label(aes(label = scales::comma(after_stat(y)), group= year), stat = 'summary',size=3.5, fun = sum,fill="white", vjust =-0.1)+
  scale_fill_manual(values=c("#e890ee","#41b6e6","grey","lightgrey","#41b6e6"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::comma,expand=expansion(mult=c(0,0.1)))+
  scale_x_continuous(breaks = scales::pretty_breaks())+
  labs(title="September Divvy Ridership",fill='Vehicle:',x="Year",subtitle="2019 - 2024",y="Total Rides")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_blank(), axis.text.x=element_text(color="black"),
        panel.grid = element_blank(), panel.border = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

plot3

ggsave(plot=plot1, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/DivvyData/plots/divvyannual.jpeg",height=6,width=8)
ggsave(plot=plot2, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/DivvyData/plots/divvyannualcumulative.jpeg",height=6,width=8)
ggsave(plot=plot3, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/DivvyData/plots/divvymonthly.jpeg",height=6,width=8)



divvy2019_2024summary$date <- as.Date(paste(divvy2019_2024summary$year,divvy2019_2024summary$month,sep="-"), "$Y-%M")


annualsum <- ggplot(data=filter(divvy2019_2024summary,year>20), aes(y=rides,x=year,fill=forcats::fct_rev(rideable_type)))+
  geom_col()+
  #stat_summary(aes(label=scales::comma(after_stat(y))),fun.y='sum',geom='text',position=position_stack(vjust=0.5))+
  scale_fill_manual(values=c("#e890ee","#41b6e6","grey","lightgrey","#41b6e6"))+
  geom_label(aes(label = scales::comma(after_stat(y)),group= year), stat = 'summary',size=3.5,fill="white", fun = sum, vjust =-0.5)+
  labs(title="Annual Divvy Ridership",subtitle="2019 - 2024",fill='Vehicle Type:',x="Year",y="Rides",caption="*2024 data through September")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),label=scales::comma,,expand=expansion(mult=c(0,0.1)))+
  scale_x_continuous(breaks = scales::pretty_breaks())+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.x=element_text(color="black"), axis.text.y=element_text(color="black"),
        panel.border = element_blank(),  plot.caption = element_text( color="#404040"),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

annualsum

ggsave(plot=annualsum, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/DivvyData/plots/divvyannualsum.jpeg",height=6,width=8)
