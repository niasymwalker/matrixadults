library(ggplot2)
library(tidyverse)
library(fs)
library(readxl)
library(dplyr)
library(expss)
library(vctrs)
library(tidyr)
library(stringr)
library(janitor)
library(ggpubr)
library(cowplot)


#####Figure 1#####
####Temperature and Light Data####
d4<-read_csv("controller04 SP and PV Time Series-data-2023-04-27 14_54_26.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash)%>%
  mutate(time=ymd_hms(Time))%>%select(-Time)%>%
  mutate(temp=as.numeric(temp))%>%
  mutate(HS=temp-27.5)%>%
  mutate(HS=case_when(HS<1~0,TRUE~as.numeric(HS)))%>%
  mutate(date=date(time),hour=hour(time),minute=minute(time))%>%
  mutate(cumulative=cumsum(HS)/60/24/7)%>%
  filter(hour==19)%>%
  group_by(date)%>%
  slice_min(order_by=minute,n=1)%>%
  select(date,cumulative)%>%
  rename(c4=cumulative)

d13<-read_csv("controller13 SP and PV Time Series-data-2023-04-27 14_54_53.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash)%>%
  mutate(time=ymd_hms(Time))%>%select(-Time)%>%
  mutate(temp=as.numeric(temp))%>%
  mutate(HS=temp-27.5)%>%
  mutate(HS=case_when(HS<0~0,TRUE~as.numeric(HS)))%>%
  mutate(date=date(time),hour=hour(time),minute=minute(time))%>%
  mutate(cumulative=cumsum(HS)/60/24/7)%>%
  filter(hour==19)%>%
  group_by(date)%>%
  slice_min(order_by=minute,n=1)%>%
  select(date,cumulative)%>%
  rename(c13=cumulative)

d14<-read_csv("controller14 SP and PV Time Series-data-2023-11-01 10_47_23.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash)%>%
  mutate(time=ymd_hms(Time))%>%select(-Time)%>%
  mutate(temp=as.numeric(temp))%>%
  mutate(HS=temp-27.5)%>%
  mutate(HS=case_when(HS<0~0,TRUE~as.numeric(HS)))%>%
  mutate(date=date(time),hour=hour(time),minute=minute(time))%>%
  mutate(cumulative=cumsum(HS)/60/24/7)%>%
  filter(hour==19)%>%
  group_by(date)%>%
  slice_min(order_by=minute,n=1)%>%
  select(date,cumulative)%>%
  rename(c14=cumulative)

output<-full_join(d4,d13,by="date")%>%full_join(.,d14,by="date")%>%
  gather(controller,temp,-date)%>%group_by(date)%>%mutate(cumulative=mean(temp))%>%
  select(-controller,-temp)%>%distinct()%>%
  filter(date!="2023-01-21",date!="2023-01-22")%>%
  rownames_to_column(var="dummy")%>%
  mutate(dummy=as.numeric(dummy)-1)%>%
  mutate(hs_timepoint=paste0("hs",dummy))%>%
  select(date,cumulative,hs_timepoint)

#write.table(output,"./output.txt",sep="\t",quote=FALSE,row.names=FALSE)

A1_c4<-read_csv("controller04.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

A2_c5<-read_csv("controller05.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Ambient")) %>%
  na.omit()

A3_c14<-read_csv("controller14.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

A4_c15<-read_csv("controller15.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

resistrecover_data <- read_excel("resist_recover_data.xlsx", na="NA") %>% clean_names() %>%
  distinct() %>%
  mutate(percentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(percentdecline_pam = na_if(percentdecline_pam, percentdecline_pam[percentdecline_pam <30])) %>%
  mutate(mortpercentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(mortpercentdecline_pam = na_if(mortpercentdecline_pam, mortpercentdecline_pam[percentdecline_pam >29.99])) %>%
  mutate(genet=as.character(genet)) %>%
  distinct()

M1A_group <- data.frame(M1A_group=c(A1_c4$group,A2_c5$group,A3_c14$group,A4_c15$group))
M1A_time <- data.frame(M1A_time=c(A1_c4$time,A2_c5$time,A3_c14$time,A4_c15$time))
M1A_temp <- data.frame(M1A_temp=c(A1_c4$temp,A2_c5$temp,A3_c14$temp,A4_c15$temp))
M1A_dataset <- data.frame(M1A_group,M1A_time,M1A_temp)
M1A_dataset$M1A_group <- factor(M1A_dataset$M1A_group, levels = c("Heated", "Ambient"))

M1A <- ggplot() +
  geom_line(data=A1_c4, aes(x=time,y=temp,color=group), alpha=0.6) +
  geom_line(data=A2_c5, aes(x=time,y=temp,color=group)) +
  geom_line(data=A3_c14, aes(x=time,y=temp), color=c("firebrick1"), alpha=0.6) +
  geom_line(data=A4_c15, aes(x=time,y=temp), color=c("firebrick2"), alpha=0.6) +
  scale_color_manual(values=c("Heated" = "firebrick", "Ambient" = "dodgerblue"), name="") +
  scale_y_continuous(breaks = c(23,24,26,28,30,32),
                     labels = c(23,24,26,28,30,32)) +
  scale_x_datetime(date_labels = "%b-%d", date_breaks = "2 days") +
  annotate("text", x=as.POSIXct("2023-1-23 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-1-27 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-1-28 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-2-1 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-2-2 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-2-3 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-2-5 00:00:00"),y=c(32.5), label=c(~Delta),hjust=1.3,size=4, parse=T) +
  annotate("text", x=as.POSIXct("2023-1-27 00:00:00"),y=c(22), label=c(~Delta*"= Juvenile iPAM collection dates"),parse=T,size=3) +
  labs(x="Heat Tolerance Profile Dates", y=expression("Temperature "(degree*C)~" ")) +
  theme_bw() + theme(axis.title = element_text(size=10), legend.title = element_blank(),
                     legend.background = element_blank(), legend.position = c(0.5,0.9),
                     legend.justification = c("left", "top"),
                     legend.box.just = "left",
                     legend.margin = margin(6, 6, 6, 6))

resistrecover_data2 <- resistrecover_data %>%
  mutate(genet=as.factor(genet)) %>%
  mutate(genet = fct_reorder(genet,days_to_bleach,.fun ="mean",.na_rm = TRUE)) %>%
  filter(genet!="666") %>%
  filter(genet!="669") %>%
  filter(genet!="671") %>%
  filter(genet!="775") %>%
  select(genet,days_to_bleach) %>%
  add_column(group="1+ Ramets Alive")

resistrecover_data3 <- resistrecover_data %>%
  mutate(genet=as.factor(genet)) %>%
  filter(genet=="666" | genet=="669" | genet=="671" | genet=="775") %>%
  select(genet,days_to_mortality) %>%
  add_column(group="All Ramets Dead")

days <- data.frame(days=c(resistrecover_data2$days_to_bleach,resistrecover_data3$days_to_mortality))
genet <- data.frame(genet=c(resistrecover_data2$genet,resistrecover_data3$genet))
groups <- data.frame(groups=c(resistrecover_data2$group,resistrecover_data3$group))
M1B_dataset <- data.frame(genet,days,groups)
M1B_dataset <- M1B_dataset %>%
  na.omit() %>%
  mutate(genet=as.factor(genet)) %>%
  mutate(days=as.numeric(days)) %>%
  mutate(genet = fct_reorder(genet,days,.fun ="mean",.na_rm = TRUE)) %>%
  left_join(.,profiles_clades,by="genet")
M1B_dataset$clade <- factor(M1B_dataset$clade, levels = c("C","D","mixed"))

M1B <- ggplot(M1B_dataset, aes(x=genet,y=days,fill=clade,shape=groups)) +
  geom_line() +
  scale_fill_manual(labels=c(expression(italic("Cladocopium"),italic("Durusdinium"),"Mixed",)), values = c("darkorange","blue","black")) +
  guides(fill=guide_legend(override.aes=list(color=c("darkorange","blue","black")),order=2)) +
  stat_summary(geom="point",fun="mean",size=2) +
  scale_shape_manual(values = c(21,24)) +
  labs(x = "Genet", y = expression("Days")) + 
  scale_y_continuous(breaks=seq(4,16,2)) +
  annotate("text", x=c(30),y=c(3.5), label=c("average days to removal after ≥30% Fv/Fm decline or mortality"),size=3) +
  theme_bw() + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title = element_text(size=10),axis.text = element_text(),
                     legend.title = element_blank(),
                     legend.key.size = unit(2,"mm"),
                     legend.box="horizontal",
                     legend.background = element_blank(), legend.position = c(0.01,1.05),
                     legend.justification = c("left", "top"),
                     legend.box.just = "left",
                     legend.margin = margin(1, 1, 1, 1),
                     legend.text = element_text(hjust = 0))

