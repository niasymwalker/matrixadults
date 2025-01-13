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
library(moments)
library(cowplot)
library(rstatix)
library(reshape2)
library(survival)
library(survminer)
library(utile.visuals)
library(bdscale)
library(scales)

####Figure 1####
d4<-read_csv("data/controller04 SP and PV Time Series-data-2023-04-27 14_54_26.csv")%>%
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

d13<-read_csv("data/controller13 SP and PV Time Series-data-2023-04-27 14_54_53.csv")%>%
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

d14<-read_csv("data/controller14 SP and PV Time Series-data-2023-11-01 10_47_23.csv")%>%
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

A1_c4<-read_csv("data/controller04.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

A2_c5<-read_csv("data/controller05.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Ambient")) %>%
  na.omit()

A3_c14<-read_csv("data/controller14.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

A4_c15<-read_csv("data/controller15.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  add_column(group= c("Heated")) %>%
  na.omit()

resistrecover_data <- read_excel("data/resist_recover_data.xlsx", na="NA") %>% clean_names() %>%
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
  scale_y_continuous(breaks = c(22,24,26,28,30,32),
                     labels = c(22,24,26,28,30,32)) +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "2 days") +
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

profiles <- read_excel("data/ITS2_typeprofile.xlsx") %>%
  mutate(genet=as.factor(genet)) %>%
  mutate(coral_id=as.factor(coral_id)) %>%
  mutate(sum = rowSums(.[4:31],na.rm=TRUE)) %>%
  gather(type_profile,profile_present,4:31,factor_key=TRUE) %>%
  mutate(type_profile_prop = profile_present/sum) %>%
  mutate(type_profile=as.factor(type_profile)) %>%
  mutate_all(na_if,"0") %>%
  arrange(genet) %>%
  drop_na() %>%
  add_column(CvsD = NA) %>%
  mutate(CvsD = if_else(grepl("C31",type_profile),"Cladocopium", CvsD)) %>%
  mutate(CvsD = if_else(grepl("D1",type_profile),"Durusdinium", CvsD)) %>%
  filter(!genet=="651")

#Above profiles plus Cladocopium and Durusdinium specific column calculations
remove_D <- c("Durusdinium")
remove_C <- c("Cladocopium")
remove_value <- c("NA")

profiles_clades <- profiles %>%
  distinct(coral_id,sum, .keep_all = TRUE) %>%
  group_by(genet) %>%
  summarise(sum_colony = sum(sum)) %>%
  left_join(.,profiles,"genet") %>%
  group_by(genet,CvsD) %>%
  mutate(clade_sum = sum(profile_present)) %>%
  mutate(Cladocopium = clade_sum/sum_colony) %>%
  mutate(Cladocopium = if_else(CvsD %in% remove_D, NA_real_,Cladocopium)) %>%
  mutate(Durusdinium_working = 1-(clade_sum/sum_colony)) %>%
  mutate(Durusdinium_working = if_else(CvsD %in% remove_C, NA_real_,Durusdinium_working)) %>%
  unite(Cladocopium_prop, Cladocopium, Durusdinium_working, sep = "") %>%
  mutate(Cladocopium_prop = str_remove(Cladocopium_prop, remove_value)) %>%
  mutate(clade=case_when(Cladocopium_prop>=0.8~"C",
                         Cladocopium_prop<=0.2~"D",
                         TRUE~"mixed"))%>%
  mutate(Cladocopium_prop=as.numeric(Cladocopium_prop)) %>%
  mutate(Cladocopium_prop = round(Cladocopium_prop, 5)) %>%
  group_by(genet) %>%
  select(genet,Cladocopium_prop,clade) %>%
  distinct()

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

quartz(w=4,h=3.5)
plot_grid(M1A,M1B, rel_heights=c(1.1,1), nrow=2, labels=c("A","B"), axis="tb", align="h",label_size=12)
quartz.save("output/Fig1_final.pdf", type="pdf")

profiles2 <- profiles %>%
  mutate(genet = as.factor(genet)) %>%
  left_join(.,profiles_clades,by="genet") %>%
  mutate(genet = fct_reorder(genet, Cladocopium_prop, .fun = mean)) %>%
  mutate(Durusdinium_prop = (1-Cladocopium_prop)) %>%
  select(genet,type_profile,type_profile_prop,CvsD,clade,Cladocopium_prop,Durusdinium_prop)

my_colors_profile <- c("C31-C17d-C31.1-C21-C31a-C31f-C21ac-C31l"="#fcf992",
                       "C31-C17d-C21-C31.1-C31a-C17e-C31f"="#dbd865",
                       "C31-C17d-C31.1-C21-C31a-C17e-C21ac-C31.10"="#b8b546",
                       "C31-C31.1-C17d-C31a-C21-C31f-C31b-C31l"="#5e5c08",
                       "C31-C17d-C21-C31a-C21ac-C31.9"="#8ef6fa",
                       "C31-C17d-C21-C31a-C31.9"="#5ed2d6",
                       "C31-C17d-C21-C31.9-C31.5-C17i-C31.10-C21ac-C17"="#41bbbf",
                       "C31/C17d-C21-C31.9-C21ac-C31i-C31h"="#36b1b5",
                       "C31/C17d-C21-C21ac-C31a-C17e-C31.9-C17f"="#27a4a8",
                       "C31/C17d-C21-C21ac-C31k-C31.9"="#0a9dab",
                       "C31-C17d-C21-C21ac-C31k-C31a"="#0b7580",
                       "C31/C17d-C21-C21ac-C31j"="#0b4d54",
                       "C31/C17d"="#adadad",
                       "C31/C17d-C21-C31a-C21ac-C17i-C17"="#8a8a8a",
                       "C31/C17d-C21-C21ac-C17i-C17-C17e-C31j"="#5c5c5c",
                       "C31-C21-C17d-C21ac-C17i-C17"="#363636",
                       "C31-C17d-C21-C21ac-C17i-C31a"="#121212",
                       "D1/D4/D6-D1ab-D3h"="#e9b8fc",
                       "D1/D4/D1ab-D6-D4d"="#de98fa",
                       "D1/D4-D6-D1ab-D17d-D17j"="#cd7eed",
                       "D1/D4-D6-D17d-D1r-D17e-D17c"="#b45ed6",
                       "D1-D1ab-D4-D6-D1ca"="#ae41d9",
                       "D1/D6-D4-D1r"="#9814cc",
                       "D1/D4/D6/D1ab"="#793594",
                       "D1-D4-D6-D1ab-D17d"="#6c218a",
                       "D1/D4-D6-D1d"="#631085",
                       "D1/D6/D4"="#4f1d63",
                       "D1-D4-D1ab-D6-D10"="#450d5c")

#Restrict type profiles in legend to what are actually used
subset_profile <- c("C31-C17d-C31.1-C21-C31a-C31f-C21ac-C31l"=="#fcf992",
                    "C31-C17d-C21-C31.1-C31a-C17e-C31f"="#dbd865",
                    "C31-C17d-C31.1-C21-C31a-C17e-C21ac-C31.10"="#b8b546",
                    "C31-C31.1-C17d-C31a-C21-C31f-C31b-C31l"="#5e5c08",
                    "C31-C17d-C21-C31a-C21ac-C31.9"="#8ef6fa",
                    "C31-C17d-C21-C31a-C31.9"="#5ed2d6",
                    "C31-C17d-C21-C31.9-C31.5-C17i-C31.10-C21ac-C17"="#41bbbf",
                    "C31/C17d-C21-C31.9-C21ac-C31i-C31h"="#36b1b5",
                    "C31/C17d-C21-C21ac-C31a-C17e-C31.9-C17f"="#27a4a8",
                    "C31/C17d-C21-C21ac-C31k-C31.9"="#0a9dab",
                    "C31-C17d-C21-C21ac-C31k-C31a"="#0b7580",
                    "C31/C17d-C21-C21ac-C31j"="#0b4d54",
                    "C31/C17d"="#adadad",
                    "C31/C17d-C21-C31a-C21ac-C17i-C17"="#8a8a8a",
                    "C31/C17d-C21-C21ac-C17i-C17-C17e-C31j"="#5c5c5c",
                    "C31-C17d-C21-C21ac-C17i-C31a"="#121212",
                    "D1/D4/D6-D1ab-D3h"="#e9b8fc",
                    "D1/D4/D1ab-D6-D4d"="#de98fa",
                    "D1/D4-D6-D1ab-D17d-D17j"="#cd7eed",
                    "D1/D4-D6-D17d-D1r-D17e-D17c"="#b45ed6",
                    "D1/D6-D4-D1r"="#9814cc",
                    "D1/D4/D6/D1ab"="#793594",
                    "D1-D4-D6-D1ab-D17d"="#6c218a")

#Order ITS2 type profiles
profiles$type_profile = factor(profiles$type_profile,levels=c("C31-C17d-C31.1-C21-C31a-C31f-C21ac-C31l",
                                                              "C31-C17d-C21-C31.1-C31a-C17e-C31f",
                                                              "C31-C17d-C31.1-C21-C31a-C17e-C21ac-C31.10",
                                                              "C31-C31.1-C17d-C31a-C21-C31f-C31b-C31l",
                                                              "C31-C17d-C21-C31a-C21ac-C31.9",
                                                              "C31-C17d-C21-C31a-C31.9",
                                                              "C31-C17d-C21-C31.9-C31.5-C17i-C31.10-C21ac-C17",
                                                              "C31/C17d-C21-C31.9-C21ac-C31i-C31h",
                                                              "C31/C17d-C21-C21ac-C31a-C17e-C31.9-C17f",
                                                              "C31/C17d-C21-C21ac-C31k-C31.9",
                                                              "C31-C17d-C21-C21ac-C31k-C31a",
                                                              "C31/C17d-C21-C21ac-C31j",
                                                              "C31/C17d",
                                                              "C31/C17d-C21-C31a-C21ac-C17i-C17",
                                                              "C31/C17d-C21-C21ac-C17i-C17-C17e-C31j",
                                                              "C31-C21-C17d-C21ac-C17i-C17",
                                                              "C31-C17d-C21-C21ac-C17i-C31a",
                                                              "D1/D4/D6-D1ab-D3h",
                                                              "D1/D4/D1ab-D6-D4d",
                                                              "D1/D4-D6-D1ab-D17d-D17j",
                                                              "D1/D4-D6-D17d-D1r-D17e-D17c",
                                                              "D1-D1ab-D4-D6-D1ca",
                                                              "D1/D6-D4-D1r",
                                                              "D1/D4/D6/D1ab",
                                                              "D1-D4-D6-D1ab-D17d",
                                                              "D1/D4-D6-D1d",
                                                              "D1/D6/D4",
                                                              "D1-D4-D1ab-D6-D10"))


M1C <- ggplot(profiles2, aes(x=reorder(genet,-Cladocopium_prop), y=type_profile_prop))+
  geom_col(aes(fill = type_profile), colour="grey", size=0.005)+
  labs(x="Genet", y="Symbiodiniaceae Proportion")+
  theme_bw(base_size = 12)+
  scale_y_continuous(labels=function(Prop)Prop+1)+
  scale_fill_manual(values=my_colors_profile, name="ITS2 Type Profile")+
  scale_y_continuous(labels=c("1" = "0", "2" = "0.33", "3" = "0.67","4" = "1.00")) +
  guides(color = guide_legend(override.aes = list(linewidth=1.5)), fill=guide_legend(ncol=1, title.theme = element_text(angle = 0)))+
  theme(axis.title.x = element_text(size=10), axis.title.y = element_text(size=10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = c(rep("darkorange",9),rep("grey15",6),rep("blue",45))),
        axis.ticks.x=element_blank(), axis.text.y = element_text(size=8), text=element_text(size=7),panel.grid = element_blank(), 
        strip.text = element_text(size = 8), legend.key.size = unit(0.2, "cm"))
#, legend.position = "right", title.theme = element_text(angle = 90)

quartz(w=7.2,h=5)
M1AB <- plot_grid(M1B,M1A,nrow=1, labels=c("B","C"),rel_widths = c(1,0.8), axis="tb",align="hv")
plot_grid(M1C,NULL,M1AB, nrow=3, labels=c("A","",""), rel_heights = c(1.1,0.01,1.05), axis="tb", align="h",label_size=12)
quartz.save("output/Fig1_final.pdf", type="pdf")

#####Figure 2#####
corals_list <- read_excel("data/matrixcorals_list.xlsx") %>% clean_names()

pam_data <- read_excel("data/resist_recover_data.xlsx", na="NA") %>% clean_names() %>%
  gather(hs_day,hs_pam,pam_hs0:pam_hs14) %>%
  separate(hs_day,into = c("trash","hs_timepoint"), sep = "_") %>%
  mutate(percentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(percentdecline_pam = na_if(percentdecline_pam, percentdecline_pam[percentdecline_pam <30])) %>%
  mutate(mortpercentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(mortpercentdecline_pam = na_if(mortpercentdecline_pam, mortpercentdecline_pam[percentdecline_pam >29.99])) %>%
  mutate(percentdecline_rec = (1-(rec_pam/pre_pam))*100) %>%
  select(treatment, genet, coral_id, pre_pam, last_pam, rec_pam, hs_timepoint, hs_pam, percentdecline_pam, mortpercentdecline_pam, percentdecline_rec, days_to_mortality) %>%
  distinct()

#ED30 model values calculated using PAM data and heat logger data! ADULTS#
#DHW <- read_tsv("./output.txt")
resistrecover_pam <- pam_data %>%
  filter(is.na(days_to_mortality)) %>%
  select(hs_timepoint,hs_pam,coral_id,genet,treatment)%>%
  left_join(.,output)%>%
  filter(treatment=="Heated")%>%
  mutate(initial_pam=case_when(hs_timepoint=="hs0"~hs_pam))%>%
  arrange(coral_id)%>%
  group_by(coral_id)%>%
  fill(initial_pam,.direction=c("down"))%>%
  mutate(relative_pam=hs_pam/initial_pam)%>%
  drop_na()%>%
  mutate(genet=as.factor(genet))%>%
  filter(genet!="765")

list <- resistrecover_pam%>%
  ungroup()%>%
  select(genet)%>%
  distinct()
out<-data.frame(genet=character(),cumulative=numeric(),predicted=numeric(),pmin=numeric(),pmax=numeric())
ED<-data.frame(genet=character(),ed30=numeric(),se=numeric(),lwr=numeric(),upper=numeric())

for(i in list$genet){
  temp<-resistrecover_pam%>%filter(genet==paste0(i))
  model <- drc::drm(relative_pam~cumulative, data=temp, fct=drc::W1.3(fixed=c(NA,1,NA),names=c("Slope","Upper Limit","ED30")))
  summary(model)  
  summary <- drc::ED(model,c(30), interval="delta")
  ED<-ED%>%add_row(genet=i,ed30=summary[1],se=summary[2],lwr=summary[3],upper=summary[4])
  newdata <- expand.grid(cumulative=seq(min(resistrecover_pam$cumulative),max(8),length=100))
  pm <- predict(model,newdata=newdata,interval="confidence")
  newdata<-bind_cols(newdata,pm)
  out<-out%>%add_row(genet=i,cumulative=newdata$cumulative,predicted=newdata$Prediction,pmin=newdata$Lower,pmax=newdata$Upper)
  
}
#write.csv(ED,"NIAROX_ED30.csv")

min(ED$ed30)
max(ED$ed30)
mean(ED$ed30)

#JUST CONTROLS
resistrecover_pam_control <- pam_data %>%
  filter(is.na(days_to_mortality)) %>%
  select(hs_timepoint,hs_pam,coral_id,genet,treatment)%>%
  left_join(.,output)%>%
  filter(treatment=="Control")%>%
  mutate(initial_pam=case_when(hs_timepoint=="hs0"~hs_pam))%>%
  arrange(coral_id)%>%
  group_by(coral_id)%>%
  fill(initial_pam,.direction=c("down"))%>%
  mutate(relative_pam=hs_pam/initial_pam)%>%
  drop_na()%>%
  mutate(genet=as.factor(genet))%>%
  filter(genet!="765") %>%
  mutate(hs_timepoint = str_remove(hs_timepoint, "hs")) %>%
  mutate(hs_timepoint=as.numeric(hs_timepoint)) %>%
  mutate(relative_pam=as.numeric(relative_pam))

resistrecover_pam_control$relative_pam[resistrecover_pam_control$relative_pam > 1] <- 1
resistrecover_pam_control$relative_pam = as.numeric(resistrecover_pam_control$relative_pam)

empty <- read_excel("data/empty plug data.xlsx") %>% clean_names() %>% select(plug_bw) %>%
  mutate(plug_bw=as.numeric(plug_bw))
x <- mean(empty$plug_bw)

#buoyant weight initial timepoint
bw_t0 <- read_excel("data/08_25_2022 NSW.xlsx") %>% clean_names() %>% separate(buoyant_weight, into=c("buoyant_weight1","trash1"), " ") %>%
  separate(bw_2, into=c("buoyant_weight2","trash2"), " ") %>%
  separate(bw_3, into=c("buoyant_weight3","trash3"), " ") %>%
  select(coral_id,contains("buoy")) %>% gather(rep, bw, buoyant_weight1:buoyant_weight3) %>%
  mutate(bw=as.numeric(bw)) %>%
  group_by(coral_id) %>% summarise(avg_bw=mean(bw)-x) 

#buoyant weight initial and final timepoints, plus percent change bw
bw_data <- read_excel("data/bw_NSW_11.16.22.xlsx") %>% clean_names() %>% separate(bw1, into=c("buoyant_weight1","trash1"), " ") %>%
  separate(bw2, into=c("buoyant_weight2","trash2"), " ") %>%
  separate(bw3, into=c("buoyant_weight3","trash3"), " ") %>%
  select(coral_id,contains("buoy")) %>% gather(rep, bw, buoyant_weight1:buoyant_weight3) %>%
  mutate(bw=as.numeric(bw)) %>%
  group_by(coral_id) %>% summarise(avg_bw=mean(bw)-x) %>%
  left_join(.,bw_t0,by="coral_id") %>% rename(bw_post=2,bw_pre=3) %>%
  mutate(percentchange_bw = ((bw_post-bw_pre)/bw_pre)*100) %>%
  left_join(.,corals_list,by="coral_id") %>%
  select(treatment,genet,coral_id,percentchange_bw,bw_post,bw_pre) %>%
  mutate(genet=as.factor(genet)) %>%
  drop_na()

#pre surface area, volume data from metashape, normalized to plug SA and volume
metashape_pre <- read_csv("data/Matrix_Reloaded_T0_data_frags.csv") %>% clean_names() %>%
  mutate(sa_pre = surface_area_cm_2 - 3.11025526687023) %>%
  mutate(volume_pre = volume_cm_3 - 0.0170400140725387) %>%
  mutate(coral_id=model) %>%
  select(coral_id,sa_pre,volume_pre)

#pre and post surface area, volume data from metashape, normalized to plug SA and volume, joined to bw data
growth_data <- read_csv("data/Matrix_Reloaded_T6_data_frags.csv") %>% clean_names() %>%
  mutate(sa_post = surface_area_cm_2 - 3.11025526687023) %>%
  mutate(volume_post = volume_cm_3 - 0.0170400140725387) %>%
  mutate(coral_id=model) %>%
  select(coral_id,sa_post,volume_post) %>%
  left_join(.,metashape_pre,by="coral_id") %>%
  mutate(percentchange_sa = ((sa_post-sa_pre)/sa_pre)*100) %>%
  mutate(percentchange_volume = ((volume_post-volume_pre)/volume_pre)*100) %>%
  left_join(.,bw_data,by="coral_id") %>%
  group_by(coral_id) %>%
  mutate(density_pre = bw_pre/volume_pre) %>%
  mutate(density_post = bw_post/volume_post) %>%
  mutate(percentchange_density = ((density_post-density_pre)/density_pre)*100) %>%
  mutate(changevolume = volume_post-volume_pre) %>%
  mutate(changebw = bw_post-bw_pre) %>%
  mutate(changedensity = changebw/changevolume) %>%
  mutate(densityextension = changedensity/density_pre) %>%
  mutate(adult_growthrate=(sa_post-sa_pre)/83) %>%
  select(treatment,genet,coral_id,adult_growthrate,bw_pre,bw_post,percentchange_bw,sa_pre,sa_post,percentchange_sa,volume_pre,volume_post,percentchange_volume,density_pre,density_post,percentchange_density,changevolume,changebw,changedensity,densityextension) %>%
  drop_na()

M2A <- ggplot() +
  geom_line(data=out,aes(x=cumulative,y=predicted,group=genet))+
  geom_smooth(data=out,aes(x=cumulative,y=predicted),color="firebrick", se=FALSE, linewidth=2)+
  geom_smooth(data=resistrecover_pam_control,aes(x=cumulative,y=relative_pam),color="dodgerblue", se=FALSE, linewidth=2)+
  geom_hline(yintercept=.7, linetype="dotted") +
  scale_x_continuous(sec.axis=sec_axis(~.*1.8181818182,name="Heat Tolerance Assay Days",breaks=seq(0,14,2))) +
  labs(x = "Degree Heating Weeks", y = expression(" \nRelative Fv/Fm")) + 
  theme_bw() + theme(legend.position="none", axis.title = element_text(size=10),axis.text = element_text(),axis.title.x.bottom = element_text(colour = "firebrick"),axis.title.x.top = element_text(colour = "dodgerblue"))

#Adult ED30 and growth data
resistrecover_ED <- ED %>%
  left_join(.,growth_data,by="genet") %>%
  select(treatment,genet,coral_id,ed30,bw_pre,bw_post,percentchange_bw,sa_pre,sa_post,percentchange_sa,volume_pre,volume_post,percentchange_volume,density_pre,density_post,percentchange_density,changevolume,changebw,changedensity,densityextension) %>%
  distinct()

#Symbionts and Adult ED30, and Growth
symbiont_metashape <- profiles %>%
  left_join(.,resistrecover_ED,by="genet",relationship="many-to-many") %>%
  mutate(coral_id=coral_id.x) %>%
  select(treatment,genet,percentchange_bw,percentchange_sa,percentchange_volume,percentchange_density,ed30,type_profile,type_profile_prop,CvsD)

symbiont_clade_metashape <- profiles_clades %>%
  left_join(.,resistrecover_ED,by="genet",relationship="many-to-many") %>%
  select(treatment,genet,percentchange_bw,percentchange_sa,percentchange_volume,percentchange_density,ed30,Cladocopium_prop,clade)

symbiont_growth <- symbiont_clade_metashape %>%
  select(genet,percentchange_sa,percentchange_bw,percentchange_volume,percentchange_density,clade) %>%
  distinct() %>%
  na.omit()

symbiont_growth$clade <- factor(symbiont_growth$clade, levels=c("C", "mixed", "D"))

#Plot ED, metashape data based on cladocopium proportion#
##clade vs. ED
symbiont_ED <- symbiont_clade_metashape %>%
  select(genet,ed30,Cladocopium_prop,clade) %>%
  distinct() %>%
  na.omit()

symbiont_ED$clade <- factor(symbiont_ED$clade, levels=c("C", "mixed", "D"))

#write.csv(symbiont_ED,"symbiont_ED.csv")

ed30_clade_model <- aov(lm(ed30 ~ clade, data = symbiont_ED))
TukeyHSD(ed30_clade_model)

#pval_CD2 <- c("**")
#pval_CM2 <- c("*")
#pval_MD2 <- c("")
pval_CD2 <- c("0.028")
pval_CM2 <- c("0.613")
pval_MD2 <- c("0.521")

M2B <- ggplot(symbiont_ED,aes(x=clade,y=ed30,color=clade)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Dominant Symbiont Type", y = expression("Tolerance (ED30)")) +
  annotate("text", x=1.5, y=5.5, label=pval_CM2, size=3) +
  annotate("text", x=2, y=6.3, label=pval_CD2, size=3) +
  annotate("text", x=2.5, y=7, label=pval_MD2, size=3) +
  annotate("segment", x = 1.1, xend = 1.9, y= 5.2, yend=5.2) +
  annotate("segment", x = 1.1, xend = 2.9, y= 6.0, yend=6.0) +
  annotate("segment", x = 2.1, xend = 2.9, y= 6.7, yend=6.7) +
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  scale_color_manual(values = c("darkorange", "black", "blue")) +
  theme_bw() + theme(axis.title = element_text(size=10),legend.position="none",axis.title.x=element_blank()) 

##Fisher's exact test for mortality Heat Resistance
sym_analysis <- matrix(c(0,28,0,18,25,111),ncol=2,nrow=3,dimnames=list(c("C spp.", "Mixed", "D spp."),c("Dead","Alive")),byrow=TRUE)
fisher.test(sym_analysis)
pairwise_fisher_test(as.matrix(sym_analysis))
tmp <- melt(sym_analysis)
names(tmp) <- c("Symbio", "Mortality", "Ramets")
tmp <- tmp %>%
  mutate(Mortality = as.factor(Mortality)) %>%
  mutate(Symbio = as.factor(Symbio))

#pval_CD <- c("*")
#pval_CM <- c("")
#pval_MD <- c("")
pval_CD <- c("0.0086")
pval_CM <- c("1.00")
pval_MD <- c("0.045")


M2C <- ggplot(tmp, aes(x=Symbio, y=Ramets, fill=Mortality)) +
  geom_bar(stat="identity", position="stack", color="black") +
  scale_fill_manual(values=c("orange4","seagreen4")) +
  annotate("text", x=1.5, y=50, label=pval_CM, size=3) +
  annotate("text", x=1.8, y=80, label=pval_CD, size=3) +
  annotate("text", x=2.2, y=110, label=pval_MD, size=3) +
  annotate("segment", x = 1.1, xend = 1.9, y= 40, yend=40) +
  annotate("segment", x = 1.1, xend = 2.5, y= 70, yend=70) +
  annotate("segment", x = 1.9, xend = 2.5, y= 100, yend=100) +
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  labs(x = "", y = "Ramet #") + 
  theme_bw() + theme(axis.title = element_text(size=10),
                     legend.title = element_blank(),legend.key.size = unit(2,"mm"),
                     legend.background = element_blank(), legend.position = c(.01, .99),
                     legend.justification = c("left", "top"),
                     legend.box.just = "left",
                     legend.margin = margin(6, 6, 6, 6))

quartz(w=7.2,h=3)
plot_grid(M2A,M2B,M2C, rel_widths = c(1.5,.9,.9), ncol=3, labels=c("A","B","C"), align="hv",axis="tb", label_size=12)
quartz.save("./output/Fig2_final.pdf", type="pdf")

#####Figure 3#####
resistrecover_data_long <- read_excel("./data/resist_recover_data.xlsx", na="NA") %>% clean_names() %>%
  gather(rec_day,rec_mort,mort_r0:mort_r80) %>%
  distinct() %>%
  separate(rec_day,into = c("trash","rec_timepoint"), sep = "_") %>%
  mutate(percentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(percentdecline_pam = na_if(percentdecline_pam, percentdecline_pam[percentdecline_pam <30])) %>%
  mutate(mortpercentdecline_pam = (1-(last_pam/pre_pam))*100) %>%
  mutate(mortpercentdecline_pam = na_if(mortpercentdecline_pam, mortpercentdecline_pam[percentdecline_pam >29.99])) %>%
  select(treatment,genet,coral_id,pre_pam,last_pam,days_to_bleach,days_to_mortality,percentdecline_pam,mortpercentdecline_pam,rec_timepoint,rec_mort,rec_pam) %>%
  distinct()

survivor_data <- resistrecover_data_long %>%
  select(treatment,genet,coral_id,rec_timepoint,rec_mort) %>%
  mutate(genet=as.factor(genet)) %>%
  left_join(.,profiles_clades,by="genet") %>%
  left_join(.,ED,by="genet",relationship="many-to-many") %>%
  select(treatment,genet,coral_id,rec_timepoint,rec_mort,clade,ed30) %>%
  add_column(time_days = NA) %>%
  mutate(time_days = if_else(grepl("r0",rec_timepoint),"0", time_days)) %>%
  mutate(time_days = if_else(grepl("r1",rec_timepoint),"1", time_days)) %>%
  mutate(time_days = if_else(grepl("r2",rec_timepoint),"2", time_days)) %>%
  mutate(time_days = if_else(grepl("r3",rec_timepoint),"3", time_days)) %>%
  mutate(time_days = if_else(grepl("r4",rec_timepoint),"4", time_days)) %>%
  mutate(time_days = if_else(grepl("r5",rec_timepoint),"5", time_days)) %>%
  mutate(time_days = if_else(grepl("r6",rec_timepoint),"6", time_days)) %>%
  mutate(time_days = if_else(grepl("r7",rec_timepoint),"7", time_days)) %>%
  mutate(time_days = if_else(grepl("r14",rec_timepoint),"14", time_days)) %>%
  mutate(time_days = if_else(grepl("r30",rec_timepoint),"30", time_days)) %>%
  mutate(time_days = if_else(grepl("r60",rec_timepoint),"60", time_days)) %>%
  mutate(time_days = if_else(grepl("r80",rec_timepoint),"80", time_days)) %>%
  mutate(time_days=as.numeric(time_days)) %>%
  mutate(treatment = case_when(
    treatment == "Control" ~ "Ambient",
    TRUE ~ treatment)) %>%
  na.omit() %>%
  distinct()

survivor_sub <- survivor_data %>%
  mutate(treatment = case_when(
    treatment == "Control" ~ "Ambient",
    TRUE ~ treatment)) %>%
  filter(!treatment=="Ambient") %>%
  filter(time_days=="80") %>%
  filter(rec_mort=="0") %>%
  select(genet) %>%
  distinct()

fit_rec <- survfit(Surv(time_days,rec_mort) ~ factor(treatment), data = survivor_data)

#extract data from survfit object
tidy_fit_rec <- tidy(fit_rec) %>%
  add_column(strata2=NA) %>%
  mutate(strata2 = if_else(grepl("Ambient",strata),"Ambient", strata2)) %>%
  mutate(strata2 = if_else(grepl("Heated",strata),"Heated", strata2))

M3A <- ggplot(tidy_fit_rec, aes(x=time,y=estimate,group=strata2,fill=strata2)) +
  geom_step(aes(time,estimate,color=strata2)) +
  geom_point(aes(time,estimate,color=strata2),shape=3,size=2) +
  geom_stepconfint(aes(ymin = conf.low, ymax = conf.high), fill="firebrick",alpha = 0.4) +
  labs(x = "Time in Days", y = expression("Survival Probability")) + 
  annotate("text", x=c(15),y=c(0.2), label=c("p < 0.0001")) +
  scale_x_continuous(breaks = c(0,7,14,30,60,80),
                     labels = c(0,7,14,30,60,80)) +
  scale_color_manual(values = c("dodgerblue","firebrick"),labels = c("Ambient","Heated")) +
  scale_fill_manual(values = c("dodgerblue","firebrick")) +
  guides(fill=guide_legend(override.aes=list(fill=c("dodgerblue","firebrick")))) +
  theme_bw() + theme(axis.title = element_text(size=10),
                     legend.title = element_blank(),legend.background=element_blank(), legend.position = c(.95, .99),
                     legend.justification = c("right", "top"),
                     legend.key.size = unit(2,"mm"),
                     legend.box.just = "right",
                     legend.margin = margin(6, 6, 6, 6)
  )

#pam mortality across recovery
rec_pam_data <- pam_data %>%
  filter(is.na(days_to_mortality)) %>%
  mutate(rec_died=case_when(rec_pam>=0~"Alive", TRUE~"Dead"))%>%
  select(coral_id,genet,treatment,pre_pam,last_pam,rec_pam,rec_died)%>%
  gather(timepoint,pam,pre_pam:rec_pam) %>%
  distinct() %>%
  filter(treatment=="Heated")%>%
  mutate(initial_pam=case_when(timepoint=="pre_pam"~pam))%>%
  arrange(coral_id)%>%
  group_by(coral_id)%>%
  fill(initial_pam,.direction=c("down"))%>%
  mutate(relative_pam=pam/initial_pam)%>%
  drop_na()%>%
  mutate(genet=as.factor(genet)) %>%
  add_column(new_time=NA) %>%
  mutate(new_time = if_else(grepl("pre_pam",timepoint),"0", new_time)) %>%
  mutate(new_time = if_else(grepl("last_pam",timepoint),"1", new_time)) %>%
  mutate(new_time = if_else(grepl("rec_pam",timepoint),"2", new_time)) %>%
  mutate(new_time=as.numeric(new_time))
#write.csv(rec_pam_data,"rec_pam_data.csv")

M3B <- ggplot(rec_pam_data, aes(x=new_time,y=relative_pam,group=coral_id,color=rec_died)) +
  geom_hline(yintercept = 0.7, linetype="dotted", color="black",linewidth=1.25) +
  geom_point(aes(color=rec_died), alpha=0.8)+
  geom_line(aes(color=rec_died), alpha=0.5) +
  scale_color_manual(values = c("seagreen4","orange4")) +
  scale_x_continuous(limits=c(0,2),breaks=seq(0,2,by=1),labels=c("0" = "Pre\nStress", "1" = "End of\nStress", "2" = "Recovery\nDay 80")) +
  labs(y="Relative Fv/Fm")+
  guides(color=guide_legend(title="Survival on\nRecovery Day 80",title.hjust = 0.5)) +
  theme_bw() + theme(axis.title = element_text(size=10),
                     axis.title.x=element_blank(),legend.position="none")

#dataset for logistic regression morality
mortdays_ED <- ED %>%
  left_join(.,resistrecover_data,by="genet") %>%
  filter(is.na(days_to_mortality)) %>%
  filter(!treatment=="Control") %>%
  select(genet,coral_id,ed30,mort_r1,mort_r2,mort_r3,mort_r4,mort_r5,mort_r6,mort_r7,mort_r14,mort_r30,mort_r60,mort_r80) %>%
  left_join(.,profiles_clades,by="genet") %>%
  select(genet,coral_id,ed30,clade,mort_r1,mort_r2,mort_r3,mort_r4,mort_r5,mort_r6,mort_r7,mort_r14,mort_r30,mort_r60,mort_r80) %>%
  add_column(surv_r80 = NA) %>%
  mutate(surv_r80 = if_else(grepl("0", mort_r80),"1", surv_r80)) %>%
  mutate(surv_r80 = if_else(grepl("1", mort_r80),"0", surv_r80)) %>%
  mutate(surv_r80=as.numeric(surv_r80))

mortdays_ED$clade <- factor(mortdays_ED$clade,levels=c("C","mixed","D"))

##survivorship logistic regression based on ED values, 
day80.glm <- glm(surv_r80 ~ ed30, mortdays_ED, family = binomial)
summary(day80.glm, corr = FALSE)
#0.00123 generalized linear model, logistic regression

M3C <- ggplot(mortdays_ED, aes(x = ed30, y = surv_r80)) +
  geom_point(position = position_jitter(width = 0.3, height = 0)) +
  stat_smooth(aes(x = ed30), method = "glm", color = "black",  
              method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(x = "Heat Tolerance",
       y = "Survival") + 
  annotate("text", x=c(6.1),y=c(0.1), label=c("p = 0.0012"),size=2.8) +
  annotate("text", x=c(3.5),y=c(0.85), label=c("Recovery Day 80"), size=2.8) +
  scale_x_continuous(breaks=seq(2, 7, 1)) +
  ylim(0,1) + theme_bw() + theme(axis.title = element_text(size=9),axis.title.y = element_text(hjust = 0.5))
  
quartz(w=7.2,h=2)
plot_grid(M3A,M3B,NULL,M3C, rel_widths=c(1.3,1.1,0.1,1), nrow=1,labels=c("A","B","","C"), label_size=12, axis="tb", align="hv")
quartz.save("./output/Fig3_final.pdf", type="pdf")

#####Figure 4#####
##surface area vs ED analysis
#transform data and normality test
resistrecover_ED_sa <- resistrecover_ED %>%
  group_by(genet) %>%
  mutate(percentchange_avgsa = mean(percentchange_sa))

resistrecover_ED_sa$percentchange_avgsa2 <- log10(resistrecover_ED_sa$percentchange_avgsa)
resistrecover_ED_sa$percentchange_sa2 <- log10(resistrecover_ED_sa$percentchange_sa)
resistrecover_ED_sa$ed30_2 <- resistrecover_ED_sa$ed30^2

resistrecover_ED_sa <- resistrecover_ED_sa %>%
  na.omit()

ggplot(resistrecover_ED_sa, aes(x=percentchange_avgsa2)) +
  geom_histogram()

ggdensity(resistrecover_ED_sa$percentchange_avgsa2, main="Density plot of sa", xlab="surface area")
ggqqplot(resistrecover_ED_sa$percentchange_avgsa2)
skewness(resistrecover_ED_sa$percentchange_avgsa2, na.rm=TRUE)

#log tranformation, added 5 constant
sa_lm <- lm(percentchange_avgsa2 ~ ed30, data=resistrecover_ED_sa) 
sa_quad <- lm(percentchange_avgsa2 ~ ed30 + ed30_2, data=resistrecover_ED_sa)
#linear and quadratic fit sig

M4A <- ggplot(resistrecover_ED_sa,aes(x=ed30,y=percentchange_avgsa2)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_smooth(method="lm",formula = y ~ x + I(x^2),color="red") +
  labs(x = "Heat Tolerance", y = expression(log*"%"*Delta~Surface~Area)) + 
  annotate("text", x=c(5.35,5.2),y=c(-0.07,.07), label=c("linear p = 0.0039","quad p = 2.27e-05"),size=2.8) +
  theme_bw() + theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_text(size = 9)) 

#SA vs. Symbiont Clade analysis
M4Baov <- aov(lm(percentchange_sa ~ clade, data = symbiont_growth))
TukeyHSD(M4Baov)
#all are not significant
#CD: 0.232
#CM: 0.324
#MD: 0.927

pval_CD3 <- c("")
pval_CM3 <- c("")
pval_MD3 <- c("")

M4B <- ggplot(symbiont_growth,aes(x=clade,y=percentchange_sa,color=clade)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Dominant Symbiont spp.", y = expression("%"~Delta~Surface~Area)) + 
  annotate("text", x=2, y=30, label=c("all n.s.")) +
  #annotate("text", x=1.5, y=22, label=pval_CM3) +
  #annotate("text", x=2, y=27, label=pval_CD3) +
  #annotate("text", x=2.5, y=32, label=pval_MD3) +
  #annotate("segment", x = 1.1, xend = 1.9, y= 20, yend=20) +
  #annotate("segment", x = 1.1, xend = 2.9, y= 25, yend=25) +
  #annotate("segment", x = 2.1, xend = 2.9, y= 30, yend=30) +
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  scale_color_manual(values = c("darkorange", "black", "blue")) +
  theme_bw() + theme(legend.position="none",axis.title.x=element_blank(),axis.title = element_text(size = 9)) 

##Buoyant Weight vs. ED analysis
resistrecover_ED_bw <- resistrecover_ED %>%
  group_by(genet) %>%
  mutate(percentchange_avgbw = mean(percentchange_bw))

resistrecover_ED_bw$percentchange_avgbw2 <- log(resistrecover_ED_bw$percentchange_avgbw)
resistrecover_ED_bw$percentchange_bw2 <- log(resistrecover_ED_bw$percentchange_bw)
resistrecover_ED_bw$ed30_2 <- resistrecover_ED_bw$ed30^2

ggplot(resistrecover_ED_bw, aes(x=percentchange_avgbw2)) +
  geom_histogram() +
  theme_bw()

ggdensity(resistrecover_ED_bw$percentchange_avgbw2, main="Density plot of bw", xlab="buoyant weight")
ggqqplot(resistrecover_ED_bw$percentchange_avgbw2)
skewness(resistrecover_ED_bw$percentchange_avgbw2, na.rm=TRUE)

#log tranformation
bw_lm <- lm(percentchange_avgbw2 ~ ed30, data=resistrecover_ED_bw)
bw_quad <- lm(percentchange_avgbw2 ~ ed30 + ed30_2, data=resistrecover_ED_bw)
#quadratic fit yields p-value 0.01494 but low R2 = 0.02914

M4C <- ggplot(resistrecover_ED_bw,aes(x=ed30,y=percentchange_avgbw2)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_smooth(method="lm",formula = y ~ x + I(x^2),color="red") +
  labs(x = "Heat Tolerance", y = expression(log*"%"*Delta~"Buoyant Weight")) + 
  annotate("text", x=c(5.65,5.3),y=c(2.15,2.3), label=c("linear p = 0.88","quad p = 0.00028"),size=2.8) +
  theme_bw() + theme(legend.position="none",axis.title = element_text(size = 9)) 

#BW vs. Symbiont clade analysis
##clade vs. BW
M4Daov <- aov(lm(percentchange_bw ~ clade, data = symbiont_growth))
TukeyHSD(M4Daov)
#all are not significant

#pval_CD4 <- c("p = 0.841")
#pval_CM4 <- c("p = 0.621")
#pval_MD4 <- c("p = 0.792")

pval_CD4 <- c("")
pval_CM4 <- c("")
pval_MD4 <- c("")

M4D <- ggplot(symbiont_growth,aes(x=clade,y=percentchange_bw,color=clade)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Dominant Symbiont spp.", y = expression("%"~Delta~Buoyant~Weight)) + 
  annotate("text", x=2, y=40, label=c("all n.s.")) +
  #annotate("text", x=1.5, y=42, label=pval_CM4) +
  #annotate("text", x=2, y=47, label=pval_CD4) +
  #annotate("text", x=2.5, y=52, label=pval_MD4) +
  #annotate("segment", x = 1.1, xend = 1.9, y= 40, yend=40) +
  #annotate("segment", x = 1.1, xend = 2.9, y= 45, yend=45) +
  #annotate("segment", x = 2.1, xend = 2.9, y= 50, yend=50) +
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  scale_color_manual(values = c("darkorange", "black", "blue")) +
  theme_bw() + theme(legend.position="none",axis.title = element_text(size = 9)) 

quartz(w=4,h=4)
plot_grid(M4A,M4B,M4C,M4D, ncol=2, labels = c("A","B","C","D"), align="hv", axis="tb", label_size=12, rel_heights = c(1,1),label_y=(1.01))
quartz.save("./output/Fig4_final.pdf", type="pdf")


#####Supplementary Figures#####
#####Supp Figure 1#####
A1_growth_c4<-read_csv("./data/controller04_growth.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

A1_growth_c4_avg <- A1_growth_c4 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Growth Period")) %>%
  distinct()

A2_growth_c6<-read_csv("./data/controller06_growth.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

A2_growth_c6_avg <- A2_growth_c6 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Growth Period")) %>%
  distinct()

A3_growth_c10<-read_csv("./data/controller10_growth.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

A3_growth_c10_avg <- A3_growth_c10 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Growth Period")) %>%
  distinct()

A1_rec_c8<-read_csv("./data/controller08_rec.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

A1_rec_c8_avg <- A1_rec_c8 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Recovery Period")) %>%
  distinct()

A2_rec_c21<-read_csv("./data/controller21_rec.csv")%>%
  separate(PV,into=c("temp","trash"),sep=" ")%>%select(-trash) %>%
  select(Time,temp) %>%
  mutate(temp=as.numeric(temp)) %>%
  mutate(time=mdy_hm(Time)) %>%
  select(time,temp) %>%
  mutate(time=as.Date(time)) %>%
  na.omit()

A2_rec_c21_avg <- A2_rec_c21 %>%
  mutate(day = floor_date(time, "day")) %>%
  group_by(day) %>%
  mutate(day_avg = mean(temp)) %>%
  select(day,day_avg) %>%
  mutate(time=day) %>%
  mutate(temp=day_avg) %>%
  select(time,temp) %>%
  add_column(group= c("Recovery Period")) %>%
  distinct()

#Light Logger Data
A1_growth_light<-read_csv("./data/12070_120_growth.csv")%>%
  mutate(datetime=dmy_hms(paste(date,time))) %>%
  select(datetime,calibrated) %>%
  mutate(calibrated=as.numeric(calibrated)) %>%
  mutate(datetime=as.POSIXct(datetime)) %>%
  mutate(day = floor_date(datetime, "day")) %>%
  add_column(group= c("Growth Period")) %>%
  add_column(coral= c("Adults")) %>%
  na.omit()

mean(A1_growth_light$calibrated[A1_growth_light$calibrated > 0])
max(A1_growth_light$calibrated)

A2_growth_light<-read_csv("./data/11848_118_growth.csv")%>%
  mutate(datetime=dmy_hms(paste(date,time))) %>%
  select(datetime,calibrated) %>%
  mutate(calibrated=as.numeric(calibrated)) %>%
  mutate(datetime=as.POSIXct(datetime)) %>%
  mutate(day = floor_date(datetime, "day")) %>%
  add_column(group= c("Growth Period")) %>%
  add_column(coral= c("Adults")) %>%
  na.omit()

mean(A2_growth_light$calibrated[A2_growth_light$calibrated > 0])
max(A2_growth_light$calibrated)

A1_niarox_light<-read_csv("./data/11848_118_NIAROX.csv")%>%
  mutate(datetime=dmy_hms(paste(date,time))) %>%
  select(datetime,calibrated) %>%
  mutate(calibrated=as.numeric(calibrated)) %>%
  mutate(datetime=as.POSIXct(datetime)) %>%
  mutate(day = floor_date(datetime, "day")) %>%
  add_column(group= c("Heat Stress Period")) %>%
  add_column(coral= c("Adults")) %>%
  na.omit()

mean(A1_niarox_light$calibrated[A1_niarox_light$calibrated > 0])
max(A1_niarox_light$calibrated)

A2_niarox_light<-read_csv("./data/12070_120_NIAROX.csv")%>%
  mutate(datetime=dmy_hms(paste(date,time))) %>%
  select(datetime,calibrated) %>%
  mutate(calibrated=as.numeric(calibrated)) %>%
  mutate(datetime=as.POSIXct(datetime)) %>%
  mutate(day = floor_date(datetime, "day")) %>%
  add_column(group= c("Heat Stress Period")) %>%
  add_column(coral= c("Adults")) %>%
  na.omit()

mean(A2_niarox_light$calibrated[A2_niarox_light$calibrated > 0])
max(A2_niarox_light$calibrated)


#Experimental tank system conditions
#S1A: temperature profiles during growth and heat stress recovery experiments
S1A_dates <- as.Date(c(A1_growth_c4_avg$time,A1_rec_c8_avg$time))
S1A_time <- data.frame(S1A_time=c(A1_growth_c4_avg$time,A2_growth_c6_avg$time,A3_growth_c10_avg$time,A1_rec_c8_avg$time,A2_rec_c21_avg$time))
S1A_temp <- data.frame(S1A_temp=c(A1_growth_c4_avg$temp,A2_growth_c6_avg$temp,A3_growth_c10_avg$temp,A1_rec_c8_avg$temp,A2_rec_c21_avg$temp))
S1A_group <- data.frame(S1A_group=c(A1_growth_c4_avg$group,A2_growth_c6_avg$group,A3_growth_c10_avg$group,A1_rec_c8_avg$group,A2_rec_c21_avg$group))

S1A_dataset <- data.frame(S1A_time,S1A_temp,S1A_group)

S1A <- ggplot(S1A_dataset, aes(x=S1A_time,y=S1A_temp,color=S1A_group)) +
  geom_point() +
  geom_line() +
  scale_x_bd(business.dates = S1A_dates, max.major.breaks = 5, labels = scales::date_format("%m/%d/%y")) +
  labs(x=expression("Growth and Heat Recovey Timeline"), y=expression("Temperature "(degree*C))) +
  scale_color_manual(values = c("black","blue")) +
  theme_bw() + theme(axis.title.x=element_blank(), legend.title = element_blank(),
                     legend.position = c(.99, .99),
                     legend.background = element_blank(),
                     legend.justification = c("right", "top"),
                     legend.box.just = "right",
                     legend.margin = margin(6, 6, 6, 6))

#S1B: light logger profiles during growth period and heat stress experiment period for adults
S1B_dates <- as.Date(c(A1_growth_light$day,A1_niarox_light$day))
S1B_datetime <- data.frame(S1B_datetime=c(A1_growth_light$datetime,A2_growth_light$datetime,A1_niarox_light$datetime,A2_niarox_light$datetime))
S1B_day <- data.frame(S1B_day=c(A1_growth_light$day,A2_growth_light$day,A1_niarox_light$day,A2_niarox_light$day))
S1B_calibrated <- data.frame(S1B_calibrated=c(A1_growth_light$calibrated,A2_growth_light$calibrated,A1_niarox_light$calibrated,A2_niarox_light$calibrated))
S1B_group <- data.frame(S1B_group=c(A1_growth_light$group,A2_growth_light$group,A1_niarox_light$group,A2_niarox_light$group))
S1B_coral <- data.frame(S1B_coral=c(A1_growth_light$coral,A2_growth_light$coral,A1_niarox_light$coral,A2_niarox_light$coral))

S1B_dataset <- data.frame(S1B_datetime,S1B_day,S1B_calibrated,S1B_group,S1B_coral)


S1B <- ggplot(S1B_dataset, aes(x=S1B_day,y=S1B_calibrated,color=S1B_coral)) +
  geom_point(alpha=0.5) +
  geom_line(alpha=0.5) +
  labs(x="", y=expression(PAR~Light~mu*mol~m^{"−2"}~s^{"−1"})) +
  geom_vline(xintercept = as.POSIXct(as.Date("2023-01-24")), linetype = "dotted", color="black") +
  geom_vline(xintercept = as.POSIXct(as.Date("2023-02-06")), linetype = "dotted", color="black") +
  scale_x_bd(business.dates = S1B_dates, max.major.breaks = 5, labels = scales::date_format("%m/%d/%y")) +
  theme_bw() + theme(axis.title.x=element_blank(), legend.title = element_blank(),
                     legend.position = "none")

quartz(w=7.2,h=4.2)
plot_grid(S1A,S1B, labels=c("A","B"), rel_heights = c(1,1.2), label_size=12,nrow=2, axis="tblr", align="v")
quartz.save("./output/FigS1.pdf", type="pdf")

#####Supp Figure 2#####
#Adult fragment growth information
#Fig. S2A: percent change surface area of adult ramets
S2A_main <- ggplot(resistrecover_ED_sa, aes(x=percentchange_avgsa)) +
  geom_histogram() +
  labs(x = Delta~"Percent Surface Area", y = expression("Adult Ramet Count")) + 
  theme_bw() + theme(legend.position="none") 

S2A_inset <- ggplot(resistrecover_ED_sa, aes(x=percentchange_avgsa2)) +
  geom_histogram() +
  labs(x = "SA log", y=expression("")) + 
  theme_bw() + theme(legend.position="none") 

S2A <- ggdraw() +
  draw_plot(S2A_main) +
  draw_plot(S2A_inset, x=.55,y=.62,width=.4,height=.35)

#Fig. S2B: percent change surface area of adult genets
S2B <- ggplot(resistrecover_ED_sa, aes(x=reorder(genet,percentchange_sa2),y=percentchange_sa2)) +
  geom_line() +
  stat_summary(geom="point",fun="mean",size=2) +
  labs(x = "Genet", y=expression(log*"%"*Delta~Surface~Area)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() + theme(axis.text.x=element_text(angle=90,vjust=1,hjust=1),legend.position="none") 

adult_sa_aov <- aov(percentchange_avgsa2~genet, data=resistrecover_ED_sa)
summary(adult_sa_aov)

#Fig. S2C: percent change buoyant weight ramets

S2C_main <- ggplot(resistrecover_ED_bw, aes(x=percentchange_avgbw)) +
  geom_histogram() +
  labs(x = Delta~"Percent Buoyant Weight", y = expression("Adult Ramet Count")) + 
  theme_bw() + theme(legend.position="none") 

S2C_inset <- ggplot(resistrecover_ED_bw, aes(x=percentchange_avgbw2)) +
  geom_histogram() +
  labs(x = "BW log", y=expression("")) + 
  theme_bw() + theme(legend.position="none") 

S2C <- ggdraw() +
  draw_plot(S2C_main) +
  draw_plot(S2C_inset, x=.55,y=.62,width=.4,height=.35)

#Fig. S2D: percent change buoyant weight of adult genets

S2D <- ggplot(resistrecover_ED_bw, aes(x=reorder(genet,percentchange_bw2),y=percentchange_bw2)) +
  geom_line() +
  stat_summary(geom="point",fun="mean",size=2) +
  labs(x = "Genet", y=expression(log*"%"*Delta~Buoyant~Weight)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() + theme(axis.text.x=element_text(angle=90,vjust=1,hjust=1),legend.position="none") 

adult_bw_aov <- aov(percentchange_avgbw2~genet, data=resistrecover_ED_bw)
summary(adult_bw_aov)

quartz(w=8,h=6)
FigS2_AB <- plot_grid(S2A,S2B, labels=c("A","B"), rel_widths = c(1,1.2), ncol=2)
FigS2_CD <- plot_grid(S2C,S2D, labels=c("C","D"), rel_widths = c(1,1.2), ncol=2)
plot_grid(FigS2_AB,FigS2_CD, labels=c("",""),nrow=2, align="hv", axis="tb")
quartz.save("./output/FigS2.pdf", type="pdf")

#####Supp Figure 3#####
#Adult ED30 curves by each genet
S3 <- ggplot() +
  geom_line(data=out,aes(x=cumulative,y=predicted))+
  geom_hline(yintercept=.7, linetype="dotted") +
  facet_wrap(~genet) +
  labs(x = "Degree Heating Weeks (DHW)", y = expression("Genet Relative Fv/Fm")) + 
  theme_bw() + theme(legend.position="none") 

quartz(w=5,h=6)
plot_grid(S3)
quartz.save("./output/FigS3.pdf", type = "pdf")


#####Supp Figure 4#####
#Recovery/Mortality by Dominant Symbiont spp.
sym_analysis_rec80 <- matrix(c(19,9,13,5,75,36),ncol=2,nrow=3,dimnames=list(c("C spp.", "Mixed", "D spp."),c("Dead","Alive")),byrow=TRUE)
fisher.test(sym_analysis_rec80)
pairwise_fisher_test(as.matrix(sym_analysis_rec80))

sym_analysis_rec7 <- matrix(c(16,12,12,6,69,42),ncol=2,nrow=3,dimnames=list(c("C spp.", "Mixed", "D spp."),c("Dead","Alive")),byrow=TRUE)
fisher.test(sym_analysis_rec7)
pairwise_fisher_test(as.matrix(sym_analysis_rec7))

tmp2 <- melt(sym_analysis_rec80)
names(tmp2) <- c("Symbio", "Mortality", "Ramets")
tmp2 <- tmp2 %>%
  mutate(Mortality = as.factor(Mortality)) %>%
  mutate(Symbio = as.factor(Symbio))

pval_CD_rec <- c("p = 1")
pval_CM_rec <- c("p = 1")
pval_MD_rec <- c("p = 0.79")

S4 <- ggplot(tmp2, aes(x=Symbio, y=Ramets, fill=Mortality)) +
  geom_bar(stat="identity", position="stack", color="black") +
  scale_fill_manual(values=c("orange4","seagreen4")) +
  annotate("text", x=1, y=48, label=c("all n.s."), size=3) +
  annotate("text", x = 1.48, y= 120, label=c("Recovery Day 80"), size=3) +
  labs(x = "", y = "Ramet #") + 
  scale_x_discrete(labels=c('C', 'Mixed', 'D')) +
  theme_bw() + theme(axis.title = element_text(size=10),
                     legend.title = element_blank(),legend.background = element_blank(), 
                     legend.position = c(.01, .99),legend.justification = c("left", "top"),
                     legend.key.size = unit(2,"mm"),legend.box.just = "left",legend.margin = margin(6, 6, 6, 6))

quartz(w=4,h=4)
plot_grid(S4)
quartz.save("./output/FigS4.pdf",type="pdf")
