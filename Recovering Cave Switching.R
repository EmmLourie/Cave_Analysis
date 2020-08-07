
library(tidyverse)
# functions:
source("Functions/Switching function.R")

# Data:
bat_trees_raw<-read_csv("data/stops_for_caves.csv")
bat_trees_raw<-bat_trees_raw[!duplicated(  bat_trees_raw[,c("TAG", "start")]),]

#Take tags that have less than 3 nights out, since it is not enough time to observe changes in the tree set after switch (or, when there wasn't a switch, to create a random distribution of tree set overlap).
length(unique(bat_trees_raw$TAG))
tag_short<-bat_trees_raw$TAG[bat_trees_raw$Number_of_Nights<3]
length(unique(tag_short))
bat_trees_raw<-bat_trees_raw[!bat_trees_raw$TAG %in% tag_short,]
length(unique(bat_trees_raw$TAG))
hist(bat_trees_raw$duration, breaks=400,xlim=c(0,50))
bat_trees_raw<-bat_trees_raw[bat_trees_raw$duration>summary(bat_trees_raw$duration)[[2]],] # larger thqn 1st quarantile
summary(bat_trees_raw$duration)
bat_trees_raw<-bat_trees_raw[order(bat_trees_raw$start),]
tg_ex<-subset(bat_trees_raw, TAG=="6271")
tg_ex<-tg_ex[!duplicated(tg_ex$night_number),]
TagList<-unique(bat_trees_raw$TAG)
# Find out how many cave switches in total:
Cave_switches_df<-data.frame("TAG"=rep(NA,length(TagList)), "Number_of_Switches"=rep(NA,length(TagList)), "Nights_Tracked"=rep(NA,length(TagList)),"nights_of_switch"=rep(NA,length(TagList)))
for (tg in TagList){
tg_df<-subset(bat_trees_raw, TAG==tg)
tg_df<-tg_df %>% filter(!is.na(cave_origin))
if(nrow(tg_df)>0){
tg_df<-tg_df[order(tg_df$night_number),]
tg_cave_org<-tg_df[!duplicated(tg_df$night_number),]
tg_cave_org$caves_switch<-c(NA,diff(as.factor(tg_cave_org$cave_origin)))
nnights<-length(unique(tg_cave_org$night_number))
tg_cave_org<-tg_cave_org %>% filter (!is.na(caves_switch))
nswitch<-length(tg_cave_org$caves_switch[tg_cave_org$caves_switch!=0])
nights_of_switch<-toString(tg_cave_org$night_number[tg_cave_org$caves_switch!=0])
tg_switch<-c(tg,nswitch,nnights,c(nights_of_switch))
Cave_switches_df<-rbind(Cave_switches_df,tg_switch)
Cave_switches_df<-Cave_switches_df[Cave_switches_df$Number_of_Switches>0,]
Cave_switches_df$TAG<-as.double(Cave_switches_df$TAG)
}}
Cave_switches_df<-Cave_switches_df[!is.na(Cave_switches_df$TAG),]

# check switches:
tg_ex<-subset(bat_trees_raw, TAG=="284")
tg_ex<-tg_ex[!duplicated(tg_ex$night_number),]

# Conclusion - some Einan cave origins might be false (hard to say). To make sure caves switches are correct, I will only select events were the bat stayed in the cave for at least 2 days in a row so that 1-night mistakes be excluded.
#aa<-ind_switch(bat_trees_raw,"New_Cluster_ID",2)
#compare accumulated trees per night before and after switch.
#tg<-"5745"
switch_all_bats<-data.frame(matrix(nrow = 0.1*nrow(bat_trees_raw), ncol = 8))
colnames(switch_all_bats)<-c("TAG","Night_of_Switch", "From", "To","accumulated_before", "nights_before", "accumulated_after", "nights_after")
for (tg in TagList){
tg_df<-subset(bat_trees_raw, TAG==tg)
if(nrow(tg_df)>0){
tg_df<-tg_df[order(tg_df$night_number),]
tg_df$caves_switch<-c(NA,diff(as.factor(tg_df$cave_origin)))
# To make sure cave-switches are correct, I will use only "stable" cave switches i.e. that after the switch a bat stayed more than a single night in that cave (otherwise maybe an error).
tg_df_unq<-tg_df[!duplicated(tg_df$night_number),]
tg_df_unq<-tg_df_unq[!is.na(tg_df_unq$caves_switch),]
switch_inx<-which(tg_df_unq$caves_switch!=0)
wrong_nights<-NULL
for (s in switch_inx){
cave1<- tg_df_unq$cave_origin[s]
cave2<-tg_df_unq$cave_origin[s+1]
if (cave1!=cave2 & !is.na(cave1) & !is.na(cave2)){
wrong_night<-tg_df_unq$night_number[s]
wrong_nights<-c(wrong_nights,wrong_night)
}}
wrong_nights<-unique(wrong_nights)
tg_df$cave_origin[tg_df$night_number %in% wrong_nights ]<-NA
tg_df$caves_switch<-c(NA,diff(as.factor(tg_df$cave_origin)))
# Now calculate accumulated trees before and after eaxh switch
nights_of_switch<-tg_df$night_number[which(tg_df$caves_switch!=0)]
if(length(nights_of_switch)>0){
tg_df$new_t<-cumsum(!duplicated(tg_df$cluster_ID))
switch_acc_df<-data.frame(matrix(nrow = length(nights_of_switch), ncol = 8))
colnames(switch_acc_df)<-c("TAG","Night_of_Switch", "From", "To","accumulated_before", "nights_before", "accumulated_after", "nights_after")
for (n in 1:(length(nights_of_switch))){
if (n==1){
before_df<-tg_df[tg_df$night_number<nights_of_switch[n],]
}else{
before_df<-tg_df[tg_df$night_number<nights_of_switch[n] & tg_df$night_number>nights_of_switch[n-1],]
}
if (n==length(nights_of_switch)){
after_df<-tg_df[tg_df$night_number>=nights_of_switch[n],]
}else {
after_df<-tg_df[tg_df$night_number>=nights_of_switch[n] & tg_df$night_number<nights_of_switch[n+1],]
}
before_cave<-before_df$cave_origin[nrow(before_df)]
after_cave<-after_df$cave_origin[2]
before_night_length<-length(unique(before_df$night_number))
after_night_length<-length(unique(after_df$night_number))
before_acc_night<-(max(before_df$new_t)-min(before_df$new_t))/before_night_length
after_acc_night<-(max(after_df$new_t)-max(before_df$new_t))/after_night_length
switch<-c(tg, nights_of_switch[n],before_cave,after_cave, before_acc_night,before_night_length,after_acc_night,after_night_length )
switch_acc_df[n,]<-switch
}
switch_all_bats<-rbind(switch_all_bats,switch_acc_df)
}}}
switch_all_bats<-na.omit(switch_all_bats)
switch_all_bats<-switch_all_bats[!duplicated(switch_all_bats[,c("TAG","Night_of_Switch")]),]
switch_all_bats$accumulated_after<-as.double(switch_all_bats$accumulated_after); switch_all_bats$accumulated_before<-as.double(switch_all_bats$accumulated_before)
switch_all_bats$tree_change<-switch_all_bats$accumulated_after-switch_all_bats$accumulated_before
switch_all_bats_long<-switch_all_bats[switch_all_bats$nights_before>2 & switch_all_bats$nights_after > 2,]
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
hist(switch_all_bats$accumulated_before, breaks=10, xlim=c(0,8), col=c1)
hist(switch_all_bats$accumulated_after, breaks=10, xlim=c(0,8),col=c2, add=T)
hist(switch_all_bats$tree_change)
length(unique(bat_trees_raw$TAG_Night))

#write.csv(switch_all_bats, "results/Switching_df_Jul2020.csv")
names(switch_all_bats)
View(switch_all_bats)
