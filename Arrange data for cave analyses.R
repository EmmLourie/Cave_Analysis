rm(list=ls())
library(tidyverse)
source("Functions/dist_to_focal.R")
# Data:

bat_trees_raw<-read_csv("data/Stops_all_bats_LONG_DURATION2020-07-17.csv")
bat_trees_raw$TAG_Night<-paste0(bat_trees_raw$TAG,"_",bat_trees_raw$night_number)
bat_trees_raw<-bat_trees_raw[!duplicated(  bat_trees_raw[,c("TAG", "start")]),]
length(unique(bat_trees_raw$TAG))
#tag_eight<-read.csv("data/Tag_Eight.csv") # If I want to use a subset of good-quality tags the 8-secs are it!


#### Filter data for cave analysis purposes: ####

# add column of total nights tracked:
tag_duration<-as.data.frame(aggregate(bat_trees_raw$night_number,list(bat_trees_raw$TAG),function(x) length(unique(x))))
colnames(tag_duration)<-c("TAG", "Number_of_Nights")
bat_trees_raw<-left_join(bat_trees_raw,tag_duration, by="TAG")

# remove first night (Movements during the first night could be driven by the capture (e.g. in the cave)):
bat_trees_raw<-bat_trees_raw[bat_trees_raw$night_number>1,]
length(unique(bat_trees_raw$TAG)) # 2 tags lost



# Some caves don't have  "X_tree & Y_tree"" data, so for the cases where there is no such data, fill it in with the medX medY information:
indx<-which(bat_trees_raw$dummy=="cave" & is.na(bat_trees_raw$X_tree))
bat_trees_raw$X_tree[indx]<-bat_trees_raw$medX[indx]
bat_trees_raw$Y_tree[indx]<-bat_trees_raw$medY[indx]

# There are repeated false stops that are attributed to Einan, but are actually shoot-offs from Gershom
Einan_false<-c(254425,777107)
dist_einan_false<-250


dummies_Einan<-which(bat_trees_raw$dummy=="cave" & bat_trees_raw$tree_sp=="Einan")
dummies_Einan_df<-bat_trees_raw[dummies_Einan,]
data_no_dEinan<-bat_trees_raw[-dummies_Einan,]
dummies_Einan_df$dist_false<-focal.dist(dummies_Einan_df$medX, dummies_Einan_df$medY, Einan_false[1],Einan_false[2])
dummies_Einan_df$tree_sp<-ifelse(dummies_Einan_df$dist_false<=dist_einan_false,"Har Gershom","Einan")
dummies_Einan_df$cluster_ID<-ifelse(dummies_Einan_df$dist_false<=dist_einan_false,1,6)
dummies_Einan_df<-dummies_Einan_df[,-length(dummies_Einan_df)]

bat_trees_raw<-rbind(dummies_Einan_df,data_no_dEinan)

####Add cave (of exit) membership to each bat-night: 

# create cave_id list:
caves_id<-c(1:11)
caves_id_uk<-unique(bat_trees_raw$cluster_ID[grep("^UK_Cave_", bat_trees_raw$cluster_ID)])
caves_id<-c(caves_id,caves_id_uk)


bat_trees_raw$cave_origin<-rep(NA,nrow(bat_trees_raw))

for (tg_nt in unique(bat_trees_raw$TAG_Night)){
  inx<-which(bat_trees_raw$TAG_Night == tg_nt)  
  TAG_Night<-bat_trees_raw[inx,]
  TAG_Night<-TAG_Night[order(TAG_Night$start),]
  caves<-as.character(unique(TAG_Night$tree_sp[which(TAG_Night$cluster_ID %in% caves_id & TAG_Night$time_start<hms::as.hms("20:00:00") & TAG_Night$time_start>hms::as.hms("13:00:00") )]))
  if(length(caves)>0){
    if (caves[1]=="Uknown_cave"){
      prev_night<-(as.double(str_split(tg_nt, "_")[[1]][2])-1)
      tg<-str_split(tg_nt, "_")[[1]][1]
      pre_tag_night<-paste0(tg,"_",prev_night)
      prev_df<-bat_trees_raw[which(bat_trees_raw$TAG_Night == pre_tag_night & bat_trees_raw$dummy=="cave"),]
      if(nrow(prev_df)>0){
        caves<-prev_df$tree_sp[prev_df$start==max(prev_df$start)]
      }}}
  cave_select<-ifelse(length(caves)==0,NA, caves[1])
  bat_trees_raw$cave_origin[inx]<-rep(cave_select,nrow(bat_trees_raw[inx,]))
}

table(bat_trees_raw$cave_origin)
bat_trees_raw$TAG<-as.character(bat_trees_raw$TAG)
aggregate(bat_trees_raw$TAG, list(bat_trees_raw$cave_origin), function(x) length(unique(x)))

# Check:#####

# how many cave origin are NAs?
bat_Trees_unique<-bat_trees_raw[!duplicated(bat_trees_raw$TAG_Night),]
sum(is.na(bat_Trees_unique$cave_origin))/length(bat_Trees_unique$cave_origin)

length(unique(bat_trees_raw$TAG)) 
tg_ex<-subset(bat_trees_raw, TAG=="5777")
tg_ex<-tg_ex[!duplicated(tg_ex$night_number),]

# UK caves:
uk_caves_df<-bat_trees_raw[bat_trees_raw$cave_origin=="Uknown_cave" & bat_trees_raw$dummy=="cave" ,]
uk_caves_df<-uk_caves_df[uk_caves_df$time_start<hms::as.hms("20:00:00") & uk_caves_df$time_start>hms::as.hms("12:00:00") & !is.na(uk_caves_df$cave_origin),]
length(unique(uk_caves_df$TAG))


# Kedesh visits are less affirmative. Plus, I visited it many times during 2019-20 and didn't see bats there. Hence I will replace Kedesh with Zemer for thoes year.

bat_trees_raw$cave_origin[which(bat_trees_raw$cave_origin=="Kedesh" & lubridate::year(bat_trees_raw$date)>2018)]<-"Zemer cave"


# remove caves (cave origin remains):

bat_trees_raw<-bat_trees_raw[!bat_trees_raw$cluster_ID %in% caves_id,]
bat_trees_raw<-bat_trees_raw[,-c(1:2)]
write.csv(bat_trees_raw, "data/stops_for_caves.csv",row.names =F)
