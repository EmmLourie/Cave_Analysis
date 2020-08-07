# Upload packages:
rm(list=ls())
library("asnipe") # many things damien is doing
library('igraph') 
library('assortnet') # for assortativity (Damien)  
library('sna')
library(dplyr)
library(tidyverse)
library(vegan)
options(digits = 12)
source('Caves_Null_Modeling.R')

path<-"C:/Bat data/networks2018/"
bat_trees_raw<-read_csv(paste0(path,"net_raw_all_bats_long_20200322.csv"))
bat_trees_raw<-bat_trees_raw[,-c(1:2)]
bat_trees_raw$TAG_night<-paste0(bat_trees_raw$TAG,"_",bat_trees_raw$night_number)
str(bat_trees_raw)

trees<-read_csv(paste0(path,"Tree_Detailes20191703.csv"))
trees<-trees[,-1]

caves_ID<-c(1:11)

# example tag to follow:
tag_4579<-subset(bat_trees_raw, TAG==4579)

# Add cave (of exit) membership to each bat-night: ####
bat_trees_raw$cave_origin<-rep(NA,nrow(bat_trees_raw))
for (tg_nt in unique(bat_trees_raw$TAG_night)){
inx<-which(bat_trees_raw$TAG_night == tg_nt)  
tag_night<-bat_trees_raw[inx,]
tag_night<-tag_night[order(tag_night$start),]
caves<-as.character(unique(tag_night$tree_sp[which(tag_night$cluster_ID %in% caves_ID)]))
cave_select<-ifelse(length(caves)==0,NA, caves[1])
bat_trees_raw$cave_origin[inx]<-rep(cave_select,nrow(bat_trees_raw[inx,]))
}
bat_trees_raw$year<-lubridate::year(bat_trees_raw$date)

# choose examples to check
#tag_4579_3<-subset(bat_trees_raw, TAG_night=="4579_3")
#tag_5660_6<-subset(bat_trees_raw, TAG_night=="5660_6")
#tag_5660_5<-subset(bat_trees_raw, TAG_night=="5660_5")
#tag_5680_3<-subset(bat_trees_raw, TAG_night=="5680_3")


# Have a look at the trees used by bats of different caves

# Starting with one month:
#June_18<-bat_trees_raw[which(bat_trees_raw$month==6 & bat_trees_raw$year==2018),]
#June_19<-bat_trees_raw[which(bat_trees_raw$month==6 & bat_trees_raw$year==2019),]
#
#table(June_18$cave_origin,June_18$TAG)
#table(June_19$cave_origin,June_19$TAG)

#write.csv(June_18, "Bats_of_June_18.csv")
#write.csv(June_19, "Bats_of_June_19.csv")


# Get per-month overlap:

# First give all the same date
bat_trees_raw$date_global<-rep(NA,nrow(bat_trees_raw))
TAG_List<-unique(bat_trees_raw$TAG)
for (tg in TAG_List){
  tg.ix<-which(bat_trees_raw$TAG==tg)
  nnights<-unique(bat_trees_raw$night_number[tg.ix])
  for (ngt in 1:length(nnights)){
    nt.ix<-which(bat_trees_raw$night_number==nnights[ngt] & bat_trees_raw$TAG==tg)
    date_global<-as.Date(min(bat_trees_raw$date[nt.ix]))
    if (ngt >1){
      date_global<-date_global+1}
    bat_trees_raw$date_global[nt.ix]<-rep(as.character(date_global), length(nt.ix))
  }
}


# for (date in unique(bat_trees_raw$date_global)){
#   night<-bat_trees_raw[bat_trees_raw$date_global==date,]
# print(length(unique(night$TAG)))
# }


bat_trees_raw$month_year<-paste0(bat_trees_raw$month,"_",bat_trees_raw$year)
how_many_tags<-aggregate(bat_trees_raw$TAG, list(bat_trees_raw$month_year),function(x) length(unique(x)))
colnames(how_many_tags)<-c("Month_y","Count")
how_many_caves<-aggregate(bat_trees_raw$cave_origin, list(bat_trees_raw$month_year),function(x) length(unique(x)))
colnames(how_many_caves)<-c("Month_y","Count")

# First, remove month that have only 1 cave:
keep_months1<-how_many_caves$Month_y[how_many_caves$Count>1]

# then also months that have less than 4 bats:
keep_months2<-how_many_tags$Month_y[how_many_tags$Count>4]


keep_months<-intersect(keep_months1,keep_months2)




# Use only data of bats from two largest caves:
caves<-c("Har Gershom","Zemer cave")
caves_df<-bat_trees_raw[bat_trees_raw$cave_origin %in% caves,]
caves_id<-c(1:10)
# Take out caves:
caves_df<-caves_df[!caves_df$cluster_ID %in% caves_id,]
caves_df<-caves_df[caves_df$month_year %in% keep_months,]


rm(night,how_many_caves,how_many_tags,tag_night) # remove unused dataframes
rm(cave_select, caves, caves_ID,date,date_global,inx,keep_months, keep_months1, keep_months2, ngt, nnights, nt.ix,path,tg,tg_nt,tg.ix)



obs_overlap_df<-cave_overlap(caves_df)
obs_overlap__all_years_df<-cave_overlap_all_years (caves_df)


#Permutate the cave membership and re-run the overlap anlysis: ####
 

P<-10000
overlap_per_df<-NULL 
avg_overlap_df<-NULL
for (i in 1:P){
  caves_df2<-NULL
  for (my in unique(caves_df$month_year)){
    my_df<-subset(caves_df,month_year==my)
    prob_gershom<-length(unique(my_df$TAG_Night[my_df$cave_origin=="Har Gershom"]))/length(unique(my_df$TAG_Night))
    prob_zemer<-length(unique(my_df$TAG_Night[my_df$cave_origin=="Zemer cave"]))/length(unique(my_df$TAG_Night))
    
    probs<-c(prob_gershom,prob_zemer)
    rand_my_df<-my_df%>%
                group_by(cave_origin, TAG_Night)%>%
                summarise("random_cave"=sample(c("Har Gershom","Zemer cave"),1,replace=F, prob = probs))
    rand_my_full<-left_join(my_df,rand_my_df,by=c("TAG_Night","cave_origin"))
    #print(paste0("caves ration after perm:",sum(table(rand_my_full$cave_origin)-table(rand_my_full$random_cave))))
    caves_df2<-rbind(caves_df2,rand_my_full)
    }
  
  
  caves_df2<-caves_df2[, -which(names(caves_df2) %in% c("cave_origin"))]
  colnames(caves_df2)<-gsub("random_cave", "cave_origin",  colnames(caves_df2))
  
  tmp_df<-cave_overlap(caves_df2)
  mean_overlap<-mean(as.double(tmp_df$Overlap_caves), na.rm=T)
  #print(head(tmp_df),n=10)


  overlap_per_df<-rbind(overlap_per_df,tmp_df)
  avg_overlap_df<-cbind(avg_overlap_df,mean_overlap)
}

# For month (all years)
overlap_per_df_all_years<-NULL 
for (i in 1:P){
  caves_df2<-NULL
  for (m in unique(caves_df$month)){
    m_df<-subset(caves_df,month==m)
    prob_gershom<-length(unique( m_df$TAG_Night[ m_df$cave_origin=="Har Gershom"]))/length(unique( m_df$TAG_Night))
    prob_zemer<-length(unique( m_df$TAG_Night[ m_df$cave_origin=="Zemer cave"]))/length(unique( m_df$TAG_Night))
    
    probs<-c(prob_gershom,prob_zemer)
    rand_m_df<- m_df%>%
      group_by(cave_origin, TAG_Night)%>%
      summarise("random_cave"=sample(c("Har Gershom","Zemer cave"),1,replace=F, prob = probs))
    rand_m_full<-left_join(m_df,rand_m_df,by=c("TAG_Night","cave_origin"))
    #print(paste0("caves ration after perm:",sum(table(rand_my_full$cave_origin)-table(rand_my_full$random_cave))))
    caves_df2<-rbind(caves_df2,rand_m_full)
  }
  
  
  caves_df2<-caves_df2[, -which(names(caves_df2) %in% c("cave_origin"))]
  colnames(caves_df2)<-gsub("random_cave", "cave_origin",  colnames(caves_df2))
  
  tmp_df<-cave_overlap_all_years(caves_df2)
  #print(head(tmp_df),n=10)
  
  
  overlap_per_df_all_years<-rbind(overlap_per_df_all_years,tmp_df)
}


# I saved the perumtated results (long time to calculate)

# Load permutated data:

overlap_per_df<-read.csv("permurated_overlap_per_months.csv")
avg_overlap_df<-read.csv("permurated_overlap_means.csv")

# Plot results per year_month (table and histograms):
par(mfrow=c(2,2))
my_list<-unique(overlap_per_df$Month_Year)
results<-data.frame("Month_Year"=rep(NA,length(my_list)),"Observed_Overlap"=rep(NA,length(my_list)),"Mean_Random_Overlap"=rep(NA,length(my_list)),"Number of bats_cave"=rep(NA,length(my_list)),"evenness"=rep(NA,length(my_list)),"P_value"=rep(NA,length(my_list)))
for (my in 1:length(my_list)){
  obs<-obs_overlap_df$Overlap_caves[obs_overlap_df$Month_Year==my_list[my]]
  obs<-as.double(obs)
  if (length(obs)>0){
  rand<-overlap_per_df$Overlap_caves[overlap_per_df$Month_Year==my_list[my]]
  rand<-as.double(rand)
     
# p-value:
Pvalue<- (1+sum(obs>=rand))/(P+1)# the sum of an equation is the number they are TRUE... 
gershom_zemer_bats<-paste0(obs_overlap_df$Gershom[obs_overlap_df$Month_Year==my_list[my]],"_",obs_overlap_df$Zemer[obs_overlap_df$Month_Year==my_list[my]])
evenness<-max(c(as.numeric(obs_overlap_df$Zemer[obs_overlap_df$Month_Year==my_list[my]]),as.numeric(obs_overlap_df$Gershom[obs_overlap_df$Month_Year==my_list[my]])))/(as.numeric(obs_overlap_df$Zemer[obs_overlap_df$Month_Year==my_list[my]])+as.numeric(obs_overlap_df$Gershom[obs_overlap_df$Month_Year==my_list[my]]))
hist(rand, breaks=50, col="lightgray",xlim=c(0,0.1), main=paste0("Month_Year=",my_list[my],"# 0f bats=",gershom_zemer_bats))
abline(v=obs, col="red")   
results[my,]<-cbind(as.character(my_list[my]),obs,mean(rand),gershom_zemer_bats,evenness,Pvalue)
}}
results<-na.omit(results)

write.csv(results, "results/Cave_Overlap_null_month_year_results.csv")

# Plot results per month (table and histograms):
par(mfrow=c(3,3))
m_list<-unique(overlap_per_df_all_years$Month)
results<-data.frame("Month"=rep(NA,length(m_list)),"Observed_Overlap"=rep(NA,length(m_list)),"Mean_Random_Overlap"=rep(NA,length(m_list)),"Number of bats_cave"=rep(NA,length(m_list)),"evenness"=rep(NA,length(m_list)),"P_value"=rep(NA,length(m_list)))
for (m in 1:length(m_list)){
  obs<-obs_overlap__all_years_df$Overlap_caves[obs_overlap__all_years_df$Month==m_list[m]]
  obs<-as.double(obs)
  if (length(obs)>0){
    rand<-overlap_per_df_all_years$Overlap_caves[overlap_per_df_all_years$Month==m_list[m]]
    rand<-as.double(rand)
    
    # p-value:
    Pvalue<- (1+sum(obs>=rand))/(P+1)# the sum of an equation is the number they are TRUE... 
    gershom_zemer_bats<-paste0(obs_overlap__all_years_df$Gershom[obs_overlap__all_years_df$Month==m_list[m]],"_",obs_overlap__all_years_df$Zemer[obs_overlap__all_years_df$Month==m_list[m]])
    evenness<-max(c(as.numeric(obs_overlap__all_years_df$Zemer[obs_overlap__all_years_df$Month==m_list[m]]),as.numeric(obs_overlap__all_years_df$Gershom[obs_overlap__all_years_df$Month==m_list[m]])))/(as.numeric(obs_overlap__all_years_df$Zemer[obs_overlap__all_years_df$Month==m_list[m]])+as.numeric(obs_overlap__all_years_df$Gershom[obs_overlap__all_years_df$Month==m_list[m]]))
    results[m,]<-cbind(m_list[m],obs,mean(rand),gershom_zemer_bats,evenness,Pvalue)
    if (evenness<0.8){
    hist(rand, breaks=50, col="lightgray",xlim=c(0,0.1), main=paste0("Month ="," ",m_list[m],"; evenness= ",round(evenness,digits=2)))
    abline(v=obs, col="red")   
  }}}
results<-na.omit(results)

write.csv(results, "results/Cave_Overlap_null_month_results (all_years).csv")


# averaged months results:
par(mfrow=c(1,1))
avg_overlap_df<-as.numeric(avg_overlap_df[1,])
Pvalue<- (1+sum(mean_obs>=as.double(avg_overlap_df)))/(P+1)
hist(avg_overlap_df,breaks=1000, col="lightgray", xlim=c(0,0.1), main="Low overlap between the two large caves (p<0.001)", xlab = "Proportion of Cave Overlap", ylab="Frequency (permutations)")
mean_obs<-mean(as.double(obs_overlap_df$Overlap_caves))
abline(v=mean_obs, col="red")

### Now the same proccess, onlt using the area-covered instead of the tree overlap:

# get area covered in a week and mean distance between trees used
net_all$TAG_week<-paste(net_all$TAG,"_",net_all$week_num,sep="")
weekl<-unique(net_all$TAG_week)
library("dismo")# to compute convex hull
TAG_week_dist_area<-NULL
for(w in weekl) {
  TAG_week<-subset(net_all, TAG_week==w)
  mean_dist<-mean(dist(TAG_week[,c("X_tree", "Y_tree")]))
  # convex hull
  hpts<-chull(TAG_week$X_tree, TAG_week$Y_tree)
  hpts <- c(hpts, hpts[1])
  xy.coords <- cbind(TAG_week$X_tree, TAG_week$Y_tree)
  chull.coords <- xy.coords[hpts,]
  chull.poly <- Polygon(chull.coords, hole=F)
  area <- chull.poly@area
  dist_area<-as.data.frame(cbind(TAG_week$TAG[1], TAG_week$TAG_week[1],mean_dist, area))
  TAG_week_dist_area<-rbind(TAG_week_dist_area, dist_area)
}
colnames(TAG_week_dist_area)<-c("TAG", "TAG_week", "mean_dist", "area")

night_time<-join_all(list(net_all_ind,mean_time_tree,forage_time_sub,sum_time_tree,dist_roost,number_species), by="TAG_Night", type='left')
night_time$prop_forage<-night_time$total_duration_trees/night_time$total_forage_time


# Identify cave switching events:

bat_trees_raw$cave_ID<-as.factor(bat_trees_raw$cave_origin)
# take care of NAs'

bat_trees_raw<-bat_trees_raw %>% arrange(TAG, start) %>% 
  group_by(TAG) %>% 
  mutate(cave_switch = c(NA,diff(cave_ID)))

bat_trees_raw$cave_switch[is.na(bat_trees_raw$cave_switch)]<-0


Tag_Nights_of_switch<-bat_trees_raw[which(bat_trees_raw$cave_switch!=0), c("TAG","TAG_Night")]
table(Tag_Nights_of_switch$TAG) 

unique(Tag_Nights_of_switch$TAG_Night)
# (check in Kanmdata thoes tag)

# 1. what happens to the tree-set of a bat that switched caves?

# example: 
tmp<-"5680_11"




# 2. what happens to resident bats following an outsider bat visit? 


### Create movement networks to compare degree distributions of shared trees between caves:




# A. Create a network per night of all bats ####
# Need to include cluster_Id, X,Y, caveId,night number, date, TAGs, number of bats of this cave (unrelated to cluster_ID)


# A1. create netwroks per night


Edge_List<-NULL
gnights<-unique(bat_trees_raw$date_global)
for (gngt in gnights){
  night_df<-subset(bat_trees_raw, date_global==gngt)
  TAG_night_list<- unique(night_df$TAG)
  for (tgn in  TAG_night_list){
    tg.nt.ix<-which(night_df$TAG==tgn)
    night.tg.df<-night_df[tg.nt.ix,]
    night.tg.df<-night.tg.df[order(night.tg.df$time_start),]
    # create edge list
    LOC <- as.factor(night.tg.df$cluster_ID)
    dLOC <- c(0, diff(LOC))
    MOVE <- (dLOC!=0)
    # initialise empty vectors
    A <- rep(NA, length(MOVE))
    B <- rep(NA, length(MOVE))
    
    A[MOVE] <- night.tg.df$cluster_ID[(which(MOVE)-1)] #Nodes of Origin
    B[MOVE] <- night.tg.df$cluster_ID[MOVE]#Target Nodes
    
    # Construct edge_list per night 
    AB <- data.frame(From=A,To=B)
    AB <- na.omit(AB)
    AB$Capture<-rep(night.tg.df$Capture[1], nrow(AB))
    AB$TAG<-rep(night.tg.df$TAG[1],nrow(AB))
    AB$date<-rep(gngt,nrow(AB))
    AB$Night<-rep(night.tg.df$night_number[1],nrow(AB))
    Edge_List<-rbind(Edge_List,AB)
    }
  }
  

# One month to have a look:
Edge_List$month<-lubridate::month(Edge_List$date)
Edge_List$year<-lubridate::year(Edge_List$date)

table(Edge_List$month, Edge_List$year)

aMonth<-Edge_List[Edge_List$month==10 & Edge_List$year==2019,]
table(aMonth$TAG)

# Add cave identity to each TAG:
tag_5685<-subset(aMonth, TAG=="5685")



night_edge<-subset(Edge_List, date=="2019-08-30")
length(unique(night_edge$TAG))
  G <- graph.data.frame(night_edge[,c("From","To")],directed=FALSE)
  net_adj <- as_adjacency_matrix(G)
  network <- as.matrix(net_adj)
  net <- graph.adjacency(net_adj , mode = "undirected", diag = FALSE, weighted = TRUE)
  plot(net,vertex.size=3, edge.width=E(net)$weight)
``


