rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
source("functions/model_formula.R")

# Observed bats - accumulated trees ~ time ####

data<-read_csv("data/Stops_all_bats_LONG_DURATION2020-07-26.csv")
data$TAG_Night<-paste0(data$TAG,"_",data$night_number)
data$TAG_Week<-paste0(data$TAG,"_",data$week_num)
TagList<-unique(data$TAG)
data<-data[!duplicated(data[,c("TAG", "start")]),]

caves_id<-c(1:11)
caves_id_uk<-unique(data$cluster_ID[grep("^UK_Cave_", data$cluster_ID)])
caves_id<-c(caves_id,caves_id_uk)
data<-data[!data$cluster_ID %in% caves_id,]

switch_df<-read.csv("results/Switching_df_Jul2020.csv")
switch_df$Tag_Night<-paste0(switch_df$TAG,"_",switch_df$Night_of_Switch)

tg_ex<-subset(data, TAG=="5744")


#Peak_Months<-c(11,12,1,2,5,6)
#data$season_type<-ifelse(data$month %in% Peak_Months, "Peak", "Transition")

# Add information of switching events
sw_nights<-unique(switch_df$Tag_Night)

tag_nights<-unique(data$TAG_Night)
data$switch<-rep(0,nrow(data))
for (tgn in tag_nights){
  tgn_ix<-which(data$TAG_Night==tgn)
  str<-strsplit(tgn, "_")
  tg<-str[[1]][1]; night<-as.double(str[[1]][2])
  for (s in sw_nights){
    str_s<-strsplit(s, "_")
    tg_s<-str_s[[1]][1]; night_s<-as.double(str_s[[1]][2])
    if(tg==tg_s & night<=night_s+7) # if the night is at or within 7 days after the switch - then mark it as 1 
      data$switch[tgn_ix]<-1
  }
}



# 1. Make histogram of tree-use frequency - observed.

TWList<-unique(data$TAG_Week)
data<-data%>% arrange(TAG_Night, start)
tree_use_all<-NULL
for (tw in TWList){
tw_idx<-which(data$TAG_Week==tw)
wd<-data[tw_idx,]
tree_use<-as.data.frame(aggregate(wd$cluster_ID, list(wd$cluster_ID), function(x) length(x)))
if(nrow(tree_use)>0){
colnames(tree_use)<-c("Tree_ID", "Revisits")
tree_use$TAG_Week<-rep(tw,nrow(tree_use))
tree_use$Season<-rep(wd$fruit_season[1], nrow(tree_use))
tree_use$Season_type<-rep(wd$season_type[1], nrow(tree_use))
tree_use$week_prop<-sum(wd$switch)/7
tree_use_all<-rbind(tree_use_all, tree_use)
}}


tree_use_all$switch<-ifelse(tree_use_all$week_prop>0.4,"Switch","No_Switch")

tree_use_all<-tree_use_all[tree_use_all$Revisits<(10*7),] # remove extreme values (less likly for a bat to visit the same tree more than 4 times a night X 7 nights a week)
tree_use_all$log_revisits<-log10(tree_use_all$Revisits)

tree_use_seasons<-NULL
models_results<- data.frame(matrix(nrow=5,ncol=3))
colnames(models_results)<-c("Season","Slope","R_sqr")

for (s in unique(tree_use_all$Season)){
tree_use_season<-tree_use_all[tree_use_all$Season==s,]
tree_use_season$count <- as.numeric(ave( tree_use_season$Revisits,  tree_use_season$Revisits, FUN = length))
tree_use_season$log_count<-log10(tree_use_season$count)
tree_use_season<-na.omit(tree_use_season[!duplicated(tree_use_season$Revisits),])
tree_use_seasons<-rbind(tree_use_seasons,tree_use_season)

season_mod<-lm(tree_use_season$log_count ~ tree_use_season$log_revisits)
R<-round(summary(season_mod)[[8]],digits=2)
slope<-summary(season_mod)$coefficients[2]
models_results[s,]<-c(s,slope,R)
}
models_results<-na.omit(models_results)

tree_use_switching<-NULL
models_results_switch<- data.frame(matrix(nrow=2,ncol=3))
colnames(models_results_switch)<-c("Switch","Slope","R_sqr")

for (sw in unique(tree_use_all$switch)){
  tree_use_switch<-tree_use_all[tree_use_all$switch==sw,]
  tree_use_switch$count <- as.numeric(ave( tree_use_switch$Revisits,  tree_use_switch$Revisits, FUN = length))
  tree_use_switch$log_count<-log10(tree_use_switch$count)
  tree_use_switch<-na.omit(tree_use_switch[!duplicated(tree_use_switch$Revisits),])
  tree_use_switching<-rbind(tree_use_switching,tree_use_switch)
  
  switch_mod<-lm(tree_use_switch$log_count ~ tree_use_switch$log_revisits)
  R<-round(summary(switch_mod)[[8]],digits=2)
  slope<-summary(switch_mod)$coefficients[2]
  models_results_switch[sw,]<-c(sw,slope,R)
}
models_results_switch<-na.omit(models_results_switch)

rm(tree_use_season,wd,R,s,slope,tw,tw_idx,TWList,caves_id_uk,season_mod,tree_use)



tree_use_seasons$Season_order<-factor(tree_use_seasons$Season, levels = c("Washingtonia", "China_Berry", "Mulberry", "Common_Fig", "Transition"))

p<-ggplot(data=tree_use_seasons, aes(x=log_revisits, y=log_count))+geom_point()+ggtitle("Tree Revisits Frequency per Week (190 bats)")+xlab("log (revisits to tree)")+ylab("log (frequency of trees)")
p<-p+facet_wrap(.~Season_order)+geom_smooth(method="lm")+ theme(panel.spacing = unit(2, "lines"))
p+theme_bw()+theme(strip.text = element_text(size = 14))

ps<-ggplot(data=tree_use_switching, aes(x=log_revisits, y=log_count))+geom_point()+ggtitle("Tree Revisits Frequency per Week")+xlab("log (revisits to tree)")+ylab("log (frequency of trees)")
ps<-ps+facet_wrap(.~switch)+geom_smooth(method="lm")+ theme(panel.spacing = unit(2, "lines"))
ps+theme_bw()+theme(strip.text = element_text(size = 14))


q<-ggplot(data=tree_use_all, aes(x=log_revisits, y=log_count))+geom_point()+ggtitle("Tree Revisits Frequency per Week (190 bats)")+xlab("log (revisits to tree)")+ylab("log (frequency of trees)")
q<-q+geom_smooth(method="lm")+ theme(panel.spacing = unit(2, "lines"))
q+theme_bw()


# Same for season type
tree_use_seasons_type<-NULL
models_results_type<- data.frame(matrix(nrow=2,ncol=3))
colnames(models_results_type)<-c("Season_type","Slope","R_sqr")

for (s in unique(tree_use_all$Season_type)){
  tree_use_season<-tree_use_all[tree_use_all$Season_type==s,]
  tree_use_season$count <- as.numeric(ave( tree_use_season$Revisits,  tree_use_season$Revisits, FUN = length))
  tree_use_season$log_count<-log10(tree_use_season$count)
  tree_use_season<-na.omit(tree_use_season[!duplicated(tree_use_season$Revisits),])
  tree_use_seasons_type<-rbind(tree_use_seasons_type,tree_use_season)
  
  season_mod<-lm( tree_use_seasons_type$log_count ~  tree_use_seasons_type$log_revisits)
  R<-round(summary(season_mod)[[8]],digits=2)
  slope<-summary(season_mod)$coefficients[2]
  models_results_type[s,]<-c(s,slope,R)
}
models_results_type<-na.omit( models_results_type)

# All data (all seasons) togehter:
tree_use_all$count <- as.numeric(ave( tree_use_all$Revisits,  tree_use_all$Revisits, FUN = length))
tree_use_all<-tree_use_all[!duplicated(tree_use_all$Revisits),]
tree_use_all$log_count<-log10(tree_use_all$count)
# model
tree_use_mod<-lm(tree_use_all$log_count ~ tree_use_all$log_revisits)
summary(tree_use_mod)
formula(tree_use_mod)


# OR all period but for bats with more than 10 nights of track


data2<-data%>% arrange(TAG_Night, start)
tag_nights<-as.data.frame(aggregate(data2$night_number, list(data2$TAG), function(x) length(unique(x))))
colnames(tag_nights)<-c("TAG", "Total_Nights")
data2<-left_join(data2,tag_nights, by="TAG")
data2<-data2[data2$Total_Nights>9,]

TagList<-unique(data2$TAG)
tree_use_all2<-NULL
for (tg in TagList){
  tg_df<-data2[data2$TAG==tg,]
  tree_use<-as.data.frame(aggregate(tg_df$cluster_ID, list(tg_df$cluster_ID), function(x) length(x)))
  colnames(tree_use)<-c("Tree_ID", "Revisits")
  tree_use$TAG<-rep(tg,nrow(tree_use))
  tree_use_all2<-rbind(tree_use_all2, tree_use)
}


# See what is the slope vatiability per bat
tag_visits_df<-NULL
for (tg in TagList){
  tg_tree_use<-subset(tree_use_all2, TAG==tg)
  if(nrow(tg_tree_use)>2){
  max_revisits<-tg_tree_use$no.of.nights*3
  tg_tree_use<-tg_tree_use[tg_tree_use$Revisits<=max_revisits,]
  tg_tree_use<-tg_tree_use[!duplicated(tg_tree_use$Revisits),]
  tg_mod<-summary(lm(tg_tree_use$log_count~ tg_tree_use$log_revisits))
  coef<-round(coefficients(tg_mod)[2],2)
  R<-tg_mod$r.squared
  no.of.nights<-tg_tree_use$no.of.nights[1]
  }
  tag_visits_df<-as.data.frame(rbind(tag_visits_df,c(tg,no.of.nights,coef,R)))
  colnames(tag_visits_df)<-c("TAG", "Nu.of.Nights","Slope","R^2")
  }

hist(tag_visits_df$Slope, breaks=60, main="", xlab="Slope (linear model)", ylab="Number of bats with the same slope value")
summary(tag_visits_df$`R^2`)


### Accumulated trees ####


Tag_list<-unique(data$TAG)
night_acc<-NULL
week_acc<-NULL
for (t in 1:length(Tag_list)){
  trees_nights <- data[which(data$TAG==Tag_list[t]),c("TAG","cluster_ID","week_num","switch","night_number","season_type", "fruit_season")]
  trees_nights<- trees_nights[order(trees_nights$night_number),]
  trees_nights$new_t<-cumsum(!duplicated(trees_nights$cluster_ID))
  trees_night_sum<-trees_nights %>% group_by(TAG,week_num,night_number,switch,fruit_season,season_type) %>% summarise("new_trees_night"=max(new_t))
  trees_week_sum<-trees_night_sum%>% group_by(TAG,week_num,fruit_season) %>% summarise("new_trees_week"=max(new_trees_night), "switch_prop"=sum(switch)/7)
  trees_night_sum$tree_rate<-diff(c(NA,trees_night_sum$new_trees_night))/(diff(c(NA,trees_night_sum$night_number)))
  trees_week_sum$tree_rate<-diff(c(NA,trees_week_sum$new_trees_week))/(diff(c(NA,trees_week_sum$week_num)))
  night_acc<-rbind(night_acc,trees_night_sum)
  week_acc<-rbind(week_acc,trees_week_sum)
}

week_acc<-na.omit(week_acc)
week_acc<-week_acc[!is.infinite(week_acc$tree_rate),]


# for seach season:
week_acc_season<-week_acc
week_acc_season <- week_acc_season %>% group_by(fruit_season) %>% mutate(
  "count"=ave(tree_rate,  tree_rate, FUN = length))
week_acc_season$log_count<-log10(week_acc_season$count)


acc_model_results<- data.frame(matrix(nrow=5,ncol=3))
colnames(acc_model_results)<-c("Season","Slope","R_sqr")

for (s in unique(week_acc_season$fruit_season)){
  acc_season<-week_acc_season[week_acc_season$fruit_season==s,]
  acc_season_mod<-lm(acc_season$log_count ~ acc_season$log_tree_rate)
  R<-round(summary(acc_season_mod)[[8]],digits=2)
  slope<-summary(acc_season_mod)$coefficients[2]
  acc_model_results[s,]<-c(s,slope,R)
}

acc_model_results<-na.omit(acc_model_results)

week_acc_season$Fruiting_Season<-factor(week_acc_season$fruit_season, levels = c("Washingtonia", "China_Berry", "Mulberry", "Common_Fig", "Transition"))
ggplot(week_acc_season, aes(x=tree_rate)) + geom_density()+ facet_wrap(.~Fruiting_Season)+xlab("Rate of accumulating trees")+theme_bw()+theme(strip.text = element_text(size = 14))
#ggplot(week_acc_season, aes(x=Fruiting_Season, y=tree_rate))+geom_boxplot()+
# stat_summary(fun=mean, geom="point", shape=13, size=4, color="red")


length(unique(week_acc$TAG))
week_acc$tree_rate<-floor(week_acc$tree_rate)
hist(week_acc$tree_rate, breaks=100)
week_acc$log_tree_rate<-log10(1+week_acc$tree_rate)
week_acc$count <- as.numeric(ave( week_acc$tree_rate,  week_acc$tree_rate, FUN = length))
week_acc$log_count<-log10(1+week_acc$count)

hist(week_acc$switch_prop)
week_acc$switch<-ifelse(week_acc$switch_prop>0.5,"Switch","No_Switch")
week_acc_switch<-subset(week_acc,switch=="Switch")
week_acc_no_switch<-subset(week_acc,switch=="No_Switch")

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
hist(week_acc_no_switch$tree_rate, breaks=20, col=c1, main="Rate of accumulating trees", xlab="Number of trees added per week")
hist(week_acc_switch$tree_rate, breaks=20, col=c2, add=T)

#week_acc2<-week_acc[week_acc$tree_rate>0,]

pp<-ggplot(data=week_acc, aes(x=log_tree_rate, y=count))+geom_point()+facet_grid(.~switch)
pp<-pp+theme_bw()+geom_smooth(method="lm")+ylab("log (Frequency)") + xlab("Rate of accumulating trees\n (per week)")
pp+theme(text = element_text(size = 18))

# for seasons:
p<-ggplot(data=week_acc, aes(x=log_tree_rate, y=log_count))+geom_point()+facet_wrap(.~fruit_season)
p<-p+theme_bw()+geom_smooth(method="lm")+ylab("log (Frequency)") + xlab("log (Rate of accumulating trees)")
p+theme(strip.text = element_text(size = 14))

# models:
mod_acc_switch<-lm(week_acc$count[week_acc$switch=="Switch"] ~ week_acc$log_tree_rate[week_acc$switch=="Switch"])
summary(mod_acc_switch)

mod_acc_no_switch<-lm(week_acc$count[week_acc$switch=="No_Switch"] ~ week_acc$log_tree_rate[week_acc$switch=="No_Switch"])
summary(mod_acc_no_switch)

# Some tags have a crazy-large number of new trees- I will have a look at them seperatly:
many_trees_tags<-unique(all_bats_acc$TAG[which(all_bats_acc$new_trees_night>50)])
many_trees_df<-data2[data2$TAG %in% many_trees_tags,]
tag_ex<-"5744"
ex<-data2[data2$TAG==tag_ex,]
write.csv(ex,paste0("exaples/Check_",tag_ex,".csv"))
length(unique(ex$cluster_ID))
# After removing short visits (under 15 minutes) the number of new trees was improved significantlly).


#accu_mean<-data2[,c("TAG","cluster_ID","night_number","season_type", "fruit_season")]
#accu_mean<-  accu_mean%>%  arrange(night_number)
#accu_mean$new_t<-cumsum(!duplicated(accu_mean$cluster_ID))
#accu_mean_sum<-accu_mean %>% group_by(night_number) %>% summarise("total_new_trees_night"=max(new_t))
#mean_tag_night<-accu_mean %>% group_by(night_number) %>% summarise("tags_per_night"=mean(length(unique(TAG))))
#mean(mean_tag_night$tags_per_night)
#accu_mean_sum<-left_join(accu_mean_sum, mean_tag_night, by="night_number")
#accu_mean_sum$new_trees_night<-accu_mean_sum$total_new_trees_night/max(mean_tag_night$tags_per_night)


mean<-aggregate(all_bats_acc$new_trees_night, list(all_bats_acc$night_number), mean)
colnames(mean)<-c("night_number", "new_trees_night")
stdev<-aggregate(all_bats_acc$new_trees_night, list(all_bats_acc$night_number), sd)
stdev<-stdev[,2]
mean$sd<-stdev
mean$sim<-rep("Observed", nrow(mean))


mean_season<-aggregate(all_bats_acc$new_trees_night, list(all_bats_acc$night_number,all_bats_acc$season_type), mean)
colnames(mean_season)<-c("night_number", "season_type","new_trees_night")
stdev_season<-aggregate(all_bats_acc$new_trees_night, list(all_bats_acc$night_number,all_bats_acc$season_type), sd)
stdev_season<-stdev_season[,3]
mean_season$sd<-stdev_season

mean_fruit_season<-aggregate(all_bats_acc$new_trees_night, list(all_bats_acc$night_number,all_bats_acc$fruit_season), mean)
colnames(mean_fruit_season)<-c("night_number", "fruit_season","new_trees_night")
stdev_fruit_season<-aggregate(all_bats_acc$new_trees_night, list(all_bats_acc$night_number,all_bats_acc$fruit_season), sd)
stdev_fruit_season<-stdev_fruit_season[,3]
mean_fruit_season$sd<-stdev_fruit_season
mean_fruit_season$sim<-rep("Observed", nrow(mean_fruit_season))


# all bats all seasons
ggplot(all_bats_acc,aes(x=night_number, y=new_trees_night))+geom_line(aes(colour=TAG))+ xlim(0,100)+
  theme_classic()+ theme(legend.position = "none")+geom_line(data=mean[,c(1:2)],size = 1)+geom_ribbon(data=mean,aes(ymax = new_trees_night + sd, ymin = new_trees_night - sd),alpha = 0.1)

ggsave("results/Obs_bats_accumulated_trees(at least 30 nights).png")

#ggplot(all_bats_acc,aes(x=night_number, y=new_trees_night))+geom_line(aes(colour=TAG))+
#  theme_classic()+ theme(legend.position = "none")+geom_line(data=accu_mean_sum[,c(1,4)],size = 1)+ xlim(1,100)
#ggsave("results/Obs_bats_accumulated_trees.png")


# per season type
ggplot(all_bats_acc,aes(x=night_number, y=new_trees_night))+geom_line(aes(colour=TAG))+
  theme_classic()+ theme(legend.position = "none")+ xlim(0,30)+ ylim(0,100)+ facet_grid(.~season_type)+geom_line(data=mean[,c(1:3)],size = 0.5)+geom_ribbon(data=mean,aes(ymax = new_trees_night + sd, ymin = new_trees_night - sd),alpha = 0.1)
ggsave("results/Obs_bats_accumulated_trees_per_Season_Type (at least 30 nights).png")

# per fruit_season
ggplot(all_bats_acc,aes(x=night_number, y=new_trees_night))+geom_line(aes(colour=TAG))+
  theme_classic()+ theme(legend.position = "none")+ facet_grid(.~fruit_season)+ xlim(0,100)+ ylim(0,150)+geom_line(data=mean_fruit_season[,c(1:3)],size = 0.5)+geom_ribbon(data=mean_fruit_season,aes(ymax = new_trees_night + sd, ymin = new_trees_night - sd),alpha = 0.1)
ggsave("results/Obs_bats_accumulated_trees_per_Fruiting_Season (at least 30 nights).png")

# Compare to simuated bats based on values from the real data: ####
# (Using Ohad simulations for tree accumulation based on tendency to revisit trees)


# q0 - strength of revisiting tendency. A number between 0 and 1, with 1 being very strong  (the animal only visits places it visited in the past)
# Trees - the number of trees. 
# x0 - initial condition
# steps - number of steps in simulation.  
# output - the locations of the walker at each discrete time step




RWPR<-function(q0, trees, x0, steps){
  
  X<- rep(0,steps) # list of trees per bat
  r<- runif(steps, min=0, max=1)
  X[1]<-x0
  q<-q0
  
  for (i in 1:steps) { 
    
    if (r[i]<q) { # memory step
      g<-sample.int(i,1)
      x <- X[g]
    }else{ # random step
      x<-sample.int(trees,1)
    }
    X[i] <- x
  }
  return(X)
}


# number of trees per night:
max(aggregate(data2$cluster_ID, list(data2$TAG), function(x) length(unique(x)))[,2])

# number of steps (nights)
summary(data2$night_number)

# P - number of tags
length(unique(data2$TAG))

trees=100
x0<-1
steps=80 # nights
q0<- 0.5
P<-100
sim<-data.frame("Run"=rep(NA,P*steps), "night_number"=rep(NA,P*steps), "new_trees_night"=rep(NA,P*steps))
idx<-1
for (n in 1:P){
  X <- RWPR(q0, trees, x0, steps)
  new_t<-cumsum(!duplicated(X))
  sim[idx:(idx+steps-1),]<-cbind(rep(n,steps),seq(1,steps),new_t)
  idx<-n*steps+1
}
sim$Run<-as.character(sim$Run)
mean_sim<-aggregate(sim$new_trees_night, list(sim$night_number), mean)
colnames(mean_sim)<-c("night_number", "new_trees_night")
sd<-aggregate(sim$new_trees_night, list(sim$night_number), sd)
sd<-sd[,2]
mean_sim<-cbind(mean_sim,sd)
mean_sim$sim<-rep("Simulated", nrow(mean_sim))
mean_sim$fruit_season<-rep("Simulated", nrow(mean_sim))

ggplot(sim,aes(x=night_number, y=new_trees_night))+geom_line(aes(colour=Run))+
  geom_line(data=mean_sim,size = 1) +theme_classic()+ theme(legend.position = "none")+ylim(0,30)

ggsave("results/Simulated_bats_accumulated_trees_q=0.8.png")

sim_obs_means<- rbind(mean_fruit_season,mean_sim)

ggplot(mean_fruit_season,aes(x=night_number, y=new_trees_night))+geom_line(aes(colour=fruit_season))+geom_line(data=mean_sim,size = 1)+
  theme_classic()+ xlim(1,30)+ylim(0,80)
ggsave("results/Simulated_Vs_Obs_q=0.5 (only aboive 30 nights).png")
