cave_overlap<-function(caves_df){
  # create months list
  my_l<-unique(caves_df$month_year)
  
  overlap_df<-data.frame("Month_Year"=rep(NA,length(my_l)),
                         "Overlap_caves"=rep(NA,length(my_l)),
                         "Gershom"=rep(NA,length(my_l)),
                         "Zemer"=rep(NA,length(my_l)),
                         "Total_Tags"=rep(NA,length(my_l)))

  for (my in 1:length(my_l)){
    month<-subset(caves_df,month_year==my_l[my])  
    month_unq<- month[!duplicated(month[c("TAG","cluster_ID")]),]
    
    
    Gershom_trees<-month_unq$cluster_ID[month_unq$cave_origin=="Har Gershom"]
    Gershom_trees<-Gershom_trees[!duplicated(Gershom_trees)]
    Zemer_trees<-month_unq$cluster_ID[month_unq$cave_origin=="Zemer cave"]
    Zemer_trees<-Zemer_trees[!duplicated(Zemer_trees)]
    
    if (length(Gershom_trees)!=0 & length(Zemer_trees)!=0){
      #shared<-month_unq$cluster_ID[duplicated(month_unq$cluster_ID)]
      unique<-month_unq$cluster_ID[!duplicated(month_unq$cluster_ID)] # all tree
      overlap<-length(intersect(Gershom_trees,Zemer_trees))/length(unique)
      #p.overlap_all_caves<-(nrow(shared2)/nrow(unique))
      tags_gershom<-length(unique(month$TAG[month$cave_origin=="Har Gershom"]))
      tags_zemer<-length(unique(month$TAG[month$cave_origin=="Zemer cave"]))
      
      if (tags_gershom>1 & tags_zemer>1){
        total_tags<-sum(tags_gershom,tags_zemer)
        
        overlap_df[my,]<-cbind(my_l[my],overlap,tags_gershom,tags_zemer,total_tags)
      }}}
  overlap_df<-na.omit(overlap_df)
  return(overlap_df)
}

cave_overlap_all_years<-function(caves_df){
  # create months list
  m_l<-unique(caves_df$month)
  
  overlap_df<-data.frame("Month"=rep(NA,length(m_l)),
                         "Overlap_caves"=rep(NA,length(m_l)),
                         "Gershom"=rep(NA,length(m_l)),
                         "Zemer"=rep(NA,length(m_l)),
                         "Total_Tags"=rep(NA,length(m_l)))
  
  for (m in 1:length(m_l)){
    month<-subset(caves_df,month==m_l[m])  
    month_unq<- month[!duplicated(month[c("TAG","cluster_ID")]),]
    
    
    Gershom_trees<-month_unq$cluster_ID[month_unq$cave_origin=="Har Gershom"]
    Gershom_trees<-Gershom_trees[!duplicated(Gershom_trees)]
    Zemer_trees<-month_unq$cluster_ID[month_unq$cave_origin=="Zemer cave"]
    Zemer_trees<-Zemer_trees[!duplicated(Zemer_trees)]
    
    if (length(Gershom_trees)!=0 & length(Zemer_trees)!=0){
      #shared<-month_unq$cluster_ID[duplicated(month_unq$cluster_ID)]
      unique<-month_unq$cluster_ID[!duplicated(month_unq$cluster_ID)] # all trees
      
      overlap<-length(intersect(Gershom_trees,Zemer_trees))/length(unique)
      #p.overlap_all_caves<-(nrow(shared2)/nrow(unique))
      tags_gershom<-length(unique(month$TAG[month$cave_origin=="Har Gershom"]))
      tags_zemer<-length(unique(month$TAG[month$cave_origin=="Zemer cave"]))
      
      if (tags_gershom>1 & tags_zemer>1){
        total_tags<-sum(tags_gershom,tags_zemer)
        
        overlap_df[m,]<-cbind(m_l[m],overlap,tags_gershom,tags_zemer,total_tags)
      }}}
  overlap_df<-na.omit(overlap_df)
  return(overlap_df)
}



Cave_area_overlap<-function(caves_df){
  library(adehabitatHR)
  library(rgeos)
  library(rgdal)
  m_l<-unique(caves_df$month)
  
  area_overlap_df<-data.frame("Month"=rep(NA,length(m_l)),
                         "Area_overlap"=rep(NA,length(m_l)),
                         "Proportion_overlap"=rep(NA,length(m_l)),
                         "Gershom_MCP"=rep(NA,length(m_l)),
                         "Zemer_MCP"=rep(NA,length(m_l)),
                         "Tags_Gershom"=rep(NA, length(m_l)),
                         "Tags_Zemer"=rep(NA, length(m_l))
                         )
  
  for (m in 1:length(m_l)){
    month<-subset(caves_df,month==m_l[m])  

    Gershom_trees<-month[month$cave_origin=="Har Gershom",]
    Zemer_trees<-month[month$cave_origin=="Zemer cave",]
    
    
    if (length(unique(Gershom_trees$cluster_ID))>10 & length(unique(Zemer_trees$cluster_ID))>10){
      #  MCP - using a per-month home range to include 95 % of most used trees and checking if they overlap.
      itm<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
      #wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      
      g_sp<-Gershom_trees[,c('X_tree','Y_tree')]
      g_sp<-SpatialPoints(g_sp)
      proj4string(g_sp)<-CRS(itm)
      #g_sp<-spTransform(g_sp, wgs84)
      
      z_sp<-Zemer_trees[,c('X_tree','Y_tree')]
      z_sp<-SpatialPoints(z_sp)
      proj4string(z_sp)<-CRS(itm)
      #z_sp<-spTransform(z_sp, wgs84)
      
      #plot(g_sp)
      #plot(z_sp, add=T, col="red")
      
      G_area<-mcp(g_sp, percent=99,unout = "km2")
      Z_area<-mcp(z_sp,percent = 99,unout = "km2")
     # g_save<-paste0("results/Shape_Files/Gershom_MCP_month = ",m_l[m],".shp")
     # z_save<-paste0("results/Shape_Files/Zemer_MCP_month = ",m_l[m],".shp")
     # 
     # writeOGR(G_area, dsn=g_save,layer="G_area" ,driver = "ESRI Shapefile")
     # writeOGR(Z_area, dsn=z_save,layer="Z_area" ,driver = "ESRI Shapefile")
      
     # plot(G_area,add=T)
     # plot(Z_area, add=T, col="red")
      overlap_polygon <- gIntersection(G_area,Z_area)
     # plot(overlap_polygon, add=T, col="blue")
      overlap_area_meters<-gArea(overlap_polygon)
      overlap_area<-floor(overlap_area_meters/10^6)
      overlap_prop<-overlap_area_meters/(gArea(G_area) + gArea(Z_area))
      
      # Check there are enough tags in each cave
      tags_gershom<-length(unique(month$TAG[month$cave_origin=="Har Gershom"]))
      tags_zemer<-length(unique(month$TAG[month$cave_origin=="Zemer cave"]))
      
      if (tags_gershom>2 & tags_zemer>2){
        total_tags<-sum(tags_gershom,tags_zemer)
      area_overlap_df[m,]<-cbind(m_l[m],overlap_area,overlap_prop,floor(G_area$area), floor(Z_area$area),tags_gershom,tags_zemer)
      }
    }
  }
  area_overlap_df<-na.omit(area_overlap_df)
  return(area_overlap_df)
}  
