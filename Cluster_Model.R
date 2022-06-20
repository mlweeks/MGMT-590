#data prep#############################################################################################################################################################

#load from excel file
install.packages("tidyverse")

library(readxl)
data = read_xlsx("2021 Player Sample.xlsx", sheet=1)
#data = read_xlsx("C:\\Users\\cmbar\\OneDrive - purdue.edu\\MGMT 590 - R for Analytics\\Final Project\\Rookie Year Stats.xlsx", sheet=1)
#data = na.omit(data)

#check variable correlation with subject variable########################################################################


# 2021 Player Sample.xlsx data set evaluation
    #dependent variable
      #27 - overall
    #potential determinant variables
      #5 - BA
      #6 - xslg
      #7 - xwoba
      #8 - xobp
      #9 - xiso
      #10 - exit_velocity_avg
      #11 - launch_angle_avg
      #12 - barrel_batted_rate
      #13 - weight
      #18 - rbi
      #26 - avg

#choose variables
data = data[c(
  27  #overall
  ,5  #BA
  ,6  #xslg
  ,7  #xwoba
  ,8  #xobp
  ,9  #xiso
  ,10 #exit_velocity_avg
  ,11 #launch_angle_avg
  ,12 #barrel_batted_rate
  ,13 #weight
  #,18 #rbi
  #,26 #avg
  )]




#standardize variables
data_z = scale(data)
data_z = data.frame(scale(data))
for (i in 1:ncol(data_z)) 
  {
  names(data_z)[i] = paste0(names(data_z)[i],"_z")
  }

#create test and train data sets
set.seed(1234)
rows = sample(1:nrow(data), round(nrow(data)*.5,0))



#k-means clustering####################################################################################################################################################

#test different number of clusters
clust_count_df = data.frame()
for (k in 1:15)
  {
  kmeans_trn = kmeans(x=data_z[ rows,], centers=k, nstart=20, iter.max=50)
  kmeans_tst = kmeans(x=data_z[-rows,], centers=k, nstart=20, iter.max=50)
  clust_count_df = rbind(clust_count_df, cbind(k, kmeans_trn$tot.withinss
                                               , kmeans_tst$tot.withinss))
  }
names(clust_count_df) <- c("cluster", "tr_cost", "te_cost")


#elbow plot of cluster count performance
par(mfrow=c(1,1))
clust_count_df[,2:3] <- clust_count_df[,2:3]/1000
plot(x=clust_count_df$cluster, y=clust_count_df$tr_cost, main="k-Means Elbow Plot"
     , col="blue", pch=19, type="b", cex.lab=1.2
     , xlab="Number of Clusters", ylab="MSE (in 1000s)")
points(x=clust_count_df$k, y=clust_count_df$te_cost, col="green")

length(clust_count_df$cluster)
length(clust_count_df$tr_cost)
#set number of clusters
k = 4

#generate train and test kmeans cluster data sets
kmeans_trn = kmeans(x=data_z[ rows,], centers=k, nstart=20, iter.max=50)
kmeans_tst = kmeans(x=data_z[-rows,], centers=k, nstart=20, iter.max=50)

#visualize the clusters
#library(ggplot2)
#autoplot(,data_z,frame=TRUE)

install.packages("misc3d")
#install.packages("plot3D", repos="http://R-Forge.R-project.org")
library(plot3D)
par(mfrow=c(1,2))
scatter3D(x = data_z[rows,2], y = data_z[rows,3], z = data_z[rows,1]
          , surface=F, gridlines=26, grid=T, pch=19, point.col = "blue"
          , colvar=kmeans_trn$cluster, colkey = F, border = "black"
          , ticktype = "detailed", bty = "g", lwd = 2, phi = 20
          , main = "k-Means clustering", 
            xlab="exit_velocity_avg_z", 
            ylab="launch_angle_avg_z"
          , zlab="barrel_batted_rate_z")








