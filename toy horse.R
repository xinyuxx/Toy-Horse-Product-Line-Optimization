require("cluster")
require("fpc")
require("factoextra")
require("gridExtra")
library(cluster)
library(fpc)
library(factoextra)
library(gridExtra)

#####################################################################    PART 1
#new df to input part-utilities
new_df <- data.frame(ID= rep(1:200), Intercept = NA, price= NA, size = NA, motion = NA, style = NA)

# part utilities
for (i in 1:200){
  reg = lm(ratings ~ price + size + motion + style, data = conjointData[conjointData$ID == i,])
  new_df[i, 2:6] = reg$coefficients
}


# filling the Na's
for (user in 1:200){
  
  #Regression for each ID
  reg = lm(ratings ~ price + size + motion + style, data = conjointData[conjointData$ID == user,])
  
  #geeting NA's in conjoint df
  na_s <- which(is.na(conjointData[conjointData$ID == user,]$ratings))
  
  #filling na's in coinjoint data
  conjointData[conjointData$ID == user,]$ratings[na_s] = predict(reg, newdata = conjointData[conjointData$ID == user, c(4:7)][na_s,])
  
}


#####################################################################    PART 2
##Evaluate number of clusters to use on data with visualizations
##Arguments: 
##  toClust, the data to do kmeans cluster analysis
##  maxClusts=15, the max number of clusters to consider
##  seed, the random number to initialize the clusters
##  iter.max, the max iterations for clustering algorithms to use
##  nstart, the number of starting points to consider
##Results:
##  a list of weighted sum of squares and the pamk output including optimal number of clusters (nc)
##  to create visualizations need to print tmp
clustTest = function(toClust,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(scale){ toClust = scale(toClust);}
  set.seed(seed);   # set random number seed before doing cluster analysis
  wss <- (nrow(toClust)-1)*sum(apply(toClust,2,var))
  for (i in 2:maxClusts) wss[i] <- sum(kmeans(toClust,centers=i,nstart=nstart,iter.max=iter.max)$withinss)
  ##gpw essentially does the following plot using wss above. 
  #plot(1:maxClusts, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  gpw = fviz_nbclust(toClust,kmeans,method="wss",iter.max=iter.max,nstart=nstart,k.max=maxClusts) #alternative way to get wss elbow chart.
  pm1 = pamk(toClust,scaling=TRUE)
  ## pm1$nc indicates the optimal number of clusters based on 
  ## lowest average silhoutte score (a measure of quality of clustering)
  #alternative way that presents it visually as well.
  gps = fviz_nbclust(toClust,kmeans,method="silhouette",iter.max=iter.max,nstart=nstart,k.max=maxClusts) 
  if(print){
    grid.arrange(gpw,gps, nrow = 1)
  }
  list(wss=wss,pm1=pm1$nc,gpw=gpw,gps=gps)
}

results_clustTest <- clustTest(new_df[,3:6])
results_clustTest

##Runs a set of clusters as kmeans
##Arguments:
##  toClust, data.frame with data to cluster
##  nClusts, vector of number of clusters, each run as separate kmeans 
##  ... some additional arguments to be passed to clusters
##Return:
##  list of 
##    kms, kmeans cluster output with length of nClusts
##    ps, list of plots of the clusters against first 2 principle components
runClusts = function(toClust,nClusts,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(length(nClusts)>4){
    warning("Using only first 4 elements of nClusts.")
  }
  kms=list(); ps=list();
  for(i in 1:length(nClusts)){
    kms[[i]] = kmeans(toClust,nClusts[i],iter.max = iter.max, nstart=nstart)
    ps[[i]] = fviz_cluster(kms[[i]], geom = "point", data = toClust) + ggtitle(paste("k =",nClusts[i]))
    
  }
  library(gridExtra)
  if(print){
    tmp = marrangeGrob(ps, nrow = 2,ncol=2)
    print(tmp)
  }
  list(kms=kms,ps=ps)
}

results_Run_Clusts <- runClusts(new_df[,3:6],3)
results_Run_Clusts


##Plots a kmeans cluster as three plot report
##  pie chart with membership percentages
##  ellipse plot that indicates cluster definitions against principle components
##  barplot of the cluster means
plotClust = function(km,toClust,discPlot=FALSE){
  nc = length(km$size)
  if(discPlot){par(mfrow=c(2,2))}
  else {par(mfrow=c(3,1))}
  percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
  pie(km$size,labels=percsize,col=1:nc)
  
  clusplot(toClust, km$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components
  
  if(discPlot){
    plotcluster(toClust, km$cluster,col=km$cluster); #plot against discriminant functions ()
  }
  rng = range(km$centers)
  dist = rng[2]-rng[1]
  locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
  bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
  text(bm,locs,formatC(km$centers,format="f",digits=1))
}

plotClust(results_Run_Clusts[["kms"]][[1]], new_df[,2:6])

#Test at least two cluster analysis schemes (i.e., number of clusters) and select the
#best one in your view. Justify this decision.

# Answer: We tried 3 different numbers of cluster (2,3,4). Looking at the pm1$nc which 
# indicates the optimal number of clusters based on lowest average silhoutte score (a measure 
# of quality of clustering) we identified that 3 was the optimal number. Also, if we look at the 
# attributes it is farly easy to say that price, size, motion and style are better distributed with 
# 3 different clusters. Besides that, here is no overlap in the clusters based on Dimension 1 and Dimension 2.


#Interpret the segments in the chosen scheme and identify the ideal product for each segment

# Answer: For the chosen Cluster with 3 segments, we can say that: 
# Segment 1 ->  price 8.9, size: 16.6, motion: 7, style: 11. 
# Therefore we choose Profile 16 (1,1,1,1), which is $119.99 (low price), 26 inches (larger), Rocking motion, Glamour style
# Segment 2 ->  price 11.6, size: -6.6, motion: 10, style: 0.5 
# Therefore we choose Profile 14 (1,0,1,1), which is $119.99 (low price), 18 inches (smaller), Rocking motion, Glamour style
# Segment 3 ->  price 22.5, size: 5.3, motion: -9.4, style: -6.6
# Therefore we choose Profile 4 (1,1,0,0), which is $119.99 (low price), 26 inches (larger), Bouncing motion, Racing style


####3
#####################
df = merge(conjointData, respondentData, by = "ID")
#new df to input part-utilities
new_df2 <- data.frame(ID= rep(1:200), Intercept = NA, price= NA, size = NA, motion = NA, style = NA,price_gender=NA,size_gender=NA,motion_gender=NA,style_gender=NA)

# part utilities
reg = lm(ratings ~ price+ size + motion + style+ price:gender+size:gender+motion:gender+style:gender, data = df)
reg = lm(ratings ~ price+ size + motion + style+ price:age+size:age+motion:age+style:age, data = df)

df<-cbind(df,results_Run_Clusts[["kms"]][[1]][["cluster"]])
colnames(df)[10]<-'Cluster'

# Test whether these a priori segmentation variables affect the part-utilities. 
respondentData$group = NA
for (i in 1:nrow(respondentData)){
    if (respondentData[i,2]==0 && respondentData[i,3] == 0){
        respondentData[i,4]=1
    }else if (respondentData[i,2]==0 && respondentData[i,3] == 1){
        respondentData[i,4]=2
    }else if (respondentData[i,2]==1 && respondentData[i,3] == 0){
        respondentData[i,4]=3
    }else if (respondentData[i,2]==1 && respondentData[i,3] == 1){
        respondentData[i,4]=4
    }
}

df = merge(conjointData, respondentData, by = "ID")
    
# What does this test tell you about these as segmentation schemes?
reg_priori = lm(ratings ~ price+ size + motion + style+ price:factor(group)+size:factor(group)+motion:factor(group)+style:factor(group), data = df)
reg_priori

# Answer: It basically tells us that we have differences between ages and groups, in the way their utility increases or decreases
# based on the feautures the customers can choose. Therefore it would be good to differentiate 


# If the differences are meaningful, profile the segment-level attribute preferences 
# and identify the ideal product for the a priori segments.


# Answer: when we look at the segmentation with Gender and Age we can say that:
# MALE AGE 2 ->   price +, size: +, motion: +, style: -  
# Therefore we choose Profile 8 (1,1,1,0), which is $119.99 (low price), 26 inches (larger), Rocking motion, Racing style

# FEMALE AGE 2 ->  price +, size: +, motion: +, style: + 
# Therefore we choose Profile 16 (1,1,1,1), which is $119.99 (low price), 26 inches (larger), Rocking motion, Glamour style

# MALE AGE 3/4 ->   price +, size: +, motion: -, style: -
# Therefore we choose Profile 4 (1,1,0,0), which is $119.99 (low price), 26 inches (larger), Bouncing motion, Racing style

# FEMALE AGE 3/4 ->  price +, size: +, motion: +, style: -
# Therefore we choose Profile 16 (1,1,1,1), which is $119.99 (low price), 26 inches (larger), Rocking motion, Glamour style


####4
##Create a new dataframe with columns in rating of different products
##and rows in different users
df_sim <- data.frame(ID= rep(1:200), p1=NA,p2=NA,p3=NA,p4=NA,p5=NA,p6=NA,p7=NA,
                     p8=NA,p9=NA,p10=NA,p11=NA,p12=NA,p13=NA,p14=NA,p15=NA,p16=NA)
for(i in 1:200){
  for(j in 1:16){
    df_sim[i,j+1]=conjointData[conjointData$ID==i,][j,3]
  }
}

#calculate the newest one
#df_sim<-df_sim[,c(1,6,8,14)]
#df_sim$same=NA
i=1

scens=list()
scens[[1]]=c(1,5,7,13)   
scens[[2]]=c(2,5,7,13)  
scens[[3]]=c(3,5,7,13) 
scens[[4]]=c(4,5,7,13) 
scens[[5]]=c(5,7,13) 
scens[[6]]=c(5,6,7,13) 
scens[[7]]=c(5,7,8,13) 
scens[[8]]=c(5,7,9,13)
scens[[9]]=c(5,7,10,13)
scens[[10]]=c(5,7,11,13)
scens[[11]]=c(5,7,12,13)
scens[[12]]=c(5,7,13,14)
scens[[13]]=c(5,7,13,15)
scens[[14]]=c(5,7,13,16)



###calculate the market share
simFCSharesA = function(scen,data,ascend=TRUE){ 
  inmkt = data[,scen] #construct the subsetted matrix of options
  if(ascend){ #if ranks 1 is best 
    bestOpts = apply(inmkt,1,which.max)  #identify which option is best = min
  } else { #else the best rank is the largest number
    bestOpts = apply(inmkt,1,which.min) #identify which option is best = max
  }
  ms<-c(0,0,0,0)
  for(i in 1:200){
    if(bestOpts[i]==1){
      ms[1]=ms[1]+1
    }else if(bestOpts[i]==2){
      ms[2]=ms[2]+1
    }else if(bestOpts[i]==3){
      ms[3]=ms[3]+1
    }else{
      ms[4]=ms[4]+1
    }
  }
  ms<-ms/200
}
statusQuo =c(1,5,7,13)
a<-simFCSharesA(statusQuo,df_sim[,2:17])
df_ms<-data.frame(Num= rep(1:14), p1=0,p2=0,p3=0,p4=0,p5=0,p6=0,p7=0,
                  p8=0,p9=0,p10=0,p11=0,p12=0,p13=0,p14=0,p15=0,p16=0)

for(j in 1:14){
  statusQuo=scens[[j]]
  a<-simFCSharesA(statusQuo,df_sim[,2:17])
  for(i in 1:length(statusQuo)){
    df_ms[j,statusQuo[i]+1]=a[i]    
  }
}

##calculate the profit

cost_inf<-data.frame(ID=rep(1:16),sell_price=0.8*(profilesData$priceLabel),
                     variable_cost=c(21,21,29,29,33,33,41,41,21,21,29,29,33,33,41,41))
cost_inf$profit<-cost_inf$sell_price-cost_inf$variable_cost
df_ms$profits<-0

for(i in 1:14){
  for(j in 1:16){
    df_ms$profits[i]=df_ms$profits[i]+4000*df_ms[i,j+1]*cost_inf$profit[j]
  }
}


