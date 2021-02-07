

summer <- subset(dfCopy, season == 'Summer')
winter <- subset(dfCopy, season == 'Winter')
fall <- subset(dfCopy, season == 'Fall')
spring <-  subset(dfCopy, season == 'Spring')


##################

summary(dfCopy$adr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6.38   70.67   95.00  103.57  126.00 5400.00 

summary(winter$adr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6.38   58.00   75.00   77.58   90.00  451.50 

summary(fall$adr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.56   62.00   80.00   86.06  105.00  300.00 

summary(spring$adr)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  2.0    80.1   101.0   107.1   130.0  5400.0 


###################
# Summer

summary(summer$adr)
#  Min. 1st Qu.  Median  Mean    3rd Qu.    Max. 
#  1.56   89.68  116.94   127.06  155.00  508.00 
sd(summer$adr) # 53.17961

plot(summer$adr)
boxplot(summer$adr)