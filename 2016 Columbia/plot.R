# by Jun Soo (Derek) Shin
# 2016 Columbia Diamond Dollars Case Competition
# Creates line charts for Marcel projections and for custom "KNN" projections
# and bar charts for top 5 position players.

library(ggplot2)

# Picking more than 9 colors on ggplot2 
# http://stackoverflow.com/questions/16295440/r-ggplot-better-gradient-color
require(RColorBrewer)
?brewer.pal
mypal <- colorRampPalette(brewer.pal(1, "RdBu"))
mypal(10)
scale_fill_manual(values = mypal(8))

# Making some lines and points bolder
# http://stackoverflow.com/questions/11467965/r-ggplot2-highlighting-selected-points-and-strange-behavior
marcel = read.csv("MarcelTop10WAR.csv")
(ggplot(marcel, aes(x=Year, y=WAR, color=Name))
 + ggtitle("Marcel Top 10 WAR")
 + geom_point(size=2) + geom_line(size=1.2)
 + geom_point(data=marcel[1:23, ], aes(x=Year, y=WAR), size=5)
 + geom_line(data=marcel[1:23, ], aes(x=Year, y=WAR), size=3)
 + scale_fill_manual(values = mypal(8)))

knn = read.csv("KNNTop5WAR.csv")
(ggplot(knn, aes(x=Year, y=WAR, color=Name))
  + ggtitle("KNN Top 5 WAR")
  + geom_point(size=2.5) + geom_line(size=1)
  + geom_point(data=knn[1:23, ], aes(x=Year, y=WAR), size=5)
  + geom_line(data=knn[1:23, ], aes(x=Year, y=WAR), size=3)
  + scale_fill_manual(values = mypal(8))
  + expand_limits(y=c(0, 10)))

average = read.csv("AverageTop5WAR.csv")
(ggplot(data=average, aes(x=Name, y=WAR, fill=Name, width=0.5))
+ ggtitle("Averaged Top 5 Total WAR")
+ geom_bar(stat="identity"))
