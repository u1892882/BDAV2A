# tell R where your file is located
setwd('C:/Users/metl015/Desktop')

# tell R what file to use
mydata <- read.csv("customer cleaned 1203.csv")

# convert to a R data frame
mydata <- data.frame(mydata)

# fit the K-means model to mydata
fit <- kmeans(mydata, 10) #10 cluster solution

# get cluster means
means <- aggregate(mydata, by=list(fit$cluster), FUN=mean)

# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

# write the data back out to Excel
write.csv(mydata, file="FirstCluster.csv", row.names=FALSE) # full data file with cluster assignments
write.csv(means, file="FirstClusterMeans.csv", row.names=FALSE) # list of means by cluster

#end

# get cluster means
means <- aggregate(mydata, by=list(fit$cluster), FUN=mean)

# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

# write the data back out to Excel
write.csv(mydata, file="SecondCluster.csv", row.names=FALSE) # full data file with cluster assignments
write.csv(means, file="SecondClusterMeans.csv", row.names=FALSE) # list of means by cluster

#end
# this code checks if the dummies package is installed and if not installs it
if ("dummies" %in% installed.packages()) {
	library(dummies)
} else {
	install.packages("dummies")
	library(dummies)
}


# tell R what file to use
mydata <- read.csv("customer cleaned 1203.csv")

# convert to a R data frame
mydata <- data.frame(mydata)

# exclude unimportant variables
dummyVals <- c("Professional", "Crafts", "Other", "Self","Clerical","Homemaker","Retired","Student")
mydata <- dummy.data.frame(mydata, names=c(dummyVals), sep="_")

# fit the K-means model to mydata
fit <- kmeans(mydata, 3) #3 cluster solution

# get cluster means
means <- aggregate(mydata, by=list(fit$cluster), FUN=mean)

# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

# write the data back out to Excel
write.csv(mydata, file="ThirdCluster.csv", row.names=FALSE) # full data file with cluster assignments
write.csv(means, file="ThirdClusterMeans.csv", row.names=FALSE) # list of means by cluster

#end

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i, iter.max=30)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


#end
