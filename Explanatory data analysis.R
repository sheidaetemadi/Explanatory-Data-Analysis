
setwd("File Location")
#part-1   ##find distribution of data
data <- read.table(file.choose(), header = TRUE)
names(data)
attach(data)
datamean <- c(mean(rad),mean(temp),mean(wind),mean(ozone))
datavariance <- c(var(rad),var(temp),var(wind),var(ozone))
datastdev <- c(sd(rad),sd(temp),sd(wind),sd(ozone))
datastats <- matrix(c(datamean,datavariance,datasd),ncol = 3,dimnames = list(c("rad","temp","wind","ozone"),c("mean","variance","stdev")))
datastats                    
write.csv(datastats,file = "statistics data matrix.csv",row.names = TRUE)

#part-2  ##find about skewed distribution
datasummary <- summary(data)
write.csv(datasummary,file = "datasummary.csv",row.names = TRUE)
names(data)
datadF <- matrix(c(fivenum(rad),fivenum(temp),fivenum(wind),fivenum(ozone)), ncol= 4)
datadF
write.csv(datadF,file = "fivenum.csv", row.names = TRUE)
vardata <-var(data)
write.csv(vardata,file = "var.csv", row.names = TRUE)
cordata <- cor(data)
write.csv(cordata,file = "cor.csv", row.names = TRUE)

#time series plot
plot.ts(data,main="data time series by sample order")
data

#lag plot
detach(data)
dataTSlag <- read.table(file.choose(),header = TRUE)
attach(dataTSlag)
par(mfrow=c(2,2))
plot(rad1,rad)
plot(temp1,temp)
plot(wind1,wind)
plot(ozone1,ozone)

#ACF winds
detach(dataTSlag)
attach(data)
par(mfrow=c(1,1))
acf(data$wind,main="ACF of winds")

#boxplot
par(mfrow=c(2,2))
attach(data)
names(data)
ozoneboxdata <- ozone
ozoneboxlabel <- factor(rep("ozone",111))
boxplot(ozoneboxdata~ozoneboxlabel,main="ozone")
radboxdata <- rad
radboxlabel <- factor(rep("rad",111))
boxplot(radboxdata~radboxlabel,main="radiation")
tempboxdata <- temp
tempboxlabel <- factor(rep("temp",111))
boxplot(tempboxdata~tempboxlabel,main="temperature")
windboxdata <- wind
windboxlabel <- factor(rep("wind",111))
boxplot(windboxdata~windboxlabel,main="wind")

#histogram
par(mfrow=c(2,2))
hist(ozoneboxdata,main="ozone")
hist(radboxdata,main = "radiation")
hist(tempboxdata,main = "temperature")
hist(windboxdata,main = "wind")

#stem and leaf plot
names(data)
stem(ozone)
stem(rad)
stem(temp)
stem(wind)

#Q-Q normal plot
par(mfrow=c(2,2))
qqnorm(ozoneboxdata,main = "ozone Q-Q norm plot")
qqline(ozoneboxdata)
qqnorm(radboxdata,main = "radiation Q-Q norm plot")
qqline(radboxdata)
qqnorm(tempboxdata,main = "temperature Q-Q norm plot")
qqline(tempboxdata)
qqnorm(windboxdata,main = "wind Q-Q norm plot")
qqline(windboxdata)

#scatterplot matrix
par(mfrow=c(1,1))
plot(data)
