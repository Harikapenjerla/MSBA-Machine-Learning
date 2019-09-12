# Superstore Sales sample dataset,source is from Tableau.com

#First we have to set "working Directory" which menas where data file is stored.Data file format I'm using is "Comma Separated Values" format.  
getwd()
setwd("C:/Users/hpenj/OneDrive/Desktop")

# Import data into R using read function
Mydata <- read.csv("new.csv")

# Finding column names in this data
colnames(Mydata)

# Let quantitative variable I'm using is Sales and qualitative variable is Category
quan <- Mydata$Sales
qual <- Mydata$Category

#Descriptive Analysis of Quantitative Variable
mean(quan)              #mean 
median(quan)            #median
var(quan)               #variance
sd(quan)                #standard Deviation 
quantile(quan)          #quantile 
IQR(quan, na.rm = FALSE)  #Interquartile range
range(quan)             #range
min(quan)               #To calculate min and max value individually using min(),max() respectively
max(quan)
which.max(quan)         #To find out exactly which column is having max value in given data
which.min(quan)         #To find out exactly which column is having min value in given data
bommi[which.max(quan),]
order(quan)     #To find top two sales value in this data
Mydata[c(28, 150),]     #To get all other column details of top two sales values.
library(moments)        #For calculating skewness we need library "moments"
skewness(quan)          #skewness
summary(quan)    #It provides 5-point summary and also mean

#Showing multiple plots 
par(mfrow = c(3,3))
#plot0 <- boxplot(quan)
plot1 <- hist(quan, n=100, main='Original Histogram', col='red') #To generate histogram

#For quantitative variable in this data 
summary(qual)
c <- table(qual)
plot2 <- barplot(c, main = 'Barplot', xlab='Category', ylab='Number of sales in particular category', col='orange')      #Barplot of Category column in the data

#Correlation plot for different variables
library(corrplot)        #corrplot library for correlation plot
plot3 <- corrplot(cor(Mydata[,5:8]), main='Correlation Plot') #correlation  plot

#Transformation of variable in dataset
log_transformation <- log10(quan)    # log Transformation 
summary(log_transformation)
skewness(log_transformation)
plot3 <- hist(log_transformation, main='Histogram of Log_Transformation', col='green')    #Calculating transformed histogram

sqrt_transformation <- sqrt(quan)   #squareroot transformation
summary(sqrt_transformation)
skewness(sqrt_transformation)
plot4 <- hist(sqrt_transformation, main='Histogram of SquareRoot Transformation', col='yellow')

#Adding transformed data values column to the data
Mydata$log_trans <- log_transformation
Mydata$sqrt_trans <- sqrt_transformation

# plot
x <- table(quan)
plot5 <- barplot(x, main='Barplot', col='violet')

#scatterplot
plot6 <- plot(Mydata$Sales, Mydata$Discount, main='Scatterplot', xlab='Sales', ylab='Discount', xlim=c(0,50), col = 'purple', cex=1, pch=5)

#density
j <- density(quan)
plot(j)







