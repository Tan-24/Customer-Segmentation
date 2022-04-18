library(readxl)
customer_data <- read_excel("C:/Users/Tanvi/Downloads/Mall_Customers.xlsx")
View(customer_data)

dim(customer_data)
str(customer_data)
names(customer_data)


head(customer_data)
summary(customer_data$Age)
sd(customer_data$Age)



summary(customer_data$Annualincome)
sd(customer_data$Annualincome)

summary(customer_data$Spendingscore)
sd(customer_data$Spendingscore)

a=table(customer_data$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))


pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")

summary(customer_data$Age)


hist(customer_data$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

boxplot(customer_data$Age,
        col="red",
        main="Boxplot for Descriptive Analysis of Age")


summary(customer_data$Annualincome)
hist(customer_data$Annualincome,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)


plot(density(customer_data$Annualincome),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(customer_data$Annualincome),
        col="#ccff66")


summary(customer_data$Spendingscore)

boxplot(customer_data$Spendingscore,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")


hist(customer_data$Spendingscore,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#6600cc",
     labels=TRUE)


library(cluster) 
set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)


k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6


