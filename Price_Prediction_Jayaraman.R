install.packages('dplyr')
library(dplyr)
install.packages("corrplot", dependencies = TRUE) 
library(corrplot)
install.packages("psych", dependencies = TRUE) 
library(psych)
install.packages('Amelia', dependencies = TRUE)
library(Amelia)
install.packages('caret', dependencies = TRUE)
library(caret)
install.packages('plotly', dependencies = TRUE)
library(plotly)
Toyota<-read.csv(file="C:/Users/santh/Desktop/Master of Business Analytics-Australia/Semester 3/Predictive Analytics (PA)/Assignment-1/ToyotaCorolla.csv")
hist(Toyota$Price,col ="#66CCFF",border = "#000099",prob=TRUE,main = "Distribution in Price of ToyotaCorollo Models",xlab = "Price")
lines(density(Toyota$Price),lwd=2,col="red")
boxplot(Toyota$Price)
Toyota$Model<-trimws(gsub('\\?'," ",Toyota$Model),which="both")
Toyota$Model <- as.factor(Toyota$Model)
ggplot(Toyota, aes(y = Price)) +
  geom_boxplot(colour = "Red", fill = "#56B4E9",outlier.colour = "Red")+ggtitle("Boxplot Of Price")+
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 20, family="xkcd-Regular"),
        text=element_text(size = 16, family="xkcd-Regular"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))

missmap(Toyota,col=c('yellow','#FF0033'),y.at=1,y.labels='',legend=TRUE)

#Linear Regression

Toyota$Fuel_Type=as.numeric(Toyota$Fuel_Type)
is.numeric(Toyota$Fuel_Type)
glimpse(Toyota$Fuel_Type)
glimpse(Toyota)
Toyota$Fuel_Type
View(Toyota)
Toyota$Cylinders=NULL
M <- data.matrix(Toyota)
corrM <- cor(M)
corrplot(corrM,method = 'circle')
highlyCorrM <- findCorrelation(corrM,cutoff = 0.7)
names(Toyota)[highlyCorrM]
Toyota.selected <- data.frame(M[,-highlyCorrM])
Toyota.selected$Price <- Toyota$Price
View(Toyota.selected)
names(Toyota.selected)
corrplot(cor(Toyota.selected))
smp_size <- floor(2/3 * nrow(Toyota.selected))
set.seed(2)
Toyota.selected <-
  Toyota.selected[sample(nrow(Toyota.selected)), ]
Toyota.train <- Toyota.selected[1:smp_size, ]
Toyota.test <-
  Toyota.selected[(smp_size+1):nrow(Toyota.selected), ] 
formula = Price~.

#Model-1

model <- lm(formula = formula, data =Toyota.train)
summary(model)
Toyota.train$predicted.Price <- predict(model, Toyota.train)
Toyota.test$predicted.Price <- predict(model, Toyota.test)
print("Actual Values")
head(Toyota.test$Price[1:5])
print("Predicted Values")
head(Toyota.test$predicted.Price[1:5])
pl1 <-Toyota.test %>%
  ggplot(aes(Price,predicted.Price)) +
  geom_point(alpha=0.5) +
  stat_smooth(aes(colour='Black')) +
  xlab('Actual value of ToyotaCorolla Price') +
  ylab('Predicted value of ToyotaCorolla Price')+
  theme_bw()
ggplotly(pl1)
error <- Toyota.test$Price-Toyota.test$predicted.Price
rmse <- sqrt(mean(error^2))
print(paste("Root Mean Square Error: ", rmse))


#Model-2

Toyota.selected$Mfg_Year<-Toyota$Mfg_Year
Toyota.selected$Mfg_Year=NULL
Toyota.selected$Age_08_04<-Toyota$Age_08_04
View(Toyota.selected)
smp_size <- floor(2/3 * nrow(Toyota.selected))
set.seed(2)
Toyota.selected <-
  Toyota.selected[sample(nrow(Toyota.selected)), ]
Toyota.train1 <- Toyota.selected[1:smp_size, ]
Toyota.test1 <-
  Toyota.selected[(smp_size+1):nrow(Toyota.selected), ] 
formula = Price~.
model1<-lm(formula = formula,data=Toyota.train1)
summary(model1)
Toyota.train1$predicted.Price <- predict(model1, Toyota.train1)
Toyota.test1$predicted.Price <- predict(model1, Toyota.test1)
print("Actual Values")
head(Toyota.test1$Price[1:5])
print("Predicted Values")
head(Toyota.test1$predicted.Price[1:5])
pl1 <-Toyota.test1 %>%
  ggplot(aes(Price,predicted.Price)) +
  geom_point(alpha=0.5) +
  stat_smooth(aes(colour='Black')) +
  xlab('Actual value of ToyotaCorolla Price') +
  ylab('Predicted value of ToyotaCorolla Price')+
  theme_bw()
ggplotly(pl1)
error1 <- Toyota.test1$Price-Toyota.test1$predicted.Price
rmse1 <- sqrt(mean(error1^2))
print(paste("Root Mean Square Error: ", rmse1))

#Model-3

Toyota.selected$poweredwinfows<-Toyota$Powered_Windows
Toyota.selected$quaterlyTax<-Toyota$Quarterly_Tax
smp_size <- floor(2/3 * nrow(Toyota.selected))
set.seed(2)
Toyota.selected <-
  Toyota.selected[sample(nrow(Toyota.selected)), ]
Toyota.train2 <- Toyota.selected[1:smp_size, ]
Toyota.test2 <-
  Toyota.selected[(smp_size+1):nrow(Toyota.selected), ] 
formula = Price~.
model2<-lm(formula = formula,data=Toyota.train2)
summary(model2)
Toyota.train2$predicted.Price <- predict(model2, Toyota.train2)
Toyota.test2$predicted.Price <- predict(model2, Toyota.test2)
print("Actual Values")
head(Toyota.test2$Price[1:5])
print("Predicted Values")
head(Toyota.test2$predicted.Price[1:5])
pl1 <-Toyota.test2 %>%
  ggplot(aes(Price,predicted.Price)) +
  geom_point(alpha=0.5) +
  stat_smooth(aes(colour='Black')) +
  xlab('Actual value of ToyotaCorolla Price') +
  ylab('Predicted value of ToyotaCorolla Price')+
  theme_bw()
ggplotly(pl1)
error2 <- Toyota.test2$Price-Toyota.test2$predicted.Price
rmse2 <- sqrt(mean(error2^2))
print(paste("Root Mean Square Error: ", rmse2))




#Decision Tree

install.packages('rpart', dependencies = TRUE)
library(rpart)
install.packages('rpart.plot', dependencies = TRUE)
library(rpart.plot)

#Original Model

smp_size <- floor(2/3 * nrow(Toyota))
set.seed(2)
Toyota.selected1 <- Toyota[sample(nrow(Toyota)), ]
Toyota.selected1$Model=NULL
Toyota.selected1$Id=NULL
Toyota.traintree <- Toyota.selected1[1:smp_size, ]
Toyota.testtree <-
  Toyota.selected1[(smp_size+1):nrow(Toyota.selected1), ] 
formula = Price ~.
dtree <- rpart(formula, data=Toyota.traintree, method="anova")
rpart.plot(dtree, type = 4, fallen.leaves = FALSE)
print(dtree)
plotcp(dtree)
dtree$variable.importance
Toyota.testtree$predicted.price <- predict(dtree, Toyota.testtree)
error_Dtree <- Toyota.testtree$Price-Toyota.testtree$predicted.price
rmse_Dtree <- sqrt(mean(error_Dtree^2))
print(paste("Root Mean Square Error: ", rmse_Dtree))
printcp(dtree)


#Pruned_Tree_Model_1

pruned_dtree <- prune(dtree, cp = 0.014)
rpart.plot(pruned_dtree, type = 4, fallen.leaves = FALSE)
pruned_dtree$variable.importance
Toyota.testtree$predicted_pruned.price <- predict(pruned_dtree,
                                                      Toyota.testtree)
error_PDtree <- Toyota.testtree$Price-Toyota.testtree$predicted_pruned.price
rmse_PDtree <- sqrt(mean(error_PDtree^2))
print(paste("Root Mean Square Error: ", rmse_PDtree))

#Pruned_Tree_Model_2

pruned_dtree1 <- prune(dtree, cp = 0.024)
rpart.plot(pruned_dtree1, type = 4, fallen.leaves = FALSE)
pruned_dtree1$variable.importance
Toyota.testtree$predicted_pruned.price <- predict(pruned_dtree1,
                                                  Toyota.testtree)
error_PDtree1 <- Toyota.testtree$Price-Toyota.testtree$predicted_pruned.price
rmse_PDtree1 <- sqrt(mean(error_PDtree1^2))
print(paste("Root Mean Square Error: ", rmse_PDtree1))

#Pruned_Tree_Model_3

pruned_dtree2 <- prune(dtree, cp = 0.010)
rpart.plot(pruned_dtree2, type = 4, fallen.leaves = FALSE)
pruned_dtree2$variable.importance
Toyota.testtree$predicted_pruned.price <- predict(pruned_dtree2,
                                                  Toyota.testtree)
error_PDtree2 <- Toyota.testtree$Price-Toyota.testtree$predicted_pruned.price
rmse_PDtree2 <- sqrt(mean(error_PDtree2^2))
print(paste("Root Mean Square Error: ", rmse_PDtree2))

