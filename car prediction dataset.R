car<-read.csv(choose.files())
dim(car)
head(car)
car<-car[,-c(1,3)]
head(car)
str(car)

colSums(is.na(car))
install.packages("mice")
library(mice)
colSums(is.na(car))
str(car)

boxplot(car)

install.packages("caret")
library(caret)
?dummyVars

dummy<-dummyVars("~ .",data=car)
dummy
car<-data.frame(predict(dummy,newdata=car))
car                
head(car)

str(car)
car
head(car,50)
names(car)
car<-car[,-c(2,4,6,8,14,16,19,24,30,38)]
str(car)
boxplot(car)


######################
#model for training & Test
install.packages("caTools")
library(caTools)

set.seed(123)
split<-sample.split(car$price,SplitRatio=0.75)
split
table(split)

training<-subset(car,split==T)
test<-subset(car,split=F)
nrow(training)
nrow(test)
cor(car)# to check the correlation
#?heatmap
install.packages("corrgram")
library(corrgram)
heatmap(cor(car))
#corrgram(Boston)
################################
#building a linear regression model with training dataset
names(car)
reg<-lm(price~.,data=training)
summary(reg)
#reg_1<-lm(price~.-symboling-aspirationturbo-doornumbertwo-drivewheel4wd
          #-drivewheelrwd-carheight)instead of removing all non significant values, we can write only significant using forward approach
#forward approach
reg_1<-lm(price~fueltypegas+carbodyhardtop+carbodyhatchback+carbodysedan
          +carbodywagon+enginelocationrear+carwidth+curbweight+cylindernumberfive
          +cylindernumberfour+cylindernumbersix+enginesize+boreratio+stroke+compressionratio
          +peakrpm,data=training)
summary(reg_1)

reg_1<-lm(price~fueltypegas+enginelocationrear+carwidth+curbweight
          +cylindernumberfive+cylindernumberfour+cylindernumbersix+enginesize+stroke
          +compressionratio+peakrpm,data=training)
summary(reg_1)

#predict the model with test dataset
car_pred<-predict(reg_1,newdata=test)
car_pred

test_cbind<-cbind(test$price,car_pred)
test_cbind

plot(test$price~test$peakrpm,type='l')
plot(car_pred)

install.packages("faraway")
library(faraway)
install.packages("lmtest")
library(lmtest)
vif(reg_1)
dwtest(reg_1)
