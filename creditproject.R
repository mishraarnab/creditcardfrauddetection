#importing dataset
```{r}
credit_card<-read.csv(file.choose(),header = T)
str(credit_card)
#converting interger value of Class into factor
credit_card$Class<-factor(credit_card$Class, levels= c(0,1))
 str(credit_card)
 summary(credit_card)
 #cheacking for missing value
 sum(is.na(credit_card))
 
 ```
 
#-----------------------------------------------------
 #get the distribution of fraud and legit cases in the dataset
 table(credit_card$Class)
#to get the percentage of fraud and legit cases
 prop.table(table(credit_card$Class))
#pie chart of credit card transactions
 labels<-c("legit","fraud")
 labels<-paste(labels, round(100*prop.table(table(credit_card$Class)),2))
 labels<-paste0(labels,"%")

 pie(table(credit_card$Class),labels, col=c("orange","red"),main=("pie chart distribution of legit and fraud cases"))


 #cheak the accuracy by no model prediction
  predictions<-rep.int(0,nrow(credit_card))
predictions<-factor(predictions,levels=c(0,1))
predictions

#to cheak the confusion matrix
install.packages('caret')
library(caret)
library(e1071)
confusionMatrix(data=predictions,reference=credit_card$Class)
#---------------------------------------------------

#taking small subset of data because it is easy to compute faster

install.packages("dplyr")
library(dplyr)
set.seed(1)
credit_card<-credit_card %>% sample_frac(0.1)
View(credit_card)
table(credit_card)


#plotting the scatter plot
install.packages("ggplot2")
library(ggplot2)
ggplot(data=credit_card,aes(x=V1,y=V2,color=Class))+geom_point()+theme_bw()+scale_color_manual(values=c('blue','red'))

#-----------------------------------------------------------------------------------------------

#creating test and training dataset

install.packages('caTools')
library(caTools)
set.seed(123)
data_sample=sample.split(credit_card$Class,SplitRatio = 0.80)
train_data=subset(credit_card,data_sample==TRUE)
test_data=subset(credit_card,data_sample==FALSE)
dim(train_data)
dim(test_data)

#---------------------------------------------------------

table(train_data$Class)
n_legit<-22750
n_frac_legit<-0.50
new_n_total<-n_legit/n_frac_legit
new_n_total

install.packages('ROSE')
library(ROSE)
oversampling_result<-ovun.sample(Class~.,data=train_data,method="over",N=new_n_total,seed=2020)
View(oversampling_result)
oversampled_credit<-oversampling_result$data
table(oversampled_credit$Class)
#plotting of the distribution
ggplot(data=oversampled_credit,aes(x=V1,y=V2,color=Class))+geom_point(position=position_jitter(width=0.1))+theme_bw()+scale_color_manual(values=c('blue','red'))


#---------------------------------------------------------------------------

#random undersampling
table(train_data$Class)

#plotting of the distributionn_fraud<-35
n_frac_fraud<-0.50
new_n_total<-n_fraud/n_frac_fraud
new_n_total


undersampling_result<-ovun.sample(Class~.,data=train_data,method="under",N=new_n_total,seed=2020)
View(undersampling_result)
undersampled_credit<-undersampling_result$data
table(undersampled_credit$Class)
ggplot(data=undersampled_credit,aes(x=V1,y=V2,color=Class))+geom_point()+theme_bw()+scale_color_manual(values=c('blue','red'))


#-------------------------------------------------------------------------------------

#doing random over sample and random under sample

n_new<-nrow(train_data)
fraction_fraud_new<-0.50

sampling_result<-ovun.sample(Class~.,data=train_data,method="both",N=n_new,p=fraction_fraud_new,seed=2020)
View(sampling_result)
sampled_credit<-sampling_result$data
table(sampled_credit$Class)
prop.table(table(sampled_credit$Class))
ggplot(data=sampled_credit,aes(x=V1,y=V2,color=Class))+geom_point(position=position_jitter(width=0.2))+theme_bw()+scale_color_manual(values=c('blue','red'))

#----------------------------------------------------------

#using SMOTE to balance the dataset

install.packages('smotefamily')
library(smotefamily)
table(train_data$Class)


n0<-22750
n1<-35
r0<-0.55


ntimes<-((1-r0)/r0)*(n0/n1)-1
smote_output=SMOTE(X=train_data[,-c(1,31)],target=train_data$Class,K=5,dup_size=ntimes)
view(smote_output)
credit_smote<-smote_output$data
colnames(credit_smote)[30]<-"Class"
prop.table(table(credit_smote$Class))

ggplot(train_data,aes(x=V1,y=V2,color=Class))+geom_point()+scale_color_manual(values=c('blue','red'))

ggplot(data=credit_smote,aes(x=V1,y=V2,color=Class))+geom_point()+scale_color_manual(values=c('blue','red'))

#--------------------------------------------------------------


#lets build a decision tree
install.packages('rpart')
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)
CART_model<-rpart(Class~.,credit_smote)
rpart.plot(CART_model,extra=0,type=5,tweak = 1.2)

predicted_val<- predict(CART_model,test_data,type = 'class')

#building confussion matrix with respect to test_data
library(caret)
confusionMatrix(predicted_val,test_data$Class)
#---------------------------------------------------

#confusion matrix of the whole credit_card data using SMOTE

predicted_val<-predict(CART_model,credit_card[-1],type = 'class')
confusionMatrix(predicted_val,credit_card$Class)

#-----------------------------------------------------------------------------
#decision tree without SMOTE

CART_model<-rpart(Class~.,train_data[,-1])
rpart.plot(CART_model,extra = 0,type=5,tweak=1.2)

#predict the confusion data with respect to the test_data

predicted_val<-predict(CART_model,test_data[,-1],type = 'class')
library(caret)
confusionMatrix(predicted_val,test_data$Class)

#--------------------------------------------------
#confusion matrix of the whole credit_card data without using SMOTE

predicted_val<-predict(CART_model,credit_card[,-1],type = 'class')
confusionMatrix(predicted_val,credit_card$Class)

#----------------------------------------------------------