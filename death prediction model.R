### install packages and library
library(caret)
library(kernlab)
library(foreign)
library(mlbench)
library(rpart)
library(rpart.plot)
library(ipred)
library(randomForest)
library(gbm)
library(nnet)
library(DMwR)
library(pROC)
library(ggplot2)
library(ROSE)
library(doParallel)
registerDoParallel(6)
getDoParWorkers()

### Set working space and read data
setwd("C:/Users/user/Desktop/Model Selection/Model Selection/model selection 20170801")
mydata=read.csv("529 prospecive dataset 20170615.csv",header=TRUE,sep=",")

### Calculate the proportion of missing data in each column and row
pMiss=function(x){sum(is.na(x))/length(x)*100}
apply(mydata,2,pMiss)

### Imputating by mice
symptomvars=c("headache","dizziness","sore_thro","ches_tigh","dyspnea","ches_pain","nasa_cong","sneezing","runn_nose","dry_coug","prod_coug","sputum","hemoptysis","abdo_dist","abdo_pain","diarrhea","constipation","nausea","vomit","anorexia","urin_freq","oliguria","hematuria","diff_void","dysuria","flan_pain","gene_sore","musc_sore","convulsion","gene_weak","chills","shak_chil","cyanosis","fever","sweat","dry_lips","thirsty","cold_extr","malaise","drawsy","syncope","confusion","tachycardia","agitation","acut_conf","fluc_cour","inattention","diso_thin",
              "chfsum","cdsum","dementia","cpdsum","rdsum","pudsum","mldsum","hopsum","renalsum","leukemiasum","lymphomasum","msldsum","mstsum","tumorsum","dmsum","ckdsum","copdsum","liversum")
labtestvars=c("age","tmp","pulse","sbp","dbp","rr","spaO2","map","gcs","wbc","band","plt","bilt","aptt","pt","rdw","rbc","hb","mcv","alb","bun","inr","ast","mchc","eos","lactate","pct","crp","cre","sugar","c3_case","ca_case","uric_case","clchl_case","cortisol_case","inorP_case","ddimer_case","proteinc_case","fdp_case")
infectionvars=c("res_infection","GU_infection","skin_infection","abdominal_infection","cns_infection","msk_infection","other_infection")
labtestdata=mydata[labtestvars]
infectiondata=mydata[infectionvars]
symptomdata=mydata[symptomvars]
for(j in 1:ncol(symptomdata)){
  symptomdata[,j]=ifelse(is.na(symptomdata[,j]),0,symptomdata[,j])
}
for(j in 1:ncol(infectiondata)){
  infectiondata[,j]=ifelse(is.na(infectiondata[,j]),0,infectiondata[,j])
}
require(mice)
labtestdata=mice(labtestdata,
                 m=10,           
                 maxit=20,      
                 method="cart",
                 seed=1331)
numericdata=complete(labtestdata,10)
mydata=cbind(death=mydata$death,numericdata,infectiondata,symptomdata)
mydata=as.data.frame(mydata)

### Data engineering
mydata$death=as.factor(mydata$death)
mydata$si=mydata$pulse/mydata$sbp
mydata$pp=mydata$sbp-mydata$dbp
mydata$buncre=mydata$bun/mydata$cre
mydata$rdwrbc=mydata$rdw/mydata$rbc
mydata$ddimerfdp=mydata$ddimer_case/mydata$fdp_case
mydata$proteincc3=mydata$proteinc_case/mydata$c3_case
mydata$ppi=mydata$lactate/mydata$map

dim(mydata)

###¡@Zero- and Near Zero-Variance Predictors
nzv<-nearZeroVar(mydata)
filteredDescr<-mydata[,-nzv]
dim(filteredDescr)

### Identifying Correlated Predictors
descrCor<-cor(filteredDescr[,-1])
highlyCorDescr<-findCorrelation(descrCor,cutoff=0.75)
mydata_1<-filteredDescr[,-highlyCorDescr]
mydata=mydata_1
mydata=as.data.frame(mydata)

N=300
overallvars=c(colnames(mydata))
result=matrix(0,nrow=length(overallvars),ncol=N)

for(i in 1:N){
              mydata1=mydata[which(mydata$death=="Survival"),]
              mydata2=mydata[which(mydata$death=="Death"),]
              index1=sample(dim(mydata1)[1],nrow(mydata1)*0.25)
              index2=sample(dim(mydata2)[1],nrow(mydata2)*0.25)
              testing=rbind(mydata1[index1,],mydata2[index2,])
              training=rbind(mydata1[-index1,],mydata2[-index2,])
              smote_train<-SMOTE(death~.,data=training)
              table(smote_train$death)
                              
            ### Feature selction by RFE algorithm
              control=rfeControl(functions=rfFuncs,method="cv",number=10)
              results_RFE=rfe(smote_train[,-1],smote_train[,1],sizes=c(1:10,15,20,25,30,35,39),rfeControl=control)
              print(results_RFE)
              predictors(results_RFE)
              for (j in 1:length(overallvars)){
                                               result[j,i]=results_RFE[[6]][j]                        
                  }
              cat('Processing', i, 'of', N,'\n')
}
result=as.matrix.data.frame(result)
dim(result)

for (i in 1:nrow(result)){
  for (j in 1:ncol(result)){
        if (is.na(result[i,j])){result[i,j]=0} 
  }
}

count=array(0,length(overallvars))
for (i in 1:nrow(result)){
  for (j in 1:ncol(result)){
      for(k in 1:length(overallvars)){                          
          if (result[i,j]==overallvars[k]){count[k]=count[k]+1}
     }
  }
}
count=as.data.frame(count)
colnames(count)<-c("selectnum")
rownames(count)<-overallvars
printCoefmat(count)
count$rowname<-overallvars
selected.list<-count[which(count$selectnum>=N*0.9),]
selected.list

mydata1=mydata[which(mydata$death=="Survival"),]
mydata2=mydata[which(mydata$death=="Death"),]
index1=sample(dim(mydata1)[1],nrow(mydata1)*0.25)
index2=sample(dim(mydata2)[1],nrow(mydata2)*0.25)
testing=rbind(mydata1[index1,],mydata2[index2,])
training=rbind(mydata1[-index1,],mydata2[-index2,])
smote_train<-SMOTE(death~.,data=training)
table(smote_train$death)

fitControl=trainControl(method="repeatedcv",
                        number=10,
                        repeats=10,
                        classProbs=TRUE,   
                        summaryFunction=twoClassSummary,
                        search="random")
### 1. Training for tree bagging
system.time(train_treebag<-train(death~alb+inorP_case+cortisol_case+ppi+lactate+rr+rdwrbc+gcs+pt,
                                 data=smote_train,
                                 method="treebag",
                                 metric="ROC",
                                 tuneLength=5000,
                                 trControl=fitControl))
trellis.par.set(caretTheme())
plot(train_treebag)

train_treebag
predictions_train=predict(train_treebag,newdata=training)
predictions_test=predict(train_treebag,newdata=testing)
predictions_train
predictions_test
confusionMatrix(predict(train_treebag,training),training$death)
confusionMatrix(predict(train_treebag,testing),testing$death)
train_results=predict(train_treebag,training,type="prob")
test_results=predict(train_treebag,testing,type="prob")
train_results$obs=training$death
train_results$pred=predictions_train
test_results$obs=testing$death
test_results$pred=predictions_test
train_results
test_results
treebagmodelROC_train<-roc(training$death,train_results[,"Death"],levels=c("Survival","Death"))
treebagmodelROC_test<-roc(testing$death,test_results[,"Death"],levels=c("Survival","Death"))
windows()
plot(treebagmodelROC_train)
treebagmodelROC_train
treebagmodelROC_test
plot(treebagmodelROC_test,add=TRUE,col="red")
legend("bottomright",legend=c("Training, AUC=0.9641","Testing, AUC=0.8660"),
       col=c("black","red"),lwd=2)


### 2. Training for Support Vector Machine
grid<-expand.grid(sigma=10^(-4:-1),C=c(1:100)/30)
svm_train<-train(death~alb+inorP_case+cortisol_case+ppi+lactate+rr+rdwrbc+gcs+pt,
                 data=smote_train,
                 method="svmRadial",
                 preProc=c("center","scale"),
                 metric="ROC",
                 tuneGrid=grid,
                 trControl=fitControl)
svm_train
predictions_train=predict(svm_train,newdata=training)
predictions_test=predict(svm_train,newdata=testing)
predictions_train
predictions_test
confusionMatrix(predict(svm_train,training),training$death)
confusionMatrix(predict(svm_train,testing),testing$death)
train_results=predict(svm_train,training,type="prob")
test_results=predict(svm_train,testing,type="prob")
train_results$obs=training$death
train_results$pred=predictions_train
train_results$pred1=ifelse(train_results[,1]>=mean(train_results[,1]),"Death","Survival")
train_results$pred2=ifelse(train_results[,1]>=median(train_results[,1]),"Death","Survival")
test_results$obs=testing$death
test_results$pred=predictions_test
test_results$pred1=ifelse(test_results[,1]>=mean(train_results[,1]),"Death","Survival")
test_results$pred2=ifelse(test_results[,1]>=median(train_results[,1]),"Death","Survival")
A=table(train_results$pred1,training$death)
B=table(test_results$pred1,testing$death)
C=table(train_results$pred2,training$death)
D=table(test_results$pred2,testing$death)
sensitivity_train=A[1,1]/(A[1,1]+A[2,1])
specificity_train=A[2,2]/(A[1,2]+A[2,2])
sensitivity_test=B[1,1]/(B[1,1]+B[2,1])
specificity_test=B[2,2]/(B[1,2]+B[2,2])
accuracy_train=(A[1,1]+A[2,2])/(A[1,1]+A[2,2]+A[2,1]+A[1,2])
accuracy_test=(B[1,1]+B[2,2])/(B[1,1]+B[2,2]+B[2,1]+B[1,2])
sensitivity_train
specificity_train
sensitivity_test
specificity_test
accuracy_train
accuracy_test
mean(train_results[,1])
mean(test_results[,1])
median(train_results[,1])
median(test_results[,1])

sensitivity_train1=C[1,1]/(C[1,1]+C[2,1])
specificity_train1=C[2,2]/(C[1,2]+C[2,2])
sensitivity_test1=D[1,1]/(D[1,1]+D[2,1])
specificity_test1=D[2,2]/(D[1,2]+D[2,2])
accuracy_train1=(C[1,1]+C[2,2])/(C[1,1]+C[2,2]+C[2,1]+C[1,2])
accuracy_test1=(D[1,1]+D[2,2])/(D[1,1]+D[2,2]+D[2,1]+D[1,2])
sensitivity_train1
specificity_train1
sensitivity_test1
specificity_test1
accuracy_train1
accuracy_test1
dim(train_results)
hist(train_results[1:367,1])
hist(train_results[368:397,1],add=TRUE,col="red")

dim(test_results)
hist(test_results[1:122,1])
hist(test_results[123:132,1],add=TRUE,col="red")

ggplot(test_results,aes(Death, fill = death)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
svmmodelROC_train<-roc(training$death,train_results[,"Death"],levels=c("Survival","Death"))
svmmodelROC_test<-roc(testing$death,test_results[,"Death"],levels=c("Survival","Death"))
windows()
plot(svmmodelROC_train)
svmmodelROC_train
svmmodelROC_test
plot(svmmodelROC_test,add=TRUE,col="red")
legend("bottomright",legend=c("Training, AUC=0.9568","Testing, AUC=0.782"),
       col=c("black","red"),lwd=2)

### 2-D plots for SVM classification plots
require('manipulate')
# Train a nonlinear SVM
nonlinear.svm<-ksvm(death~rr+rdwrbc,
                    data=smote_train,
                    type='C-svc',
                    kernel='rbf',
                    kpar=list(sigma=1),
                    C=100,
                    scale=c())
plot(nonlinear.svm,data=smote_train)

## 3-D plots for SVM classification plots
library(e1071)
library(rgl)
library(misc3d)

## death~alb+ppi+cortisol_case+inorP_case+lactate+rr+rdwrbc
# Fit SVM
smote_train1=smote_train[,c("death","alb","ppi","rdwrbc")]
smote_train1=as.data.frame(smote_train1)
smote_train1$death=as.factor(smote_train1$death)
svm_model=svm(death~alb+ppi+rdwrbc,
              data=smote_train1,
              type='C-classification',
              kernel='linear',
              scale=FALSE)

w<-t(svm_model$coefs)%*%svm_model$SV
detalization<-100 
grid<-expand.grid(seq(from=min(smote_train1$alb),
                      to=max(smote_train1$alb),
                      length.out=detalization),     
                  seq(from=min(smote_train1$ppi),
                      to=max(smote_train1$ppi),
                      length.out=detalization)) 
rdwrbc<-(svm_model$rho-w[1,1]*grid[,1]-w[1,2]*grid[,2])/w[1,3] 
plot3d(grid[,1],
       grid[,2],
       rdwrbc,
       xlab="alb",
       ylab="ppi",
       zlab="rdwrbc",
       col="blue",
       main="3-D plot for SVM classification",
       top=TRUE,
       lwd=8)

plot3d(smote_train1$alb[which(smote_train1$death=="Death")],
       smote_train1$ppi[which(smote_train1$death=="Death")], 
       smote_train1$rdwrbc[which(smote_train1$death=="Death")], 
       col='red',
       size=12,
       add=TRUE) 

plot3d(smote_train1$alb[which(smote_train1$death=="Survival")],
       smote_train1$ppi[which(smote_train1$death=="Survival")], 
       smote_train1$rdwrbc[which(smote_train1$death=="Survival")], 
       col='green',
       add=TRUE,
       size=8)

### 3. Training for Gradient boosting model
system.time(gbm_train<-train(death~alb+inorP_case+cortisol_case+gcs+ast+rdw,
                             data=smote_train,
                             method="gbm",                 
                             tuneLength=25,                 
                             verbose=FALSE,
                             metric="ROC",
                             trControl=fitControl))
gbm_train
predictions_train=predict(gbm_train,newdata=training)
predictions_test=predict(gbm_train,newdata=testing)
predictions_train
predictions_test
confusionMatrix(predict(gbm_train,training),training$death)
confusionMatrix(predict(gbm_train,testing),testing$death)
train_results=predict(gbm_train,training,type="prob")
test_results=predict(gbm_train,testing,type="prob")
train_results$obs=training$death
train_results$pred=predictions_train
test_results$obs=testing$death
test_results$pred=predictions_test
train_results
test_results
gbmmodelROC_train<-roc(training$death,train_results[,"Death"],levels=c("Survival","Death"))
gbmmodelROC_test<-roc(testing$death,test_results[,"Death"],levels=c("Survival","Death"))
windows()
plot(gbmmodelROC_train)
gbmmodelROC_train
gbmmodelROC_test
lines(gbmmodelROC_test,add=TRUE,col="red")

### 4. Training for Neural Network
system.time(nnet_train<-train(death~alb+inorP_case+cortisol_case+gcs+ast+rdw,
                              data=smote_train,
                              method="nnet",                  
                              tuneLength=1000,                 
                              verbose=FALSE,
                              metric="ROC",
                              trControl=fitControl))
nnet_train
predictions_train=predict(nnet_train,newdata=training)
predictions_test=predict(nnet_train,newdata=testing)
predictions_train
predictions_test
confusionMatrix(predict(nnet_train,training),training$death)
confusionMatrix(predict(nnet_train,testing),testing$death)
train_results=predict(nnet_train,training,type="prob")
test_results=predict(nnet_train,testing,type="prob")
train_results$obs=training$death
train_results$pred=predictions_train
test_results$obs=testing$death
test_results$pred=predictions_test
train_results
test_results
nnetmodelROC_train<-roc(training$death,train_results[,"Death"],levels=c("Survival","Death"))
nnetmodelROC_test<-roc(testing$death,test_results[,"Death"],levels=c("Survival","Death"))
windows()
plot(nnetmodelROC_train)
nnetmodelROC_train
nnetmodelROC_test
lines(nnetmodelROC_test,add=TRUE,col="red")


### 5. Traingin for general linear model boosting
glmBoostGrid=expand.grid(mstop=c(50,100,150,200,250,300),
                         prune=c('yes','no'))

glmboost_train=train(death~alb+inorP_case+cortisol_case+gcs+ast+rdw,
                     data=smote_train,
                     method="glmboost",
                     trControl=fitControl,
                     tuneGrid=glmBoostGrid,
                     metric='ROC')
glmboost_train
predictions_train=predict(glmboost_train,newdata=training)
predictions_test=predict(glmboost_train,newdata=testing)
predictions_train
predictions_test
confusionMatrix(predict(glmboost_train,training),training$death)
confusionMatrix(predict(glmboost_train,testing),testing$death)
train_results=predict(glmboost_train,training,type="prob")
test_results=predict(glmboost_train,testing,type="prob")
train_results$obs=training$death
train_results$pred=predictions_train
test_results$obs=testing$death
test_results$pred=predictions_test
train_results
test_results
glmmodelROC_train<-roc(training$death,train_results[,"Death"],levels=c("Survival","Death"))
glmmodelROC_test<-roc(testing$death,test_results[,"Death"],levels=c("Survival","Death"))
windows()
plot(glmmodelROC_train)
glmmodelROC_train
glmmodelROC_test
lines(glmmodelROC_test,add=TRUE,col="red")

### 6. Training for generalized additive model
system.time(gam_train<-train(death~alb+inorP_case+cortisol_case+gcs+ast+plt+rdw+lactate+rbc+mcv+pt+inr+pct,
                             data=smote_train,
                             method="gamLoess",            
                             tuneLength=100,                 
                             verbose=FALSE,
                             metric="ROC",
                             trControl=fitControl))
gam_train
predictions_train=predict(gam_train,newdata=training)
predictions_test=predict(gam_train,newdata=testing)
predictions_train
predictions_test
confusionMatrix(predict(gam_train,training),training$death)
confusionMatrix(predict(gam_train,testing),testing$death)
train_results=predict(gam_train,training,type="prob")
test_results=predict(gam_train,testing,type="prob")
train_results$obs=training$death
train_results$pred=predictions_train
test_results$obs=testing$death
test_results$pred=predictions_test
train_results
test_results
gammodelROC_train<-roc(training$death,train_results[,"Death"],levels=c("Survival","Death"))
gammodelROC_test<-roc(testing$death,test_results[,"Death"],levels=c("Survival","Death"))
windows()
plot(gammodelROC_train)
gammodelROC_train
gammodelROC_test
lines(gammodelROC_test,add=TRUE,col="red")
legend("bottomright",legend=c("Training, AUC=0.9552","Testing, AUC=0.9094"),
       col=c("black","red"),lwd=2)


### 7. Training for K-nearest Neighbor method
system.time(knn_train<-train(death~alb+inorP_case+cortisol_case+gcs+ast+plt+rdw+lactate+rbc+mcv+pt+inr+pct,
                             data=smote_train,
                             method="knn",
                             metric="ROC",
                             tuneLength=100,
                             trControl=fitControl))
knn_train
predictions_train=predict(knn_train,newdata=training)
predictions_test=predict(knn_train,newdata=testing)
predictions_train
predictions_test
confusionMatrix(predict(knn_train,training),training$death)
confusionMatrix(predict(knn_train,testing),testing$death)
train_results=predict(knn_train,training,type="prob")
test_results=predict(knn_train,testing,type="prob")
train_results$obs=training$death
train_results$pred=predictions_train
test_results$obs=testing$death
test_results$pred=predictions_test
train_results
test_results
knnmodelROC_train<-roc(training$death,train_results[,"Death"],levels=c("Survival","Death"))
knnmodelROC_test<-roc(testing$death,test_results[,"Death"],levels=c("Survival","Death"))
windows()
plot(knnmodelROC_train)
knnmodelROC_train
knnmodelROC_test
lines(knnmodelROC_test,add=TRUE,col="red")


### 8. Training for Naive Bayesian Classification method
system.time(nb_train<-train(death~alb+inorP_case+cortisol_case+gcs+ast+plt+rdw+lactate+rbc+mcv+pt+inr+pct,
                            data=smote_train,
                            method="nb",
                            metric="ROC",
                            tuneLength=100,
                            trControl=fitControl))
nb_train
predictions_train=predict(nb_train,newdata=training)
predictions_test=predict(nb_train,newdata=testing)
predictions_train
predictions_test
confusionMatrix(predict(nb_train,training),training$death)
confusionMatrix(predict(nb_train,testing),testing$death)
train_results=predict(nb_train,training,type="prob")
test_results=predict(nb_train,testing,type="prob")
train_results$obs=training$death
train_results$pred=predictions_train
test_results$obs=testing$death
test_results$pred=predictions_test
train_results
test_results
nbmodelROC_train<-roc(training$death,train_results[,"Death"],levels=c("Survival","Death"))
nbmodelROC_test<-roc(testing$death,test_results[,"Death"],levels=c("Survival","Death"))
windows()
plot(nbmodelROC_train)
nbmodelROC_train
nbmodelROC_test
lines(nbmodelROC_test,add=TRUE,col="red")

