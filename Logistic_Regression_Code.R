#------------------------------------------
############## FINAL PROJECT ##############
#------------------------------------------

# Clear workspace
rm(list=ls())

# Load packages
library(caret) #sampling
install.packages("mice")
library(mice) #missing values imputation

## Read in Data
data <- read.csv("data.csv")

############## DATA PROCESSING ##############
## Get dataset with CKD disease status 
#(The original dataset has observations without CKD disease status, used as testing set by professor) )
out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status
summary(data_in)

## Impute data using mice package
imputed <- mice(data_in,m=5,meth='pmm')  #m=5 generates 5 different imputations
imputed_in <- complete(imputed,3) #choose the 3rd imputation. can choose whichever out of 5

summary(imputed_in)

## Remove 2 obs with missing value in CareSource - can't impute for categorical variable
which(imputed_in$CareSource==" ")
imputed_in=imputed_in[-c(925, 5934),]
dim(imputed_in)
summary(imputed_in)

imputed_in$CareSource=factor(imputed_in$CareSource)

## Split Training & Validation set
samp <- createDataPartition(imputed_in$CKD, p=.75, list=FALSE)
train = imputed_in[samp, ] 
test = imputed_in[-samp, ]

############## MODEL DEVELOPMENT ##############
## FULL MODEL 
# Run the Logistic Regression on all data, explore backward elimination
dim(train)
model=glm(CKD~.,family="binomial",data=train)
summary(model)

## BACKWARD ELIMINATION MODEL
# Run backward Elimination
model3=step(model,direction="backward")

# Explore your new model
formula(model3)
summary(model3)

## REDUCED MODEL
# Add 1 more model to use significant variables only from the backward model
model4=glm(CKD~Age+Female+Racegrp+PVD+Hypertension+Diabetes+CVD+CHF+Anemia,family="binomial",data=train)
summary(model4)

############## EVALUATION ##############
## PREDICT & CLASSIFICATION
# Note: the code at the end needs to be run first
# Backward Elimination Model
phat3=predict(model3,newdata = validation, type="response")
summary(phat3)  # probabilities range 0.03% to 79%

classify=ifelse(phat3>.5,1,0)  #try first at cutoff level of 0.5
summary(classify)  

c_accuracy(validation$CKD,classify)  
cc=c_accuracy(validation$CKD,classify)
round(cc,5)

# Reduced Model
phat5=predict(model4,newdata = validation, type="response")
summary(phat4) #prob range 0.07% to 76.9%

classify=ifelse(phat4>.5,1,0)  #try first at cutoff level of 0.5
summary(classify)  

c_accuracy(validation$CKD,classify)  
cc=c_accuracy(validation$CKD,classify)
round(cc,5)

## Caclculate Costs & profit
acc=c_accuracy(validation$CKD,classify)
c1=1300   # earn $1300 for a true positive
c2=100  #  penalize $100 for a false positive
cost = acc[9]*c2
revenue = acc[7]*c1
profit=revenue-cost

cost
revenue
profit   

## Cutoff value vs cost/profit
i=1
k=0.01 ## cutt off value
profit ={}
tpr={}
fpr={}
accuracy={}
m={}

## run a while loop for cuttoff values 0.01 to 1

while( k <= 1){  
  classify=ifelse(phat3> k,1,0) 
  acc=c_accuracy(validation$CKD,classify)
  c1=1300   # earn $1300 for a true positive
  c2=100  # penalize $100 for a false positive
  
  profit[i]= acc[7]*c1-acc[9]*c2 #care about profit only
  tpr[i] = acc[4]
  fpr[i]=acc[5]
  accuracy[i]= acc[3]
  m[i]=k
  
  
  
  k=k+0.01 ## increment the cuttoff value by 0.01
  
  i=i+1 ## i represents the index of the cutoff value. 
  
  
  
}

plot(m, profit, xlab="cutoff value", main = "Cut off value vs Profit")
lines(tpr)


## --------------------------------
## Function Below, RUN THIS FIRST
##  Built by Matthew J. Schneider

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(scores);
}