#     Installing Package                 #
#----------------------------------------#
install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")
install.packages("caret")
install.packages("corrplot")
install.packages("cowplot")
install.packages('e1071')
install.packages("randomForest")
install.packages('RColorBrewer')
install.packages('gridExtra')
install.packages('corrplot')
install.packages('ggthemes')
install.packages("Amelia")
install.packages("rlang")
install.packages("ggthemes")
##########################################
#                                        #
#     Loading Libraries                  #
#                                        #
##########################################


# corrplot is needed for correlation plots
library(data.table) # used for reading and manipulation of data
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plyr)
library(corrplot)
library(gridExtra)
library(e1071)
library(gridBase) # Draw Viz
library(gtable) # Viz
library(vcd) # mosaic
library(Hmisc) # rcorr
library(Amelia) # missmap
library(ROCR) 
library(e1071)
library(rpart)
library(randomForest)
library(caret)      # used for modeling
#Remove objects from R memory and collect garbage
rm(list=ls()) ; gc()

options("scipen" = 10) # make sure all the plot have no exponential mark -> show real values

path <- "C:/Users/altynbek/Documents/MasterDataScience/data"
#set working directory
setwd(path)
getwd()

data_raw<- read.csv("creditcard.csv", skip = 1)
#  1.Data Preparation and Visualization
## Data Data Preparation and Visualization procedures consists of the following:
## 1. Initial Summary and Dimension of Datasets;
## 2. Data Manupulation  Datasets;
## 3. Identify Missing Value;
## 4. Addign New Feature Variable;
## 5. Univariate Graphs (Categorical anf Numerical);
## 6. Bivariate Graphs (Categorical anf Numerical);
## 7. Multivariate Graphs (Grouping);
## 8. Maps (density);
##------------------------------------------------------------
# 1. Initial Summary and Dimension of Datasets
##------------------------------------------------------------
dim(data_raw)
str(data_raw)
summary(data_raw)

##------------------------------------------------------------
# 2. Data Manupulation and Univariate Analysis;
##------------------------------------------------------------
## Distribution by gender
data_raw$SEX <- as.factor(data_raw$SEX)
levels(data_raw$SEX) <- c("Male","Female")
dib1=data_raw %>% group_by(SEX) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = SEX, y = pct)) + geom_bar(stat = "identity", fill='blue',aes(color = I('black')), size = 0.1)+xlab("Gender") +
  ylab("Percent")+ theme_few()

# Distrubution bt Education
# Combining Unkown and Other categories as Other.
data_raw$EDUCATION <- as.factor(data_raw$EDUCATION)
levels(data_raw$EDUCATION) <- c("Other","Graduate","University","High School","Other","Other","Other")
dib3=data_raw  %>% group_by(EDUCATION) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = EDUCATION, y = pct)) + geom_bar(stat = "identity", fill ="#FF5555", aes(color = I('black')), size = 0.1)+xlab("EDUCATION") +
  ylab("Percent")+ theme_few()

## Distrubution by marriage
data_raw$MARRIAGE <- as.factor(data_raw$MARRIAGE)
levels(data_raw$MARRIAGE) <- c("Other","Married","Single","Other")
dib2=data_raw  %>% group_by(MARRIAGE) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = MARRIAGE, y = pct)) + geom_bar(stat = "identity", fill ="blue", aes(color = I('black')), size = 0.1)+xlab("Marriage") +
  ylab("Percent")+ theme_few()


# Distrubition by default payment
# Duplicate Default payment column 
data_raw <- data_raw %>%   mutate(DEFAULT= default.payment.next.month)

#data_raw <- data_raw %>% dplyr::rename("DEFAULT"="default.payment.next.month")
data_raw$DEFAULT <- as.factor(data_raw$DEFAULT)
data_raw %>% group_by(DEFAULT) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = DEFAULT, y = pct)) + geom_bar(stat = "identity", fill ="#FF6666", aes(color = I('black')), size = 0.1)+xlab("DEFAULT") +
  ylab("Percent")+ theme_few()

# Convert default.payment.next.month level Data (0,1) with (No, Yes)
data_raw$default.payment.next.month <- as.factor(data_raw$default.payment.next.month)
levels(data_raw$default.payment.next.month) <- c("No" , "Yes")
data_raw$default.payment.next.month <- as.factor(data_raw$default.payment.next.month)
# Distrubution by Age
# divide into age group
age_diff <- c(18,25,30,40,50,60,100)
age_cat <- c("18-24","25-29","30-39","40-49","50-59","60-100")
data_raw$AGE_GROUP <- cut(data_raw$AGE,breaks= age_diff,include.lowest=TRUE, right=FALSE, labels=age_cat)
summary(data_raw$AGE_GROUP)
dib4=data_raw  %>% group_by(AGE_GROUP) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = AGE_GROUP, y = pct)) + geom_bar(stat = "identity", fill ="#FF5555", aes(color = I('black')), size = 0.1)+xlab("AGE_GROUP") +
  ylab("Percent")+ theme_few()

grid.arrange(dib1,dib2,dib3) 

# Distribution of repayment status
# * -1 = pay duly
# * 1 = payment delay for one month
# * 2 = payment delay for two months 
# Convert Repayment Status columns to Factors
data_raw$PAY_0 <- as.factor(data_raw$PAY_0)
data_raw$PAY_2 <- as.factor(data_raw$PAY_2)
data_raw$PAY_3 <- as.factor(data_raw$PAY_3)
data_raw$PAY_4 <- as.factor(data_raw$PAY_4)
data_raw$PAY_5 <- as.factor(data_raw$PAY_5)
data_raw$PAY_6 <- as.factor(data_raw$PAY_6)

pay1<-data_raw %>% group_by(PAY_0) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_0, y = pct)) + geom_bar(stat = "identity", fill ="#6670ff", aes(color = I('black')), size = 0.1)+xlab("PAY_0") +
  ylab("Percent")+ theme_few()

pay2<-data_raw %>% group_by(PAY_2) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_2, y = pct)) + geom_bar(stat = "identity", fill ="#6670ff", aes(color = I('black')), size = 0.1)+xlab("PAY_2") +
  ylab("Percent")+ theme_few()
pay3<-data_raw %>% group_by(PAY_3) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_3, y = pct)) + geom_bar(stat = "identity", fill ="#6670ff", aes(color = I('black')), size = 0.1)+xlab("PAY_3") +
  ylab("Percent")+ theme_few()
pay4<-data_raw %>% group_by(PAY_4) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_4, y = pct)) + geom_bar(stat = "identity", fill ="#6670ff", aes(color = I('black')), size = 0.1)+xlab("PAY_4") +
  ylab("Percent")+ theme_few()
pay5<-data_raw %>% group_by(PAY_5) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_5, y = pct)) + geom_bar(stat = "identity", fill ="#6670ff", aes(color = I('black')), size = 0.1)+xlab("PAY_5") +
  ylab("Percent")+ theme_few()

grid.arrange(pay1,pay2,pay3,pay4,pay5)



## Distrubution by Age

# Plot a graph to check which age group is mostly the defaulter for UCI credit card
ggplot(data = data_raw, aes(x = AGE)) + 
  geom_histogram(bins = 50, fill = "purple", col = "blue", alpha = 0.3) + 
  scale_x_continuous(breaks = seq(min(0), max(90), by = 5), na.value = TRUE)

##------------------------------------------------------------
# 3.   Identify Missing Value;
##------------------------------------------------------------

# Drop the ID Column
#data_raw$ID <- NULL

# Do any columns contain NAs?
# Do any columns contain NAs?
 cc_na<- data_raw %>% select_if(function(col) any(is.na(col))) %>% summarize_all(funs(sum(is.na(.)))) 
summary(cc_na)
missmap(data_raw, main = "Deafult credit card missing data heatmap", 
        col=c("red", "black"), legend=F)
missmap(data_raw, main = "Missing Map")
colSums(sapply(data_raw, is.na))

##------------------------------------------------------------
# 5. Bivariate Graphs (Categorical anf Numerical);
##------------------------------------------------------------
#Default rate by gender
biv1<-ggplot(data_raw,aes(x = SEX, fill = DEFAULT)) + geom_bar(position='fill') 

#Default rate by education level
#As Default rate decreses with increase in level of education, education could be an important variable
biv2<-ggplot(data_raw, aes(x =EDUCATION, fill = DEFAULT)) + geom_bar(stat='count', position='fill') + labs(x = 'EDUCATION', y= 'Percent of default Vs No default')

#Default rate by Age Group
#As expected default rate increses with delay in payment
biv3<-ggplot(data_raw, aes(x =AGE_GROUP, fill = DEFAULT)) + geom_bar(position='fill') + labs(x = 'Age Group',y = "Percent of default Vs No default")

#Default rate by Marital Status
#As expected default rate increses with delay in payment
biv4<-ggplot(data_raw, aes(x =MARRIAGE, fill = DEFAULT)) + geom_bar(position='fill') + labs(x = 'Marital Status',y = "Percent of default Vs No default")

#Default rate by delay PAY_0
#As expected default rate increses with delay in payment
biv5<-ggplot(data_raw, aes(x =PAY_0, fill = DEFAULT)) + geom_bar(position='fill') + labs(x = 'PAY_0',y = "Percent of default Vs No default")

#Default rate by delay PAY_2
#As expected default rate increses with delay in payment
biv6<-ggplot(data_raw, aes(x =PAY_2, fill = DEFAULT)) + geom_bar(position='fill') + labs(x = 'PAY_2',y = "Percent of default Vs No default")

#Default rate by delay PAY_3
#As expected default rate increses with delay in payment
biv7<-ggplot(data_raw, aes(x =PAY_3, fill = DEFAULT)) + geom_bar(position='fill') + labs(x = 'PAY_3',y = "Percent of default Vs No default")

#Default rate by delay PAY_4
#As expected default rate increses with delay in payment
biv8<-ggplot(data_raw, aes(x =PAY_4, fill = DEFAULT)) + geom_bar(position='fill') + labs(x = 'PAY_4',y = "Percent of default Vs No default")

#Default rate by delay PAY_5
#As expected default rate increses with delay in payment
biv9<-ggplot(data_raw, aes(x =PAY_5, fill = DEFAULT)) + geom_bar(position='fill') + labs(x = 'PAY_5',y = "Percent of default Vs No default")


# limits by gender
biv10<-ggplot(data_raw, aes(factor(MARRIAGE), (LIMIT_BAL/1000), fill=SEX)) + 
  geom_boxplot() +
  xlab("Marital Status") + 
  ylab("Balance Limit ( x 1000 NT$)") + 
  coord_cartesian(ylim = c(0,350)) +
  scale_fill_brewer(palette = "Paired")

grid.arrange(biv1,biv2,biv3,biv4)
grid.arrange(biv5,biv6,biv7,biv8,biv9)

# Plot a count graph to determine which age group have more limit balance
theme_set(theme_bw())  
g <- ggplot(data = data_raw, aes(x = AGE, y = LIMIT_BAL))
g + geom_count(col="blue", show.legend=F) +
  labs(title="Counts Plot", 
       subtitle="Age Vs Credit Amount", 
       caption="source: UCI Credit Card")
# Plot a Violin graph between EDUCATION and Credit Amount
ggplot(data = data_raw,aes(x = EDUCATION, y = LIMIT_BAL,
                                 fill = EDUCATION,
                                 color = EDUCATION))  + 
  geom_boxplot() +
  labs(title="Violin Plot", 
       subtitle="Education Vs Credit Amount", 
       caption="source: UCI Credit Card") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
# Plot a Violin graph between MARRIAGE and Credit Amount
ggplot(data = data_raw,aes(x = MARRIAGE, y = LIMIT_BAL,
                                 fill = MARRIAGE,
                                 color = MARRIAGE
)) + geom_violin() +
  labs(title="Violin Plot", 
       subtitle="Marital Status Vs Credit Amount", 
       caption="source: UCI Credit Card")
##------------------------------------------------------------
# 6. Multivariate Graphs (Grouping);
##------------------------------------------------------------
# Balance limits by gender and education
plot1 <- ggplot(data_raw, aes(factor(SEX), (LIMIT_BAL/1000), fill=EDUCATION)) + 
  geom_boxplot() +
  xlab("Gender") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Accent")

# Balance limits by education and gender
plot2 <- ggplot(data_raw, aes(factor(EDUCATION), (LIMIT_BAL/1000), fill=SEX)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Paired")

# Balance limits by workstate and education
plot3 <-ggplot(data_raw, aes(factor(SEX), (LIMIT_BAL/1000), fill=AGE_GROUP)) + 
  geom_boxplot() +
  xlab("Gender") + 
  ylab("BLimit(x1000 NT$)") 

# Balance limits by education and age group
plot4 <- ggplot(data_raw, aes(factor(EDUCATION), (LIMIT_BAL/1000), fill=AGE_GROUP)) + 
    geom_boxplot() +
  xlab("Education") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Paired")
grid.arrange(plot1, plot2, plot3)

# Analysis of Default Payments

plot5 <- ggplot(data_raw, aes(x=DEFAULT),aes(y=stat_count(gender))) + 
  geom_bar(aes(fill=factor(data_raw$EDUCATION))) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  facet_wrap(~SEX)+
  scale_fill_discrete(name="Education")
plot6 <- ggplot(data_raw, aes(x=DEFAULT),aes(y=stat_count(gender))) + 
  geom_bar(aes(fill=factor(data_raw$AGE_GROUP))) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  facet_wrap(~SEX)+
  scale_fill_discrete(name="Age Group")
plot7 <- ggplot(data_raw, aes(x=DEFAULT),aes(y=stat_count(gender))) + 
  geom_bar(aes(fill=factor(data_raw$MARRIAGE))) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  facet_wrap(~SEX)+
  scale_fill_discrete(name="Marital Status")

grid.arrange(plot5, plot6, plot7, ncol=1)

plot8 <- ggplot(data_raw, aes(x=DEFAULT),aes(y=stat_count(marriage))) + 
  geom_bar(aes(fill=factor(data_raw$EDUCATION))) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  facet_wrap(~MARRIAGE)+
  scale_fill_discrete(name="Education")
plot9 <- ggplot(data_raw, aes(x=DEFAULT),aes(y=stat_count(marriage))) + 
  geom_bar(aes(fill=factor(data_raw$SEX))) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  facet_wrap(~MARRIAGE)+
  scale_fill_discrete(name="Gender")

grid.arrange(plot8, plot9, ncol=1)

plot10 <- ggplot(data_raw, aes(x=DEFAULT),aes(y=stat_count(marriage))) + 
  geom_bar(aes(fill=factor(data_raw$SEX))) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  facet_wrap(~EDUCATION)+
  scale_fill_discrete(name="Education")
plot11 <- ggplot(data_raw, aes(x=DEFAULT),aes(y=stat_count(marriage))) + 
  geom_bar(aes(fill=factor(data_raw$MARRIAGE))) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  facet_wrap(~EDUCATION)+
  scale_fill_discrete(name="Gender")

grid.arrange(plot10, plot11, ncol=1)

# plot salary histograms by rank and sex
ggplot(data_raw, aes(data_raw$LIMIT_BAL/1000)) +
  geom_histogram(color = "white",
                 fill = "cornflowerblue") +
  facet_grid(SEX ~ EDUCATION) +
  labs(title = "Limit Balance histograms by sex and rank",
       x = "Limit Balance($1000)")

ggplot(data_raw, aes(data_raw$LIMIT_BAL/1000)) +
  geom_histogram(color = "white",
                 fill = "cornflowerblue") +
  facet_grid(MARRIAGE ~ EDUCATION) +
  labs(title = "Limit Balance histograms by sex and rank",
       x = "Limit Balance($1000)")


##------------------------------------------------------------
# 7.  Correlation;
##------------------------------------------------------------
#Correlations Between Limit Balance, Bill Amounts & Payments
M <- cor(subset(data_raw, select = c(LIMIT_BAL,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)))
corrplot(M, method="number")
############################################
#                                          #
#  2.Data Pre-processing                  #
#                                          #
############################################
## Data pre-processing procedures consists of the following:
## 1. Scaling and Centering Data;
## 2. Preparation training and test dataset
## 3. Feature engineering using knn methods
#########################################################
##------------------------------------------------------------
# 1. Scaling and Centering Data;
##------------------------------------------------------------
trans_data_raw = preProcess(data_raw[,c(1,5,12:23)],  method=c("scale", "center"))
#?preProcess
# Transfrom data using predict() from 'trans' model
# Returns scaled, centered data
predictuci_data_raw = predict(trans_data_raw, data_raw[,c(1,5,12:23)])  
dim(predictuci_data_raw )    
class(predictuci_data_raw )    

# Reconstruct complete data 
combi_data_raw <- cbind(predictuci_data_raw,default.payment.next.month=data_raw$default.payment.next.month,Default=data_raw$DEFAULT)

##------------------------------------------------------------
# 2. Preparation training and test dataset;
##------------------------------------------------------------

## Separate the dataset onto training (70%) and testing (30%).
set.seed(5012)
temp_index <- createDataPartition(y=data_raw$DEFAULT, p=0.7, list=F)
train.batch <- data_raw[temp_index ,]
test.batch <- data_raw[-temp_index ,]

cat("--- Distribution of training dataset --")
table(train.batch$DEFAULT)
round(prop.table(table(train.batch$Default)), 2)

cat("\n--- Distribution of testing dataset --")
table(test.batch$DEFAULT)
round(prop.table(table(test.batch$Default)), 2)


###############################################################
#                                                             #
#  3.Model Building and Testing of Machine Learining Methods  #
#                                                             #
###############################################################
## Machine Learning Methods consists of the following:
## 1. Linear Regression;
## 2. Lasso Regression;
## 3. XGBoost;
## 4. Random Forest;
## 5. Decision Tree Methods;
################################################################################



##------------------------------------------------------------
#  1.Logistic Regression;
##------------------------------------------------------------
lg_m3 <- glm(DEFAULT~ LIMIT_BAL+SEX+AGE+BILL_AMT1+BILL_AMT2+PAY_AMT1+PAY_AMT2, data=train.batch,
            family=binomial)
summary(lg_m3)


## removie DEFAULT from test dataset
test_x <- dplyr::select(test.batch, - DEFAULT)

## adding model score to dataset
test.batch$LGM_SCORE<- predict(lg_m3 , type="response", test_x)

pred <- prediction(test.batch$LGM_SCORE, test.batch$DEFAULT)
lgm_perf <- performance(pred, "tpr", "fpr")

ggplot(test.batch, aes(DEFAULT, LGM_SCORE)) +
  geom_boxplot() + theme_bw() +
  ggtitle("LGM_SCORE distribution") +
  xlab("Default Payment") +
  ylab("Default Score")

# ROC
plot(lgm_perf , lty=1, col="red", main="Logistic Regression Performance")
abline(a=0, b= 1)

auc.tmp <- performance(pred, measure = "auc")
acc.tmp <- performance(pred, measure = "acc")
auc<- as.numeric(auc.tmp@y.values)
print ("AUC::"); auc

## Confusion Matrix
# Get accuracy and cutoff
ind <- which.max( slot(acc.tmp, "y.values")[[1]] )
acc <- slot(acc.tmp, "y.values")[[1]][ind]
cutoff <- slot(acc.tmp, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

# Draw Accuracy with cutoff
plot(acc.tmp)
abline(v=cutoff)

lgm_score_tmp <- dplyr::select(test.batch, LGM_SCORE, DEFAULT)
lm_score_tmp<- dplyr::mutate(lgm_score_tmp,predict_class = ifelse(LGM_SCORE>= cutoff, 1, 0))

# Making the Confusion Matrix
cm = table(lm_score_tmp$predict_class,lm_score_tmp$Default)
cm
confusionMatrix(cm)
##------------------------------------------------------------
#  2. Decision Tree Model;
##------------------------------------------------------------

dt_m <- rpart(Default ~.,data=train.batch, method = "class")
plotcp(dt_m)

fitted_dt<-predict(dt_m, newdata=test_x,type="class")
## Confusion Matrix
cm_dt= table(fitted_dt,test.batch$Default )
cm_dt
confusionMatrix(cm_dt)

test.batch$DT_SCORE <- predict(dt_m,type='prob',test_x)
pred_dtm <- prediction(test.batch$DT_SCORE[, 2], test.batch$Default)
dt_perf <- performance(pred_dtm,"tpr","fpr")

plot(dt_perf , col='red', main='Rpart Tree performance')
abline(a=0, b= 1)

##------------------------------------------------------------
#  3. Random Forest Model;
##------------------------------------------------------------


rf_m <- randomForest(as.factor(Default)~.,data=train.batch)

print(rf_m)
summary(rf_m)
## plot displaying RMSE scores for different tuning parameters
par(mar=c(1,1,1,1))
plot(rf_m)

## plot variable importance
importance(rf_m)
varImpPlot(rf_m)

fitted_rf<- predict(rf_m, test_x, type="class")
cm_rf= table(fitted_rf,test.batch$Default)
cm_rf
confusionMatrix(cm_rf)


test.batch$RF_SCOR <- predict(rf_m,type='prob', test_x)
pred_rf <- prediction(test.batch$RF_SCOR [, 2],test.batch$Default)

rf_perf <- performance(pred_rf,"tpr","fpr")




##------------------------------------------------------------
#  5. Naive Bayes ;
##------------------------------------------------------------
?naiveBayes
nvb_m=naiveBayes(Default ~.,data=train.batch, method = "class")
summary(nvb_m)


fitted_nvb<- predict(nvb_m, test_x, type="class")
cm_nvb= table(fitted_nvb,test.batch$Default)
cm_nvb
confusionMatrix(cm_nvb)


test.batch$NVB_SCOR <- predict(nvb_m,type='raw', test_x)
pred_nvb <- prediction(test.batch$NVB_SCOR [, 2],test.batch$Default)

nvb_perf <- performance(pred_nvb,"tpr","fpr")

##------------------------------------------------------------
#  6. Machine Learning Methods Evaluation ;
##------------------------------------------------------------

plot(lgm_perf,col='darkblue',lty=1, main='ROC Logistic VS. Rpart tree VS. Random Forest'); 
plot(dt_perf, col='darkred',lty=2,add=T); 
plot(rf_perf, col='green', lty=3, add=T);
plot(nvb_perf, col='blue',lty=4, add=T);
legend(0.6,0.6,c('logistic regression','rpart tree', 'randomForest','naiveBayes'),
       col=c('darkblue','darkred','green','blue'),lwd=3,
       fill = 1:4, ncol = 1,
       cex = 0.5, pch=1,  text.font = 3, inset=.02, bg='gray90', pt.cex = 1)








## Data pre-processing procedures
# 3. Feature Engineering using KNN methods;
##------------------------------------------------------------

# Feature engineering using fastknn
# fastknn generates k * c new features, where c is the number of class labels.
# The new features are computed from the distances between the observations
# and their k nearest neighbors inside each class. Total classes: 9
library("fastknn")
library("mlbench")
library("caTools")
library("fastknn")
library("glmnet")
system.time(
  newfeature   <- knnExtract(                       # It is FastKNN function
    xtr = data.matrix(train.batch[,-15]),      # matrix containing training instances.97 is target
    ytr = train.batch$default.payment.next.month,     # factor array with training labels.
    xte = data.matrix(test.batch[,-15]),   # matrix containing test instances.
    k = 3,                        # number of neighbors considered (default is 1)
    normalize = NULL
  )
)

head(newfeature$new.tr)          # New training features. Total generated: 9 * 3 = c * k
head(newfeature$new.te)          # New test features

# Column-wise stack complete set of predictors, original + New features (exclude target)
fx2<-cbind(train.batch[,-15], predictuci_data_raw[1:nrow(newfe$new.tr), ]) 




# Develop prediction model using Decision tree algorithms ,Model building and testing 

fxp<-predictuci_data_raw[1:nrow(fx2),]
vxp<-predictuci_data_raw[(nrow(fx2)+1) : nrow(predictuci_data_raw), ]

dim(fxp)       # 24002    14
dim(vxp)       # 5998     




# Neural network model - First with unprocessed data, cicreditcard_tr and ucicreditcard_valid, Data is unscaled+unbalanced (though converetd to dummy)
nnet_m<- nnet(default.payment.next.month ~ .,
              data=train.batch,
              size= 12, 
              decay= 0.01, 
              rang=0.6,
              trace=TRUE,
              maxit=200,
              MaxNWts=2000
)

plotnet(nnet_m)




##------------------------------------------------------------
# 7.  Correlation;
##------------------------------------------------------------
#Correlations Between Limit Balance, Bill Amounts & Payments
M <- cor(subset(raw.credit, select = c(LIMIT_BAL,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)))
corrplot(M, method="number")


#  2.Data Pre-processing                  #
set.seed(5012)
intraining <- createDataPartition(y=raw.credit$DEFAULT, p=0.7, list=F)
train.batch <- raw.credit[intraining,]
test.batch <- raw.credit[-intraining,]

cat("--- Distribution of training dataset --")
table(train.batch$DEFAULT)
round(prop.table(table(train.batch$Default)), 2)

cat("\n--- Distribution of testing dataset --")
table(test.batch$DEFAULT)
round(prop.table(table(test.batch$Default)), 2)

