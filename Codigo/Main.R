# LDA/ QDA
library(MASS)

# RDA
library(klaR)

# Multinomial
library(nnet)

# Cross-Validation
library(TunePareto)

# Naive Bayes
library(e1071)

# k-NN
library(class)

# Correspondence analysis
library(ca)

# Cross-validation nn
library(caret)


### 1. Read data
set.seed(2105)
clev <- read.csv("../data/cleveland.csv", header=F)

### 2. Preprocess data
source("Preprocessing.R")

# Missings
clev <- clev[,much.na.cols(clev,60)]
dummy <- c("V1", "V2", "V36", "V69", "V70", "V71", "V72", "V73", "V28", "location")
clev <- remove.var(clev, dummy)
clev <- knn.imputation(clev, 7)

# Multicollinearity
# Uncomment this lines to see which variables have big correlation

#corr.factors <- cor(clev)
#which(abs(corr.factors)-diag(diag(corr.factors))>0.9, arr.ind=T)
clev <- remove.var(clev, c("V57", "V55"))

# Factors
factores <- c("V58", "V4", "V9", "V16", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V38", "V39", "V41", "V51", "V56", "V11", "V59", "V60", "V61", "V63", "V65", "V67", "V68")
for (f in factores){
  clev[,f] <- as.factor(clev[,f])
}
clev <- move.value(clev, "V25", 2, 1)

#### 2.1 Visualizations
source("Visualizations.R")

histograms(clev)
boxplot.num(clev)
histograms(clev, F)
show.cor(clev)

#### 2.2 Modification of values
qqplots(clev)
boxcox.plots(clev)

# Variables with many zeros:
boxcox.plot.special(clev, c("V14", "V15", "V40"))

# Apply log / sqrt transformation
clev <- apply.trans(clev, sqrt.neg.vars=c("V10", "V12", "V31", "V43"), sqrt.vars = c("V14", "V40"))
clev <- scale.num(clev)


#### 2.3. Feature extraction

# Separe train and test data, seed for reproducibility.
set.seed(2000)
n <- nrow(clev)
train.lenght <- round(2*n/3)

clev <- clev[sample(n),]
train <- clev[1:train.lenght,]
test <- clev[(train.lenght+1):n,]

col.class <- as.numeric(train$V58)
col.class[col.class==1] <- "red"
col.class[col.class==2] <- "green"
col.class[col.class==3] <- "blue"
col.class[col.class==4] <- "yellow"
col.class[col.class==5] <- "purple"

pca <- pca.num(train)
par(mfrow=c(1,1))
plot.pca(train, col.class, pca = pca)


fda <- plot.fda(train, V58~.-V21-V22-V59, col.class)

train <- extract.fda(fda, train)
test <- extract.fda(fda, test)


# Correspondence analysis
mca.features <- mcaplot(train, factores)


### 3. Resampling protocol
source("Resampling.R")


### 4. Models
rda.model <- rda(V58~V3+V4+V9+V10+V11+V12+V14+V15+V16+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V29+V31+V32+V33+V34+V35+V37+V38+V39+V40+V41+V43+V44+V51+V56+V60+V61+V63+V65+V67+V68, data=train)
cross.validation(train, train$V58, rda.model, 1, 10, T)

rda.model.fda <- rda(V58~.,data=train)
cross.validation(train, train$V58, rda.model.fda, 2, 10, T)

cross.validation.naive(train, train$V58, naive.model, 10, 10)

err <- c()
for (k in 1:20){
  err <- c(err, cross.validation.knn(train, train$V58, 10,10, k))
}
plot(err, type = "l")
err


multinomial.model <- multinom(V58~., data=train)

cross.validation(train, train$V58, multinomial.model, 10, 10, F)

multinomial.model.step <- step(multinomial.model)

cross.validation(train, train$V58, multinomial.model.step, 10, 10, F)

multinomial.model.noFDA <- multinom(V58~.-LD1-LD2-LD3-LD4, data=train)

cross.validation(train, train$V58, multinomial.model.noFDA, 10, 10, F)

multinomial.model.noFDA.step <- step(multinomial.model.noFDA)

cross.validation(train, train$V58, multinomial.model.noFDA.step, 10, 10, F)



### Test error
rda.model <- update(rda.model.fda, data=train)
pred.test <- predict(rda.model.fda, test)
pred.test <- pred.test$class
(err.table <- table(True=test$V58, Pred=pred.test))
(err.test <- 1-sum(diag(err.table))/sum(err.table))


## Hungarian data
hung <- read.csv("../data/hungarian.csv", header=F)


# Missings
hung <- hung[,much.na.cols(hung,60)]
dummy <- c("V1", "V2", "V36", "V69", "V70", "V71", "V72", "V73", "V28")
hung <- remove.var(hung, dummy)
hung <- knn.imputation(hung, 7)

# Multicollinearity
#corr.factors <- cor(hung)
#which(abs(corr.factors)-diag(diag(corr.factors))>0.9, arr.ind=T)
hung <- remove.var(hung, c("V57", "V55"))

# Factors
factores <- c("V58", "V4", "V9", "V16", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V38", "V39", "V56", "V11")
for (f in factores){
  hung[,f] <- as.factor(hung[,f])
}
hung <- move.value(hung, "V25", 2, 1)


histograms(hung)
boxplot.num(hung)
histograms(hung, F)
show.cor(hung)

qqplots(hung)

boxcox.plots(hung)

# Variable with many 0
boxcox.plot.special(hung, c("V40"))

# Apply transformations
hung <- apply.trans(hung, sqrt.neg.vars=c("V10", "V12"), sqrt.vars = c("V31", "V29", "V42", "V43"), log.vars = c("V6", "V7", "V40"))
hung <- scale.num(hung)

# Remove variables and levels which causes troubles
hung <- remove.var(hung, c("V23", "V39"))
hung <- move.value(hung, "V19", 2, 1)


# Train / test
set.seed(2000)
n <- nrow(hung)
train.lenght <- round(2*n/3)

hung <- hung[sample(n),]
train <- hung[1:train.lenght,]
test <- hung[(train.lenght+1):n,]

col.class <- as.numeric(train$V58)
col.class[col.class==1] <- "red"
col.class[col.class==2] <- "green"
col.class[col.class==3] <- "blue"
col.class[col.class==4] <- "yellow"
col.class[col.class==5] <- "purple"


col.class2 <- as.numeric(train$V58)
col.class2[col.class2==1] <- "red"
col.class2[col.class2==2] <- "green"
col.class2[col.class2==3] <- "blue"
col.class2[col.class2==4] <- "yellow"
col.class2[col.class2==5] <- "purple"
pca <- pca.num(hung)
par(mfrow=c(1,1))
plot.pca(hung, col.class2, pca = pca)
# Uncomment if you want to try the result with PCA features (Disclaimer: results are bad)
# If you execute this line, you will have to remove the pca features later on.
# hung <- extract.pca(pca, hung)


fda <- plot.fda(train, V58~.-V56, col.class)

train <- extract.fda(fda, train)
test <- extract.fda(fda, test)


plot(test$LD1, test$LD2, col=c("red","green","yellow","blue","purple")[as.numeric(test$V58)], xlab="LD1", ylab="LD2")
legend("topleft", legend=c("0","1","2","3","4"), fill=c("red","green","yellow","blue","purple"))


mca.features <- mcaplot(train, factores)


# Models
rda.model <- rda(V58~.-LD1-LD2-LD3-LD4, data=train)

cross.validation(train, train$V58, rda.model, 1, 10, T)

rda.model.fda <- rda(V58~.,data=train)

cross.validation(train, train$V58, rda.model.fda, 1, 10, T)

cross.validation.naive(train, train$V58, naive.model, 10, 10)

err <- c()
for (k in 1:20){
  err <- c(err, cross.validation.knn(train, train$V58, 10,10, k))
}

plot(err, type = "l")
err


multinomial.model <- multinom(V58~., data=train)

cross.validation(train, train$V58, multinomial.model, 10, 10, F)

multinomial.model.step <- step(multinomial.model)

cross.validation(train, train$V58, multinomial.model.step, 10, 10, F)

multinomial.model.noFDA <- multinom(V58~.-LD1-LD2-LD3-LD4, data=train)

cross.validation(train, train$V58, multinomial.model.noFDA, 10, 10, F)

multinomial.model.noFDA.step <- step(multinomial.model.noFDA)

cross.validation(train, train$V58, multinomial.model.noFDA.step, 10, 10, F)


# Uncomment this line to see that there are small groups
#qda.model <- qda(V58~.-LD1-LD2-LD3-LD4, data=train)



## Redes neuronales
# 10x10 CV, is temporary
trc <- trainControl (method="repeatedcv", number=10, repeats=10)

# First try, only FDA features
decays <- c(0.0001, 0.001, 0.01, 0.1, 1)
nn.model10x10CV <- train(V58~LD1+LD2+LD3+LD4, data = train, method = 'nnet', 
                         trace=F, maxit=1000,
                         tuneGrid = expand.grid(.size=9,.decay=decays), trControl=trc)


nn.model10x10CV$results
nn.model10x10CV$bestTune

# Second try, with all the data
trc <- trainControl (method="repeatedcv", number=5, repeats=1)
decays <- c(0, 0.01, 0.1, 1)
nn.model1x5CV <- train(V58~., data = train, method = 'nnet', 
                         trace=F, maxit=1000, MaxNWt=2000,
                         tuneGrid = expand.grid(.size=9,.decay=decays), trControl=trc)

nn.model1x5CV$results
nn.model1x5CV$bestTune

# Train on the rest of the data
nn <- nnet(V58~., data=train, maxit=1000, size=9, decay=1, MaxNWt=2000)
# Training error
table(train$V58, apply(nn$fitted.values, 1, which.max)-1)


# Third try, more neurons, and quittid variables with many levels
trc <- trainControl (method="repeatedcv", number=10, repeats=1)
decays <- c(0.5, 0.67, 0.66, 0.68, 0.7, 1)
nn.model1x10CV <- train(V58~.-LD1-LD2-LD3-LD4-V56-V20-V21-V22, data = train,
                         method = 'nnet',
                         trace=F, maxit=1000, MaxNWts=10000,
                         tuneGrid = expand.grid(.size=20,.decay=decays),
                         trControl=trc)

nn.model1x10CV$results
nn.model1x10CV$bestTune


# Train on the rest of the data
nn <- nnet(V58~.-LD1-LD2-LD3-LD4-V56-V20-V21-V22, data=train, maxit=1000, size=20, decay=0.68, MaxNWts=10000)
# Training error
(tab <- table(train$V58, apply(nn$fitted.values, 1, which.max)-1))
(err <- 1 - sum(diag(tab))/sum(tab))
# Test error
pred <- predict(nn, test)
(tab <- table(test$V58, apply(pred, 1, which.max)-1))
(err <- 1 - sum(diag(tab))/sum(tab))
# Joining labels to binarize
target <- as.numeric(test$V58)-1
target[target > 1] <- 1

pred <- apply(pred, 1, which.max)-1
pred[pred>1] <- 1

(tab <- table(target, pred))
(err <- 1 - sum(diag(tab))/sum(tab))


## Two-classes
# Be careful to remove PCA features if they have been added
# If they do are added, try simply reexecuting the hung preprocessing part of the code.
aux <- hung
aux$V58[aux$V58 != 0] <- 1
aux$V58 <- droplevels(aux$V58)

# Train / test
set.seed(2000)
n <- nrow(aux)
train.lenght <- round(2*n/3)

aux <- aux[sample(n),]
train_aux <- aux[1:train.lenght,]
test_aux <- aux[(train.lenght+1):n,]

# A model that achieves 0 training error
nn <- nnet(V58~.-V56-V20-V21-V22, data=train_aux, maxit=1000, size=30, decay=0, MaxNWts = 10000)
(tab<- table(train_aux$V58, (nn$fitted.values > 0.5)*1))
(err.train <- 1 - sum(diag(tab))/sum(tab))

# Tuning the decay for it
decays <- c(0.4, 0.52, 0.55, 0.57, 0.6, 0.7)
nn.model1x10CV <- train(V58~.-V56-V20-V21-V22, data = train_aux,
                         method = 'nnet',
                         trace=F, maxit=1000, MaxNWts=10000000,
                         tuneGrid = expand.grid(.size=30,.decay=decays), trControl=trc)

nn.model1x10CV$results
nn.model1x10CV$bestTune


# Update weights
nn <- nnet(V58~.-V56-V20-V21-V22, data=train_aux, maxit=1000, size=30, decay=0.55, MaxNWt=10000)
# Training error
(tab<- table(train_aux$V58, (nn$fitted.values > 0.5)*1))
(err.train <- 1 - sum(diag(tab))/sum(tab))
# Test error
pred <- predict(nn, test_aux)
(tab<- table(test_aux$V58, (pred > 0.5)*1))
(err.test <- 1 - sum(diag(tab))/sum(tab))
