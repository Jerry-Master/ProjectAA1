# 
# 
# Principal utility functions for doing cross-validation. 
# 
# 

### General CV

# Valid for models with predict and update methods
cross.validation <- function(data, target, model, times, nfolds, need.class, verbose=T){
  set.seed(0202)
  CV.folds <- generateCVRuns(target, ntimes=times, nfold=nfolds, stratified=TRUE)
  
  err.total <- c()
  for (i in 1:times){
    err.onetime <- c()
    for (j in 1:nfolds){
      if (verbose) print(paste0("Fold: ", j))
      val <- unlist(CV.folds[[i]][[j]])
      
      tr <- data[-val,]
      va <- data[val,]
      
      model <- update(model, data=tr)
      pred <- predict(model, newdata=va)
      if (need.class){
        pred <- pred$class
      }
      
      err.table <- table(True=target[val], Predicted=pred)
      err <- 1-sum(diag(err.table))/sum(err.table)
      err.onetime <- c(err.onetime, err)
    }
    err.total <- c(err.total, mean(err.onetime))
    if(verbose) print(paste0("Iteration ", i, ", mean error: ", mean(err.onetime)))
  }
  mean(err.total)
}


### Cross-validation for k-Nearest Neighbour

# 
cross.validation.knn <- function(data, target, times, nfolds, K, verbose=F){
  set.seed(0202)
  CV.folds <- generateCVRuns(target, ntimes=times, nfold=nfolds, stratified=TRUE)
  
  err.total <- c()
  for (i in 1:times){
    err.onetime <- c()
    for (j in 1:nfolds){
      if (verbose) print(paste0("Fold: ", j))
      val <- unlist(CV.folds[[i]][[j]])
      
      tr <- data[-val,]
      va <- data[val,]
      
      pred <- knn(tr, va, target[-val], k = K)
      
      err.table <- table(True=target[val], Predicted=pred)
      err <- 1-sum(diag(err.table))/sum(err.table)
      err.onetime <- c(err.onetime, err)
    }
    err.total <- c(err.total, mean(err.onetime))
    if(verbose) print(paste0("Iteration ", i, ", mean error: ", mean(err.onetime)))
  }
  mean(err.total)
}



### Cross-validation Naive-Bayes

# 
cross.validation.naive <- function(data, target, model, times, nfolds, verbose=T){
  set.seed(0202)
  CV.folds <- generateCVRuns(target, ntimes=times, nfold=nfolds, stratified=TRUE)
  
  err.total <- c()
  for (i in 1:times){
    err.onetime <- c()
    for (j in 1:nfolds){
      if (verbose) print(paste0("Fold: ", j))
      val <- unlist(CV.folds[[i]][[j]])
      
      tr <- data[-val,]
      va <- data[val,]
      
      model <- naiveBayes(V58~.-LD1-LD2-LD3-LD4, data=tr)
      pred <- predict(model, newdata=va)
      
      err.table <- table(True=target[val], Predicted=pred)
      err <- 1-sum(diag(err.table))/sum(err.table)
      err.onetime <- c(err.onetime, err)
    }
    err.total <- c(err.total, mean(err.onetime))
    if (verbose) print(paste0("Iteration ", i, ", mean error: ", mean(err.onetime)))
  }
  mean(err.total)
}