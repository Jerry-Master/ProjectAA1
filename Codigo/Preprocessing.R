# 
# 
# RScript with the functionality needed to preprocess the data.
# 
# 


### NA Treatment

# Returns columns with more NA than a given threshold.
# If threshold is "all" then it gives columns with all NA
# Precond: NA are -9
# Postcond: rmlist contains negative indices for removing.
much.na.cols <- function(dd, threshold="all"){
  if (threshold == "all"){
    rmlist <- c()
    for (i in 1:ncol(dd)){
      if (min(dd[,i]) == max(dd[,i]) & min(dd[,i])==-9){
        rmlist <- c(rmlist, i)
      }
    }
  } else {
    rmlist <- c()
    for (i in 1:ncol(dd)){
      if (sum(dd[,i]==-9) > threshold){
        rmlist <- c(rmlist, i)
      }
    }   
  }
  -rmlist
}


# Applies k-nearest neighbour imputation for a given variable
knn.imputation.aux <- function(dd, variable, varname, k){  
  aux = subset (dd, select = names(dd)[names(dd) != varname])
  aux1 = aux[!is.na(variable),]
  aux2 = aux[is.na(variable),]
  
  # Neither of aux1, aux2 can contain NAs
  knn.inc = knn (aux1,aux2, variable[!is.na(variable)], k)
  variable[is.na(variable)] = knn.inc
  variable
}
# Returns the dataset with all NA imputated by k-nearest neighbour
# Precond: NA are -9
knn.imputation <- function(dd, k){
  na.names <- names(dd)[-much.na.cols(dd, 0)]
  for (name in na.names){
    dd[, name][dd[, name] == -9] <- NA
    dd[, name] <- knn.imputation.aux(dd, dd[,name], name, k)
  }
  dd
}



### Removing variables / values

# Returns the same dataframe without given columns
remove.var <- function(dd, vars){
  dd[,!(names(dd) %in% vars)]
}


# Move one value with low incidence to another
move.value <- function(dd, var, varin, varout){
  dd[,var][dd[,var] == varin] <- varout
  dd[,var] <- droplevels(dd[,var])
  dd
}



### Transformations

# Apply a sqrt or log transformation to given columns
apply.trans <- function(dd, sqrt.vars=NULL, sqrt.neg.vars=NULL, log.vars=NULL, log.neg.vars=NULL){
  for (var in sqrt.vars){
    dd[,var] <- 2*sqrt(dd[,var])
  }
  for (var in sqrt.neg.vars){
    dd[,var] <- 2*sqrt(dd[,var]-min(dd[,var])+1e-6)
  }
  for (var in log.vars){
    dd[,var] <- log(1+dd[,var])
  }
  for (var in log.neg.vars){
    dd[,var] <- log(dd[,var]-min(dd[,var])+1)
  }
  dd
}


# Scale numerical columns
scale.num <- function(dd){
  for (i in 1:length(dd)){
    if (!is.factor(dd[,i])){
      dd[,i] <- scale(dd[,i])
    }
  }
  dd
}



### Feature extraction

# Returns pca of numerical columns
pca.num <- function(dd){
  names_num <- c()
  for(i in 1:ncol(dd)){
    if (!is.factor(dd[,i])) {
      names_num <- c(names_num, i)
    }
  }
  dd_num <- dd[,names_num]
  princomp(dd_num)
}


# Returns the FDA projections of a trained lda model
extract.fda <- function(fda, dd){
  loadings <- predict(fda, newdata = dd)
  for (i in 1:(dim(loadings$x)[2])){
    dd[,paste("LD", i, sep="")] <- loadings$x[,i]
  }
  dd
}

extract.pca <- function(pca, dd){
  loadings <- pca$scores
  for (i in 1:(dim(loadings)[2])){
    dd[,paste("PC", i, sep="")] <- loadings[,i]
  }
  dd
}






