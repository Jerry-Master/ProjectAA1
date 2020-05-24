# 
# 
# Utility functions to visualize the data
# 
# 

### Preprocessing

# Shows histograms of variables, numerical or not
histograms <- function(dd, is.num=T){
  if (is.num){
    par(mfrow = c(2,3))
    for(i in 1:ncol(dd)){
      if (!is.factor(dd[,i])) {
        hist(dd[,i], main = names(dd)[i], xlab="Values")
      }
    }
  } else {
    par(mfrow = c(3,3))
    for(i in 1:ncol(dd)){
      if (is.factor(dd[,i])) {
        hist(as.numeric(as.character(dd[,i])), main = names(dd)[i], xlab="Values")
      }
    }
  }
}


# Shows boxplots of numerical variables
boxplot.num <- function(dd){
  par(mfrow = c(2,3))
  for(i in 1:ncol(dd)){
    if (!is.factor(dd[,i])) {
      boxplot(dd[,i], xlab = names(dd)[i])
    }
  }
}


# Shows correlations
show.cor <- function(dd){
  names_num <- c()
  for(i in 1:ncol(dd)){
    if (!is.factor(dd[,i])) {
      names_num <- c(names_num, i)
    }
  }
  dd_numeric <- dd[,names_num]
  
  dd_cor <- cor(dd_numeric)
  print(which(dd_cor > 0.5 & dd_cor < 1, arr.ind = TRUE))
  print(which(-dd_cor > 0.5 & -dd_cor < 1, arr.ind = TRUE))
}


# Shows the qq-plots
qqplots <- function(dd){
  par(mfrow = c(2,3))
  for(i in 1:length(dd)){
    if (!is.factor(dd[,i])){
      qqnorm(dd[,i], main = c("Q-Q Plot: ", names(dd)[i]))
      qqline(dd[,i], col=2)
    }
  }
}


# Shows the boxcox graphic to get the transformation
boxcox.plots <- function(dd){
  par(mfrow = c(2,3))
  for(i in 1:length(dd)){
    if (!is.factor(dd[,i])){
      aux <- dd[,i]-min(dd[,i])+1e-6
      boxcox(aux~1,lambda = seq(-1, 1.5, by=0.1), xlab = c(names(dd))[i])
    }
  }
}

# Shows the boxcox for positive variables with many zeros
boxcox.plot.special <- function(dd, vars){
  par(mfrow=c(1,3))
  for (var in vars){
    aux <- dd[,var]
    aux <- aux[aux !=0]
    boxcox(aux~1,lambda = seq(-2, 1.5, by=0.1))
  }
}



### Feature extraction

# Plots the PCA with colors
plot.pca <- function(dd, col.class, scaling.factor=2, pca=NULL){
  names_num <- c()
  for(i in 1:ncol(dd)){
    if (!is.factor(dd[,i])) {
      names_num <- c(names_num, i)
    }
  }
  dd_num <- dd[,names_num]
  if (is.null(pca)) {
    pca <- princomp(dd_num)
  }
  
  Fp <- pca$scores
  Gs <- pca$loadings
  
  Fs <- Fp %*% diag(1/pca$sdev)
  Gp <- Gs %*% diag(pca$sdev) * scaling.factor
  
  plot(Fs[,1], Fs[,2], asp=1, col = col.class, xlab = "First principal component", ylab = "Second principal component")
  arrows(rep(0,dim(Gs)[1]),rep(0,dim(Gs)[1]), Gp[,1], Gp[,2])
  text(Gp[,1], Gp[,2], names(dd_num), col = "black")
  legend("bottomright", fill=c("red","green", "blue", "yellow", "purple"), legend=c('0','1','2','3', '4'))
}


# Plots the FDA with colors and returns the fda object
plot.fda <- function(dd, formula, col.class){
  fda <- do.call("lda", list(formula=formula, data=dd))
  loadings <- predict(fda)$x
  plot(loadings, col = col.class)
  legend("bottomright", fill=c("red","green", "blue", "yellow", "purple"), legend=c('0','1','2','3', '4'))
  fda
}


# Plots the MJCA
mcaplot <- function(dd, factores, ret=T){
  aux <- dd[,names(dd) %in% factores]
  par(mfrow=c(1,1))
  ac <- mjca(aux, lambda="Burt")
  plot(ac, main="MCA biplot of Burt matrix with data")
  
  ac_ind <- mjca(aux, lambda="indicator", reti = T)
  plot(ac_ind$rowcoord, col = col.class)
  legend("bottomright", fill=c("red","green", "blue", "yellow", "purple"), legend=c('0','1','2','3', '4'))
  
  if (ret) {
    ac_ind$rowcoord
  }
}





