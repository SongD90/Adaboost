# library(entropy)
# library(FSelector)
data <- read.csv("spambase.csv",header = FALSE)
head(data)

data[,58] <- replace(data[,58],data[,58]==0,-1)

##Funciton of AvgEntropy Calculation
## Only for numeric attributes
## y = 0,1
## Ordered by x
AvgEn_f <- function(x,y){
  
  n <- length(y)
  
  ## Create a space for y_hat
  y_hat <- matrix(c(1),n+1,n)
  for(i in 1:n){
    y_hat[i,] <- (x<x[i])+0
  }
  ## replace 0 to -1 in y_hat
  y_hat <- replace(y_hat,y_hat==0,-1)
  
  ## sort the data by x descending
  data_t <- rbind(x,y,y_hat)
  data_t_sort <- data_t[,order(x)]
  
  x <- data_t_sort[1,]
  y <- data_t_sort[2,]
  y_hat <- data_t_sort[3:nrow(data_t_sort),]
  
  
  
  ## Define the first and the last Entropy
  En <- c(rep(0,time=11))
  En[1] <- -(sum(y_hat[1,]==y)/length(y_hat[1,])*log2(sum(y_hat[1,]==y)/length(y_hat[1,]))+
               sum(y_hat[1,]!=y)/length(y_hat[1,])*log2(sum(y_hat[1,]!=y)/length(y_hat[1,])))
  En[n+1] <- -(sum(y_hat[n+1,]==y)/n*log2(sum(y_hat[n+1,]==y)/n)+
                 sum(y_hat[n+1,]!=y)/n*log2(sum(y_hat[n+1,]!=y)/n))
  
  ## Calculate the rest of Entropies.
  for(i in 2:n){
    #i <- 5
    en_l <- -(i-1)/n*(sum(y_hat[i,1:i-1]==y[1:i-1])/(i-1)*log2(sum(y_hat[i,1:i-1]==y[1:i-1])/(i-1))+
                        sum(y_hat[i,1:i-1]!=y[1:i-1])/(i-1)*log2(sum(y_hat[i,1:i-1]!=y[1:i-1])/(i-1)))
    if(is.nan(en_l)){en_l <- 0}
    
    en_r <- -(1-(i-1)/length(y))*(sum(y_hat[i,i:n]==y[i:n])/(n-i+1)*log2(sum(y_hat[i,i:n]==y[i:n])/(n-i+1))+
                                    sum(y_hat[i,i:n]!=y[i:n])/(n-i+1)*log2(sum(y_hat[i,i:n]!=y[i:n])/(n-i+1)))
    if(is.nan(en_r)){en_r <- 0}
    En[i] <- en_l+en_r
  }
  return(En)
}
###


  ## reshuffle the original data
data_rsf <- data[sample(nrow(data),nrow(data)),]
  
  ## take the first 100 rows
data_1 <- as.data.frame(data_rsf[1:100,]) 
names(data_1)[58] <- c('spam')
data_1$spam <- as.factor(data_1$spam)
data_train <- data_1[1:50,]
data_test <- data_1[51:100,]
#########################################################

adaboost <- function (data_train,round){
  #round <- 5
  
  data.org <- t(data_train)
  
  N_att <- dim(data.org)[1]
  MinEn <- rep(0,time=(N_att-1))
  N_obj <- dim(data.org)[2]
  
  y_hat.mat <- matrix(1,round+1,N_obj)
  
  e.mat <- rep(0,round)
  alpha.mat <- rep(0,round)
  w.mat <- matrix(1/N_obj,round+1,N_obj)
  colnames(w.mat) <- colnames(data.org)
  splitNum <- c(rep(0,round))
  ## Find the attibute that has the minimum entropy
  for(i in 1:(N_att-1)){
    MinEn[i] <- min(AvgEn_f(data.org[i,],data.org[N_att,]))
  }
  min(MinEn)
  Num_att <- which.min(MinEn)
  
  x <- data.org[Num_att,]
  y <- data.org[N_att,]
  
  data_t <- rbind(x,y)
  data_t <- data_t[,order(data_t[1,])]
  #add the true y to the last row of y_hat.mat
  y_hat.mat[(round+1),] <- (data_t[2,])
  
  
  for (r in 1:round){ 
    ## data for round 1 
    data_t_r <- data_t[,sample(ncol(data_t),ncol(data_t),prob = w.mat[r,],replace = TRUE)]
    ## order data_t_r by x  
    data_t_r <- data_t_r[,order(data_t_r[1,])]
    ## find the split point
    split_num <- which.min(AvgEn_f(data_t_r[1,],data_t_r[2,]))
    split_name <- colnames(data_t_r)[split_num-1]
    ## prediction in data_t_r
    y_hat_t <- rep(1,N_obj)
    y_hat_t[1:(split_num-1)] <- -1
    data_t_r<-rbind(data_t_r,y_hat_t)
    
    ## prediction in data_t
    # find the split number in data_t base on the split_name 
    split_num_t <-  which(colnames(data_t)==split_name)
    splitNum[r] <- split_num_t
    y_hat.mat[r,1:split_num_t] <- -1  
    
    
    ## error in data_t_r
    e_t_r <- (sum(data_t_r[2,1:(split_num-1)]!=data_t_r[3,1:(split_num-1)])+
                sum(data_t_r[2,split_num:N_obj]!=data_t_r[3,split_num:N_obj]))/N_obj
    e.mat[r] <- e_t_r
    ## alpha 1
    alpha.mat[r] <- 0.5*log((1-e_t_r)/e_t_r)
    ## weight 2
    colname_mis <- colnames(data_t_r[,((data_t_r[3,]!=data_t_r[2,])+0)==1])
    w.mat[(r+1):round,which(names(w.mat[r+1,])%in% colname_mis)] <- w.mat[r+1,which(names(w.mat[r+1,])%in% colname_mis)]*exp(alpha.mat[r])
    w.mat[(r+1):round,which(!(names(w.mat[r+1,])%in% colname_mis))] <- w.mat[r+1,which(!(names(w.mat[r+1,])%in% colname_mis))]*exp(-alpha.mat[r])
  }
  
  result <- list(yhat=y_hat.mat,weight=w.mat,alpha=alpha.mat,error=e.mat,splitPointId=split_name,
                 weakLearer=splitNum,att=Num_att)
  return(result)
}

###############################################################
r.t <- 2
mat.1 <- adaboost(data_test,round = r.t)

alpha.t <- unlist(mat.1[3])
alpha.t
y_hat.t <- matrix(unlist(mat.1[1]),(r.t+1),50)
for(i in 1:r.t){
 y_hat.t[i,] <-  as.numeric(y_hat.t[i,])*alpha.t[i]
}

y_sign <- sign(colSums(matrix(as.numeric(y_hat.t[1:r.t,]),r.t,50)))
y_hat.t <- rbind(y_hat.t,y_sign)
e.t <- sum(y_hat.t[(r.t+2),]!=as.numeric(y_hat.t[(r.t+1),]))/50
e.t

###############################################################
# Testing error#
################
weakLearer <- mat.1$weakLearer
alpha <- mat.1$alpha
round <- r.t

data_test_t <- t(data_test)
data_test_t.t <- data_test_t[c(att,58),]
data_test_t.t <- data_test_t.t[,order(data_test_t.t[1,])]

yhat.test <- matrix(1,r.t,dim(data_test_t.t)[2])
for (i in 1:r.t) {
  yhat.test[i,1:weakLearer[i]] <- -1
}


yhat.test <- rbind(yhat.test,data_test_t.t[2,])
