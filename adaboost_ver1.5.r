library(entropy)
library(FSelector)
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






function(data_train,round){

t_data_train <- t(data_train)

N_att <- dim(t_data_train)[1]
MinEn <- rep(0,time=(N_att-1))
N_obj <- dim(t_data_train)[2]

y_hat.mat <- matrix(1,round,N_obj)

e.mat <- rep(0,round)
alpha.mat <- rep(0,round)
w.mat <- matrix(1/N_obj,round,N_obj)
colnames(w.mat) <- colnames(t_data_train)

## Find the attibute that has the minimum entropy
for(i in 1:(N_att-1)){
  MinEn[i] <- min(AvgEn_f(t_data_train[i,],t_data_train[N_att,]))
}
min(MinEn)

Num_att_1 <- which.min(MinEn)

###############
## Round 1
###############



## Find the 1st Split point of the attribute
Num_split <- which.min(AvgEn_f(t_data_train[Num_att_1,],t_data_train[N_att,]))

## order the data_t_1 by the attribute
data_t_1 <- rbind(t_data_train[Num_att_1,],t_data_train[N_att,])
data_t_1 <- data_t_1[,order(data_t_1[1,])]



## make the prediciton (y_hat_1)

# y_hat_1 <- rep(1,N_obj)
# y_hat_1[1:(Num_split-1)] <- -1
# data_t_1 <- rbind(data_t_1,y_hat_1)
## make the prediciton (y_hat_2)
y_hat.mat[1,] <- rep(1,N_obj)
y_hat.mat[1,1:(Num_split_2-1)] <- -1



y_hat.mat[1,1:(Num_split-1)] <- -1
data_t_1 <- rbind(data_t_1,y_hat.mat[1,])



## Calculate the error rate and the alpha
e.mat[1] <- (sum(data_t_1[2,1:(Num_split-1)]!=data_t_1[3,1:(Num_split-1)])+
               sum(data_t_1[2,Num_split:dim(data_t_1)[2]]!=data_t_1[3,Num_split:dim(data_t_1)[2]]))/dim(data_t_1)[2]
alpha_1 <- 0.5*log((1-e.mat[1])/e.mat[1])
alpha.mat[1] <- 0.5*log((1-e.mat[1])/e.mat[1])

## The weight of data point
#the columnames that are misclassified.
colname_1 <- colnames(data_t_1[,((data_t_1[3,]!=data_t_1[2,])+0)==1])
w_1 <- t(as.matrix(rep(1/N_obj,N_obj)))
colnames(w_1) <- colnames(t_data_train)
w.mat[2,] <- w.mat[1,]
w.mat[2,which(names(w.mat[2,])%in%colname_1)] <- w_1[,which(names(w.mat[2,])%in%colname_1)]*exp(alpha.mat[1])
w.mat[2,which(!(names(w.mat[2,])%in%colname_1))] <- w_1[,which(!(names(w.mat[2,])%in%colname_1))]*exp(-alpha.mat[1])
###############
## Round 2
###############

t_data_train_2 <- t_data_train[,sample(ncol(t_data_train),ncol(t_data_train),prob = w.mat[2,],replace = TRUE)]

### find the attribute that has the minimum entropy
# MinEn_2 <- rep(0,time=57)
# for(i in 1:57){
#   MinEn_2[i] <- min(AvgEn_f(t_data_train_2[i,],t_data_train_2[58,]))
# }
# min(MinEn_2)
# 
# Num_att_2 <- which.min(MinEn_2)

##still use att 53
Num_att_2 <- Num_att_1
MinEn_2 <- min(AvgEn_f(t_data_train_2[Num_att_1,],t_data_train_2[N_att,]))



## Find the Split point of the attribute
Num_split_2 <- which.min(AvgEn_f(t_data_train_2[Num_att_2,],t_data_train_2[N_att,]))
data_t_2 <- rbind(t_data_train_2[Num_att_2,],t_data_train_2[N_att,])
data_t_2 <- data_t_2[,order(data_t_2[1,])]


## make the prediciton (y_hat_2)
y_hat.mat[2,] <- rep(1,N_obj)
y_hat.mat[2,1:(Num_split_2-1)] <- -1
data_t_2 <- rbind(data_t_2,y_hat.mat[2,])

## Calculate the error rate and the alpha
e.mat[2] <- (sum(data_t_2[2,1:(Num_split_2-1)]!=data_t_2[3,1:(Num_split_2-1)])+
               sum(data_t_2[2,Num_split_2:dim(data_t_2)[2]]!=data_t_2[3,Num_split_2:dim(data_t_2)[2]]))/dim(data_t_2)[2]
#alpha_2 <- 0.5*log((1-e_2)/e_2)
alpha.mat[2] <- 0.5*log((1-e.mat[2])/e.mat[2])

## calculate the y hat in data_t
name_split <- colnames(data_t_2)[Num_split_2-1]

y_hat.mat[2,]
Num_plit_t <- which(colnames(data_t)==name_split)
y_hat.mat[2,] <- 1
y_hat.mat[2,1:Num_plit_t] <- -1

## The weight of data point for resample round 3
#the columnames that are misclassified.
colname_2 <- colnames(data_t_2[,((data_t_2[3,]!=data_t_2[2,])+0)==1])
w_3 <- w.mat[2,]
colnames(w_3) <- colnames(t_data_train_2)

for(i in 1:N_obj){
  i=1
  w_3[,i] <- w_2[,names(w_3[,i])]
}

w_3[which(colnames(w_3) %in% colname_2)] <- w_3[which(colnames(w_3) %in% colname_2)]*exp(alpha_2)
w_3[which(!(colnames(w_3) %in% colname_2))] <- w_3[which(!(colnames(w_3) %in% colname_2))]*exp(-alpha_2)


## the weight matrix
round <- 5


for (i in 1:N_obj){
  if (names(w[3,i]) %in% colnames(w_3)){
    w.mat[3:round,i] <- w_3[,names(w[3,i])]
  }
  else {w.mat[3:round,i]=w[3:round,i]}
}

}