## Attributes
x <- c(1,10,3,4,5,5,1,10,10,2)
## response var
y <- c(-1,1,1,-1,-1,1,1,1,1,1)

###Test
AvgEn(x,y)
min(AvgEn(x_t,y_t))
#

##Funciton of AvgEntropy Calculation
## Only for numeric attributes
AvgEn <- function(x,y){

  ## Attributes
  x <- c(1,10,3,4,5,5,1,10,10,2)
  ## response var
  y <- c(-1,1,1,-1,-1,1,1,1,1,1)  
  
n <- length(y)

## Create a space for y_hat
y_hat <- matrix(c(1),n+1,n)

y_hat <- matrix(c(1),n+1,n)
for(i in 1:n){
  y_hat[i,] <- (x<x[i])+0
}

## sort the data by x descending
data_t <- rbind(x,y,y_hat)
data_t_sort <- data_t[,order(x)]

x <- data_t_sort[1,]
y <- data_t_sort[2,]
y_hat <- data_t_sort[3:nrow(data_t_sort),]

for(i in 1:n){
  y_hat[i,] <- (x<x[i])+0
}
## replace 0 to -1 in y_hat
y_hat <- replace(y_hat,y_hat==0,-1)

#order 
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