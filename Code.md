# stochastic-project

#iid model

#define transition matrix:
S <- matrix(c(0.8412,0.1588,0.0405, 0.9595), #transition matrix
            nrow=2, ncol=2, byrow=TRUE)
P<-matrix(c(0.0631, 1, 0.1963, 0, 0.7406, 0), nrow=3, ncol=2, byrow=TRUE) #call probabilities

S0<-c(1, 0) # inital state
P0<-P %*% S0 #inital call distribution
S1<-S0 %*% S


P1<-P %*% t(S1)
P0
P1

#iid model:
x<-c()
for (i in 1:20) {
  
  k <- sample(c('k','q', 's'), 60, replace=T, prob=c(0.4921, 0.1064, 0.4015)) #generates a sample from the iid model
  t<-ftable(k) #frequency table
  x<-c(x,t)#add frequency to array
  
}
x <- matrix(x, nrow=60, ncol=3, byrow=T) #matrix of frequencies
x
#how to draw a stacked area plot???



#2-state model

#define transition matrix:
S <- matrix(c(0.8412,0.1588,0.0405, 0.9595), #transition matrix
            nrow=2, ncol=2, byrow=TRUE)
P<-matrix(c(0.0631, 1, 0.1963, 0, 0.7406, 0), nrow=3, ncol=2, byrow=TRUE) #call probabilities


x<-c()
v<-c()
n<-40 #number of samples
t<-c(0,0,0)
SC<-c(0, 1) # inital state
PC<-P%*%SC #probabilities from inital state

for(j in 1:n){
for (i in 1:60) {
  
  k <- sample(c("k","q", "s"), 1, replace=T, prob=c(PC[1],PC[2] ,PC[3] )) #generates a sample from the 2-state model
  SC<-SC%*%S
  PC<-P%*%t(SC)
  v<-c(v,k)
}
  t<-c(table(v),0,0,0) #add zeros in case of nill return
  t<-t[1:3] #
  x<-c(x,t)
v<-c()

}

x <- matrix(x, nrow=n, ncol=3, byrow=T)

write(x,file="test.txt")
colnames(x)<-c('k','q','s')


write(x,file="test.csv")
