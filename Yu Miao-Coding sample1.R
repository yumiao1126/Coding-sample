# Q1. 
library(DEoptim)
library(ggplot2)

# a. 
n<-1000
cost<-2
price<-6
Profit1<-function(rolls){
  demand<-round(rnorm(1, 100, 40))
  sales<-pmin(rolls, demand)
  profit<-(-(sales*price-rolls*cost))
  return(profit)
}

v_roll1<-c()
v_pro1<-c()
for(i in 1:1000){
  res<-DEoptim(Profit1, 0,  300)
  v_roll1<-c(v_roll1, res$optim$bestmem['par1'])
  v_pro1<-c(v_pro1, res$optim$bestval)
}

ggplot()+geom_density(aes(x=v_roll1))
ggplot()+geom_density(aes(x=v_pro1))

# b. 
demand_90<-quantile(demand, 0.90)
demand_90

Profit2<-function(rolls){
  demand<-rnorm(1, 100, 40)
  demand_90<-round(quantile(demand, 0.90))
  sales<-pmin(rolls, demand_90)
  profit<-(-(sales*price-rolls*cost))
  return(profit)
}

v_roll2<-c()
v_pro2<-c()
for(i in 1:1000){
  res<-DEoptim(Profit2, 0,  300)
  v_roll2<-c(v_roll2, res$optim$bestmem['par1'])
  v_pro2<-c(v_pro2, res$optim$bestval)
}

ggplot()+geom_density(aes(x=v_roll2))
ggplot()+geom_density(aes(x=v_pro2))


# Q2. 
n<-1000
pnight<-200
roomnum<-100

compensation<-1.2*pnight
refund<-0.3*pnight


qlnorm2<-function(p, mean, sd){
  qlnorm(p, log(mean*(1+sd^2/mean^2)^-0.5), log(1+sd^2/mean^2)^0.5)
}

Revenue<-function(rooms){
  no_shows<-qlnorm2(runif(1), 0.2*rooms, 0.05*rooms)
  com_customer<-pmax(0, rooms-no_shows-roomnum)
  revenue<-pnight*rooms-no_shows*refund-com_customer*compensation
  return(-(revenue))
}

v_rooms<-c()
v_pro<-c()
for(i in 1:1000){
  res<-DEoptim(Revenue, 100,  500)
  v_rooms<-c(v_rooms, res$optim$bestmem['par1'])
  v_pro<-c(v_pro, res$optim$bestval)
}

ggplot()+geom_density(aes(x=v_rooms))
ggplot()+geom_density(aes(x=v_pro))

