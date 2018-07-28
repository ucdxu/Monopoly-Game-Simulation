findRR=function(pos){
  if(pos==7){return(15)
    if(pos==22){return(25)}
    if(pos==36){return(5)}}
}
###1. Write a function simulate_monopoly() that simulates n turns by a player in a game of Monopoly using two d-sided dice. The inputs to your function should be n and d. The output of your function should be a length n + 1 vector of positions, encoded as numbers from 0 to 39. 
simulate_monopoly <- function(n,d){
  chest <- c(2, 17, 33)
  chance <- c(7, 22, 36)
  poscol=c()
  pos=0
  
  dice1<-sample(1:d,n, replace = T)
  dice2<-sample(1:d,n, replace = T)
  equ<-dice1==dice2
  i<-1
  j<-0
  h<-c()
  
  for(i in 1:n)
  {
    if(equ[i]==1)
    {j<-j+1}
    else{j<-0}
    if(j==3)
    {h<-c(h,i)
    j<-0
    }}

  for(num in 1:n){
    pos=pos+dice1[num]+dice2[num]
    pos=pos%%40
    if(pos%in%chest|pos%in%chance){
      ran=sample(1:16,1)
      if(pos%in%chest&ran==1){pos=1}
      if(pos%in%chest&ran==2){pos=1}
      if(pos%in%chance&ran==1){pos=1}
      if(pos%in%chance&ran==2){pos=10}
      if(pos%in%chance&ran==3){pos=11}
      if(pos%in%chance&ran==4){pos=24}
      if(pos%in%chance&ran==5){pos=39}
      if(pos%in%chance&ran==6){pos=6}
      if(pos%in%chance&ran==7){
        if(pos==7){pos=15}
        if(pos==22){pos=25}
        if(pos==36){pos=5}
      }
      ## go to next RR
      if(pos%in%chance&ran==8){ 
        if(pos==7){pos=15}
        if(pos==22){pos=25}
        if(pos==36){pos=5}}
      ## Chance Rule8 
      if(pos%in%chance&ran==9){ 
        if(pos==7){pos=12}
        if(pos==22){pos=28}
        if(pos==36){pos=12}}
      if(pos%in%chance==10){pos=pos-3}}
      ## Chance rule 9 
  if(num%in%h){pos=10}
  poscol=c(poscol,pos)
    }
  return(poscol)
}
###2. Write a function estimate_monopoly() that uses your simulation to estimate the long-term probabilities of ending a turn on each Monopoly square. What are the 3 most likely squares to end a turn on if you play Monopoly with 6-sided dice? What if you play with 4-sided dice? Display graphically the long-term probabilities for 3, 4, 5, and 6-sided dice. 
estimate_monopoly <- function(k,n,d){
  return(replicate(k,simulate_monopoly(n,d)))
}
result_6 = estimate_monopoly(1000,10000,6)
sort(table(result_6))
result_5 = estimate_monopoly(1000,10000,5)
sort(table(result_5))
result_4 = estimate_monopoly(1000,10000,4)
sort(table(result_4))
result_3 = estimate_monopoly(1000,10000,3)
sort(table(result_3))

barplot(table(result_6), ylab = "Frequency", xlab = "Square", main = "6 Side Dice's Frequency VS. Square")
barplot(table(result_5), ylab = "Frequency", xlab = "Square", main = "5 Side Dice's Frequency VS. Square")
barplot(table(result_4), ylab = "Frequency", xlab = "Square", main = "4 Side Dice's Frequency VS. Square")
barplot(table(result_3), ylab = "Frequency", xlab = "Square", main = "3 Side Dice's Frequency VS. Square")


###3. Use k = 1;000 simulations with n = 10;000 turns each to estimate the standard error for the long-term probability of ending a turn in jail. 
jailp=function(i){sum(i==10)/1000}
p =apply(result_6,1,jailp)
sd(p)

###4. Use the non-parametric bootstrap with b = 1;000 samples from a simulation of n = 10;000 turns to estimate the standard error for the long-term probability of ending a turn in jail.  
####(a) How does the boostrap estimate compare to the simulation estimate? Which do you think is more accurate? Explain. 
####(b) Which is faster to compute: the bootstrap estimate or the simulation estimate? Explain why there is a difference. 
btresult=replicate(1000,sample(result_6[,1],10000,replace = T))
btp=apply(btresult,1,jailp)
sd(btp)
### Boostrape run faster than apply()

###5. Display graphically the standard errors for the long-term probabilities for 3, 4, 5, and 6-sided dice (use the same settings you used in question 2). Discuss why some probabilities have much larger standard errors than others.  
test=function(i){
  btresult=replicate(1000,sample(get(paste0("result_",i))[,1],10000,replace = T))
  p =apply(btresult,2,function(i){prop.table(table(i))})
  p=matrix(unlist(p),ncol = 40)
   return(apply(p,2,sd))
}
sddice = sapply(3:6,test)


plot(sddice[,1],type="b", ylim=c(min(sddice),max(0.0067)), xlim = c(1,40),col="red",ylab="Value",xlab="Square")
lines(sddice[,2],type="l",col="black",lty=3,lwd=2)
lines(sddice[,3],type="l",col="blue",lty=3,lwd=2)
lines(sddice[,4],type="l",col="green",lty=3,lwd=2)

legend("topleft", legend=c("3-Side Dice", "4-Side Dice","5-Side Dice", "6-Side Dice"),
       col=c("red","black", "blue", "green"), lty=1:2, cex=0.8, bg = "white")
grid()
axis(1,at=c(1:nrow(sddice)),labels=rownames(sddice))

###6. What happens to the standard errors for the long-term probability estimates as n increases? Why does this happen? 
##### As n increase, the standard error for the long-term probability estimate would decrease. To explain this situation, I can apply this to another experiment, tossing a fair coin.  If we tossing a coin 10 time, it may produce different probability of both side. If we increase the number of tossing to thousand of times, it would gave us the almost the same probability for both side. This theory is made by Monte Carlo, he stated that the more sample we take from population, the more accurate probability we can get. It also can explained by the formula of standard error.  
