setwd('C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\Assigment 2\\Prob_Assigment2\\scripts')
n <- c(2,5,10,20,40)

cat('Binomial: ','\n')
for (i in n){
  P <- sprintf('%.5f',pbinom(2*i,10*i,0.1))
  cat('For n=',i,'\tP = ',P,'\n')
  
}
cat('\n','Poisson : ','\n')
for (i in n){
  P <- sprintf('%.5f',ppois(2*i,1.9*i))
  cat('For n=',i,'\tP = ',P,'\n')
  
}
cat('\n','Negative Binomial: ','\n')
for (i in n){
  P <- sprintf('%.5f',pnbinom(2*i,5*i,0.65))
  cat('For n=',i,'\tP = ',P,'\n')
  
}

cat('\n','Gamma: ','\n')
for (i in n){
  P <- sprintf('%.5f',pgamma(2*i,5*i,scale = 0.5))
  cat('For n=',i,'\tP = ',P,'\n')
  
}
cat('\n','Chi-Squared: ','\n')
for (i in n){
  P <- sprintf('%.5f',pgamma(2*i,(1*i)/2,scale = 2))
  cat('For n=',i,'\tP = ',P,'\n')
  
}
##########################################################
cat('\n','CLT - Binomial: ','\n')
for (i in n){
  P <- sprintf('%.5f',pnorm((2-1)/( sqrt(0.9)/sqrt(i) )) )
  cat('For n=',i,'\tP = ',P,'\n')
  
}
cat('\n','CLT - Poisson: ','\n')
for (i in n){
  P <- sprintf('%.5f',pnorm((2-1.9)/( sqrt(1.9)/sqrt(i) ) ) )
  cat('For n=',i,'\tP = ',P,'\n')
  
}
cat('\n','CLT - Negative Binomial: ','\n')
for (i in n){
  P <- sprintf('%.5f',pnorm((2 - (1.75/0.65))/((sqrt(1.75)/0.65)/sqrt(i))) )
  cat('For n=',i,'\tP = ',P,'\n')
  
}
cat('\n','CLT - Gamma: ','\n')
for (i in n){
  P <- sprintf('%.5f',pnorm((2-2.5)/( sqrt(1.25)/sqrt(i) ) ) )
  cat('For n=',i,'\tP = ',P,'\n')
  
}
cat('\n','CLT - Chi-Squared: ','\n')
for (i in n){
  P <- sprintf('%.5f',pnorm((2-1)/( sqrt(2)/sqrt(i) ) ) )
  cat('For n=',i,'\tP = ',P,'\n')
  
}

sprintf('%.5f',(5*0.5^2)) 

sprintf('%.5f',sqrt(1.25))

        