#----------------------------------------------------------------------
# R code for exercises in Assigment 1
#
# Course: Probability & Statistics for data analysis
# MSc in Data Science (PT) 2019-2021
# Vasileios Galanos, 2019-10-27
# 
#----------------------------------------------------------------------
getwd()
setwd('C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\Prob_Assigment1\\As1_Figures')
#----------------------------------------------------------------------
# Exercise 3.1
m<-100000
# Let c1 represent a ball of colour 1. In that way:
a <- c(rep('c1',3),rep('c2',3),rep('c3',3),rep('c4',3),rep('c5',3)
       ,rep('c6',3),rep('c7',3),rep('c8',3),rep('c9',3),rep('c10',3))

prob <- numeric(m)
sum<- 0
set.seed(111)

for (i in 1:m){
  s<- sample(a,10,replace = FALSE)
  if(length(unique(s)) == 10){
    sum <- sum + 1
  }
  prob[i] <- sum/i
}
sprintf("%.10f",sum/m)

#pdf(file = 'Ex3_1-figure.pdf', width = 9, height = 6)
plot(prob, xlab = 'Number of iterations', 
     ylab = 'Estimated probability of all balls different', 
     main = 'Coloured Balls experiment',
     col = 'dodgerblue3', lwd = 2, type = 'l'
     ,xaxt = "n")
axis(1, at=seq(1, m, by=10000))
abline(h = 0.001965351, col = 'firebrick1', lty = 3, lwd = 2)
legend('topright', 'true value', lty = 3, lwd = 2, col = 'firebrick1')
#
#dev.off()

# exw 3C1 tropous gia na epilexw to ena xrwma ^ 10 fores pou einai ola ta xrwmata
# kai to diairw me olous tous pithanous sunduasmous
sprintf("%.10f",(3^10)/30045015)
#----------------------------------------------------------------------
# Exercise 3.2
sum<- 0
m<- 100000
prob <- numeric(m)

for (i in 1:m){
  s<- sample(a,10,replace = FALSE)
  if(sum(table(s) == 3) >= 1){
    sum <- sum + 1
  }
  prob[i] <- sum/i
}
sprintf("%.10f",sum/m)

#pdf(file = 'Ex3_2-figure.pdf', width = 9, height = 6)
plot(prob, xlab = 'Number of interations', 
     ylab = 'Estimated probability of at least three balls of the same colour', 
     main = 'Coloured Balls experiment',
     col = 'dodgerblue3', lwd = 2, type = 'l'
     ,xaxt = "n")
axis(1, at=seq(1, m, by=10000))
abline(h = 0.2797352572, col = 'firebrick1', lty = 3, lwd = 2)
legend('right', 'true value', lty = 3, lwd = 2, col = 'firebrick1')
#dev.off()
#----------------------------------------------------------------------
# Exercise 7.1 - 100 samples
x_100 <- read.table('C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\datasets\\call_duration_100.txt')
x_100 <- unlist(x_100, use.names = FALSE)
#pdf(file = 'Ex7_1-figure_100.pdf', width = 9, height = 6)
hist(x_100 , 40, freq = FALSE, xlab = "Duration(Minutes)"
     ,main='Histogram of Call Duration - 100 samples')
xSeq <- seq(min(x_100),max(x_100), length = 1000)
points(xSeq, xSeq *exp(-xSeq ), type = "l", col = "red", lwd = 2)
legend("topright", lty = 1, col = 2, bty = "n",
       legend=bquote(italic(f)*"("*italic(x)*") ="~x~italic(e)^{- ~x} ))
#dev.off()

#Exercise 7.1 - 10000 samples
x_10000 <- read.table('C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\datasets\\call_duration_10000.txt')
x_10000 <- unlist(x_10000, use.names = FALSE)

#pdf(file = 'Ex7_1-figure_10000.pdf', width = 9, height = 6)
hist(x_10000 , 40, freq = FALSE, xlab = "Duration(Minutes)"
     ,main='Histogram of Call Duration - 10000 samples')
xSeq <- seq(min(x_10000),max(x_10000), length = 1000)
points(xSeq, xSeq *exp(-xSeq ), type = "l", col = "red", lwd = 2)
legend("topright", lty = 1, col = 2, bty = "n",
       legend=bquote(italic(f)*"("*italic(x)*") ="~x~italic(e)^{- ~x} ))
#dev.off()

# Exercise 7.2
n_100 <- length(x_100)
n_10000 <- length(x_10000)

#a At most 1 minute
sprintf("%.4f", sum((x_100 <= 1)*1)/n_100 )
sprintf("%.4f", sum((x_10000 <= 1)*1)/n_10000 )

#b At least 2 minute
sprintf("%.4f",sum((x_100 >= 2)*1)/n_100)
sprintf("%.4f",sum((x_10000 >= 2)*1)/n_10000)

#c Mean and Variance
sprintf("%.4f",mean(x_100))
sprintf("%.4f",mean(x_10000))

sprintf("%.4f",var(x_100))
sprintf("%.4f",var(x_10000))

#d We must first calculate the cost of all calls from the samples
cost_100 <- numeric(n_100)
for (i in 1:n_100)
{
  if (x_100[i] <=3){ cost_100[i] <- 2	}
  else{ cost_100[i] <- 2 + 6*(x_100[i] - 3) }
}

cost_10000 <- numeric(n_10000)
for (i in 1:n_10000)
{
  if (x_10000[i] <=3){ cost_10000[i] <- 2	}
  else{ cost_10000[i] <- 2 + 6*(x_10000[i] - 3) }
}

sprintf("%.4f",cost_100[mean(x_10000)])
sprintf("%.4f",cost_10000[mean(x_10000)])

#e The average cost of a call is
sprintf("%.4f",mean(cost_100))
sprintf("%.4f",mean(cost_10000))

#f The first and 3rd quartiles of call durations are
quantile(x_100, prob = 1:3/4)

quantile(x_10000, prob = 1:3/4)








