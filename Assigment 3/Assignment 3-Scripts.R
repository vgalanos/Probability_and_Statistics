# R-scripts for Assignment 3, Probablity & Statistics for Data Analysis (Msc in Data Science, Part-Time)
# Vasileios Galanos, 12/2019


############################################################################################################################
## EXERCISE 1. #############################################################################################################
############################################################################################################################

rm(list = ls())

datasets <- 'C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\Assigment 3\\Prob_Assigment3\\datasets'
setwd(datasets)

images <- "C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\Assigment 3\\Prob_Assigment3\\images\\"

#install.packages("xlsx")

#call package <xlsx>
library("xlsx")

# import data
joker_numbers <- vector()
for (y in c(2017,2018,2019)){
  file <- read.xlsx(paste('Joker_',y,'.xlsx',sep=''), sheetIndex = 1
                    ,startRow=4, colIndex = 8,header = 'FALSE'
                    ,colClasses = c('numeric'))
  names(file)[1] <- 'joker'
  file = na.omit(file)
  joker_numbers <- c(joker_numbers,as.numeric(file$joker))
  
}

pdf(file = paste0(images,'Ex1_1-barplot.pdf'), width = 9, height = 6)
barplot(joker_freqs
     , col = "#69b3a2"
     , main = "Barplot of Joker numbers"
     , ylab = 'Frequency'
)
dev.off()

# default prob is discrete uniform
m <- chisq.test(joker_freqs)
cat("The p-value is ", m$p.value, "\n")

# More accurately defined:
m <- chisq.test(x = joker_freqs,y = NULL, correct = TRUE
                ,rescale.p = FALSE, simulate.p.value = FALSE, B= 2000
                ,rep(1/length(joker_freqs),length(joker_freqs))
                )
cat("The p-value is ", m$p.value, "\n")

# Theoretical calculation of Pearson's chi-squared test
sum <- 0
n <- length(joker_numbers)
for (k in (1:20)){
  o <- sum(joker_numbers == k)
  sum <- sum + (o/n - 0.05)^2/0.05
}
sprintf('%.5f',n*sum)

pchisq(n*sum,df = 19, lower.tail = FALSE)

############################################################################################################################
## EXERCISE 2. #############################################################################################################
############################################################################################################################

#read data from txt file
drug_response <- read.table('drug_response_time.txt',header = TRUE)

# create separate vectors for each drug
drug_A <- drug_response[drug_response$drug == 'A','time']
drug_B <- drug_response[drug_response$drug == 'B', 'time']

# Test for Normality - KS

ks.test(drug_A,"pnorm",mean(drug_A),sd(drug_A))
ks.test(drug_B,"pnorm",mean(drug_B),sd(drug_B))

# KS goodness of fit

pdf(file = paste0(images,'Ex2_1-KS.pdf'), width = 9, height = 6)
par(mfrow = c(1, 2))
xseq<-seq(mean(drug_A)-4*sd(drug_A),mean(drug_A)+4*sd(drug_A),.01)
cdfnorm<-pnorm(xseq, mean(drug_A), sd(drug_A))
plot(ecdf(drug_A),col="blue", xlab="",
     ylab="Cumulative Probability",
     main="Drug A")
lines(xseq, cdfnorm, col="red", lwd=3)
legend(3.7,0.8,c("ECDF","CDF"),lty=c(1,1),lwd=c(1,3)
       ,col=c("blue","red"), cex=0.5,box.lty=0)

xseq<-seq(mean(drug_B)-4*sd(drug_B),mean(drug_B)+4*sd(drug_B),.01)
cdfnorm<-pnorm(xseq, mean(drug_B), sd(drug_B))
plot(ecdf(drug_B),col="blue", xlab="",
     ylab="",
     main=" Drug B")
lines(xseq, cdfnorm, col="red", lwd=3)
legend(3.8,0.8,c("ECDF","CDF"),lty=c(1,1),lwd=c(1,3)
       ,col=c("blue","red"), cex=0.5,box.lty=0)
dev.off()

# Test for Normality - AD
library(nortest)
ad.test(drug_A)
ad.test(drug_B)

# Test for Normality - SW
shapiro.test(drug_A)
shapiro.test(drug_B)

# QQ-plots
pdf(file = paste0(images,'Ex2_1-QQ.pdf'), width = 9, height = 6)
par(mfrow = c(1, 2))
qqnorm(drug_A, main = "Normal Q-Q Plot- Drug A")
qqline(drug_A,col="red",lty=1,lwd=2)
abline(0,1,col="blue",lty=2,lwd=2)

qqnorm(drug_B, main = "Normal Q-Q Plot- Drug B")
qqline(drug_B,col="red",lty=1,lwd=2)
abline(0,1,col="blue",lty=2,lwd=2)
dev.off()

# Test for the assumption of homogeneity of variances
bartlett.test(time~drug,data = drug_response)
fligner.test(time~drug,data = drug_response)

library(car)
leveneTest(time~drug,data = drug_response)

# Difference in mean response time between the two drugs
t.test(time~drug,data = drug_response, var.equal=T)

kruskal.test(time~drug,data = drug_response)

############################################################################################################################
## EXERCISE 3. #############################################################################################################
############################################################################################################################

before <- c(121.5, 122.4, 126.6, 120.0, 129.1, 124.9, 138.8, 124.5, 116.8, 132.2)
after <- c(117.3, 128.6, 121.3, 117.2, 125.6, 121.5, 124.2, 121.6, 117.9, 125.4)

#Visualization of the data
library(ggplot2)
d <- data.frame(before = before, after = after)
d$obs <- 1:nrow(d)
d2 <- tidyr::gather(d, time, value, -obs)

images <- "C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\Assigment 3\\Prob_Assigment3\\images\\"

pdf(file = paste0(images,'Ex3_1.pdf'), width = 9, height = 6)
ggplot(d2, aes(time, value)) + 
  geom_boxplot(fill = c('mediumaquamarine','purple2'), alpha = 0.5) +
  geom_point() +
  geom_line(aes(group = obs)) +
  scale_x_discrete(limits = c('before', 'after'))
dev.off()

# Summary statistics
summary(d[,c(1,2)])

# Hypothesis testing for the claim
t.test(before,after, paired = TRUE, alternative="greater")

# new alpha
t.test(before,after, paired = TRUE, alternative="greater", conf.level = 0.99)

############################################################################################################################
## EXERCISE 4. #############################################################################################################
############################################################################################################################

a <- data.frame(skills = 'novice', score = c(22.10, 22.30 , 26.20 , 29.60 , 31.70 , 33.50 , 38.90 , 39.70 , 43.20 , 43.20))
b <- data.frame(skills = 'advanced',score = c(32.50, 37.10, 39.10, 40.50, 45.50, 51.30, 52.60, 55.70, 55.90, 57.70))
c <- data.frame(skills = 'proficient',score = c(40.10, 45.60, 51.20, 56.40, 58.10, 71.10, 74.90, 75.90, 80.30, 85.30))

poker_players <- rbind(a, b, c)
# summaries
library(psych)
describeBy(poker_players$score, poker_players$skills, mat = F)

# 1. Visualize Data:
  library(ggplot2)
# Change box plot line colors by groups
pdf(file = paste0(images,'Ex4_1.pdf'), width = 9, height = 6)
  p<-ggplot(poker_players, aes(x=skills, y=score, fill=skills)) +
    geom_boxplot()

  p + scale_fill_brewer(
    palette="Dark2") + theme(
        text = element_text(size=15),legend.position = "none")+ geom_jitter(
          shape=16, position=position_jitter(0.2))      
dev.off()

# 2. Difference between groups 
  #check if means are equal
  fit<-aov(score~factor(skills), data = poker_players)
  fit
  summary(fit)

  # assumptions Homogeneity of variance
  bartlett.test(score~factor(skills), data = poker_players)
  fligner.test(score~factor(skills), data = poker_players)
  
  library(car)
  leveneTest(score~factor(skills), data = poker_players)
  # -> novice and proficient not equal variances
  
  #test for the normality assumptions in the residuals
  pdf(file = paste0(images,'Ex4_2-QQ.pdf'), width = 9, height = 6)
  qqnorm(fit$residuals,main="NPP for residuals")
  qqline(fit$residuals,col="red",lty=1,lwd=2)
  dev.off()
  
  shapiro.test(fit$residuals)  
  
  # Diagnostic plots
  layout(matrix(1:4,2,2))
  plot(fit)
  
  #Non parametric: Kruskall-Wallis
  kruskal.test(score~factor(skills), data = poker_players)
  
  library(gplots)
  pdf(file = paste0(images,'Ex4_4-pair.pdf'), width = 9, height = 9)
  par(mfrow=c(1,1))
  plotmeans(score~factor(skills), data = poker_players
            ,xlab="Skills", ylab="Score")  
  dev.off()
  
  # all pairwise t.tests:
  t.test(a$score,b$score, var.equal = F)
  t.test(a$score,c$score)
  t.test(b$score,c$score)
  
  # Multiple comparisons (proper way):
  TukeyHSD(fit)

  pairwise.t.test(poker_players$score, factor(poker_players$skills), p.adjust.method= 'bonferroni')  
  pairwise.t.test(poker_players$score, factor(poker_players$skills), p.adjust.method= 'holm')  
  
  ## Least significant Differences Method
  DFE<-fit$df.residual
  MSE<-deviance(fit)/DFE
  library(agricolae)
  
  print(LSD.test(poker_players$score, factor(poker_players$skills),DFerror=DFE,MSerror=MSE,
                   p.adj="bonferroni"))
  
  print(scheffe.test(poker_players$score, factor(poker_players$skills),DFerror=DFE,MSerror=MSE))

############################################################################################################################
## EXERCISE 5. #############################################################################################################
############################################################################################################################
grades <- read.table('assignment_grades_data.txt',header = TRUE)

ggplot(data = grades) + 
  geom_point(mapping = aes(x = grades$assignment1, y = grades$assignment2), size = grades$size/5
             )+ scale_x_continuous(name="Assigment 1", limits=c(0, 10)
                                   ) +scale_y_continuous(name="Assigment 2", limits=c(0, 10))

library(ggrepel)
pdf(file = paste0(images,'Ex5_1.pdf'), width = 9, height = 9)
ggplot(grades, aes(assignment1, assignment2, label = size, size = size )) +
geom_text_repel() +
geom_point(color = 'red', size = 2) +
scale_x_continuous(breaks = round(seq(0, 10, by = 1)),labels = factor(0:10),limits = c(0,10) )+ 
scale_y_continuous(breaks = round(seq(0, 10, by = 1),1),labels = factor(0:10),limits = c(0,10))+
theme_set(theme_gray(base_size = 18) )+ theme(legend.position='none') 
dev.off()

###############################################################################################
# 2 Normal Linear Regression
cor(grades$assignment1, grades$assignment2)
cor(grades$size, grades$assignment2)

cor.test(grades$assignment1, grades$assignment2)
cor.test(grades$size, grades$assignment2)

# fit model
fit<-lm(assignment2 ~ assignment1, data=grades)
summary(fit)
anova(fit)

# manual way:
newx <- seq(0, 10, by=0.05)

conf_interval <- predict(fit, newdata=data.frame(assignment1=newx), interval="confidence",
                         level = 0.90)
conf_interval <- data.frame(newx, conf_interval)

pred_interval <- predict(fit, newdata=data.frame(assignment1=newx), interval="prediction",
                         level = 0.90)
pred_interval <- data.frame(newx, pred_interval)

pdf(file = paste0(images,'Ex5_1-conf.pdf'), width = 9, height = 6)
ggplot(grades, aes(assignment1, assignment2)) +
  geom_point(color = 'red', size = 2) +
  scale_x_continuous(breaks = round(seq(0, 10, by = 1)),labels = factor(0:10),limits = c(0,10) )+ 
  scale_y_continuous(breaks = round(seq(0, 10, by = 1),1),labels = factor(0:10),limits = c(0,10))+
  theme_set(theme_gray(base_size = 18)) + 
  geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1], size = 1)+
  geom_line(data = conf_interval,aes(newx, lwr), size = 1, color='steelblue',linetype = "dashed")+
  geom_line(data = conf_interval,aes(newx, upr), size = 1, color='steelblue',linetype = "dashed")+
  geom_line(data = pred_interval,aes(newx, lwr), size = 1, color='orange')+
  geom_line(data = pred_interval,aes(newx, upr), size = 1, color='orange')
dev.off()

# alternative:
pred.int <- predict(fit, interval = "prediction",level = 0.90)
mydata <- cbind(grades, pred.int)

p <- ggplot(mydata, aes(assignment1, assignment2)) +
  geom_point(color = 'red', size = 2) +
  theme_set(theme_gray(base_size = 18) + theme(legend.position = 'none'))+ 
  stat_smooth(method = lm, level = 0.9)

p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

############################################################################
# 3 
fit2<-lm(assignment2 ~ assignment1 + size, data=grades)
summary(fit2)
anova(fit,fit2)


# Normality of residuals:
library(nortest)
ad.test(fit2$residuals)
shapiro.test(fit2$residuals)
t.test(fit2$residuals)

par(mfrow = c(2,2))
plot(fit)

pdf(file = paste0(images,'Ex5_4-summ.pdf'), width = 9, height = 6)
par(mfrow = c(2,2))
plot(fit2)
dev.off()
anova(fit,fit2)

# 5
predict(fit2, newdata = list(assignment1 = 6, size = 10), interval = 'confidence', level = 0.90)

# 6
predict(fit2, newdata = list(assignment1 = 6, size = 10), interval = 'prediction', level = 0.90)

# 7 better model:

# outlier removal
cooksd <- cooks.distance(fit2)

# panw apo 4 mean tou cooks distance thewreitai outlier kai to afairoume
par(mfrow = c(1,1))
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

grades2 <- grades[-c(12),]
#grades2<- grades[-c(10,24),]
#grades2<- grades2[-c(6,24),]
cooksd <- cooks.distance(fit3)

#0.70 : fit3<-lm(assignment2 ~  size + I(assignment1*size) + poly(size,2)+ poly(size,3) , data=grades)

fit3<-lm(assignment2 ~  I(log(assignment1)*size) + poly(size,3) , data=grades)
fit4<-lm(assignment2 ~  I(log(assignment1)*size) + poly(size,2) , data=grades)
anova(fit3,fit4)
cat('Model: 3: ',summary(fit3)$r.squared, summary(fit3)$adj.r.squared)
cat('Model: 4: ',summary(fit4)$r.squared, summary(fit4)$adj.r.squared)

summary(fit3)
plot(fit3)

rownames(grades) <- seq(length=nrow(grades))

par(mfrow=c(2,2))
plot(log(assignment2) ~ log(assignment1), data=grades, xlab="assignment1",
       ylab="assignment2", main="Original data")
abline(lm(log(grades$assignment2) ~ log(grades$assignment1)))
plot(I(1/grades$assignment2) ~ I(1/grades$assignment1), xlab="1/BodyWt",
       ylab="1/assignment2", main="Inverse transformation")
abline(lm(I(1/grades$assignment2) ~ I(1/grades$assignment1)))
plot(I(grades$assignment2^(1/3)) ~ I(grades$assignment1^(1/3)),
       xlab=expression(paste(assignment1^{1/3})),
       ylab=expression(paste(assignment2^{1/3})),
       main="Cubic root transformation")
abline(lm(I(grades$assignment2^(1/3)) ~ I(grades$assignment1^(1/3))))
plot(I(grades$assignment2^(1/2)) ~ I(grades$assignment1^(1/2)),
       xlab=expression(paste(assignment1^{1/2})),
       ylab=expression(paste(assignment2^{1/2})),
       main="Square root transformation")
abline(lm(I(grades$assignment2^(1/2)) ~ I(grades$assignment1^(1/2))))

plot(fit3)

############################################################################################################################
## EXERCISE 6. #############################################################################################################
############################################################################################################################

# 1. Load the data into R
require(stats)
data(mtcars)

head(mtcars)

# 2. Define: am , vs and cyl as factors
str(mtcars)
mtcars$am <- as.factor(mtcars$am)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$cyl <- as.factor(mtcars$cyl)
str(mtcars)


# 3 Illustration and Descriptive stats
describe(mtcars)[c('n','mean','median','sd','min','max','range')]

# mpg per cyl
pdf(file = paste0(images,'Ex6_3-mpg_cyl.pdf'), width = 9, height = 6)
my_boxplot <- ggplot(mtcars,aes(x=cyl,y=mpg)) +
              geom_boxplot(aes(fill = cyl), alpha = 0.5) +
              theme(legend.position = 'none')+
              xlab('Cylinders') +
              ylab('Miles per Gallon')
my_boxplot + coord_flip()
dev.off()

# mpg vs cyl vs weight

pdf(file = paste0(images,'Ex6_3-mpg_weight_cyl.pdf'), width = 9, height = 6)
my_boxplot <- ggplot(mtcars,aes(x=am,y=mpg)) + 
  geom_boxplot(aes(fill = am), alpha = 0.5) + 
  theme(legend.position = 'none')+
  xlab('Transmission (0 = Auto/1=Man)') + ylab('Miles per Gallon')+
  ggtitle("MPG for Auto vs Manual Transmissions")+theme_gray()
p1 <- my_boxplot

my_scatplot <- ggplot(mtcars,aes(x=wt,y=mpg,col=cyl)) + geom_point()
p2 <- my_scatplot + labs(x='Weight (x1000lbs)',y='Miles per Gallon',colour='Number of\n Cylinders') +theme_gray()

figure <- ggarrange(p1, p2,
                    labels = c("", ""),
                    ncol = 2, nrow = 1)
figure
dev.off()

# hp and disp
pdf(file = paste0(images,'Ex6_3-hp_disp.pdf'), width = 9, height = 6)
par(mfrow=c(2,2))
p1 <- ggplot(mtcars, aes(hp)) +
  geom_histogram(binwidth=20) + xlab('horsepower') + ylab('Number of Cars') +
  ggtitle('Distribution of Cars by Horsepower') +theme_gray()

p2 <- ggplot(mtcars, aes(disp)) +
  geom_histogram(binwidth=20) + xlab('Displacement (cu.in.)') + ylab('Number of Cars') +
  ggtitle('Distribution of Cars by Displacement') +theme_gray()

figure <- ggarrange(p1, p2,
                    labels = c("", ""),
                    ncol = 1, nrow = 2)
figure
dev.off()

# rest
pdf(file = paste0(images,'Ex6_3-rest.pdf'), width = 9, height = 6)
my_scatplot <- ggplot(mtcars,aes(x=carb,y=mpg,col=gear)) + geom_point()
p1 <- my_scatplot + labs(x='Number of carburetors',y='Miles per Gallon',colour='Number of\n Cylinders') +theme_gray()
my_scatplot <- ggplot(mtcars,aes(x=drat,y=mpg,col=vs)) + geom_point()
p2 <- my_scatplot + labs(x='Rear axle ratio'
                         ,y='Miles per Gallon'
                         ,colour='Engine (0 = V-shaped, 1 = straight)') +
                  theme_gray()+
                  theme(legend.position="bottom")
figure <- ggarrange(p1, p2,
                    labels = c("", ""),
                    ncol = 1, nrow = 2)
figure
dev.off()


# 4
library(corrplot)
CM<-cor(mtcars[, c('mpg','disp','hp','drat','wt','qsec','gear','carb')])
pdf(file = paste0(images,'Ex6_4-ell.pdf'), width = 9, height = 9)
corrplot(CM, method="ellipse",type="lower")
dev.off()

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(mtcars[, c('mpg','disp','hp','drat','wt','qsec','gear','carb')])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))

pdf(file = paste0(images,'Ex6_4-corr.pdf'), width = 9, height = 9)
corrplot(CM, method="color", col=col(200),
         type="lower", order="hclust",
         addCoef.col = "black", # Add coefficient of corr
         tl.col="black",tl.srt=45, #Text color & rotation
         # Combine with significance
           p.mat = p.mat, sig.level = 0.01, insig = "blank",
         # hide corr. coefficient on the principal diagonal
           diag=FALSE
         )
dev.off()
library(PerformanceAnalytics)
Data<-mtcars[, c('mpg','disp','hp','drat','wt','qsec','gear','carb')]
pdf(file = paste0(images,'Ex6_4-pairs.pdf'), width = 9, height = 6)
chart.Correlation(Data, histogram=TRUE, pch=19)
dev.off()

# 5 Difference in consumption between manual and auto cars

 #normality:
  library(nortest)
  ad.test(mtcars[mtcars$am == 0,'mpg'])
  ad.test(mtcars[mtcars$am == 1,'mpg'])
  
  #homogeneity of variance
  bartlett.test(mpg~am,data = mtcars)
  fligner.test(mpg~am,data = mtcars)
  
  library(car)
  leveneTest(mpg~am,data = mtcars)
 
  #t-test:
  t.test(mpg~am,data = mtcars, var.equal = FALSE)

# 6 Difference in consumption mpg vs cyl
  fit<-aov(mpg~cyl,data=mtcars)
  summary(fit)
  
  # assumptions Homogeneity of variance
  bartlett.test(mpg~cyl,data=mtcars)
  fligner.test(mpg~cyl,data=mtcars)
  
  library(car)
  leveneTest(mpg~cyl,data=mtcars)
  # -> novice and proficient not equal variances
  
  shapiro.test(fit$residuals)  
  
  kruskal.test(mpg~cyl,data=mtcars)
  
  # Diagnostic plots
  layout(matrix(1:4,2,2))
  plot(fit)
  
  
# 7
fitall <- lm(mpg ~  cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb ,data = mtcars)
summary(fitall)
n <- length(mtcars$mpg)

### Backward Elimination method
stepBE<-step(fitall, scope=list(lower = ~ 1,
                                upper= ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb),
             direction="backward",k = log(n), criterion = "BIC", data=mtcars)
stepBE$anova

# Forward
fitnull<-lm(mpg ~ 1, data = mtcars)
stepFS<-step(fitnull, scope=list(lower = ~ 1,
                                 upper= ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb),
             direction="forward",k = log(n), criterion = "BIC", data=mtcars)
stepFS$anova

# stepwise selection method
stepSR<-step(fitall, scope=list(lower = ~ 1,
                                 upper= ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb ),
             direction="both",k = log(n), criterion = "BIC", data=mtcars)
stepSR$anova

# best model:
fitbest <- lm(mpg ~  wt + qsec + am ,data = mtcars)
summary(fitbest)

par(mfrow=c(2,2))
plot(fitbest)