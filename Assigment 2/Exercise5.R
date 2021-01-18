getwd()

setwd("C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\datasets")
images <- "C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\Assigment 2\\Prob_Assigment2\\images\\"

#read data from txt file
drug_response <- read.table('drug_response_time.txt',header = TRUE)

# create separate vectors for each drug
drug_A <- drug_response[drug_response$drug == 'A','time']
drug_B <- drug_response[drug_response$drug == 'B', 'time']

#summary statistics for each drug
tapply(drug_response$time, drug_response$drug, summary)

# The following vectors will help us with the plots
drug_xlim <- c(min(drug_response$time) - 0.1, max(drug_response$time) + 0.1)
drug_xticks <- round(seq(drug_xlim[1], drug_xlim[2], by = 0.1), 2)
xSeq_A <- seq(min(drug_A), max(drug_A), length = 1000)
xSeq_B <- seq(min(drug_B), max(drug_B), length = 1000)
drug_colors <- rainbow(2, v = 0.7, alpha = 0.7)

#Boxplots
pdf(file = paste0(images,'Ex5_1-boxplot.pdf'), width = 9, height = 6)
boxplot(drug_response$time ~ drug_response$drug, col = drug_colors
        , main = "Boxplots of Drug Time Responses"
        , ylab = "Type of Drug", xlab = "Response Time"
        , xaxt = 'n'
        , horizontal=TRUE)
axis(1, at = drug_xticks, labels = drug_xticks)
dev.off()

#Histograms
pdf(file = paste0(images,'Ex5_1-hist.pdf'), width = 9, height = 6)
par(mfrow = c(2, 1))
hist(drug_A
        , col = drug_colors[1]
        , freq = FALSE
        , main = "Histograms of Drug Time Responses"
        , ylab = 'Drug A - Frequency'
        #, cex.axis = 1.5
        , xlab = ''
        , xlim = drug_xlim
        , xaxt = 'n'
        )
points(xSeq_A, dnorm(xSeq_A, mean(drug_A), sqrt(var(drug_response$time))), type = "l", lty = 1, col = "gold", lwd = 2)
axis(1, at = drug_xticks, labels = drug_xticks)
hist(drug_B, col = drug_colors[2]
        , freq = FALSE
        , main = ''
        , xlim = drug_xlim
        , ylab = 'Drug B - Frequency'
        #, cex.axis = 1.5
        , xlab = 'Response Time'
        #, breaks = drug_xticks
        , xaxt = 'n'
        )
axis(1, at = drug_xticks, labels = drug_xticks)
points(xSeq_B, dnorm(xSeq_B, mean(drug_B), sqrt(var(drug_response$time))), type = "l", lty = 1, col = "darkviolet", lwd = 2)
#points(xSeq, dnorm(xSeq, mean(drug_A), sqrt(var(drug_B))/sqrt(length(drug_B))), type = "l", lty = 1, col = "blue", lwd = 2)
dev.off()

#MLE estimates of mean responses times within each group
#mean estimators:
cat('\tMLE-drug A :',sum(drug_A) / length(drug_A),'\n'
    ,'\tMLE-drug B :',sum(drug_B) / length(drug_B),'\n'
    )
#variance estimators
cat('MLE-variance biased:',(sum((drug_A - mean(drug_A)) ^ 2) 
                            + sum((drug_B - mean(drug_B)) ^ 2))/(length(drug_A)+length(drug_B)))
cat('MLE-variance unbiased:',(sum((drug_A - mean(drug_A)) ^ 2) 
                            + sum((drug_B - mean(drug_B)) ^ 2)) / (length(drug_A) + length(drug_B) - 2))
var(drug_response$time)

#Confidence Intervals
Sp <- (sum((drug_A - mean(drug_A)) ^ 2) + sum((drug_B - mean(drug_B)) ^ 2)) / (length(drug_A) + length(drug_B) - 2)
alpha_seq <- c(0.05, 0.01, 0.001)

for (alpha in alpha_seq) {
    drug_t <- qt(p = alpha / 2, df = length(drug_A) + length(drug_B) - 2, lower.tail = F)
    CI_lower <- mean(drug_A) - mean(drug_B) - drug_t * sqrt(Sp) * sqrt((1 / length(drug_A)) + (1 / length(drug_B)))
    CI_upper <- mean(drug_A) - mean(drug_B) + drug_t * sqrt(Sp) * sqrt((1 / length(drug_A)) + (1 / length(drug_B)))
    cat('Confidence Interval for mean(A) - mean(B): ', 100 * (1 - alpha), "% \t [", CI_lower, ' , ', CI_upper, ']','\n')
}

# Confirmation with t.test
for (alpha in alpha_seq) {
cat('CI with t.test: ',100*(1-alpha),'%\t'
    ,'[',as.numeric(t.test(drug_A, drug_B, var.equal = TRUE, conf.level = 1-alpha)$conf.int),']','\n')
}

