getwd()

setwd("C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\datasets")
images <- "C:\\Users\\Owner\\Desktop\\Msc Data Science\\Probability and Statistics\\Assigment 2\\Prob_Assigment2\\images\\"


nIter <- 30000

n <- 10
mu_1 <- 4
mu_2 <- 4.1
sigma2 <- 0.01
sigma <- sqrt(sigma2)
ci_probability <- mu_2_hat <- mu_1_hat <- s2_hat <- s2_p <- numeric(nIter)
mse1 <- mse2 <- numeric(nIter)
freq <- freq1 <- freq2 <- 0

t_dist <- qt(p = 0.05 / 2, df = 2*n - 2, lower.tail = F)

for (i in 1:nIter) {
    x <- rnorm(n, mean = mu_1, sd = sqrt(sigma2))
    y <- rnorm(n, mean = mu_2, sd = sqrt(sigma2))
    mu_1_hat[i] <- mean(x)
    mu_2_hat[i] <- mean(y)
    s2_p[i] <- (sum((x - mu_1_hat[i]) ^ 2) + sum((y - mu_2_hat[i]) ^ 2)) / 18
    s2_hat[i] <- (2*n - 2) * s2_p[i] / (2*n)
    freq1 <- freq1 + (s2_hat[i] - sigma2) ^ 2
    freq2 <- freq2 + (s2_p[i] - sigma2) ^ 2
    mse1[i] <- freq1 / i
    mse2[i] <- freq2 / i
    CI_lower <- mu_1_hat[i] - mu_2_hat[i] - t_dist * sqrt(s2_p[i]) * sqrt(2/n)
    CI_upper <- mu_1_hat[i] - mu_2_hat[i] + t_dist * sqrt(s2_p[i]) * sqrt(2/n)
    #ci <- as.numeric(t.test(x)$conf.int)
    freq <- freq + ((CI_lower <= mu_1 - mu_2) & (CI_upper >= mu_1 - mu_2)) * 1
    ci_probability[i] <- freq / i
    if (i %% 1000 == 0) {
        cat(paste0("   iteration ", i), "\r")
    }
}

#6.1
sum(ci_probability)/nIter

#########################################################################

x <- rnorm(n, mean = mu_1, sd = sqrt(sigma2))
y <- rnorm(n, mean = mu_2, sd = sqrt(sigma2))

Sp <- (sum((x - mean(x)) ^ 2) + sum((y - mean(y)) ^ 2)) / 18
var(c(x, y))

CI_lower <- mean(x) - mean(y) - t_dist * sqrt(Sp) * sqrt(2 / n)
CI_upper <- mean(x) - mean(y) + t_dist * sqrt(Sp) * sqrt(2 / n)
((CI_lower <= mu_1 - mu_2) & (CI_upper >= 0.1)) * 1

cat(CI_lower, CI_upper,'\n')
cat(as.numeric(t.test(x, y, var.equal = TRUE, conf.level = 0.95)$conf.int),'\n')
#########################################################################
pdf(file = paste0(images,'Ex6_1.pdf'), width = 9, height = 6)
par(mfrow = c(2, 2), mar = c(4,5,4,0))
hist(mu_1_hat, freq = F, 30, xlab = bquote(bar(X)), main = "", cex.axis = 1.5, cex.lab = 1.5) #, xlim = c(6, 16))
abline(v = c(mu_1, mean(mu_1_hat)), col = c("blue", "red"), lwd = 3, lty = c(1, 2))
xSeq <- seq(min(mu_1_hat), max(mu_1_hat), length = 1000)
points(xSeq, dnorm(xSeq, mu_1, sigma / sqrt(n)), type = "l", lty = 1, col = "red", lwd = 2)
legendText <- c(as.expression(bquote(mu)),
            "empirical mean",
            bquote('N(' * mu * '1,' * sigma ^ 2 * '/' * n * ')'))
legend("topleft", legend = legendText, col = c("blue", "red", "red"), lty = c(1, 2, 1), lwd = 2, bty = "n", cex = 0.7)

hist(mu_2_hat, freq = F, 30, xlab = bquote(bar(Y)), main = "", cex.axis = 1.5, cex.lab = 1.5)#, xlim = c(6, 16))
abline(v = c(mu_2, mean(mu_2_hat)), col = c("blue", "red"), lwd = 3, lty = c(1, 2))
xSeq <- seq(min(mu_2_hat), max(mu_2_hat), length = 1000)
points(xSeq, dnorm(xSeq, mu_2, sigma / sqrt(n)), type = "l", lty = 1, col = "red", lwd = 2)
legendText <- c(as.expression(bquote(mu)),
            "empirical mean",
            bquote('N(' * mu * '2,' * sigma ^ 2 * '/' * m * ')'))
legend("topleft", legend = legendText, col = c("blue", "red", "red"),lty = c(1, 2, 1), lwd = 2, bty = "n", cex = 0.7)

hist(s2_hat, freq = F, 30, xlab = bquote(hat(sigma) ^ 2), main = "", cex.axis = 1.5, cex.lab = 1.5)
abline(v = c(sigma ^ 2, mean(s2_hat)), col = c("blue", "red"), lwd = 3, lty = c(1, 2))
xSeq <- seq(min(s2_hat), max(s2_hat), length = 1000)
points(xSeq, dgamma(xSeq, shape = (2*n - 2) / 2, scale = 2 * (sigma ^ 2 )/( 2*n)), type = "l", lty = 1, col = "red", lwd = 2)
legendText <- c(as.expression(bquote(sigma ^ 2)),
                "empirical mean",
                bquote('G(' * '(m+n-2)/2' * ',' * '2' * sigma ^ 2 * '/m+n)'))
legend(0.020,120, legend = legendText, col = c("blue", "red", "red"), lty = c(1, 2, 1), lwd = 2, bty = "n", cex = 0.6)

hist(s2_p, freq = F, 30, xlab = bquote(S[p] ^ 2), main = "", cex.axis = 1.5, cex.lab = 1.5)
abline(v = c(sigma ^ 2, mean(s2_p)), col = c("blue", "red"), lwd = 3, lty = c(1, 2))
xSeq <- seq(min(s2_p), max(s2_p), length = 1000)
points(xSeq, dgamma(xSeq, shape = (2*n - 2) / 2, scale = 2 * (sigma ^ 2 )/ (2*n - 2)), type = "l", lty = 1, col = "red", lwd = 2)
legendText <- c(as.expression(bquote(sigma ^ 2)),
                "empirical mean",
                bquote('G(' * '(m+n-2)/2' * ',' * '2' * sigma ^ 2 * '/(m+n-2))'))
legend(0.020,120, legend = legendText, col = c("blue", "red", "red"), lty = c(1, 2, 1), lwd = 2, bty = "n", cex = 0.6)
dev.off()

#6.2
pdf(file = paste0(images,'Ex6_2-ci.pdf'), width = 9, height = 6)
plot(ci_probability[1:i], type = 'l', xlab = 'iteration', ylab = 'Coverage probability')
abline(h = 1 - 0.05, lty = 2)
legend('topright', c('True value'), lty = 2)
dev.off()

#6.3
pdf(file = paste0(images,'Ex6_2-mse.pdf'), width = 9, height = 6)
matplot(cbind(mse1[1:i],mse2[1:i]), type = "l", lty = 1, col = 2:3, xlab = 'iteration', ylab = 'MSE', lwd = 2)
abline(h=2*sigma^4/(2*n), col = 2, lty = 2)
abline(h=2*sigma^4/(2*n-2), col = 3, lty = 2)
legend('bottomright', 
       c('Biased estimator (MLE)', 'Unbiased estimator (UMVUE)', 'True value'), 
       col = c(2:3,1), lty = c(1,1,2),cex = 0.8)
dev.off()

