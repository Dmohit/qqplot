library(MASS)
data(Cars93)

gprice <- fitdistr(Cars93$Price, 'gamma')
sgprice <-  fitdistr(sqrt(Cars93$Price), 'gamma')
lnprice <- fitdistr(log(Cars93$Price), 'normal')

gprice$estimate
gprice$sd
gprice$estimate / gprice$sd

sgprice$estimate
sgprice$sd
sgprice$estimate / sgprice$sd

##Evaluating the gamama fit
##graph 1
dev.new()
hist(Cars93$Price, freq = FALSE)
curve(dgamma(x, gprice$estimate[1], gprice$estimate[2]), from = 0.001, to = 70, add = TRUE)

##graph 2
dev.new()
qqplot(qgamma(ppoints(93), gprice$estimate[1], gprice$estimate[2]),
        Cars93$Price)
qqline(Cars93$Price,
        distribution = function(p) qgamma(p, gprice$estimate[1], gprice$estimate[2]), probs = c(0.2, 0.8), col = 'blue', lty = 'dashed')
grid(10,10)

## graph 3
length(Cars93$Price)
y <- ((1:93) - 0.5)/ 93
x <- pgamma(sort(Cars93$Price), gprice$estimate[1], gprice$estimate[2])

dev.new()
plot(y ~ x)
abline(0,1, lty = 'dashed', col = 'blue')
grid(10,10)

##Evaluating gamma + sqrt transform
## graph 1
dev.new()
hist(sqrt(Cars93$Price), freq = FALSE)
curve(dgamma(x, sgprice$estimate[1], sgprice$estimate[2]),
        from = 0.001, to = 9, add = TRUE)

##graph 2
dev.new()
qqplot(qgamma(ppoints(93), sgprice$estimate[1], sgprice$estimate[2]),
        sqrt(Cars93$Price))
qqline(sqrt(Cars93$Price),
        distribution = function(p) qgamma(p, sgprice$estimate[1], sgprice$estimate[2]), probs = c(0.2, 0.8), col = 'blue', lty = 'dashed')
grid(10,10)

## graph 3
length(Cars93$Price)
y <- ((1:93) - 0.5)/ 93
x <- pgamma(sort(sqrt(Cars93$Price)), sgprice$estimate[1], sgprice$estimate[2])

dev.new()
plot(y ~ x)
abline(0,1, lty = 'dashed', col = 'blue')
grid(10,10)

##Evaluating normal + log transform
##graph 1
dev.new()
hist(log(Cars93$Price), freq = FALSE)
curve(dnorm(x, lnprice$estimate[1], lnprice$estimate[2]), from = 1, to = 5, add = TRUE)

##graph 2
dev.new()
qqplot(qnorm(ppoints(93), lnprice$estimate[1], lnprice$estimate[2]),
        log(Cars93$Price))
qqline(log(Cars93$Price),
        distribution = function(p) qnorm(p, lnprice$estimate[1], lnprice$estimate[2]), probs = c(0.2, 0.8), col = 'blue', lty = 'dashed')
grid(10,10)

## graph 3
length(Cars93$Price)
y <- ((1:93) - 0.5)/ 93
x <- pnorm(sort(log(Cars93$Price)), lnprice$estimate[1], lnprice$estimate[2])

dev.new()
plot(y ~ x)
abline(0,1, lty = 'dashed', col = 'blue')
grid(10,10)

