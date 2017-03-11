rm(list=ls())
# Clear Console:
cat("\014")
library(simpleboot)
library(dplyr)
library(repr)
read.file = function(file = 'grad-students.csv'){
  ## Read the csv file
  gradstudents <- read.csv(file, header = TRUE, 
                         stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  #numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  #auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  gradstudents[complete.cases(gradstudents), ]
}

# Load and cleanse the csv file.
gradstudents = read.file()
# Show first 10 records.
head(gradstudents)
#Log of price data
logprice <- log(price)

#z-Score Normalization of price data
mean_price <- mean(price)
sd_price <- sd(price)
normalized_price <- (price - mean_price) / sd_price
mean_normalized_price <- mean(normalized_price)

#z-Score Normalization of log price data
mean_logprice <- mean(logprice)
sd_logprice <- sd(logprice)
normalized_logprice <- (logprice - mean_logprice) / sd_logprice
mean_normalized_logprice <- mean(normalized_logprice)

#Add log price and normalized log price to the data frame.
auto.price = mutate(auto.price, logprice = logprice, normalized_price = normalized_price, normalized_logprice = normalized_logprice)

#Stratifying the data.
#Stratify By Fuel Type
logpricegroupedbyfueltypegas = auto.price %>% filter(fuel.type == 'gas')
logpricegroupedbyfueltypediesel = auto.price %>% filter(fuel.type == 'diesel')
#Stratify by Aspiration
logpricegroupedbyaspirationstd = auto.price %>% filter(aspiration == 'std')
logpricegroupedbyaspirationsturbo = auto.price %>% filter(aspiration == 'turbo')

#Stratify data as per body style.
stratified_pricebody_con = auto.price %>% filter(body.style == 'convertible')
stratified_pricebody_hat = auto.price %>% filter(body.style == 'hatchback')
stratified_pricebody_sed = auto.price %>% filter(body.style == 'sedan')
stratified_pricebody_wag = auto.price %>% filter(body.style == 'wagon')
stratified_pricebody_har = auto.price %>% filter(body.style == 'hardtop')

#Standard mean.
stdmeanftgas <- mean(logpricegroupedbyfueltypegas$normalized_logprice)
stdmeanftdiesel <- mean(logpricegroupedbyfueltypediesel$normalized_logprice)
stdmeanaspstd <- mean(logpricegroupedbyaspirationstd$normalized_logprice)
stdmeanasptur <- mean(logpricegroupedbyaspirationsturbo$normalized_logprice)
stdmeanbsconv <- mean(stratified_pricebody_con$normalized_logprice)
stdmeanbshar <- mean(stratified_pricebody_har$normalized_logprice)
stdmeanbshat <- mean(stratified_pricebody_hat$normalized_logprice)
stdmeanbssed <- mean(stratified_pricebody_sed$normalized_logprice)
stdmeanbswag <- mean(stratified_pricebody_wag$normalized_logprice)

## Student T-Test to calculate the mean based on Fuel Type.
ttestftgas <- t.test(logpricegroupedbyfueltypegas$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")
ttestftgasestimate <- ttestftgas$estimate
ttestftgasmean <- ttestftgasestimate[1]
ttestftdiesel <- t.test(logpricegroupedbyfueltypediesel$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")
ttestftdieselestimate <- ttestftdiesel$estimate
ttestftdieselmean <- ttestftdieselestimate[1]

## Student T-Test to calculate the mean based on Aspiration.
ttestaspstd<- t.test(logpricegroupedbyaspirationstd$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")
ttestaspstdestimate <- ttestaspstd$estimate
ttestaspstdmean <- ttestaspstdestimate[1]
ttestaspturbo <- t.test(logpricegroupedbyaspirationsturbo$normalized_logprice, auto.price$normalized_logprice, alternative = "two.sided")
ttestaspturboestimate <- ttestaspturbo$estimate
ttestaspturbomean <- ttestaspturboestimate[1]

## Bootstrap the mean of the auto price grouped by aspiration
mean.boot.aspstd = one.boot(logpricegroupedbyaspirationstd$normalized_logprice, mean, R = 100000)
mean.boot.aspturb = one.boot(logpricegroupedbyaspirationsturbo$normalized_logprice, mean, R = 100000)
# Retrieve mean for numerical comparision between the means using ONE BOOT method.
NumericBoostrapMeanAspStd <- mean.boot.aspstd$t0
NumericBoostrapMeanAspTur <- mean.boot.aspturb$t0

## Bootstrap the mean of the auto price grouped by fuel type
mean.boot.ftgas = one.boot(logpricegroupedbyfueltypegas$normalized_logprice, mean, R = 100000)
mean.boot.ftdiesel = one.boot(logpricegroupedbyfueltypediesel$normalized_logprice, mean, R = 100000)
# Retrieve mean for numerical comparision between the means using ONE BOOT method.
NumericBoostrapMeanftgas <- mean.boot.ftgas$t0
NumericBoostrapMeanftdiesel <- mean.boot.ftdiesel$t0


cat("Difference between ONE bootstrap resampled mean and standard mean of Turbo cars" , "is:",NumericBoostrapMeanAspTur - stdmeanasptur,"\n")
cat("Difference between ONE bootstrap resampled mean and standard mean of Standard cars" , "is:",NumericBoostrapMeanAspStd - stdmeanaspstd,"\n")
cat("Difference between ONE bootstrap resampled mean of turbo and t-test mean of turbo cars" , "is:",NumericBoostrapMeanAspTur - ttestaspturbomean,"\n")
cat("Difference between ONE bootstrap resampled mean of standard and t-test mean of standard cars" , "is:",NumericBoostrapMeanAspStd - ttestaspstdmean,"\n")

cat("Difference between ONE bootstrap resampled mean and standard mean of Diesel cars" , "is:",NumericBoostrapMeanftdiesel - stdmeanftdiesel,"\n")
cat("Difference between ONE bootstrap resampled mean and standard mean of Gas cars" , "is:",NumericBoostrapMeanftgas - stdmeanftgas,"\n")
cat("Difference between ONE bootstrap resampled mean of Diesel and t-test mean of Diesel cars" , "is:",NumericBoostrapMeanftdiesel - ttestftdieselmean,"\n")
cat("Difference between ONE bootstrap resampled mean of Gas and t-test mean of gas cars" , "is:",NumericBoostrapMeanftgas - ttestftgasmean,"\n")

#Plot Mean
plot.hist <- function(a, maxs,mins, cols = c('pop_A', 'pop_B'), nbins = 80, p = 0.05){
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, main = paste('Histogram of Means'), xlab = cols[1])
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = 0, lwd = 4, col = 'blue')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}

plot.t <- function(a, b, cols = c('pop_A', 'pop_B'), nbins = 80, p = 0.05){
  maxs = max(c(max(a), max(b)))
  mins = min(c(min(a), min(b)))
  par(mfrow = c(2, 1))
  plot.hist(a, maxs, mins, cols = cols[1])
  plot.hist(b, maxs, mins, cols = cols[2])
  par(mfrow = c(1, 1))
}
#Visually see the difference in between ONE bootstrap resampled Std and Turbo means.
plot.t(mean.boot.aspturb$t,mean.boot.aspstd$t, cols = c('Turbo', 'Standard'),nbins = 80)

#Visually see the difference in between ONE bootstrap resampled Std and Turbo means.
plot.t(mean.boot.ftdiesel$t, mean.boot.ftgas$t, cols = c('Diesel', 'Gas'),nbins = 80)

# Problem # 2
## Function to plot TWO BOOT.
plot.diff <- function(a, cols = c('pop_A'), nbins = 80, p = 0.05){
  maxs = max(a)
  mins = min(a)
  plot.hist(a, maxs, mins, cols = cols[1])
}

options(repr.plot.width=6, repr.plot.height=4)

## One Bootstrap the mean of the auto price grouped by body style
mean.boot.bsconv = one.boot(stratified_pricebody_con$normalized_logprice, mean, R = 100000)
mean.boot.bshar = one.boot(stratified_pricebody_har$normalized_logprice, mean, R = 100000)
mean.boot.bshat = one.boot(stratified_pricebody_hat$normalized_logprice, mean, R = 100000)
mean.boot.bssed = one.boot(stratified_pricebody_sed$normalized_logprice, mean, R = 100000)
mean.boot.bswag = one.boot(stratified_pricebody_wag$normalized_logprice, mean, R = 100000)

# Two BootStrap mean of the auto price based on body style
## Two Bootstrap : Difference in means of Hardtop & Convertible
two.boot.mean.bsharconv = two.boot(stratified_pricebody_har$normalized_logprice,stratified_pricebody_con$normalized_logprice,mean, R = 100000)
## Two Bootstrap : Difference in means of Hatchback & Convertible
two.boot.mean.bshatconv = two.boot(stratified_pricebody_hat$normalized_logprice,stratified_pricebody_con$normalized_logprice,mean, R = 100000)
## Two Bootstrap : Difference in means of Sedan & Convertible
two.boot.mean.bssedconv = two.boot(stratified_pricebody_sed$normalized_logprice,stratified_pricebody_con$normalized_logprice,mean, R = 100000)
## Two Bootstrap : Difference in means of Wagon & Convertible
two.boot.mean.bswagconv = two.boot(stratified_pricebody_wag$normalized_logprice,stratified_pricebody_con$normalized_logprice,mean, R = 100000)
## Two Bootstrap : Difference in means of Hatchback & Hardtop
two.boot.mean.bshathar = two.boot(stratified_pricebody_hat$normalized_logprice,stratified_pricebody_har$normalized_logprice,mean, R = 100000)
## Two Bootstrap : Difference in means of Sedan & Hardtop
two.boot.mean.bssedhar = two.boot(stratified_pricebody_sed$normalized_logprice,stratified_pricebody_har$normalized_logprice,mean, R = 100000)
## Two Bootstrap : Difference in means of Wagon & Hardtop
two.boot.mean.bswaghar = two.boot(stratified_pricebody_wag$normalized_logprice,stratified_pricebody_har$normalized_logprice,mean, R = 100000)
## Two Bootstrap : Difference in means of Sedan & Hatchback
two.boot.mean.bssedhat = two.boot(stratified_pricebody_sed$normalized_logprice,stratified_pricebody_hat$normalized_logprice,mean, R = 100000)
## Two Bootstrap : Difference in means of Wagon & Hatchback
two.boot.mean.bswaghat = two.boot(stratified_pricebody_wag$normalized_logprice,stratified_pricebody_hat$normalized_logprice,mean, R = 100000)
## Two Bootstrap : Difference in means of Wagon & Sedan
two.boot.mean.bswagsed = two.boot(stratified_pricebody_wag$normalized_logprice,stratified_pricebody_sed$normalized_logprice,mean, R = 100000)

# Retrieve mean for numerical comparision between the means using ONE BOOT method.
NumericBoostrapMeanbsconv <- mean.boot.bsconv$t0
NumericBoostrapMeanbshar <- mean.boot.bshar$t0
NumericBoostrapMeanbshat <- mean.boot.bshat$t0
NumericBoostrapMeanbssed <- mean.boot.bssed$t0
NumericBoostrapMeanbswag <- mean.boot.bswag$t0

#Numerical difference.
cat("Difference between ONE bootstrap resampled mean of Hardtop and covertible cars" , "is:",NumericBoostrapMeanbshar - NumericBoostrapMeanbsconv,"\n")
cat("Difference between TWO bootstrap resampled mean of Hardtop and covertible cars" , "is:",two.boot.mean.bsharconv$t0,"\n")
cat("Difference between ONE bootstrap resampled mean of Hatchback and covertible cars" , "is:",NumericBoostrapMeanbshat - NumericBoostrapMeanbsconv,"\n")
cat("Difference between TWO bootstrap resampled mean of Hatchback and covertible cars" , "is:",two.boot.mean.bshatconv$t0,"\n")
cat("Difference between ONE bootstrap resampled mean of Sedan and covertible cars" , "is:",NumericBoostrapMeanbssed - NumericBoostrapMeanbsconv,"\n")
cat("Difference between TWO bootstrap resampled mean of Sedan and covertible cars" , "is:",two.boot.mean.bssedconv$t0,"\n")
cat("Difference between ONE bootstrap resampled mean of Wagon and covertible cars" , "is:",NumericBoostrapMeanbswag - NumericBoostrapMeanbsconv,"\n")
cat("Difference between TWO bootstrap resampled mean of Wagon and covertible cars" , "is:",two.boot.mean.bswagconv$t0,"\n")
cat("Difference between ONE bootstrap resampled mean of Hatchback and hardtop cars" , "is:",NumericBoostrapMeanbshat - NumericBoostrapMeanbshar,"\n")
cat("Difference between TWO bootstrap resampled mean of Hatchback and hardtop cars" , "is:",two.boot.mean.bshathar$t0,"\n")
cat("Difference between ONE bootstrap resampled mean of Sedan and hardtop cars" , "is:",NumericBoostrapMeanbssed - NumericBoostrapMeanbshar,"\n")
cat("Difference between TWO bootstrap resampled mean of Sedan and hardtop cars" , "is:",two.boot.mean.bssedhar$t0,"\n")
cat("Difference between ONE bootstrap resampled mean of Wagon and hardtop cars" , "is:",NumericBoostrapMeanbswag - NumericBoostrapMeanbshar,"\n")
cat("Difference between TWO bootstrap resampled mean of Wagon and hardtop cars" , "is:",two.boot.mean.bswaghar$t0,"\n")
cat("Difference between ONE bootstrap resampled mean of Sedan and hatchback cars" , "is:",NumericBoostrapMeanbssed - NumericBoostrapMeanbshat,"\n")
cat("Difference between TWO bootstrap resampled mean of Sedan and hatchback cars" , "is:",two.boot.mean.bssedhat$t0,"\n")
cat("Difference between ONE bootstrap resampled mean of Wagon and hatchback cars" , "is:",NumericBoostrapMeanbswag - NumericBoostrapMeanbshat,"\n")
cat("Difference between TWO bootstrap resampled mean of Wagon and hatchback cars" , "is:",two.boot.mean.bswaghat$t0,"\n")
cat("Difference between ONE bootstrap resampled mean of Wagon and sedan cars" , "is:",NumericBoostrapMeanbswag - NumericBoostrapMeanbssed,"\n")
cat("Difference between TWO bootstrap resampled mean of Wagon and sedan cars" , "is:",two.boot.mean.bswagsed$t0,"\n")


# Plot TWO BOOTSTRAP resample mean grouped by Body style
plot.diff(two.boot.mean.bsharconv$t,cols = c('Mean Difference--Fuel Type(Hardtop & Convertible)'))
plot.diff(two.boot.mean.bshatconv$t,cols = c('Mean Difference--Body Style(Hatchback & Convertible)'))
plot.diff(two.boot.mean.bssedconv$t,cols = c('Mean Difference--Body Style(Sedan & Convertible)'))
plot.diff(two.boot.mean.bswagconv$t,cols = c('Mean Difference--Body Style(Wagon & Convertible)'))
plot.diff(two.boot.mean.bshathar$t,cols = c('Mean Difference--Body Style(Hatchback & Hardtop)'))
plot.diff(two.boot.mean.bssedhar$t,cols = c('Mean Difference--Body Style(Sedan & Hardtop)'))
plot.diff(two.boot.mean.bswaghar$t,cols = c('Mean Difference--Body Style(Wagon & Hardtop)'))
plot.diff(two.boot.mean.bssedhat$t,cols = c('Mean Difference--Body Style(Sedan & Hatchback)'))
plot.diff(two.boot.mean.bswaghat$t,cols = c('Mean Difference--Body Style(Wagon & Hatchback)'))
plot.diff(two.boot.mean.bswagsed$t,cols = c('Body Style(Wagon & Sedan)'))
