rm(list=ls())
# Clear Console:
cat("\014")
library(dplyr)
library(repr)
source('Loaddata.R')
source('ZScoreNormalise.R')
source('plotsvdreg.R')
# Load and cleanse the csv file.
allages = Loaddata('all-ages.csv')
gradstudents = Loaddata('grad-students.csv')
recentgrads = Loaddata('recent-grads.csv')
#Check out the structure of the cleansed object.
str((recentgrads))

#z-Score Normalization of grad median income data
df_gradstudent_median <- ZScoreNormalise(gradstudents$Grad_median)
#z-Score Normalization of recent grad mediam income data
df_recent_gradstudent_median <- ZScoreNormalise(recentgrads$Median)
gradstudents = mutate(gradstudents,normalized_gradmedian = df_gradstudent_median$normalized_logColumn)
recentgrads = mutate(recentgrads,normalized_gradmedian_round = round(df_recent_gradstudent_median$normalized_logColumn),normalized_gradmedian = df_recent_gradstudent_median$normalized_logColumn)

#z-Score Normalization of grad unemployment rate data
df_gradstudent_unemployment <- ZScoreNormalise(gradstudents$Grad_unemployment_rate)
#z-Score Normalization of recent grad unemployment data
df_recent_gradstudent_unemployment <- ZScoreNormalise(recentgrads$Unemployment_rate)
gradstudents = mutate(gradstudents,normalized_gradunemployment = round(df_gradstudent_unemployment$normalized_logColumn))
recentgrads = mutate(recentgrads,normalized_gradundemploymentrate = round(df_recent_gradstudent_unemployment$normalized_logColumn))

#z-Score Normalization of recent grad employment and unemployment data
df_recent_gradstudent_unemployednum <- ZScoreNormalise(recentgrads$Unemployed)
df_recent_gradstudent_employednum <- ZScoreNormalise(recentgrads$Employed)
#Add normalized unemployment and employment numbers to the recent graduate data frame.
recentgrads = mutate(recentgrads,normalized_gradunemployednum = round(df_recent_gradstudent_unemployednum$normalized_logColumn),normalized_grademployednum = round(df_recent_gradstudent_employednum$normalized_logColumn))

#Group data by median pay by major category
Recent_Graduate_GroupbyMajorCategorybyMedian <-
  recentgrads %>% group_by(Major_category) %>% summarise(normalized_gradmedian_round = mean(normalized_gradmedian_round))

#Group data by employment rate by major category
Recent_Graduate_GroupbyMajor_unemploymentrate <-
  recentgrads %>% group_by(Major_category) %>% summarise(normalized_gradundemploymentrate = mean(normalized_gradundemploymentrate))

#Convert Categorical variables(Major Category) into numeric
recentgrads$Major_category_n <- as.numeric(factor(recentgrads$Major_category , levels=Recent_Graduate_GroupbyMajor_unemploymentrate$Major_category))
str(recentgrads)

#Retain numeric variables in the data frame
numeric_recentgrads1 <- data.frame(recentgrads[sapply(recentgrads,is.numeric)])
numeric_recentgrads <- data.frame(lapply(numeric_recentgrads1, as.numeric))
str(numeric_recentgrads)

#All the columns are num or integer hence we can calculate the summary of the complete dataset
lapply(numeric_recentgrads, summary)
# Frequency Tables.
table(numeric_recentgrads$Major_category_n)
#Calculate covariance.
cov(numeric_recentgrads)
#Calculate the Pearson's correlation of between numeric attributes.
cor(numeric_recentgrads)


#Visualization
require(ggplot2)
# Bar plot
ggplot(recentgrads, aes(x=reorder(normalized_gradmedian_round,normalized_gradmedian_round, function(x) -length(x)))) + ## Function shorts the bars
  geom_bar() + 
  xlab('Median Pay')
theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Bar plot
ggplot(recentgrads, aes(x=reorder(Major_category,Major_category, function(x) -length(x)))) + ## Function shorts the bars
  geom_bar() + 
  xlab('Major Category')
theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Create boxplot to compare two variables.
ggplot(numeric_recentgrads, aes(x = factor(Major_category_n), y = normalized_gradundemploymentrate)) + geom_boxplot() + 
  xlab('Major Category') + ylab('Normalized Unemployment Rate ') + ggtitle('Unemployment rate by Major Category')

#Create boxplot to compare two variables.
#Engineering s the top grosser as a category.
ggplot(recentgrads, aes(x = factor(Major_category), y = normalized_gradmedian_round)) + geom_boxplot() + 
  xlab('Major Category') + ylab('Normalized Median Pay') + ggtitle('Median pay by Major category')

#2D kernel density plot
ggplot(recentgrads, aes(normalized_gradundemploymentrate, normalized_gradmedian_round)) + geom_point() + 
  geom_density2d() +
  xlab('Unemployment') + ylab('Median Pay') +
  ggtitle('Relationship between Unemployment & Median Pay')

# Violin Plot
ggplot(recentgrads, aes(x = factor(Major_category), y = normalized_gradmedian_round)) + 
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab('Major Category')  + ylab('Median Pay') + ggtitle('Median by Major Category')

#Scatter plot matrix
options(repr.plot.width=8, repr.plot.height=8)
require(car)
scatterplotMatrix(~ Major_category_n + normalized_gradmedian_round + normalized_gradundemploymentrate, data = recentgrads)


#Stratifying the data.
#Group By Major Category
groupedbyMajorCategory = recentgrads %>% group_by(Major_category)

#Anova analysis for median pay per major category
aov_majcat_medianpay = aov(groupedbyMajorCategory$normalized_gradmedian_round ~ groupedbyMajorCategory$Major_category, data = groupedbyMajorCategory)
#A high Fvalue and an extreemely low p-value will reject the hypothesis that median salary is same across major category.
summary(aov_majcat_medianpay)

#Anova analysis for unemployment rate per major category
aov_majcat_unemploy = aov(groupedbyMajorCategory$normalized_gradundemploymentrate ~ groupedbyMajorCategory$Major_category, data = groupedbyMajorCategory)
#A high Fvalue and a low p-value will reject the hypothesis that unemployment is same across major category.
# One can also see a an interesting relationship between unemployment and median salary, there is lower unemployment across major categories, however the salary structure is vastly different.
summary(aov_majcat_unemploy)

#Tukey Analysis for median pay per major category
tukey_majcat_medianpay = TukeyHSD(aov_majcat_medianpay)  # Tukey's Range test:
tukey_majcat_medianpay
df_tukey_medianpay <- data.frame(tukey_majcat_medianpay$`groupedbyMajorCategory$Major_category`)
df_tukey_medianpay[order(df_tukey_medianpay$p.adj),]

#98 out of 120 comparison reject the hypothesis
#Tukey Analysis for unemployment per major category.
#You can see few categories wherein null hypothesis can be rejected
tukey_majcat_unemploy = TukeyHSD(aov_majcat_unemploy)  # Tukey's Range test:
tukey_majcat_unemploy
df_tukey_unemploy <- data.frame((tukey_majcat_unemploy$`groupedbyMajorCategory$Major_category`))
#14 out of 120 comparisons reject the hypothesis
# These numbers match overall analysis of ANOVA.


#Create a model on median pay based on linear regression.
lm.medianpay = lm(numeric_recentgrads$normalized_gradmedian_round ~ . - Median - Rank - 1 -normalized_gradmedian - normalized_gradundemploymentrate - normalized_gradunemployednum - normalized_grademployednum, data = numeric_recentgrads)
summary(lm.medianpay)
plot(lm.medianpay)

#Create a model on unemployment based on linear regression.
lm.unemployment = lm(numeric_recentgrads$normalized_gradundemploymentrate ~ . - Unemployment_rate - Rank - 1 -normalized_gradmedian - normalized_gradundemploymentrate - normalized_gradunemployednum - normalized_grademployednum -normalized_gradmedian_round, data = numeric_recentgrads)
summary(lm.unemployment)
plot(lm.unemployment)

#Apply Step Wise Regression on median pay
library(MASS)
lm.step.medianpay = stepAIC(lm.medianpay, direction = 'both')
lm.step.medianpay$anova # ANOVA of the result 
summary(lm.step.medianpay) # Summary of the best model
plot(lm.step.medianpay)

#Apply Step Wise Regression on unemployment rate
library(MASS)
lm.step.unemploymentrate = stepAIC(lm.unemployment, direction = 'both')
lm.step.unemploymentrate$anova # ANOVA of the result 
summary(lm.step.unemploymentrate) # Summary of the best model
plot(lm.step.unemploymentrate)

# Apply Singular Value Decomposition using Pseudo Inverse for median pay
# Remove intercept and redundant columns
# Copy dataframe so I can use the same function for both median pay and unemployment rate
df <- numeric_recentgrads
mod.mat <- model.matrix(df$normalized_gradmedian_round ~ . - Median - Rank - 1 -normalized_gradmedian - normalized_gradundemploymentrate - normalized_gradunemployednum - normalized_grademployednum, data = df)
M = as.matrix(mod.mat)
head(M)
MTM = t(M) %*% M
head(MTM)
dim(MTM)
# Examine the singular values and in the process check for "Rank Deficiency".
mSVD <- svd(MTM)
#Validate whether singular vectors are orthogonal.
uOrth <- t(mSVD$u) %*% mSVD$u
vOrth <- mSVD$v %*% t(mSVD$v)
uOrthSingLeftVector <- mSVD$u
vOrthSingRightVector <- mSVD$v
diagSDV <- mSVD$d
diagSDV
#By looking at singlular values we can now deduce that this matrix is rank deficient with 1 value below zero.
# Let us now compute the pseudo inverse of MTM matrix.
#cat('Compute and print the inverse singular value matrix')
d.trim = rep(0, 18)
d.trim[1:17] =1/ mSVD$d[1:17]
mD = diag(d.trim)
#cat('Compute and print the pseudo inverse')
mInv = mSVD$v %*% mD %*% t(mSVD$u)
#cat('Compute and print the dimensions of the matrix MInvM')
MInvM = mInv %*% t(M)
dim(MInvM)

# Compute the vector of model coefficients by multiplying MInvM with normalized median pay.
b <- MInvM %*% df$normalized_gradmedian_round
coeffcountmedian <- nrow(b)

# Now we can evaluate the model using this vector of model coefficient.
df$score = M %*% b + mean(df$normalized_gradmedian_round)
df$resids = df$score - df$normalized_gradmedian_round
require(repr)
options(repr.pmales.extlot.width=8, repr.plot.height=4)
#Calling function for median
plot.svd.reg(df,coeffcountmedian)

# Apply Singular Value Decomposition using Pseudo Inverse for unemployment
# Remove intercept and redundant columns
# Copy dataframe so I can use the same function for both median pay and unemployment rate
df1 <- numeric_recentgrads
mod.mat <- model.matrix(df1$normalized_gradundemploymentrate ~ . - Unemployment_rate - Rank - 1 -normalized_gradmedian - normalized_gradunemployednum - normalized_grademployednum, data = df1)
M = as.matrix(mod.mat)
head(M)
MTM = t(M) %*% M
head(MTM)
dim(MTM)
# Examine the singular values and in the process check for "Rank Deficiency".
mSVD <- svd(MTM)
#Validate whether singular vectors are orthogonal.
uOrth <- t(mSVD$u) %*% mSVD$u
vOrth <- mSVD$v %*% t(mSVD$v)
uOrthSingLeftVector <- mSVD$u
vOrthSingRightVector <- mSVD$v
diagSDV <- mSVD$d
diagSDV
#By looking at singlular values we can now deduce that this matrix is rank deficient with 1 value below zero.
# Let us now compute the pseudo inverse of MTM matrix.
#cat('Compute and print the inverse singular value matrix')
d.trim = rep(0, 19)
d.trim[1:18] =1/ mSVD$d[1:18]
mD = diag(d.trim)
#cat('Compute and print the pseudo inverse')
mInv = mSVD$v %*% mD %*% t(mSVD$u)
#cat('Compute and print the dimensions of the matrix MInvM')
MInvM = mInv %*% t(M)
dim(MInvM)

# Compute the vector of model coefficients by multiplying MInvM with normalized unemployment rate.
b <- MInvM %*% df1$normalized_gradundemploymentrate
coeffcountunemploy <- nrow(b)

# Now we can evaluate the model using this vector of model coefficient.
df1$score = M %*% b + mean(df1$normalized_gradundemploymentrate)
df1$resids = df$score - df1$normalized_gradundemploymentrate
require(repr)
options(repr.pmales.extlot.width=8, repr.plot.height=4)
#Calling function for employment rate
plot.svd.reg(df1,coeffcountunemploy)

#Elastic Net Regression on median pay
require(glmnet)
b = as.matrix(df$normalized_gradmedian_round)
mod.ridge = glmnet(M, b, family = 'gaussian', nlambda = 100, alpha = .5)
plot(mod.ridge, xvar = 'lambda', label = TRUE)
plot(mod.ridge, xvar = 'dev', label = TRUE)

#Let us now evaluate the model created by elastice net regression by calculating the score using predict function.
df$score = predict(mod.ridge, newx = M)[, 20]
df$resids = df$score - df$normalized_gradmedian_round

plot.svd.reg(df,coeffcountmedian)


#Elastic Net Regression on unemployment
require(glmnet)
b = as.matrix(df1$normalized_gradundemploymentrate)
mod.ridge = glmnet(M, b, family = 'gaussian', nlambda = 100, alpha = .5)
plot(mod.ridge, xvar = 'lambda', label = TRUE)
plot(mod.ridge, xvar = 'dev', label = TRUE)

#Let us now evaluate the model created by elastice net regression by calculating the score using predict function.
df1$score = predict(mod.ridge, newx = M)[, 20]
df1$resids = df1$score - df1$normalized_gradundemploymentrate

plot.svd.reg(df1,coeffcountunemploy)
