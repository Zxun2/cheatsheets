################################################################################
#                                                                              #
#              ST2137 Statistical Computing and Programming                    #
#                              AY22/23 Sem 1                                   #  
#                            By Lee Zong Xun                                   #
#                                                                              #
################################################################################
# Code snippets are organized by topics and usually have a coherent flow to them.
# Topics covered (in R): 
# 1. Numerical and graphical summaries
# 2. Robust estimators
# 3. Categorical Data Analysis
# 4. Hypothesis Testing (One and two samples) 
# 5. Anova
# 6. Regression Analysis
# 7. Simulation
# 8. Resampling


################################################################################
#                          Topic 1 Introduction                                #
################################################################################
rm(list= ls())

## Matrix operations
m = matrix(c(1:4), nrow = 2)
n = matrix(c(5:8), nrow = 2)

# inverse of matrix
solve(m)

# transpose of matrix
t(m)

# matrix multiplication
m%*%n

# create a diagonal matrix, 2 x 2 matrix
diag(2)

################################################################################
#                      Topic 4 Numerical Summaries                             #
################################################################################
rm(list= ls())


data<- read.csv("midterm_marks")
names(data) = c("obs", "mark")
attach(data)

####  NUMERICAL SUMMARIES:
length(mark)
summary(mark)
mean(mark)
median(mark)
quantile(mark)
range(mark)
var(mark)
sd(mark)
IQR(mark)
order(mark, decreasing = FALSE)

## SKEWNESS:
skew <- function(x){
  n <- length(x)
  m3 <- mean((x-mean(x))^3)
  m2 <- mean((x-mean(x))^2)
  sk=m3/m2^(3/2)*sqrt(n*(n-1))/(n-2)
  return(sk)
}
skew(mark)

### KURTOSIS:
kurt <- function(x){
  n <- length(x)
  m4 <- mean((x-mean(x))^4)
  m2 <- mean((x-mean(x))^2)
  kurt = (n-1)/((n-2)*(n-3))*((n+1)*m4/(m2^2)-3*(n-1))
  return(kurt)
}
kurt(mark)

## Nth central moment
nth_central_moment <- function(data, n) {
  return(mean((data - mean(data))**n))
}
nth_central_moment(mark, 2)

### Create hist with normal and density curve overlay
hist(mark, freq=FALSE, main = paste("Histogram of mark"), 
     xlab = "mark", ylab="Probability", axes = TRUE, 
     col = "grey",nclass = 10)
x <- seq(0, 30, length.out=98)
y <-dnorm(x, mean(mark), sd(mark))
lines(x, y, col = "red")
lines(density(mark))


## >1 Historgram on a single plot
par(mfrow=c(1,3))
hist(energy[which(type ==1)], include.lowest = TRUE,freq=FALSE,
     col="grey",xlab = "Type 1",main ="Histograms of Energy by types")

hist(energy[which(type ==2)], include.lowest = TRUE, freq=FALSE,
     col="grey", xlab = "Type 2",main = "")

hist(energy[which(type ==3)], include.lowest = TRUE, freq=FALSE,
     col="grey", xlab = "Type 3",main = "")

#To get back to 1 graph in one page.
par(mfrow=c(1,1))


#################   BOXPLOTS
bats<-final<-read.csv("bats.csv")
bats <-data.frame(bats)
attach(bats)
boxplot(energy ~type)


##############  QQ plots

## datax places the sample quantiles on the x-axis
qqnorm(mark, datax = TRUE, ylab = "Sample Quantiles", xlab = "Theoretical Quantiles", 
       main = "QQ Plot", pch = 20)
qqline(mark,datax = TRUE)


############ Scatter plot (with overlay and legend)
data <- read.csv("testscores.txt", sep=" ")
attach(data)
plot(A[which(gender == "M")], B[which(gender == "M")], pch=20, col="black")
points(A[which(gender == "F")], B[which(gender == "F")], pch=2, col='red')
legend("topright",legend=c("Male", "Female"),col=c("black", "red"), pch=c(20,2), cex=1.2)


################################################################################
#                        Topic 5 Robust Estimators                             #
################################################################################
rm(list= ls())


winsor<-function(x, alpha = 0.2){ 
  n = length(x)
  xq = n * alpha
  x = sort(x)
  m = x[(round(xq)+1)]
  M = x[(n - round(xq))]
  x[which(x<m)] = m
  x[which(x>M)] = M
  return(c(mean(x),var(x))) 
} 

data<-read.csv("C:/Data/heats.csv")
x = data$heat
mean(x, trim = 0.2) # Trimmed mean
winsor(x, alpha = 0.2)  # Winsorized mean

### MAD:

MAD <- function(x) {
 median(abs(x - median(x))) #MAD
}
sigma = mad(x)  # estimate of \sigma, = 1.4826*MAD

### IQR

IQR(x)


################################################################################
#                      Topic 6 Categorical Data Analysis                       #
################################################################################
rm(list= ls())

######### Single categorical Variable
data<-read.csv("C:bats.csv")
count = table(data$type)
barplot(count)

######### Two Categorical Variable

samoa<- matrix(c(22,1179,22,1409), nr=2, byrow=T)
colnames(samoa) <- c("CVD", "No CVD")
rownames(samoa) <- c("Obese", "Non-Obese")
prop.table(samoa, margin=1) # proportion along rows

## Contingency Table
chest.pain<-matrix(c(46, 474, 37, 516), ncol=2, byrow=2) # 2x2 table
dimnames(chest.pain)<-list(Gender=c("Male", "Female"), Chest.Pain=c("Yes","No"))
test <-prop.test(chest.pain, correct=FALSE)
relative_risk <-(test$estimate[1])/(test$estimate[2])
odds <-test$estimate/(1- test$estimate)
odds_ratio <-odds[1]/odds[2]
odds_ratio; relative_risk

### Odds Ratio with CI
odds_ratio_with_CI <-function(x, pad.zeros = FALSE, conf.level=0.95){
  if(pad.zeros){if(any(x==0)) {x<-x+0.5}}
  theta<-x[1,1]*x[2,2]/(x[2,1]*x[1,2])
  ASE<-sqrt(sum(1/x))
  CI<-exp(log(theta) +c(-1,1)*qnorm(0.5*(1+conf.level))*ASE)
  list(estimator=theta, ASE=ASE,conf.interval=CI, 
       conf.level=conf.level) }
odds_ratio_with_CI(chest.pain)

###############     CHI SQUARE TEST:

# Report the test statistic, pval, null distribution, df

chi <- chisq.test(chest.pain)
chi$stdres   # STANDARDIZED RESIDUALS
chi$expected # EXPECTED COUNT PER CELL (check > 5 per cell count)
chi

################    FISHER EXACT TEST (expected count < 5 for > 25% of cells)
claritin<-matrix(c(4,184,2,260), ncol=2, byrow=2)
fisher.test(claritin, alternative = "two.sided")

################ McNemar Test (dependent samples)
x = matrix(c(25,1,17,7), nrow = 2, byrow = TRUE)
mcnemar.test(x, correct = TRUE)

################ Linear by Linear test, chisquared with 1 df

## Method 1
Input =(
  "tab2         Absent    Present
whg                       
None              19         82
Some              27         54
Many               7         30
")


set = as.table(read.ftable(textConnection(Input)))

set

library(coin)
lbl_test(set,scores = list(tab2 = c(0,1), whg = c(1,2,3)))

## Method 2 (assign factors)
whg <- as.factor(ifelse(data$workhour == 0, 'None (0 hrs)',
              ifelse(data$workhour > 1 & data$workhour < 20, 'Some (1 - 19 hrs)', 
                               'Many (20 - 99 hrs)')))
whg <- factor(whg, levels = c('None (0 hrs)', 'Some (1 - 19 hrs)',  'Many (20 - 99 hrs)'))



cont_table2 <- table(whg, travel)

row_scores <- c(1, 2, 3)
col_scores <- c(0, 1)


library(coin)
lbl_test(cont_table2, scores=list("whg" = row_scores, "travel" = col_scores))

### Manual LBL

nc1<-c(17066,14464,788,126,37) # col1
nc2<-c(48,38,5,1,1) # col2

rsum<-nc1+nc2  # row sums
csum<-c(sum(nc1),sum(nc2)) # col sums
n<-sum(csum)  # total cell counts

rowp<-rsum/n  # margin prob for rows
colp<-csum/n  # margin prob for columns

########## Linear-by-Linear Association test:

v<-c(0,1);  # specifying the scores for columns
u<-c(0,.5,1.5,4,7.0);  # specifying the scores for rows

ubar=sum(u*rowp);  # weighted average scores for rows
vbar<-sum(v*colp);  # weighted average scores for columns
CV<-sum(c(sum((u-ubar)*nc1/n),sum((u-ubar)*nc2/n))*(v-vbar))  ## weighted covariance

V1<-sum((u-ubar)^2*rsum/n); # weighted variance for rows' scores
V2<-sum((v-vbar)^2*csum/n);  # weighted variance for columns' scores

r<-CV/sqrt(V1*V2)  # weighted correlation
M<-sqrt(n-1)*r  # normalized test statistic

# one-sided P-value
1-pnorm(abs(M)) # or pnorm(abs(M), lower.tail = FALSE)

# test P-value (2 sided p-value)
1-pchisq(M^2,1) # or pchisq(M^2,1, lower.tail = FALSE)



################################################################################
#                      Topic 7 Hypothesis Testing                              #
################################################################################
rm(list= ls())

data = read.csv("babyweights.csv", header = TRUE)
attach(data)
summary(weight)

############### ONE SAMPLE T TEST 
# t-test (parametric test) for comparing mean with a number:
t.test(weight, mu = 3.3, alternative = "less")

# Wilcoxon Signed Rank Test (also a nonparametric test) 
# for comparing median vs a number:
weight.non.0 = (weight[weight!=3.3]) # REMEMBER TO FILTER OUT ZEROES
wilcox.test(weight, mu=3.3, alternative="less", conf.int = TRUE)

# The code to calculate the test statistic for Wilcoxon Signed Rank test above 
d = abs(weight) # absolute value of the differences
rank(d) # assign the ranks to the values in d

# rank sum of positive ranks and negative ranks. 
# This is different from Rank Sum test which looks at the rank sums of 
# each group.
V.pos = sum(rank(d)[which(weight>0)]) # sum of positive ranks  = test statistic =51
V.neg = sum(rank(d)[which(weight<0)]) # sum of negative ranks = sum(rank(d)) - V.pos

############### TWO-SAMPLE DATA 

data = read.csv("protein_and_weight_gain.csv", header = TRUE)
attach(data)
names(data)
data$level = factor(level)

x = weight_gain[which(level == "high")]
y = weight_gain[which(level == "low")]

########### Check equal variance
shapiro.test(x) # normal
shapiro.test(y) # normal
var.test(x,y)  #use the Bartlett test (when samples assumed normality)

# t-test (parametric test) to compare means:

t.test(x,y, mu = 0, var.equal = TRUE) # if variances are equal
t.test(x,y, mu = 0, var.equal = FALSE) # if variances are NOT equal

# Mann Whitney U test (non-parametric)
wilcox.test(x,y, conf.int = TRUE)


##################   DEPENDENT/PAIRED SAMPLES
before = c(25, 25,27, 44,30,67, 53, 53,52, 60, 28)
after = c(27, 29, 37, 56, 46, 82, 57, 80,61,59,43)
t.test(after, before, mu = 0, paired = TRUE, conf.level = 0.9) # t-test

#### nonparametric test for paired samples:

drugA = c(20, 40, 30, 45, 19, 27, 32, 26)
drugB = c(18, 36, 32, 46, 15, 22, 29, 25)

diff = drugA - drugB

# Wilcoxon Signed Rank Test:
wilcox.test(diff)

################################################################################
#                             Topic 8 ANOVA                                    #
################################################################################
rm(list= ls())

## Preprocessing
data<-read.csv("tablets1.txt", sep = ",", quote = ' ', header = TRUE)
names(data) = c('lab1', 'lab2', 'lab3', 'lab4', 'lab5', 'lab6', 'lab7')
attach(data)

# Reformatting
amount <- c(lab1, lab2, lab3, lab4, lab5, lab6, lab7)
lab<-c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10), rep(6,10),rep(7,10))
newdata<-data.frame(amount = amount,lab = lab)
attach(newdata)

# declare categorical
lab = as.factor(newdata$lab)

############### Check Assumptions
# check normality for equal variance test
val = tapply(amount, lab, shapiro.test)
norm.value = vector()
for (i in val) {
  norm.value = append(norm.value, i$p.value)
}
normal = all(norm.value > 0.05)

# check for equal variance
if (!normal) {
  # just use kruskal wallis
  library(car)
  leveneTest(amount, lab)
} else {
  bartlett.test(amount ~ lab, data = newdata)
}

# check for normal residuals
anova<-aov(amount~lab, data = newdata)
resid = anova$resid
if (!shapiro.test(resid)$p.value > 0.05) {
  cat("residuals not normal")
} else {
  cat("residuals are normal")
}

fit = anova$fitted
plot(anova$fitted, anova$res, xlab="fitted", ylab= "Residuals", main = "")
abline(h=0)

############  QQ plot of Residuals :
qqnorm(anova$res, datax = TRUE, ylab = "Residuals", xlab = "Z scores", 
       main = "QQ plot of Residuals")
qqline(anova$res,datax = TRUE)

############### ANOVA TEST 
# F-test formed by two chi-squared distributed distribution
# IJ - I df and I - 1 df
anova<-aov(amount~lab, data = newdata)
summary(anova)

# apply mean to each group
tapply(amount, lab, mean)

############### KRUSKAL-WALLIS TEST
kruskal.test(amount~lab) 

############### BONFFERONI TEST
# pool.SD = FALSE if variances are not equal
pairwise.t.test(amount, lab, p.adjust.method  = "bonf")

#### for non-parametric case:
wilcox <- pairwise.wilcox.test(amount, lab, p.adjust.method = "bonf")
which(wilcox$p.value < 0.05)
wilcox

############### TUKEY TEST
tukey<-TukeyHSD(anova, conf = 0.95)
# To find which couple are significantly different at a = 0.05:
which(tukey$lab[,4]<0.05)


################################################################################
#                         Topic 9 Regression Analysis                          #
################################################################################
rm(list= ls())


##############  Testing correlation between multiple variables
data = read.table ("ex10_1.txt" ,header=T)
attach(data)

cor(cbind(height, weight, age), method="pearson") # defaults to pearson
cor.test(height, weight)
cor(height, weight)

##############   Constructing a simple linear model with interaction term
model <- lm(weight ~ height + age, data = data) 
summary(model) 
anova(model)

data$gender = as.factor(data$gender)
# Female is the reference group
model3 <- lm(weight ~ height + age + gender, data = data)


# confidence interval for regressors
confint(model, "height", level=0.95)

## Residuals of the model
model$res

## standardized residuals:
rstandard(model)

## QQ plot of SR:
qqnorm(rstandard(model),datax = TRUE, ylab = "Standardized Residuals", 
       xlab = "Z scores", main = "QQ Plot", pch = 20)
qqline(rstandard(model),datax = TRUE, col = "red")


## SR vs fitted:
plot(model$fitted.values, rstandard(model), xlab="Fitted values", 
     ylab= "Standardized Residuals", main = "SR vs Fitted", pch = 20)
abline(h=0, col = "red")

## fitted values and CIs when (height = 65, age = 40) and (height = 63, age = 36)
predict(model, newdata=data.frame(height=c(65,63), 
  age = c(40,36)), interval = "confidence",level = 0.95)

## Check influential points
checkInfluentialPoints <- function(num_regressors, response, ...) {
  p = num_regressors + 1 # k is the number of regressors
  n = length(response) # number of samples
  
  x <-cbind(c(rep(1,n)), ...) # bias column of "1s", and other variables
  hat <- x%*%solve(t(x)%*%x)%*%t(x) # hat matrix
  
  cat("diagonal hat matrix: ", diag(hat)[which(diag(hat)>2*p/n)], "\n")
  
  # Cook's distance
  cooks.distance(model) > 1 # to exclude the rows, form a vector and df[-c(points)]
}
checkInfluentialPoints(2, weight, height, age)

# add higher order terms to model
model5 <- lm(weight ~ height + I(height^2), data = data)


################################################################################
#                            Topic 10 Simulation                               #
################################################################################
rm(list= ls())

##############  Simulating T-DISTRIBUTION  
simulateTDistribution <- function(N, n, mu, sd) {
  # creating a vector of length N
  sample_mean = numeric(N)
  sample_sd = numeric(N)
  t = numeric(N) # T-statistic
  
  mu = mu # 3, or any value.
  sigma = sd # 1 or any value
  
  for (i in 1:N){ 
    x = rnorm(n,mu,sigma) # a sample of normal, size n, mean mu, sd = sigma
    sample_mean[i] = mean(x)
    sample_sd[i] = sd(x)
    t[i] = (sample_mean[i]-mu)/(sample_sd[i]/sqrt(n) )
  }
  hist(t, breaks = 20, prob = TRUE,  col = 5)
  
  # OVERLAYING A T-DENSITY CURVE ON THE HISTOGRAM:
  x <- seq(min(t), max(t), length.out=N)
  y <- dt(x, (n-1) )
  lines(x, y, col = "red",lty = 2)
}
simulateTDistribution(1000, 100, 100, 10)

############# Simulation distribution
x <- runif(ns, 0, d2) # Generate a sample of size ns from uniform(0, d/2)
x <- rbinom(10, 100, 0.3) # rbinom(n, size, prob)
x <- rchisq(10, 3) # rchisq(n, df)
x <- rnorm(10, 100, 10) # rnorm(n, mu, sigma)
x <- rexp(100, 1000) # rexp(n, lambda) -> mu = 1/lambda
x <- rpois(10, 3) # rpois(n, lambda) -> mu = lambda

############# Determine if Winsorized or Trimmed Mean is better for Skewed Data
# UNDERLYING DISTRIBUTION = EXPONENTIAL WITH MEAN = 5000, var = 5000^2
T = 50
lambda = 5000
test = c()
for (j in 1:T) { # Number of sampling distributions
  N = 50 # number of samples
  n = 100 # number of observations per sample
  Mean = rep(0, N)
  Win = rep(0, N)
  
  for (i in 1:N){ # Number of datasets
    
    x = rexp(n, rate = 1/lambda) # mean = lambda and var = lambda^2
    
    Mean[i] = mean(x, trimmed = 0.3) # trimmed mean 30% or more
    Win[i] = winsor(x, alpha = 0.3) # winsorized mean, alpha = 0.2 or can be more     
  }
  
  mse.mean = (mean(Mean) - lambda)^2 + var(Mean)# MSE
  mse.win = (mean(Win) - lambda)^2 + var(Win) # MSE
  # check if MSE of Winsorized mean is smaller than the MSE of trimmed mean
  test = append(test, isTRUE(mse.win < mse.mean)) 
}

# The winsorized mean has LARGER mse than the trimmed mean
# Hence, for skewed data, the trimmed mean MIGHT BE a better estimator of 
# location than the winsorized mean.

####################  SIMULATION FOR COMPARISON OF MEAN ESTIMATORS
# To compare 3 estimators for location, mu, through a simulation study
# The 3 estimators are sample mean, sample median, and 10% trimmed mean.

rm(list= ls())

compareMeanEstimator <- function(simulation_size, sample_size, mu, sd) {
  M     <- simulation_size # simulation size (No. of samples)
  n     <- sample_size # size of each sample
  
  mu    <- mu # Mean of the underlying normal distribution
  sd    <- sd # Standard deviation of the underlying normal distribution
  meanx <- numeric(M) # A vector of all sample means
  medx  <- numeric(M) # A vector of all sample medians
  trmx  <- numeric(M) # A vector of all sample 10% trimmed means
  stdx  <- numeric(M) # A vector of all sample standard deviations
  
  # Using the same seed number gives same result every time
  set.seed(1234)
  
  for (i in 1:M){
    x <- rnorm(n,mu,sd) # Generate a random sample of size n
    
    # compute estimators for each sample
    meanx[i] <- mean(x) 
    medx[i]  <- median(x) 
    trmx[i]  <- mean(x, trim=0.1)
    stdx[i]  <- sd(x) # It is used for computing confidence interval.
  }
  
  simumean <- apply(cbind(meanx,medx,trmx), 2, mean)
  simustd  <- apply(cbind(meanx,medx,trmx), 2, sd)
  simubias <- simumean - rep(mu, 3)
  simumse  <- simubias^2 + simustd^2 
  
  
  estimators <- c("Sample Mean", "Sample Median", "Sample 10% trimmed mean")
  names      <- c("True value", "No. of simu", "MC Mean", "MC Std Deviation","MC Bias","MC MSE")
  sumdat     <- rbind(c(mu,mu,mu), M, simumean, simustd, simubias, simumse)
  
  
  dimnames(sumdat) <- list(names,estimators) # rows, cols
  round(sumdat,4) # pick the one with the lowest MSE
}

################## COVERAGE PROBABILITY OF CONFIDENCE INTERVAL

coverageProbCI <- function(meanx, stdx, sample_size, simulation_size) {
  t05 <- qt(0.975, sample_size-1)
  
  # check if mu in interval
  cover <- (meanx-t05*stdx/sqrt(sample_size) <= mu) & 
    (mu <= meanx+t05*stdx/sqrt(sample_size))
  
  # this value should close to 0.95
  return(sum(cover)/simulation_size) 
}

################## CHECKING THE SIZE OF HYPOTHESIS TESTING
checkHypo <- function(sample_size, simulation_size) {
  t05 <- qt(0.975, sample_size-1)
  ttests <- (meanx-mu)/(stdx/sqrt(sample_size))
  size <- sum(abs(ttests) > t05)/simulation_size # probability of rejecting Ho
  size # 0.0462
}


checkBeta <- function(sample_size, simulation_size, mu) {
  t05 <- qt(0.975, sample_size-1)
  sample_means = numeric(simulation_size)
  sample_deviations = numeric(simulation_size)
  ttests = numeric(simulation_size)
  
  for (i in 1:simulation_size) {
    n <- rnorm(sample_size, mu, 2)
    sample_means[i] = mean(n) 
    sample_deviations[i] = sd(n)
    
    # find ttest for sampling distribution constructed from this sample
    ttests[i] = mean(n)/(sd(n)/sqrt(sample_size))
  }
  
  size <- sum(abs(ttests)<t05) / simulation_size # probability of not rejecting Ho
  return(size)
}


################################################################################
#                            Topic 11 Resampling                               #
################################################################################

x = c(3.39, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00)
sample(x, 7, replace = TRUE)

############ NONPARAMETRIC BOOTSTRAP TO ESTIMATE STANDARD ERROR
law = read.csv("lawschool.csv")
attach(law)
R <- 1000 # number of bootstrap replicates;
n <- length(GPA) # sample size
theta.b <- numeric(R) # storage for boostrap estimates
for (b in 1:R) {
  # for each b, randomly select the indices, sampling with replacement
  i <- sample(1:n, size=n, replace=TRUE)
  LSATb <- LSAT[i] # i is a vector of indices
  GPAb <- GPA[i]
  theta.b[b] <- cor(LSATb, GPAb)
}
sd(theta.b)

################# NONPARAMETRIC BOOTSTRAP TO ESTIMATE BIAS

theta.hat <- cor(LSAT, GPA)
R <- 1000   # can change this value
n <- nrow(law)
theta.b <- numeric(R)# storage for boostrap estimates
for (b in 1:R) {
  i <- sample(1:n, size=n, replace=TRUE)
  LSATb <- LSAT[i]
  GPAb <- GPA[i]
  theta.b[b] <- cor(LSATb, GPAb)
}
bias <- mean(theta.b)- theta.hat


############  BASIC BOOTSTRAP CI
R = 2000 # larger for estimating confidence interval
theta.b = numeric(R)
alpha = 0.05; 
CL = 100*(1-alpha)
for (b in 1:R) {
  i <- sample(1:n, size=n, replace=TRUE)
  LSATb <- LSAT[i]
  GPAb <- GPA[i]
  theta.b[b] <- cor(LSATb, GPAb)
}

low = quantile(theta.b, alpha/2)
high = quantile(theta.b, 1 - alpha/2)

# take note of the switch
cat("A",CL,"% confidence interval is ", 2*theta.hat - high, 2*theta.hat - low,"\n")


############  PERCENTILE BOOTSTRAP CI
low <- quantile(theta.b, alpha/2)
high <- quantile(theta.b,1-alpha/2)

CL <- 100*(1-alpha)

cat("A",CL,"% bootstrap CI is", low, high,"\n")


############  NORMAL BOOTSTRAP CI
bias = mean(theta.b) - theta.hat
se = sd(theta.b)

low <- theta.hat - bias - 1.96*se  # 1.96 is for 95% CI

high <- theta.hat - bias + 1.96*se

cat("A",CL,"percent bootstrap CI is", low, high,"\n")


########### Using built-in function in R:
library(boot)
bcor <- function(data, bindex){
  return(cor(data[bindex,1], data[bindex,2]))
}
boot.cor <- boot(law, statistic=bcor, R=2000)
boot.cor
boot.ci(boot.cor,type=c("basic","perc","norm"))







