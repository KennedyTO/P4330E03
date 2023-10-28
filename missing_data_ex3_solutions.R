# Exercise 3 - Missing Data

# Predicting depression (bdi) from trait anxiety (traitanx) and
# agreeableness (bfagree)

# Open the dataset 
library(psychTools)
d<-epi.bfi
names(d)
head(d)
nrow(d)

# Run the model using the full dataset
m<-lm(bdi ~ traitanx + bfagree, data = d)
summary(m)
(coeffs<-summary(m)$coefficients[,"Estimate"])
(stderrs<-summary(m)$coefficients[,"Std. Error"])
(pvals<-summary(m)$coefficients[,"Pr(>|t|)"])

# Randomly remove 15% of data points (and store in a new dataset)
library(missMethods)
d2 <- delete_MCAR(ds = d, p = .15)
(colMeans(is.na(d2)))*100


# Can the missing data be classified as MCAR?
# Note: Ho: Missing Data are MCAR
library(naniar)
mcar_test(d2)
# Hopefully the test is NS :)


# Run the model with listwise deletion
m2<-lm(bdi ~ traitanx + bfagree, data = d2)
summary(m2)
(coeffs<-rbind(coeffs,coef(m2)))
(stderrs<-rbind(stderrs,summary(m2)$coefficients[,"Std. Error"]))
(pvals<-rbind(pvals,summary(m2)$coefficients[,"Pr(>|t|)"]))

# Activate the 'mice' Package
library(mice)


# Regression Imputation
# We are going to use all dataset variables for prediction
# of potential imputation values
m_imp1<- mice(d2, method="norm.predict", m = 1)
d3 <- complete(m_imp1)
m3 <- lm(bdi ~ traitanx + bfagree, data = d3)
summary(m3)
(coeffs<-rbind(coeffs,coef(m3)))
(stderrs<-rbind(stderrs,summary(m3)$coefficients[,"Std. Error"]))
(pvals<-rbind(pvals,summary(m3)$coefficients[,"Pr(>|t|)"]))


# Stochastic Regression Imputation
m_imp2<- mice(d2, method="norm.nob", m=1)
d4 <- complete(m_imp2)
m4 <- lm(bdi ~ traitanx + bfagree, data = d4)
summary(m4)
(coeffs<-rbind(coeffs,coef(m4)))
(stderrs<-rbind(stderrs,summary(m4)$coefficients[,"Std. Error"]))
(pvals<-rbind(pvals,summary(m4)$coefficients[,"Pr(>|t|)"]))



# Multiple Imputation (using stochastic regression imputation)
m_imp3 <- mice(d2, m=10, method='norm.nob')
m5 <- with(m_imp3, lm(bdi ~ traitanx + bfagree))
(coeffs<-rbind(coeffs,summary(pool(m5))$estimate))
(stderrs<-rbind(stderrs,summary(pool(m5))$std.error))
(pvals<-rbind(pvals,summary(pool(m5))$p.value))


#Expectation-Maximization Imputation (on the whole dataset)
library(missMethods)
d5 <- impute_EM(d2)
m6 <- lm(bdi ~ traitanx + bfagree, data = d5)
summary(m6)
(coeffs<-rbind(coeffs,coef(m6)))
(stderrs<-rbind(stderrs,summary(m6)$coefficients[,"Std. Error"]))
(pvals<-rbind(pvals,summary(m6)$coefficients[,"Pr(>|t|)"]))



# Summary of Results
rownames(coeffs) <- c("full data","listwise","regression",
                       "stochastic reg","mult imp","exp-max")
rownames(stderrs) <- c("full data","listwise","regression",
                      "stochastic reg","mult imp","exp-max")
rownames(pvals) <- c("full data","listwise","regression",
                     "stochastic reg","mult imp","exp-max")
coeffs
stderrs
pvals
#For fun, re-run everything again to see if the results are 
#noticeably different
