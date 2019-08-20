---
title: "Regression Analysis on US Treasuary Yield to Maturity Dataset"
author: "Anupriya Thirumurthy"
output:
  pdf_document: default
  html_document:
    df_print: paged
  word_document: default
---

# The name of the data file for this project is RegressionAssignmentData2014.csv.

```{r clean}
# Clean the environment
rm(list=ls())
```

## Step 1

```{r step1.1}
# Read the data
# Visualize and get familiar with variables.
datapath<-'/Users/anupriyathirumurthy/Documents/AnuBackUp/University/MScA_UoC/Courses/StatisticalAnalysis/Project'
AssignmentData<- read.csv(file=paste(datapath,"regressionassignmentdata2014.csv",sep="/"),row.names=1,header=TRUE,sep=",")
head(AssignmentData)
AssignmentData1 <- AssignmentData
```

# Plotting input variables

All the inputs seemed to look correlated with each other

```{r step1.2}
matplot(AssignmentData[,-c(8,9,10)],type='l')
```


# Plotting input variables together with the output variable.

All the inputs have an effect on the output variable. For every change in the input variable, there is subsequent change in the output. When the inputs rise, the outputs also rise and when they fall, there is a similar fall in the output.  

```{r step1.3}
matplot(AssignmentData[,-c(9,10)],type='l')
```


## Step 2

#Estimating a simple regression model with each of the input and the output variable.

We could infer that all the predictors are significant in their respective individual models with one predictor and the intercept.

```{r step2.1}
# Analyze the summary.
Input <- AssignmentData[,c(1:7)]
lm <- apply(Input,2, function(linearmodel) lm(AssignmentData$Output1~linearmodel))
cat("\nSimple Linear Model for input: USGG3M")
summary(lm$USGG3M)
cat("\n\nSimple Linear Model for input: USGG6M")
summary(lm$USGG6M)
cat("\n\nSimple Linear Model for input: USGG2YR")
summary(lm$USGG2YR)
cat("\n\nSimple Linear Model for input: USGG3YR")
summary(lm$USGG3YR)
cat("\n\nSimple Linear Model for input: USGG5YR")
summary(lm$USGG5YR)
cat("\n\nSimple Linear Model for input: USGG10YR")
summary(lm$USGG10YR)
cat("\n\nSimple Linear Model for input: USGG30YR")
summary(lm$USGG30YR)
```

# Analyze the summary.

1. Residuals : We can notice that the median is almost close to zero in all these individual models and also the distribution of the residuals appears to be nearly strongly symmetrical. That means that the model predicts almost all the points close to the actual observed points.

2. Intercepts are low, indicating the predictors have significance

3. We can observe a highly significant p-values in all the models indicating that all the predictors are being significant in the respective individual models.

4. R squared and Adj R squared : In multiple regression settings, the R2 will always increase as more variables are included in the model. That's why the adjusted R2 is the preferred measure as it adjusts for the number of variables considered. However as this is a simple intercept and one predictor model, we can see R squared and say that  roughly 96% of the variance found in the response variable (output1) can be explained by the predictor variable (USGG3M) considering the summary of USGG3M model. Similarly we can infer for other models. The predictor model which explains the more variance of the output is USGG3YR and is 99.8% of the variance found in the response variable (output1). 1-R^2 would give us the variance unexplained. 

5. Correlation explained: All the predictors and the outputs are perfectly correlated as they have a strong association between them.


```{r step2.2}
#Checking relevance of the estimated parameters and the model as a whole, amount of correlation explained.

#The following code gives the analysis for the input variables and plots the output variable together with the fitted values.

cat("\nVariance and Coefficients for input: USGG3M\n")
c(Total.Variance=var(AssignmentData[,8]),Unexplained.Variance=summary(lm$USGG3M)$sigma^2)
lm$USGG3M$coefficients
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(lm$USGG3M$fitted.values,col="red")
cat("\n\nVariance and Coefficients for input: USGG6M\n")
c(Total.Variance=var(AssignmentData[,8]),Unexplained.Variance=summary(lm$USGG6M)$sigma^2)
lm$USGG6M$coefficients
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(lm$USGG6M$fitted.values,col="red")
cat("\n\nVariance and Coefficients for input: USGG2YR\n")
c(Total.Variance=var(AssignmentData[,8]),Unexplained.Variance=summary(lm$USGG2YR)$sigma^2)
lm$USGG2YR$coefficients
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(lm$USGG2YR$fitted.values,col="red")
cat("\n\nVariance and Coefficients for input: USGG3YR\n")
c(Total.Variance=var(AssignmentData[,8]),Unexplained.Variance=summary(lm$USGG3YR)$sigma^2)
lm$USGG3YR$coefficients
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(lm$USGG3YR$fitted.values,col="red")
cat("\n\nVariance and Coefficients for input: USGG5YR\n")
c(Total.Variance=var(AssignmentData[,8]),Unexplained.Variance=summary(lm$USGG5YR)$sigma^2)
lm$USGG5YR$coefficients
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(lm$USGG5YR$fitted.values,col="red")
cat("\n\nVariance and Coefficients for input: USGG10YR\n")
c(Total.Variance=var(AssignmentData[,8]),Unexplained.Variance=summary(lm$USGG10YR)$sigma^2)
lm$USGG10YR$coefficients
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(lm$USGG10YR$fitted.values,col="red")
cat("\n\nVariance and Coefficients for input: USGG30YR\n")
c(Total.Variance=var(AssignmentData[,8]),Unexplained.Variance=summary(lm$USGG30YR)$sigma^2)
lm$USGG30YR$coefficients
matplot(AssignmentData[,8],type="l",xaxt="n")
lines(lm$USGG30YR$fitted.values,col="red")
```

# Plotting the output variable together with the fitted values. Comparisons and contrasts when we situate each model relative to the expectations or to the other models.

Inference when we fit the models and plot them are: 

1. Among all the single predictor and intercept model, The predictor model which explains the more variance of the output is USGG3YR and is 99.8% of the variance found in the response variable (output1). Hence in the plot the red line and the black lines are so close to each other or they collide. 

2. Among all the single predictor and intercept model, The predictor model which explains the variance of the output relatively less is USGG30YR and is 93.5% of the variance found in the response variable (output1). Hence in the plot the red line and the black lines are a bit varied when compared with the other models. 

# Collectting all slopes and intercepts in one table

```{r step2.3}
lm_table <- apply(Input,2, function(coeff) (lm(AssignmentData$Output1~coeff))$coefficients)
lm_table
```


## Step 3.


```{r step3.1}
# Fitting linear regression models using single output (column 8 Output1) as input and each of the original inputs as outputs.
lm1 <- apply(Input,2, function(coeff) (lm(coeff~AssignmentData$Output1)))
summary(lm1$USGG3M)
summary(lm1$USGG6M)
summary(lm1$USGG2YR)
summary(lm1$USGG3YR)
```

# Collectting all slopes and intercepts in one table and print this table.

```{r step3.2}
lm1_table <- apply(Input,2, function(coeff) (lm(coeff~AssignmentData$Output1))$coefficients)
lm1_table
```

# Summary of this table  and interesting fact when compared with step 2

The intercept is the expected mean value of Y when all X=0. In a regression equation with one predictor, X. If X sometimes = 0, the intercept is simply the expected mean value of Y at that value.

Notice that the intercepts of the models in step 2 are all negative and it doesn't really makes any sense. However, the intercepts in step 3 are positive indicating that the  when the output is zero, the input increases by the intercept value. But actually i feel, the regression constant is generally not worth interpreting. Despite this, it is almost always a good idea to include the constant in your regression analysis. In the end, the real value of a regression model is the ability to understand how the response variable changes when you change the values of the predictor variables.

## Step 4.

```{r step4.1}
# Estimating logistic regression using all inputs and the data on FED tightening and easing cycles.
AssignmentDataLogistic<-data.matrix(AssignmentData,rownames.force="automatic")
# Preparing the easing-tightening data.
# Making the easing column equal to 0 during the easing periods and NA otherwise.
# Making the tightening column equal to 1 during the tightening periods and NA otherwise.
# Creating columns of easing periods (as 0s) and tightening periods (as 1s)
EasingPeriods<-AssignmentDataLogistic[,9]
EasingPeriods[AssignmentDataLogistic[,9]==1]<-0
TighteningPeriods<-AssignmentDataLogistic[,10]
# Checking easing and tightening periods
cbind(EasingPeriods,TighteningPeriods)[c(550:560,900:910,970:980),]
# Removing the periods of neither easing nor tightening.
All.NAs<-is.na(EasingPeriods)&is.na(TighteningPeriods)
AssignmentDataLogistic.EasingTighteningOnly<-AssignmentDataLogistic
AssignmentDataLogistic.EasingTighteningOnly[,9]<-EasingPeriods
AssignmentDataLogistic.EasingTighteningOnly<-AssignmentDataLogistic.EasingTighteningOnly[!All.NAs,]
AssignmentDataLogistic.EasingTighteningOnly[is.na(AssignmentDataLogistic.EasingTighteningOnly[,10]),10]<-0
```

# Plotting the data and the binary output variable representing easing (0) and tightening (1) periods.

Periods of Easing and low interest rates appear to occur in similar timeframes and seemed to be tightly coupled. Tightening periods seem to match the periods of rise in all interest rates. 

```{r step4.1.1}
# Binary output for logistic regression is now in column 10
# Plotting the data and the binary output variable representing easing (0) and tightening (1) periods.
matplot(AssignmentDataLogistic.EasingTighteningOnly[,-c(9,10)],type="l",ylab="Data and Binary Fed Mode")
lines(AssignmentDataLogistic.EasingTighteningOnly[,10]*20,col="red")
```

```{r step4.2}
# Estimate logistic regression with 3M yields as predictors for easing/tightening output.
LogisticModel.TighteningEasing_3M<-glm(AssignmentDataLogistic.EasingTighteningOnly[,10]~
                                      AssignmentDataLogistic.EasingTighteningOnly[,1],family=binomial(link=logit))
summary(LogisticModel.TighteningEasing_3M)
matplot(AssignmentDataLogistic.EasingTighteningOnly[,-c(9,10)],type="l",ylab="Data and Fitted Values")
lines(AssignmentDataLogistic.EasingTighteningOnly[,10]*20,col="red")
lines(LogisticModel.TighteningEasing_3M$fitted.values*20,col="green")
```



```{r step4.3}
#Using all inputs as predictors for logistic regression.
LogisticModel.TighteningEasing_All<-glm(AssignmentDataLogistic.EasingTighteningOnly[,10]~
                                      AssignmentDataLogistic.EasingTighteningOnly[,c(1:7)],family=binomial(link=logit))
summary(LogisticModel.TighteningEasing_All)$aic
summary(LogisticModel.TighteningEasing_All)$coefficients[,c(1,4)]

matplot(AssignmentDataLogistic.EasingTighteningOnly[,-c(9,10)],type="l",ylab="Results of Logistic Regression")
lines(AssignmentDataLogistic.EasingTighteningOnly[,10]*20,col="red")
lines(LogisticModel.TighteningEasing_All$fitted.values*20,col="green")
```

# Interpreting the coefficients of the model and the fitted values.

Logistic regression coefficients are reported as log odds.  Log odds could be converted to normal odds using the exponential function.

For instance considering the logistic coefficient of USGG6M = 4.156, we could infer that there is a multiplicative effect of logodds equals to e^4 = 55 meaning, we are multiplying the odds of tightening being 1 by 55 times with a one unit shift in one year rate, which seems to be unbelievable.  A one unit increase in the 6M rates meaning that the odds go down 55 times, one year increase in the 1 year rate is that the odds go up 55 times. 

Another example, a logistic regression coefficient of -0.97 of the USGG10YR predictor corresponds to odds of e--0.97=2.63794445935, meaning that the easing was about 3  times more likely ( about 75/25) than tightening response, for each unit of change in USGG10YR. Predictor effect on output tightening is possitive.

2. The Sign of coefficients would represents a positive or negative influence on dependent variable.

Fitted Values:

In Logistic regression, Probability of the event (Easing) occuring p  is the actual value we are interested in. Fitted values are log (p/p-1). So, in order to figure out the likelihood of p, we calculate it by using e^y/(e^y + 1)

We could infer in our case the fitted probabilities never change from a factor of 2 to 3. Therefore, the fact that the odds are going up by huge amounts does not make any sense.

# Plots

Tightening periods and easing periods seem to match the period of rise and fall of interest rates correspondingly. 

# Fitting logistic model with all predictors. 

# Interpretting the summary/coefficients, contrast to previous model, and displaying the plot which overlays the predicted values. 

Some of coefficients have positive effect while others have negative effect on tightening, But overall the model is a better fit than the previous as we compare the AIC. Plot also suggests that this model better explains the binary output than the previous model.

```{r step4.4}
# Calculating and plot log-odds and probabilities. Comparing probabilities with fitted values.
Log.Odds<-predict(LogisticModel.TighteningEasing_All)
plot(Log.Odds,type="l")
Probabilities<-1/(exp(-Log.Odds)+1)
plot(LogisticModel.TighteningEasing_All$fitted.values,type="l",ylab="Fitted Values & Log-Odds")
lines(Probabilities,col="red")
```

# Step 5.

```{r step5.1}
# Comparing linear regression models with different combinations of predictors.
# Selecting the best combination.
AssignmentDataRegressionComparison<-data.matrix(AssignmentData[,-c(9,10)],rownames.force="automatic")
AssignmentDataRegressionComparison<-AssignmentData[,-c(9,10)]
# Estimating the full model by using all 7 predictors.
AssignmentDataRegressionComparison.fullModel<-lm(AssignmentData$Output1~.,data=AssignmentData[,1:7])

AssignmentDataRegressionComparison.fullModel$coefficients
summary(AssignmentDataRegressionComparison.fullModel)$coefficients[,c(1,4)]

summary(AssignmentDataRegressionComparison.fullModel)$coefficients
c(summary(AssignmentDataRegressionComparison.fullModel)$r.squared,summary(AssignmentDataRegressionComparison.fullModel)$adj.r.squared)
summary(AssignmentDataRegressionComparison.fullModel)$df
```

# Looking at coefficients, R2, adjusted R2, degrees of freedom.

The coefficients are significant. Both R squared and adj R squared equals 1, and they say that  100% of the variance found in the response variable (output1) can be explained by the predictor variables used in the model. Thus it's a perfect fit. 

# Intepreting the fitted model.

We can infer that the p values are less than the level of significance, indicating that all predictors appear significant.

R-Squared and adjusted R squared suggests that the full model shows a perfect fit.

```{r step5.2}
# Estimating the Null model by including only intercept.
AssignmentDataRegressionComparison.nullModel<-lm(AssignmentData$Output1~1,data=AssignmentData[,1:7])
summary(AssignmentDataRegressionComparison.nullModel)$coefficients
c(summary(AssignmentDataRegressionComparison.nullModel)$r.squared,summary(AssignmentDataRegressionComparison.nullModel)$adj.r.squared)
summary(AssignmentDataRegressionComparison.nullModel)$df
```

#  Exploring the same parameters of the fitted null model as for the full model.

The predictor effects of null model are 0 and hence no R squared to explain the variance. We can also note that the p value is 1, which means insignificant, indicating that the only intercept model doesn't explain the output. 

# Why summary(RegressionModelComparison.Null) does not show R2 ?

R2 is nothing but the fraction of the total variance of Y that is "explained" by the variation in X. 

The predictor effects of null model are 0. Hence none of the variance is explained by any of the predictors (or there are NO predictors at all to explain this). Hence the summary of null model does not show R2.


```{r step5.3}
# Compare models pairwise using anova()
anova(AssignmentDataRegressionComparison.fullModel,AssignmentDataRegressionComparison.nullModel)
```

# Interpreting the results of anova().

The hypotheses for the F-test of the overall significance in the ANOVA table are as follows:

Null hypothesis: The fit of the intercept-only model(null model) and full model are equal. 
Alternative hypothesis: The fit of the intercept-only model is significantly reduced compared to the full model. 

As the P value for the F-test of overall significance test is less than the significance level, we can reject the null-hypothesis and conclude that the full model provides a better fit than the intercept-only model.

# Repeating the analysis for different combinations of input variables and selecting the best one.

```{r step5.4}
# Using drop to remove the predictors with low AIC
fwd.model <- step(AssignmentDataRegressionComparison.nullModel, direction='forward', scope=AssignmentDataRegressionComparison.fullModel)  

AssignmentData1 <- AssignmentData[,1:7]

drop1(AssignmentDataRegressionComparison.fullModel)
AssignmentDataRegressionComparison.noUSGG3YR <- lm(AssignmentData$Output1~.,data=AssignmentData1[,-4])
summary(AssignmentDataRegressionComparison.noUSGG3YR)
anova(AssignmentDataRegressionComparison.fullModel,AssignmentDataRegressionComparison.noUSGG3YR)

# We should stop with the model with noUSGG3YR, as it has the least residual error while the full model is a perfect fir. And hence I might go with the model: AssignmentDataRegressionComparison.noUSGG3YR

# Further trying to reduce, But i don't think its a best model if we reduce it any further.
# The below steps are only for analysis to drop and check

drop1(AssignmentDataRegressionComparison.noUSGG3YR)
AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR <- lm(AssignmentData$Output1~.,data=AssignmentData1[,c(-4,-6)])
summary(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR)
anova(AssignmentDataRegressionComparison.noUSGG3YR,AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR)

drop1(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR)
AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M <- lm(AssignmentData$Output1~.,data=AssignmentData1[,c(-2,-4,-6)])
summary(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M)
anova(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR,AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M)

drop1(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M)
AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR  <- lm(AssignmentData$Output1~.,data=AssignmentData1[,c(-2,-4,-5,-6)])
summary(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR)
anova(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M,AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR)

drop1(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR)
AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR.noUSGG3M  <- lm(AssignmentData$Output1~.,data=AssignmentData1[,c(-1,-2,-4,-5,-6)])
summary(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR.noUSGG3M)
anova(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR,AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR.noUSGG3M)

drop1(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR.noUSGG3M)
# Model with only USGG2YR
AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR.noUSGG3M.noUSGG30YR  <- lm(AssignmentData$Output1~USGG2YR,data=AssignmentData1)
summary(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR.noUSGG3M.noUSGG30YR)
anova(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR.noUSGG3M,AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR.noUSGG3M.noUSGG30YR)

drop1(AssignmentDataRegressionComparison.noUSGG3YR.noUSGG10YR.noUSGG6M.noUSGG5YR.noUSGG3M.noUSGG30YR)
```

Alternatively, trying for the best fit,

```{r step5.4.1}
#Since full model is a perfect fit, perform pairs to check collinearity
data.ModelSelection <-  AssignmentData[1:8]
pairs(data.ModelSelection)
```

Pairs plot tells us that Each pair of the predictors and the output are positively and linearly correlated.

Using regsubsets: Best Subset Selection - To perform best subset selection, we fit a separate least squares regression for each possible combination of the p predictors.
We can perform a best subset search using regsubsets (part of the leaps library), which identifies the best model for a given number of k predictors, where best is quantified using RSS. Here we fit up to a 7-variable model.

```{r step5.4.2}
install.packages("leaps", repos = "http://cran.us.r-project.org")
library(leaps)
best_subset <- regsubsets(Output1 ~ ., data.ModelSelection, nvmax = 7)
summary(best_subset)

dat <- AssignmentData[1:8]
bestLm <- lm(dat$Output1 ~ USGG3M + USGG6M + USGG2YR + USGG5YR + USGG10YR  + USGG30YR ,data=dat)
summary(bestLm)

```

We're attempting to fit a model against data that provide a perfect fit. So i feel there is nothing to do and further, as it by itself best describes the data. Regardless of this, we are able to get a model that fits perfectly and have only a few key predictive variables.

# Explain your selection.

Based on the fact that drop1 analysis shows all models are equally good fit and based on Best Subset results, I chose the linear model with 6 Predictors USGG3M + USGG6M + USGG2YR + USGG5YR + USGG10YR  + USGG30YR, Since A model "as simple as possible, but no simpler" is a good motto for interpretable models and also a model with lesser residual errors is the best suitable one. I went with the 6 predictor model. I did not choose the full model(7 predictor model) as the best model because its a perfect fit and in real world, no model could be a perfect.

## Step 6.

```{r step6.1}

# Performing rolling window analysis of the yields data.

Window.width<-20; Window.shift<-5
library(zoo)

# Calculating rolling mean values for each variable.
# Means
all.means<-rollapply(AssignmentDataRegressionComparison,width=Window.width,by=Window.shift,by.column=TRUE, mean)
head(all.means,10)
# Creating points at which rolling means are calculated
Count<-1:length(AssignmentDataRegressionComparison[,1])
Rolling.window.matrix<-rollapply(Count,width=Window.width,by=Window.shift,by.column=FALSE,
          FUN=function(z) z)
Rolling.window.matrix[1:10,]
# Taking middle of each window
Points.of.calculation<-Rolling.window.matrix[,10]
Points.of.calculation[1:10]
length(Points.of.calculation)
# Incerting means into the total length vector to plot the rolling mean with the original data
Means.forPlot<-rep(NA,length(AssignmentDataRegressionComparison[,1]))
Means.forPlot[Points.of.calculation]<-all.means[,1]
Means.forPlot[1:50]
# Assembling the matrix to plot the rolling means
cbind(AssignmentDataRegressionComparison[,1],Means.forPlot)[1:50,]
```


```{r step6.2}
plot(Means.forPlot,col="red")
lines(AssignmentDataRegressionComparison[,1])
```

```{r step6.3}
# Running rolling daily difference standard deviation of each variable
# Standard deviation
rolling.diff <- rollapply(AssignmentDataRegressionComparison, width = 2, by = 1, by.column = TRUE, diff)
rownames(rolling.diff) <- rownames(AssignmentDataRegressionComparison[2:8300,])
rolling.sd<-rollapply(rolling.diff,width=Window.width,by=Window.shift,by.column=TRUE, sd)
head(rolling.sd)
```

```{r step6.4}
rolling.dates<-rollapply(AssignmentDataRegressionComparison[-1,],width=Window.width,by=Window.shift,
                         by.column=FALSE,FUN=function(z) rownames(z))
head(rolling.dates)
rownames(rolling.sd)<-rolling.dates[,10]
head(rolling.sd)
```

```{r step6.5}
matplot(rolling.sd[,c(1,5,7,8)],xaxt="n",type="l",col=c("black","red","blue","green"))
axis(side=1,at=1:1656,rownames(rolling.sd))
```

# Showing periods of high volatility. 

The plot says that Interest rates of short term bonds are more volatile than the long term bonds. 3M has highhest volatality and the volatality decreases with the 30YR being the least volatile.


```{r step6.6}
# Showing periods of high volatility
high.volatility.periods<-rownames(rolling.sd)[rolling.sd[,8]>.5]
high.volatility.periods

# Fiting linear model to rolling window data using 3 months, 5 years and 30 years variables as predictors.

# Rolling lm coefficients
Coefficients<-rollapply(AssignmentDataRegressionComparison,width=Window.width,by=Window.shift,by.column=FALSE,
         FUN=function(z) coef(lm(Output1~USGG3M+USGG5YR+USGG30YR,data=as.data.frame(z))))
rolling.dates<-rollapply(AssignmentDataRegressionComparison[,1:8],width=Window.width,by=Window.shift,by.column=FALSE,
                         FUN=function(z) rownames(z))

rownames(Coefficients)<-rolling.dates[,10]
Coefficients[1:10,]
```

```{r step 6.7}
# Pairs plot of Coefficients
pairs(Coefficients)
```

# Interpreting the pairs plot.

Each of these pairs of Coefficients are negatively correlated except 3M & 30YR yields. When 5Yr yield effect decreases, the combined effect of 3M & 30YR yields increases.


```{r step6.8}
# Plot of coefficients
matplot(Coefficients[,-1],xaxt="n",type="l",col=c("black","red","green"))
axis(side=1,at=1:1657,rownames(Coefficients))
```

```{r step6.9}
high.slopespread.periods<-rownames(Coefficients)[Coefficients[,3]-Coefficients[,4]>3]
jump.slopes<-rownames(Coefficients)[Coefficients[,3]>3]
high.slopespread.periods
jump.slopes
```

# Picture of coefficients Vs picture of pairs? 

Yes, the pictures of coefficients shows the 3M & 30YR rise and fall together.

This is what we interpreted in the pairwise plot aswell. And our assumptions looks to match.

```{r step6.10}
# R-squared
r.squared<-rollapply(AssignmentDataRegressionComparison,width=Window.width,by=Window.shift,by.column=FALSE,
         FUN=function(z) summary(lm(Output1~USGG3M+USGG5YR+USGG30YR,data=as.data.frame(z)))$r.squared)
r.squared<-cbind(rolling.dates[,10],r.squared)
r.squared[1:10,]
```

```{r step6.11}
plot(r.squared[,2],xaxt="n",ylim=c(0,1))
axis(side=1,at=1:1657,rownames(Coefficients))
```

```{r step6.12}
(low.r.squared.periods<-r.squared[r.squared[,2]<.9,1])
```

# What could cause decrease of R^2 ?

I could see that in the above plot,  In every 20 day period we create the relationships for 3 of the 7 rates in output1, and usually it turns out to be very close to each other. But in some 20 day period, it turns out that the remaining 4 rates move in ways that the 3 rates(3YR, 2YR, 6M) we have don't support. That is may be indicated by the spike, when index was 2/5/1986, the r squared is around 0.8, indicating that something happened at this date, so the rates were not a good predictor of output1. This could be a cause of decrease of R squared.
In other terms I think, If output 1 is even weights of all the 7 rates, then there must be some rates where the 6M rates and the 3YR rates stay levelled, but the 2YR rates is spiked. This would have created disturbance in the output, which would have shown up in the plot(point referring to is when index was 2/5/1986, the r squared is around 0.8).


```{r step6.13}
# P-values
Pvalues<-rollapply(AssignmentDataRegressionComparison,width=Window.width,by=Window.shift,by.column=FALSE,
                        FUN=function(z) summary(lm(Output1~USGG3M+USGG5YR+USGG30YR,data=as.data.frame(z)))$coefficients[,4])
rownames(Pvalues)<-rolling.dates[,10]
Pvalues[1:10,]
```


```{r step6.14}
matplot(Pvalues,xaxt="n",col=c("black","blue","red","green"),type="o")
axis(side=1,at=1:1657,rownames(Coefficients))
```

```{r step6.15}
rownames(Pvalues)[Pvalues[,2]>.5]
rownames(Pvalues)[Pvalues[,3]>.5]
rownames(Pvalues)[Pvalues[,4]>.5]
```

# Interpreting the plot.

As we can see, p values for these predictors are mostly very small, meaning that we can reject the null hypothesis. However, we also observe, the blue values for USGG3M predictor became more frequent as time progresses, meaning during that time the predictor was not explaining the output as the p levels were higher. Interestingly, USGG30YR predictor seems to be insignificant throughout most of the time, meaning that even if R squared is high for that predictor, its p-values ovwr time shows that its actually insignificant at explaining the output whereas the USGG10YR predictor is mostly significant all the time.

## Step 7.

```{r step7.1}
# Performing PCA with the inputs (columns 1-7).
AssignmentData.Output<-AssignmentData$Output1
AssignmentData<-data.matrix(AssignmentData[,1:7],rownames.force="automatic")
dim(AssignmentData)
head(AssignmentData)
```

```{r step7.2}
# Selecting 3 variables. Exploring dimensionality and correlation 
# Explore the dimensionality of the set of 3M, 2Y and 5Y yields.
AssignmentData.3M_2Y_5Y<-AssignmentData[,c(1,3,5)]
pairs(AssignmentData.3M_2Y_5Y)
```

```{r step7.3}
#system("defaults write org.R-project.R force.LANG en_US.UTF-8")
#install.packages("rgl", dependencies = TRUE)
#install.packages("rgl",repos = "http://cran.us.r-project.org")
#install.packages("rgl")
library("rgl")
rgl.points(AssignmentData.3M_2Y_5Y)
```

```{r step7.4}
# Analyzing the covariance matrix of the data. Comparing results of manual calculation and cov().
k <- ncol(AssignmentData) #number of variables
n <- nrow(AssignmentData) #number of subjects
 
#Creating means for each column
M_mean <- matrix(data=1, nrow=n) %*% cbind(mean(AssignmentData[,1]),mean(AssignmentData[,2]),mean(AssignmentData[,3]),mean(AssignmentData[,4]),
                                           mean(AssignmentData[,5]),mean(AssignmentData[,6]),mean(AssignmentData[,7])) 
 
#Creating a difference matrix
D <- AssignmentData - M_mean
 
#Creating the covariance matrix
(Manual.Covariance.Matrix <- (C <- (n-1)^-1 * t(D) %*% D))
(Covariance.Matrix <- cov(AssignmentData))
```


```{r step7.5}
# Ploting the covariance matrix.
Maturities<-c(.25,.5,2,3,5,10,30)
contour(Maturities,Maturities,Covariance.Matrix)
```


```{r step7.6}
# Performing the PCA by manually.
rownames(AssignmentData) <- c()
Y <- AssignmentData
YMeans <- mean(Y)
# Creating Centered Matrix
Y0 <- Y - YMeans
V <-  cov(Y0)
# Eigenvalue decomposition of V
Eigen.Decomposition <- eigen(V)
# Eigenvectors
L <- Eigen.Decomposition$vectors
# Loadings
Loadings <- L[,1:3]
# Factor scores
F <- Y0 %*% L
Factors <-  F[,1:3]
Loadings[,1:3]
# Define the model
(YMeans + F %*%t(L))[1:5,]
Y[1:5,]
```

# Performing the PCA by manually calculating factors, loadings and analyzing the importance of factors.

```{r step7.7}
head(F)
L
```

# Find eigenvalues and eigenvectors. Calculate vector of means (zero loading), first 3 loadings and 3 factors.

```{r step7.8}
Eigen.Decomposition$values
Eigen.Decomposition$vectors
head(Y0)
Loadings <- L[,1:3]
Factors <-  F[,1:3]
```


```{r step7.9}
# Importance of factors.
barplot(Eigen.Decomposition$values/sum(Eigen.Decomposition$values),width=2,col = "black",
        names.arg=c("F1","F2","F3","F4","F5","F6","F7"))
```

```{r step7.10}
matplot(Maturities,Loadings,type="l",lty=1,col=c("black","red","green"),lwd=3)
```

# Interpreting the factors by looking at the shapes of the loadings.

1. All of the weights of first factor are below zero, and it almost seems to be equally weighted around -0.4 and -0.3

2. The second factor puts negative weightage on the nearest rate yields like 3M,6M and zero for say 2YR and positive for all other long term rates.

3. The third factor takes positive rates for the nearest term maturities like 3M and negative rates for the next few maturities and positive rates again. This looks like a butterfly curvature of the yield curve.

4. When there is movements, all the rates rise or fall together.

5. New term rates fall whereas long term rates are high or vice versa.

6. Looks like a butterfly where the mid term(middle yields) dips relative to the either hand(short term and long term maturities).

```{r step7.11}
# Calculating and ploting 3 selected factors
matplot(Factors,type="l",col=c("black","red","green"),lty=1,lwd=3)
```

```{r step7.12}
# Changing the signs of the first factor and the corresponding factor loading.
Loadings[,1]<--Loadings[,1]
Factors[,1]<--Factors[,1]
matplot(Factors,type="l",col=c("black","red","green"),lty=1,lwd=3)
```

```{r step7.13}
matplot(Maturities,Loadings,type="l",lty=1,col=c("black","red","green"),lwd=3)
plot(Factors[,1],Factors[,2],type="l",lwd=2)
```

# Drawing conclusions from the plot of the first two factors above.

--Factor 1 is almost the unweighted average of all the 7 yeilds. +ve vlaue of factor 1 = all yeilds are high. 

1. When we look at it as dots, we could infer that few lines are straight in the second part of the graph indicating the day to day movement is large and it also looks more deliberate and few are squirrelled in the graph may be indicating the day to day movement is clumsy and looks like it is more uncertain.

2. As volatility is the measure of standard deviation of daily differences and here we could see that the daily differences are almost the same in the first half of the data, we can conclude that there is no volatility in the first part of the data even though there is movement. whereas the second part has more volatility.

3. One factor is following the other. Factor 2 fell, factor 1 fell, factor 2 rose and factor 1 rose. Factor1 is following factor2. This is only on one direction. We can further extend our analysis on what is the direction on these circles. 

4. I could see some straight lines between one point and other point, but I couldnt infer much. MAy be they were stead and more deliberate during the particular time period. 


```{r step7.14}
# Analyzing the adjustments that each factor makes to the term curve.
OldCurve<-AssignmentData[135,]
NewCurve<-AssignmentData[136,]
CurveChange<-NewCurve-OldCurve
FactorsChange<-Factors[136,]-Factors[135,]
ModelCurveAdjustment.1Factor<-OldCurve+t(Loadings[,1])*FactorsChange[1]
ModelCurveAdjustment.2Factors<-OldCurve+t(Loadings[,1])*FactorsChange[1]+t(Loadings[,2])*FactorsChange[2]
ModelCurveAdjustment.3Factors<-OldCurve+t(Loadings[,1])*FactorsChange[1]+t(Loadings[,2])*FactorsChange[2]+
  t(Loadings[,3])*FactorsChange[3]
matplot(Maturities,
        t(rbind(OldCurve,NewCurve,ModelCurveAdjustment.1Factor,ModelCurveAdjustment.2Factors,
                ModelCurveAdjustment.3Factors)),
        type="l",lty=c(1,1,2,2,2),col=c("black","red","green","blue","magenta"),lwd=3,ylab="Curve Adjustment")
legend(x="topright",c("Old Curve","New Curve","1-Factor Adj.","2-Factor Adj.",
                      "3-Factor Adj."),lty=c(1,1,2,2,2),lwd=3,col=c("black","red","green","blue","magenta"))
```

```{r step7.15}
rbind(CurveChange,ModelCurveAdjustment.3Factors-OldCurve)
```


# Explaining how shapes of the loadings affect the adjustnents using only factor 1, factors 1 and 2, and all 3 factors.

On one day the shortest term maturities had an interest rate of 14.5 whereas the longer term maturities was around 13.5. On another day, everything seemed to be crashed and the shortest term maturities had an interest rate of 15.5 whereas the longer term maturities was around 14. We can think about this difference as the added effect of shifting according to the first factor, second factor and third factor.It also, seems like by dropping the old curve below the new curve, we have overcorrected the short rates and we have not corrected for the longest rates. We try to fit the new curve closer and closer to the factors and that is the red line in the plot. The solid data are actually the real data from day1 to some subsequent day. The dashed lines is when we take the actual day 1 curve and we shift it only along the dimensions of the first factor which is the green and then from the subsequent corrections by the subsequent factors. Hence, its basically adjusting for whatever each factor can't explain.

```{r step7.16}
# Goodness of fit for the example of 10Y yield.
# Glancing at how close is the approximation for each maturity?
# 5Y
cbind(Maturities,Loadings)
Means <- colMeans(AssignmentData)
Model.10Y<-Means[6]+Loadings[6,1]*Factors[,1]+Loadings[6,2]*Factors[,2]+Loadings[6,3]*Factors[,3]
matplot(cbind(AssignmentData[,6],Model.10Y),type="l",lty=1,lwd=c(3,1),col=c("black","red"),ylab="5Y Yield")
```

```{r step7.17}
# Repeating the PCA using princomp.

# PCA analysis using princomp()
PCA.Yields<-princomp(AssignmentData)
names(PCA.Yields)
# Checking that the loadings are the same
cbind(PCA.Yields$loadings[,1:3],Maturities,Eigen.Decomposition$vectors[,1:3])
```

```{r step7.18}
matplot(Maturities,PCA.Yields$loadings[,1:3],type="l",col=c("black","red","green"),lty=1,lwd=3)
matplot(PCA.Yields$scores[,1:3],type="l",col=c("black","red","green"),lwd=3,lty=1)
```

```{r step7.19}
# Changing the signs of the first factor and factor loading again.
PCA.Yields$loadings[,1]<--PCA.Yields$loadings[,1]
PCA.Yields$scores[,1]<--PCA.Yields$scores[,1]
matplot(Maturities,PCA.Yields$loadings[,1:3],type="l",col=c("black","red","green"),lty=1,lwd=3)
matplot(PCA.Yields$scores[,1:3],type="l",col=c("black","red","green"),lwd=3,lty=1)
```

# Output:

Output 1 is the first factor of PCA, which is the linear combination of original variables(all the 7 rates), which explains the most variance. i.e, If we are given the tomorrows 3M, 2YR, 3YR, 5YR, 10YR, 30YR, then we can tell the 6M rates with 99% confidence using factor 1. 

```{r step7.20}
# Uncovering the mystery of the Output in column 8.

matplot(cbind(PCA.Yields$scores[,1],AssignmentData.Output,Factors[,1]),type="l",col=c("black","red","green"),lwd=c(3,2,1),lty=c(1,2,3),ylab="Factor 1")
```

```{r step7.21}
#Comparing the regression coefficients from Step 2 and Step 3 with factor loadings.

#First, looking at the slopes for AssignmentData.Input~AssignmentData.Output

t(apply(AssignmentData, 2, function(AssignmentData.col) lm(AssignmentData.col~AssignmentData.Output)$coef))
cbind(PCA.Yields$center,PCA.Yields$loadings[,1])
# Checking if the same is true in the opposite direction: is there a correspondence between the coefficients of models Output1~Yield and the first loading.
AssignmentData.Centered<-t(apply(AssignmentData,1,function(AssignmentData.row) AssignmentData.row-PCA.Yields$center))
dim(AssignmentData.Centered)
t(apply(AssignmentData.Centered, 2, function(AssignmentData.col) lm(AssignmentData.Output~AssignmentData.col)$coef))
# Recovering the loading of the first factor by doing regression, using all inputs together.
t(lm(AssignmentData.Output~AssignmentData.Centered)$coef)[-1]
PCA.Yields$loadings[,1]
# This means that the factor is a portfolio of all input variables with weights.
PCA.Yields$loadings[,1]
```