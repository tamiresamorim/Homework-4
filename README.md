# homework-4
---
title: "Homework#4"
author: "Tamires Amorim, Yamei Li and Meirou Guan and Carol-Ann Jackson"
date: "10/11/2020"
output: github_document 
---

```{r}
load("workspace.RData")
```


#### Homework 4

Linear Regression Exercise: 

The following is interested in finding the relationship between wage and women with higher academic degree level, the goal is to understand if their advanced degree explains their wages. 


First we converted the Education attainment as advanced degree from a factor to a numerical value: 
```{r}
acs2017_ny$educ_advdeg <- factor(acs2017_ny$EDUCD)
acs2017_ny$educ_advdeg <- as.numeric(acs2017_ny$educ_advdeg)
```


Second we defined the subgroup, limiting the research for ages between 25 and 55 years old, currently working, and worked last year from 48 to 52 weeks intervalled, and working hours greater or equal to 35 hours per week.
```{r}
attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb) 
attach(dat_use)
```


Now we want to perform a linear regression to determine the coefficients of a linear model, in our specific case we want to predict the wage as explained by female education, here we included age to have a defined age group in the analysis. 
Using the lm function we have:
```{r}
fit <- lm(INCWAGE ~  educ_advdegCode + female + AGE, data = acs2017_ny)
```


Now we wanted to plot the variables to observe if there is a linear relation between the variables. The plot for female wages seems extremely odd and for now I do not have any reasonable explanations for that shape.
```{r}
plot(INCWAGE ~ educ_advdeg + female + AGE , col=2)
abline(fit,col=3,lwd=2)
bs <- round(coef(fit), 3)
lmlab <- paste0("INCWAGE = ", bs[1],
 ifelse(sign(bs[2])==1, " + ", " - "), abs(bs[2]), " educ_advdeg ")
mtext(lmlab, 3, line=-2)
```

Now we want to visualize the output and understand the relation the variables hold: 
```{r}
model_temp1 <- lm(INCWAGE ~ educ_advdegCode + female + AGE)
summary(model_temp1)
plot(model_temp1)
require(stargazer)
stargazer(model_temp1, type = "text")
```

Stargaze give us the summary of the data: 

1. The estimated slope of each coefficient:

a) Educ_advdeg and the y-intercept, which suggests the best fit prediction of wage is (-36377.764) + 4428.513 * educ_advdeg

b) Female and the y-intecept, which suggests the best fit prediction of wage is (-36377.764) + (-16450.841)*female ####this does not make sense

c) Age and the y-intercept, which suggests the best fit prediction of wage is (-36377.764) + (-110.985)*AGE 

2. The p-value of each coefficient, which suggests that the intercept and education are probably not due to chance, the p-value less than 0.01 under normal circumstances mean that there is substantial evidence against the null hypothesis. 

3. Overall estimates of fit such as r-squared and adjusted r-squared, which shows how much of the variation in wage is explained by the model. 


```{r}
# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(educ_advdegCode, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)

plot(INCWAGE ~ jitter(educ_advdegCode, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
# discus what you see in this plot

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, educ_advdegCode = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ educ_advdegCode, data = to_be_predicted2)
```


```{r}
detach()
```



```{r}
save.image("workspace.RData")
```


