# Multinominal Regression:-

### Problem Statement:- 

  - Need to predict the type of program a student is in based on other attributes

### Reading the Dataset:- 

```{r}
library(readxl)
mdata <- read.csv("~/desktop/Digi 360/Module 10/Dataset/mdata.csv")
```

```{r}
head(mdata)
```

```{r}
any(is.na(mdata))
```

```{r}
mdata <- mdata[ -c(1) ]
head(mdata)
```


```{r}
table(mdata$prog) # tabular representation of the Y categories
```

First, we need to choose the level of our outcome that we wish to use as our baseline and specify this in the relevel function. Then, we run our model using multinom. The multinom package does not include p-value calculation for the regression coefficients, so we calculate p-values using Wald tests (here z-tests).

```{r}
mdata$prog <- relevel(mdata$prog, ref = "academic")
```


we use the multinom function from the nnet package to estimate a multinomial logistic regression model

```{r}
library(mlogit)
library(nnet)
```


```{r}
model <- multinom(prog ~ id + female + ses + schtyp + read + write + math + science, data=mdata)
```

```{r}
summary(model)
```


```{r}
z <- summary(model)$coefficients/summary(model)$standard.errors
z
```

```{r}
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
```

```{r}
## extract the coefficients from the model and exponentiate
exp(coef(model))
```

```{r}
# predict probabilities
prob <- fitted(model)
prob
```

The table above indicates that the probability of 1st observation being `vocation` is 61.0%, it being `general` is 21.0% and it being `academic` is 18.0%. Thus we can conclude that the 1st observation is `vocation`. Similarly we can infer for all remaining observations. 

```{r}
# Find the accuracy of the model

class(prob)
prob <- data.frame(prob)
#View(prob)
prob["pred"] <- NULL
```

```{r}
# Custom function that returns the predicted value based on probability
get_names <- function(i){
  return (names(which.max(i)))
}
```

```{r}
pred_name <- apply(prob,1,get_names)
prob$pred <- pred_name
#View(prob)
```

```{r}
# Confusion matrix
table(pred_name,mdata$prog)
```


```{r}
# confusion matrix visualization
barplot(table(pred_name,mdata$prog),beside = T,col=c("red","lightgreen","blue","orange"),legend=c("academic","general","vocation"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")
```

```{r}
# Accuracy 
mean(pred_name==mdata$prog)
```

### Conclusion:- 

  - Drawn infernces out of the predicted probabilities.
  - The accuracy of the model is around 64%

