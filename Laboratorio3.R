library(MASS)
library(ISLR)
library(dplyr)
library(ggplot2)

correlacion <- cor(Boston)
corrplot(correlacion)

know_data <- Boston %>% 
  dplyr::select(lstat,medv)


names(know_data)[1] <- "input"
names(know_data)[2] <- "output"


know_data.rows <- nrow(know_data)

get_boot <- function(x){
  index<-sample(1:know_data.rows, 
                size = know_data.rows, 
                replace = TRUE)
  return(know_data[index,])
}


fit_lm <- function(dataset,degree=2){
  formula <- paste0("I(","input^",1:degree,")",collapse = '+')
  formula <- paste0("output ~ ",formula)
  fit <- lm(formula,data = dataset)
  return(fit)
}

model_plot_data <- function(fit){
  xaxis<-
    seq(
      min(know_data$input),
      max(know_data$input),
      by=0.01)
  yaxis<-predict(fit,tibble(input=xaxis))
  return(tibble(input=xaxis,output=yaxis))
}


nboots<-100
boots <- lapply(1:nboots, get_boot)


all.models <-
  lapply(boots, fit_lm, degree=45)

all.model.prediction <-
  lapply(all.models, model_plot_data)

df <- bind_rows(all.model.prediction,.id = "boot")
dfvar<-df[1:nboots,]
var1<-sum(dfvar["output"])/nboots
var2<-sum(dfvar["output"])
var3<-(var2-var1)^2/nboots-1
varfinal<-var3/nrow(df)

bias<-(var1-var2)^2
biasfinal<-bias/nrow(df)

