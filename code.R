
###### clearing environment 
rm(list = ls())
graphics.off()

################################################################################
######### Modify Dataset 
################################################################################
set.seed(24)
Diamonds <- read.table("diamonds.csv", header = TRUE, 
                       sep = ",",
                       quote = "\"",
                       fileEncoding = "UTF-8")
Diamonds <- subset(Diamonds , select = - X)
colnames(Diamonds)[5] = "depth_percentage"
colnames(Diamonds)[8] = "length"
colnames(Diamonds)[9] = "width"
colnames(Diamonds)[10] = "depth"

####shuffling####
Diamonds <- Diamonds[sample(nrow(Diamonds)), ]

####shuffling####
indexes <- sample(seq_len(nrow(Diamonds)))
Diamonds <- Diamonds[indexes,]
####

################################################################################
######### Setting Dataset 
################################################################################
Diamonds <- read.table("diamonds.csv", header = TRUE, 
                       sep = ",",
                       quote = "\"",
                       fileEncoding = "UTF-8")

### Transform Categorical Variables as factors
Diamonds$cut <- factor(Diamonds$cut,
                levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"))

Diamonds$color <- factor(Diamonds$color,
                  levels = c("J", "I", "H", "G", "F","E","D"))

Diamonds$clarity <- factor(Diamonds$clarity,
              levels=c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"))

# no nan colums
colSums(is.na(Diamonds))

View(Diamonds)
summary(Diamonds)

################################################################################
######### Histograms 
################################################################################

par(mfrow = c(2,2))

hist(Diamonds$carat, 40 , 
     xlab = "Carat",  
     main = "Carat distribution") 

barplot(table(Diamonds$cut),
      xlab = "Cut",
      ylab = "Frequency", 
      main = "Cut distribution")

barplot(table(Diamonds$color), 
      xlab = "Color", 
      ylab = "Frequency", 
      main = "Color Distribution")

barplot(table(Diamonds$clarity), 
      xlab = "Clarity",
      ylab = "Frequency",
      main = "Clarity Distribution")

hist(Diamonds$depth_percentage, 50 ,
      xlab = "Depth Percentage", 
      main = "Depth Percentage distribution")

hist(Diamonds$table, 40 , xlab = "Table",  main = "Table distribution")

hist(Diamonds$price, 40 , xlab = "Price ($)",  main = "Price distribution")

hist(Diamonds$length, 40 , xlab = "Length (mm)",  main = "Length distribution")

hist(Diamonds$width, 50 , xlab = "Width (mm)",  main = "Width distribution")

hist(Diamonds$depth, 50 , xlab = "Depth (mm)",  main = "Depth distribution")

################################################################################
#########################################  Univariate analysis
################################################################################

par(mfrow = c(2,2))

#Numerical Variables

summary(lm(Diamonds$price ~ Diamonds$carat, data = Diamonds))
plot(Diamonds$carat, Diamonds$price, 
     main = "Carat vs Price", 
     xlab = "Carat", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$carat, data = Diamonds), col = "red")

summary(lm(Diamonds$price ~ Diamonds$depth_percentage, data = Diamonds))
plot(Diamonds$depth_percentage, Diamonds$price, 
     main = "Depth percentage vs Price", 
     xlab = "Depth percentage", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$depth_percentage, 
      data = Diamonds), col = "red")

summary(lm(Diamonds$price ~ Diamonds$table, data = Diamonds))
plot(Diamonds$table, Diamonds$price, 
     main = "Table vs Price", 
     xlab = "Table", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$table, data = Diamonds), col = "red")

summary(lm(Diamonds$price ~ Diamonds$length, data = Diamonds))
plot(Diamonds$length, Diamonds$price, 
     main = "Length vs Price", 
     xlab = "Lenght (mm)", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$length, data = Diamonds), col = "red")

summary(lm(Diamonds$price ~ Diamonds$width, data = Diamonds))
plot(Diamonds$width, Diamonds$price, 
     main = "Width vs Price", 
     xlab = "Width (mm)", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$width, data = Diamonds), col = "red")

summary(lm(Diamonds$price ~ Diamonds$depth, data = Diamonds))
plot(Diamonds$depth, Diamonds$price, 
     main = "Depth vs Price", 
     xlab = "Depth (mm)", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$depth, data = Diamonds), col = "red")

#Categorical Variables

summary(lm(Diamonds$price ~ Diamonds$cut, data = Diamonds))
plot(Diamonds$cut, Diamonds$price, 
     main = "Cut vs Price", 
     xlab = "Cut", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$cut, data = Diamonds), col = "red")

summary(lm(Diamonds$price ~ Diamonds$color, data = Diamonds))
plot(Diamonds$color, Diamonds$price, 
     main = "Color vs Price", 
     xlab = "Color", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$color, data = Diamonds), col = "red")

summary(lm(Diamonds$price ~ Diamonds$clarity, data = Diamonds))
plot(Diamonds$clarity, Diamonds$price, 
     main = "Clarity vs Price", 
     xlab = "Clarity", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$clarity, data = Diamonds), col = "red")

################################################################################
######### Outliers ?????
################################################################################

detect_outlier <- function(x) {
  Quantile1 <- quantile(x, probs=.25)
  Quantile3 <- quantile(x, probs=.75)
  IQR = Quantile3 - Quantile1
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}

remove_outlier <- function(dataframe,columns=names(dataframe)) {
  for (col in columns) {
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  print("Remove outliers")
  print(dataframe)
}

Diamonds <- remove_outlier(Diamonds, c('carat', 'depth_percentage', 'table', 'price',
                               "length", 'width', "depth"))




################################################################################
######### Standardization ????
################################################################################

library(dplyr)


Diamonds$price <- scale(Diamonds$price)
Diamonds$carat <- scale(Diamonds$carat)
Diamonds$depth_percentage <- scale(Diamonds$depth_percentage)
Diamonds$table <- scale(Diamonds$table)
Diamonds$length <- scale(Diamonds$length)
Diamonds$width <- scale(Diamonds$width)
Diamonds$depth <- scale(Diamonds$depth)

Diamonds <- Diamonds %>%
  mutate_at(vars(price, carat, depth_percentage, table, length, width, depth), scale)

#check outliers (before do standardization)
boxplot(Diamonds)$out


################################################################################
######### Correlation matrix between variables ????
################################################################################

#library(ggplot2)
#library(GGally)
#library(corrplot)

cor_scores <- cor(subset(Diamonds , select = -c(color,clarity,cut)))
corrplot(cor_scores,method = "number")

#Crea un grafico di dispersione (scatterplot matrix) insieme a istogrammi per
#ciascuna variabile nel dataset.
ggpairs(Diamonds)

################################################################################
######### Linear Regression
################################################################################

#The contrasts() function returns the coding that R uses for the dummy variables.
contrasts(Diamonds$cut)
contrasts(Diamonds$color)
contrasts(Diamonds$clarity)

lm_fit = lm(price ~ . , data = Diamonds)
summary(lm_fit)

#R^2
summary(lm_fit)$r.sq 
#RSE #quantifica la deviazione media dei valori osservati dai valori previsti dal
#modello di regressione.
summary(lm_fit)$sigma 
#MSE
mean((Diamonds$price - lm_fit$fitted.values)^2)
#MSE
mean((lm_fit$residuals)^2) 

par(mfrow = c(2,2))
plot(lm_fit)


###### Analisi dei Residui ######
par(mfrow = c(1,1))

# frequenza dei residui
hist(lm_fit$residuals,60,
     xlab = "Residual",
     main = "Empirical residual distribution")

#Index vs Residuals
plot(lm_fit$residuals,ylab = "Residuals",pch = "o",
      cex = 1, col = "black",
      main = paste0("Residual plot - mean:",round(mean(lm_fit$residuals),
      digits = 4),"- var:", round(var(lm_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(lm_fit$residuals)), col= "red", lwd = 2)

#Fitted value vs Residuals
plot(lm_fit$fitted.values,lm_fit$residuals,xlab = "Fitted values",
     ylab = "Residuals",pch = "o",
     cex = 1, col = "black",
     main = "Fitted Values vs Residuals")
abline(c(0,0),c(0,length(lm_fit$residuals)), col= "red", lwd = 2)

#Grafico residui studentizzati
plot(rstudent(lm_fit),ylab = "Studentized residuals",
     cex = 1, col = "black")
abline(a=0,b=0,lwd=2,col="red")

###### Normality and heteroschdaschity ######
shapiro.test(lm_fit$residuals[1:5000])
bptest(lm_fit)

################################################################################
######### Validation 70-30 ????
################################################################################

set.seed(1) # seed for random number generator

#train and test indexes
train <- sample(nrow(Diamonds),floor(nrow(Diamonds)*0.7),replace = FALSE)
#train model
lm_fit_train <- lm(price ~ . , data = Diamonds, subset = train)
summary(lm_fit_train)
mean((lm_fit_train$residuals)^2) #Train MSE
#test MSE
pred_err = (Diamonds$price- predict(lm_fit_train,Diamonds))^2
train_mse = mean(pred_err[train]) 
test_mse = mean(pred_err[-train])
#using predict() function ???


################################################################################
############################### K-fold; k = 10
################################################################################
set.seed(1) # seed for random number generator
library(boot)

#Linear regression
glm_fit <- glm(price ~ . , data = Diamonds)
summary(glm_fit)

#Cross-validation for Generalized Linear Models: cv.glm K = 10
cv_err <- cv.glm(Diamonds , glm_fit, K = 10)
#K-fold test error
kfold_test_err <- cv_err$delta[1]

################################################################################
############################### Bootstrap ??
################################################################################



################################################################################
############################### Subset selection methods
################################################################################

###### Best subset method ######




################################################################################
############################### Ridge Regression
################################################################################

#preparing data
#creating regressor (automatic handle categorical variables in dummy variables)
x <- model.matrix ( price ~ . , Diamonds )[,-1] #tutte le righe - la prima colonna (intercetta)
y <- Diamonds$price
#lambda_grid
lambda_grid <- 10^seq(10,-2,length = 100) #generates 100 lambdas

ridge.mod <- glmnet(x[train , ], y[train], alpha = 0, 
                    lambda = lambda_grid, thresh = 1e-12)

####### Choosing the best lambda #######
set.seed(1)
#train and test indexes
train <- sample(nrow(Diamonds),floor(nrow(Diamonds)*0.7),replace = FALSE)

cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

#What is the test MSE associated with this value of lambda?
ridge.pred <- predict(ridge.mod , s = bestlam ,newx = x[-train , ])
mean((ridge.pred - y[-train])^2)

#we refit our ridge regression model on the full data set,
#using the value of lambda chosen by cross-validation, and examine the coefficient
#estimates.
ridge_fit <- glmnet(x, y, alpha = 0,lambda = lambda_grid,standardize = TRUE)
dim(coef(ridge_fit))
predict(ridge_fit , type = "coefficients", s = bestlam)[1:24, ]
#Andamento dei coefficienti vs l1 norm
plot(ridge_fit)

################################################################################
############################### Lasso Regression
################################################################################

lasso_mod <- glmnet(x[train , ], y[train], alpha = 1, lambda = lambda_grid)
plot(lasso_mod)

#We now perform cross-validation and compute the associated test error.
set.seed(1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1,nfolds = 10)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso_pred <- predict(lasso_mod , s = bestlam , newx = x[-train , ])
mean((lasso_pred - y[-train])^2) #Test MSE 

#we refit our lasso regression model on the full data set,
#using the value of lambda chosen by cross-validation, and examine the coefficient
#estimates.
lasso_fit <- glmnet(x, y, alpha = 1, lambda = lambda_grid,standardize = TRUE)
plot(lasso_fit)
lasso_coef <- predict(lasso_fit , type = "coefficients",  s = bestlam)[1:24, ]
lasso_coef  #solo un regressore a 0






