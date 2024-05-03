
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

#cut price
Diamonds <- subset(Diamonds, price  >= 500 & price <= 1000)

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
######### Correlation matrix between variables 
################################################################################

#library(ggplot2)
#library(GGally)
library(corrplot)

par(mfrow = c(1,1))

cor_scores <- cor(subset(Diamonds , select = -c(color,clarity,cut)))
corrplot(cor_scores,method = "number")

#Crea un grafico di dispersione (scatterplot matrix) insieme a istogrammi per
#ciascuna variabile nel dataset.
ggpairs(Diamonds)

################################################################################
#########################################  Remove depth percentage
################################################################################

Diamonds <- subset(Diamonds , select = -depth_percentage)


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

summary(lm(Diamonds$price ~ Diamonds$depth, data = Diamonds))
plot(Diamonds$depth, Diamonds$price, 
     main = "Depth vs Price", 
     xlab = "Depth (mm)", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$depth, data = Diamonds), col = "red")

summary(lm(Diamonds$price ~ Diamonds$width, data = Diamonds))
plot(Diamonds$width, Diamonds$price, 
     main = "Width vs Price", 
     xlab = "Width (mm)", 
     ylab = "Price ($)")
abline(lm(Diamonds$price ~ Diamonds$width, data = Diamonds), col = "red")

plot(Diamonds$depth_percentage, Diamonds$price, 
     main = "Depth percentage vs Price", 
     xlab = "Depth percentage", 
     ylab = "Price ($)")

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
library(corrplot)

par(mfrow = c(1,1))

cor_scores <- cor(subset(Diamonds , select = -c(color,clarity,cut)))
corrplot(cor_scores,method = "number")

#Crea un grafico di dispersione (scatterplot matrix) insieme a istogrammi per
#ciascuna variabile nel dataset.
ggpairs(Diamonds)

################################################################################
######### Splitting Dataset in train e test
################################################################################

#train and test indexes
train <- sample(nrow(Diamonds),floor(nrow(Diamonds)*0.7),replace = FALSE)
price_test <- Diamonds$price[-train]

################################################################################
######### Linear Regression
################################################################################

#The contrasts() function returns the coding that R uses for the dummy variables.
contrasts(Diamonds$cut)
contrasts(Diamonds$color)
contrasts(Diamonds$clarity)

#Train model
lm_model_1 = lm(price ~ . , data = Diamonds,subset = train)
summary(lm_model_1)
#R^2
summary(lm_model_1)$r.sq 
#RSE #quantifica la deviazione media dei valori osservati dai valori previsti dal
#modello di regressione.
summary(lm_model_1)$sigma 
#Train MSE
lm_train_MSE = mean((Diamonds$price - lm_model_1$fitted.values)^2)
#Train MSE
mean((lm_model_1$residuals)^2) 

#test MSE
pred_err = (Diamonds$price- predict(lm_model_1,Diamonds))^2
train_mse = mean(pred_err[train]) 
test_mse = mean(pred_err[-train])
#using predict() function 
y_hat_lm = predict(lm_model_1,newdata = Diamonds[-train,])
lm_MSE_1 = mean((y_hat_lm - Diamonds$price[-train])^2)


###### Analisi dei Residui ######
par(mfrow = c(1,1))

plot(lm_model_1)

# frequenza dei residui
hist(lm_model_1$residuals,60,
     xlab = "Residual",
     main = "Empirical residual distribution")

#Index vs Residuals
plot(lm_model_1$residuals,ylab = "Residuals",pch = "o",
      cex = 1, col = "black",
      main = paste0("Residual plot - mean:",round(mean(lm_model_1$residuals),
      digits = 4),"- var:", round(var(lm_model_1$residuals),digits = 2)))
abline(c(0,0),c(0,length(lm_fit$residuals)), col= "red", lwd = 2)

#Fitted value vs Residuals
plot(lm_model_1$fitted.values,lm_model_1$residuals,xlab = "Fitted values",
     ylab = "Residuals",pch = "o",
     cex = 1, col = "black",
     main = "Fitted Values vs Residuals")
abline(c(0,0),c(0,length(lm_model_1$residuals)), col= "red", lwd = 2)

#Grafico residui studentizzati
plot(rstudent(lm_model_1),ylab = "Studentized residuals",
     cex = 1, col = "black")
abline(a=0,b=0,lwd=2,col="red")

###### Normality and heteroschdaschity ######
shapiro.test(lm_model_1$residuals[1:5000])
bptest(lm_fit)


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
library(glmnet)

#preparing data
#creating regressor (automatic handle categorical variables in dummy variables)
x <- model.matrix ( price ~ . , Diamonds )[,-1] #tutte le righe - la prima colonna (intercetta)
y <- Diamonds$price
#lambda_grid
lambda_grid <- 10^seq(10,-2,length = 100) #generates 100 lambdas

ridge.mod <- glmnet(x[train , ], y[train], alpha = 0, 
                    lambda = lambda_grid, thresh = 1e-12)

####### Choosing the best lambda #######
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

################################################################################
############################### GAM
################################################################################

library(gam)

## carat -> natural spline
## cut -> step function
## color -> step function  
## clarity -> step function 
## depth_percentage -> smoothing spline
## table -> smoothing spline 
## lenght -> natural spline 
## width -> natural spline 
## depth -> natural spline
## price -> output variable

####### GAM on full Dataset #######
gam_model_1 <- gam(price~ ns(carat,4) + cut + color + clarity +
                   s(depth_percentage,5) + s(table,5) + ns(length,4) + 
                   ns(width,4) +ns(depth,4), data=Diamonds)

plot(gam_model_1, se = TRUE)
summary(gam_model_1)
MSE_gam_model_1 <- mean((gam_model_1$residuals)^2) #MSE

plot(gam_model_1$residuals)
plot(Diamonds$price,gam_model_1$fitted.values)
plot(gam_model_1$fitted.values,gam_model_1$residuals)
plot(Diamonds$price,gam_model_1$residuals)

####### GAM train and test #######
gam_model_train <- gam(price~ ns(carat,4) + cut + color + clarity +
                     s(depth_percentage,5) + s(table,5) + ns(length,4) + 
                     ns(width,4) +ns(depth,4), data=Diamonds[train, ])

plot(gam_model_train,se=TRUE)

gam_pred_value <- predict(gam_model_train,newdata = Diamonds[-train,])
plot(Diamonds$price[-train],gam_pred_value)
plot(Diamonds$price[-train] - gam_pred_value)
mean((Diamonds$price[-train] - gam_pred_value)^2)

################################################################################
############################### Regression Trees
################################################################################
library(tree)

####### Tree on full dataset ######
tree_model_1 <- tree(price ~ . , data = Diamonds)
summary(tree_model_1)
plot(tree_model_1)
text(tree_model_1 , pretty = 0)
##If we just type the name of the tree object, R prints output corresponding
#to each branch of the tree. R displays the split criterion ,
#the number of observations in that branch, the deviance, the overall prediction
#for the branch, and the fraction of observations in that
#branch that take on values of Yes and No. Branches that lead to terminal
#nodes are indicated using asterisks
tree_model_1

####### Test MSE ######
set.seed(2)
tree_model_2 <- tree(price ~ ., data = Diamonds, subset = train)
summary(tree_model_2)
plot(tree_model_2)
text(tree_model_2 , pretty = 0)

yhat_tree_2 <- predict(tree_model_2 , newdata = Diamonds[-train,]) #predizioni
plot(yhat_tree_2 , price_test) #Previsioni vs dati reali
mean((yhat_tree_2 - price_test)^2) #Test MSE

####### Pruning ######

#The function
#cv.tree() performs cross-validation in order to determine the optimal level of 
#tree complexity; cost complexity pruning is used in order to select a 
#sequence of trees for consideration.
cv_tree <- cv.tree(tree_model_2)
#The cv.tree() function reports the number of terminal nodes of each tree considered
#(size) as well as the corresponding error rate and the value of the
#cost-complexity parameter used (k, which corresponds to aplha )
##dev corresponds to the number of cross-validation errors.
cv_tree  #(Valori molto alti!)
plot(cv_tree$size , cv_tree$dev, type = "b")
plot(cv_tree$k , cv_tree$dev, type = "b")

#prendo la size con errore minore
best = min(cv_tree$size[cv_tree$dev == min(cv_tree$dev)])
#prendo la k con errore minore
k = min(cv_tree$k[cv_tree$dev == min(cv_tree$dev)]) #alpha in the book

#prune the tree
prune_model <- prune.tree(tree_model_2 , best = best)
plot(prune_model)
text(prune_model , pretty = 0)
#Oss: l'albero di pruning ha gli stessi terminal node dell'albero unpruned!!
#Quindi il miglior albero è quello senza pruning

##### Test MSE on the best sub-tree #####
yhat_prune <- predict(prune_model , newdata = Diamonds[-train , ]) #Predizioni
plot(yhat_prune, price_test) #Previsioni vs dati reali
mean((yhat_prune - price_test)^2) #Test MSE

################################################################################
############################### Bagging
################################################################################

# Train and test 
train<- sample(nrow(Diamonds),floor(nrow(Diamonds)*0.5),replace = FALSE)
price_test <- Diamonds$price[-train]

library(randomForest)
set.seed(1)
#### Bagging on full Dataset ####
bag_model_1 <- randomForest(price ~ ., data = Diamonds , 
                           mtry = ncol(Diamonds)-1, 
                           importance = TRUE,
                           replace=TRUE,
                           ntree=2) ##Occhio valore di tree

bag_model_1
summary(bag_model_1)
plot(bag_model_1)
importance(bag_model_1)

#### Test MSE Bagging ####
bag_model_2 <- randomForest(price ~ ., data = Diamonds , subset = train, 
                            mtry = ncol(Diamonds)-1, 
                            importance = TRUE,
                            replace=TRUE,
                            ntree=100)
bag_model_2
summary(bag_model_2)
plot(bag_model_2)
importance(bag_model_2)

yhat_bag_2 <- predict(bag_model_2 , newdata = Diamonds[-train , ])
plot(yhat_bag_2 ,price_test)
mean((yhat_bag_2 - price_test)^2) #Test MSE 

################################################################################
############################### Random Forest
################################################################################

#### Test MSE ####
rf_model_1 <- randomForest(price ~ ., data = Diamonds , subset = train, 
                            mtry = floor(sqrt(ncol(Diamonds)-1)), 
                            importance = TRUE,
                            replace=TRUE,
                            ntree=100)

rf_model_1
summary(rf_model_1)
plot(rf_model_1)
importance(rf_model_1)

yhat_rf <- predict(rf_model_1 , newdata = Diamonds[-train , ])
plot(yhat_rf ,price_test)
mean((yhat_rf - price_test)^2) #Test MSE 

#### Confronto Bagging e Random Forest ####
plot(rf_model_1,type = 'b',col="green",pch = "+")
par(new=TRUE) #per sovrapporre grafico
plot(bag_model_2,type = 'b',col="red",pch='o')


################################################################################
############################### Boosting
################################################################################

# Train and test 
train<- sample(nrow(Diamonds),floor(nrow(Diamonds)*0.7),replace = FALSE)
price_test <- Diamonds$price[-train]

library(gbm)
set.seed(1)

#### Test MSE ####

boost_model_1 <- gbm(price ~ ., data = Diamonds[train , ],
                    distribution = "gaussian", 
                    n.trees = 5000,
                    interaction.depth = 4)
summary(boost_model_1)

#We see that carat and width are by far the most important variables.

#We can also produce partial dependence plots for these two variables. These plots
#illustrate the marginal effect of the selected variables on the response after
#integrating out the other variables. In this case, as we might expect, the price
#are increasing with both variables.
plot(boost_model_1 , i = "carat")
plot(boost_model_1 , i = "width")


yhat_boost_1 <- predict(boost_model_1 , newdata = Diamonds[-train , ], 
                      n.trees = 5000)
boost_MSE_1 <- mean((yhat_boost_1 - price_test)^2)

##If we want to, we can perform boosting with a different
#value of the shrinkage parameter lambda in (8.10). The default value is 0.001,
#but this is easily modified. Here we take lambda = 0.2.
boost_model_2 <- gbm(price ~ ., data = Diamonds[train , ],
                     distribution = "gaussian", 
                     n.trees = 5000,
                     interaction.depth = 4,
                     shrinkage = 0.02,
                     verbose=F)

summary(boost_model_2)
plot(boost_model_2 , i = "carat")
plot(boost_model_2 , i = "width")

yhat_boost_2 <- predict(boost_model_2 , newdata = Diamonds[-train , ], 
                        n.trees = 5000)
boost_MSE_2 <- mean((yhat_boost_2 - price_test)^2)








