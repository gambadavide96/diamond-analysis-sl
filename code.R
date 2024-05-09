
###### clearing environment 
rm(list = ls())
graphics.off()

################################################################################
######### Modify Dataset 
################################################################################
set.seed(2)
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

#Modify dataset
Diamonds <- subset(Diamonds, price >= 2000 & price <= 8000)
Diamonds$price <- Diamonds$price / 1000
write.csv(Diamonds, "Diamonds.csv", row.names = FALSE)

View(Diamonds)
summary(Diamonds)

################################################################################
######### Outliers 
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

hist(Diamonds$depth_percentage, 40 ,
      xlab = "Depth Percentage", 
      main = "Depth Percentage distribution")

hist(Diamonds$table, 40 , xlab = "Table Percentage",  main = "Table distribution")

hist(Diamonds$price, 40 , xlab = "Price (Thousands $)",  main = "Price distribution")

hist(Diamonds$length, 30 , xlab = "Length (mm)",  main = "Length distribution")

hist(Diamonds$width, 30 , xlab = "Width (mm)",  main = "Width distribution")

hist(Diamonds$depth, 30 , xlab = "Depth (mm)",  main = "Depth distribution")


################################################################################
######### Mean e Std dataset variable
################################################################################

# Carica il pacchetto dplyr
library(dplyr)

# Seleziona solo le colonne numeriche
numeric_cols <- Diamonds %>% select(where(is.numeric))

# Calcola la media di ciascuna variabile numerica
means <- colMeans(numeric_cols)

# Calcola la deviazione standard di ciascuna variabile numerica
std_devs <- apply(numeric_cols, 2, sd)

# Combina i risultati in un dataframe
summary_stats <- data.frame(mean = means, std_dev = std_devs)

# Visualizza il dataframe con le medie e le deviazioni standard
View(summary_stats)



################################################################################
######### Correlation matrix between variables 
################################################################################

#library(ggplot2)
#library(GGally)
library(corrplot)

par(mfrow = c(1,1))

cor_scores <- cor(subset(Diamonds , select = -c(color,clarity,cut)))
corrplot(cor_scores,method = "number")


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
#confidence interval 95%
confint(lm_model_1)
#R^2
summary(lm_model_1)$r.sq 
#Train RMSE
lm_train_RMSE = sqrt(mean((Diamonds$price[train] - lm_model_1$fitted.values)^2))
lm_train_RMSE
#Train RMSE
lm_train_RMSE = sqrt(mean((lm_model_1$residuals)^2))
lm_train_RMSE

#test RMSE
pred_err = (Diamonds$price- predict(lm_model_1,Diamonds))^2
train_rmse = sqrt(mean(pred_err[train])) 
test_rmse = sqrt(mean(pred_err[-train]))
#using predict() function 
y_hat_lm = predict(lm_model_1,newdata = Diamonds[-train,])
lm_test_RMSE_1 = sqrt(mean((y_hat_lm - Diamonds$price[-train])^2))
lm_test_RMSE_1


###### Analisi dei Residui ######
par(mfrow = c(2,2))
#Grafici diagnostici
plot(lm_model_1)

par(mfrow = c(1,1))


# frequenza dei residui
hist(lm_model_1$residuals,60,
     xlab = "Residuals",
     main = "Empirical residual distribution")

# frequenza dei residui studentizzati
hist(rstudent(lm_model_1),60,
     xlab = "Studentized residuals",
     main = "Empirical residual distribution")

#Fitted values vs Residuals
plot(lm_model_1$fitted.values,lm_model_1$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Fitted values vs Residuals",
     cex = 1, col = "black")
abline(a=0,b=0,lwd=1.5,col="red")

#Predicted values vs Residuals
plot(y_hat_lm,(y_hat_lm - Diamonds$price[-train]),
     xlab = "Predicted values",
     ylab = "Residuals",
     main = "Predicted values vs Residuals",
     cex = 1, col = "black")
abline(a=0,b=0,lwd=1.5,col="red")

#Questo grafico viene utilizzato per valutare se esiste una relazione sistematica
#tra i valori predetti e i residui.  In particolare, si cerca di verificare se 
#i residui mostrano una distribuzione casuale intorno allo zero al variare 
#dei valori predetti. Se i residui mostrano una struttura sistematica, potrebbe 
#indicare che il modello non è adeguato  e che potrebbero essere necessarie 
#trasformazioni aggiuntive o l'uso di modelli più complessi.

#Fitted Values vs Studentized Residuals
plot(lm_model_1$fitted.values,rstudent(lm_model_1),
     xlab = "Fitted values",
     ylab = "Studentized residuals",
     main = "Fitted values vs Studentized residuals",
     cex = 1.5, col = "black")
abline(a=0,b=0,lwd=1.5,col="red")

#I residui studentizzati sono i residui divisi per la deviazione standard stimata 
#del residuo.
#Questo serve a rendere i residui comparabili tra loro e identificare eventuali
#osservazioni influenti o outlier.Se un residuo studentizzato ha  un valore assoluto 
#maggiore di 2 o 3, potrebbe indicare che quell'osservazione ha un impatto 
#significativo sul modello   potrebbe essere considerato un outlier 
#o un'osservazione influente.

#In sintesi, mentre i residui standardizzati dividono i residui per la deviazione 
#standard dei residui, i residui studentizzati li dividono per la deviazione standard 
#stimata dei residui, che è una stima più accurata della variabilità residua nel modello. 
#Entrambi sono utilizzati per identificare osservazioni anomale o influenti nel modello.


###### Test sui residui ######
shapiro.test(lm_model_1$residuals[1:5000])
bptest(lm_model_1)

##### Model with interaction terms #####
lm_model_2 = lm(price ~ . + (length:width:depth) , data = Diamonds,
                subset = train)
summary(lm_model_2)

#confidence interval 95%
confint(lm_model_2)

#R^2
summary(lm_model_2)$r.sq 

#Train RMSE
lm_train_RMSE_2 = sqrt(mean((lm_model_2$residuals)^2))
lm_train_RMSE_2

#Test RMSE 
fitt_value_lm_2 = predict(lm_model_2,newdata = Diamonds[-train,])
lm_test_RMSE_2 = sqrt(mean((fitt_value_lm_2 - Diamonds$price[-train])^2))
lm_test_RMSE_2

### Analisi dei Residui ###
par(mfrow = c(2,2))

#Grafici diagnostici
plot(lm_model_2)

par(mfrow = c(1,1))

# frequenza dei residui 
hist(lm_model_2$residuals,60,
     xlab = "Residuals",
     main = "Empirical residual distribution")

# frequenza dei residui studentizzati
hist(rstudent(lm_model_2),60,
     xlab = "Studentized residuals",
     main = "Empirical residual distribution")

#Fitted values vs Residuals
plot(lm_model_2$fitted.values,lm_model_2$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Fitted values vs Residuals",
     cex = 1, col = "black")
abline(a=0,b=0,lwd=1.5,col="red")

#Fitted Value vs Studentized Residuals
plot(lm_model_2$fitted.values,rstudent(lm_model_2),
     xlab = "Fitted values",
     ylab = "Studentized residuals",
     main = "Fitted values vs Studentized residuals",
     cex = 1.5, col = "black")
abline(a=0,b=0,lwd=1.5,col="red")

#Per modellare gli effetti sinergici sul prezzo che hanno le dimensioni del diamante,
#inseriamo nel modello un interaction term sulle dimensioni del diamante

#Nel caso dei diamanti, potrebbe essere plausibile che la combinazione di larghezza,
#lunghezza e profondità abbia un effetto complessivo sulla percezione del valore (prezzo) del diamante. 
#Potrebbe esserci un effetto sinergico in cui un certo rapporto tra larghezza e lunghezza, 
#o tra larghezza e profondità, influisce sul valore del diamante in modo diverso 
#rispetto alla larghezza o alla lunghezza da sole.

## Anova test per confrontare i due modelli di regressione lineare creati ##
anova(lm_model_1,lm_model_2, test='F')
#The anova() function performs a hypothesis test comparing the two models. The null hypothesis 
#is that the two models fit the data equally well, and the alternative hypothesis is that the full
#model is superior. Here the F-statistic is 213 and the associated p-value is
#virtually zero. This provides very clear evidence that the model containing
#the interaction term is better


################################################################################
############################### Subset selection methods
################################################################################
library(leaps)
###### Best subset selection ######
model_bwd <- regsubsets(price ~ .+ (length:width:depth), data = Diamonds[train,], 
                         nvmax = 24)

summary(model_bwd)

names(summary(model_bwd)) #tutte le statistiche fornite

##Test RMSE Validation approach

#We now compute the validation set error for the best
#model of each model size. We first make a model matrix from the test
#data.
#The model.matrix() function is used in many regression packages for building 
#an “X” matrix from data.
test_mat <- model.matrix(price ~ . + (length:width:depth), 
                         data = Diamonds[-train , ])

#Now we run a loop, and for each size i, we
#extract the coefficients from regfit.best for the best model of that size,
#multiply them into the appropriate columns of the test model matrix to
#form the predictions, and compute the test MSE.
val_RMSE <- rep(NA, 24)
for (i in 1:24) {
  coefi <- coef(model_bwd , id = i)
  pred <- test_mat[, names(coefi)] %*% coefi #prodotto matrici coefficienti per la previsione di price
  val_RMSE[i] <- sqrt(mean((Diamonds$price[-train] - pred)^2))
}
#Supponiamo che test_mat sia una matrice e coefi sia un vettore contenente i nomi 
#delle colonne che si desidera selezionare da test_mat. L'espressione test_mat[, names(coefi)] 
#restituirà una sotto-matrice di test_mat che contiene solo le colonne il cui 
#nome corrisponde ai valori contenuti in coefi. (cioè prende i valori di test
#da moltiplicare per i regressori selezionati nell'iterazione i-esima )

val_RMSE #Test RMSE per tutti i modelli calcolati

#We find that the best model is the one that contains  variables.
min_RMSE = which.min(val_RMSE)
min_RMSE
val_RMSE[min_RMSE]
RMSE_subselection <- val_RMSE[min_RMSE]
coef(model_bwd , min_RMSE)
#Nel miglior modello sono stati eliminati 3 regressori dal totale:
#Sono state eliminate table,depth_percentage e length
#Coerente con la regressione lineare, ha eliminato i regressori con il
#p-value più alto

#R^2 e modello con R^2 più alto in grafico
summary(model_bwd)$rsq
plot(summary(model_bwd)$rsq,xlab = "N° regressor",ylab = "R^2")
max_r2 <- which.max(summary(model_bwd)$rsq)
points(max_r2, summary(model_bwd)$rsq[max_r2], col = "red", cex = 2, pch = 20)
#Modello con test RMSE più basso
plot(val_RMSE,xlab = "N° regressor",ylab = " Test RMSE")
points(min_RMSE, val_RMSE[min_RMSE], col = "blue", cex = 2, pch = 20)
legend("topright", legend = "Min Test RMSE", col = "blue", pch = 20)

################################################################################
############################### Ridge Regression
################################################################################
library(glmnet)

#preparing data
#creating regressor (automatic handle categorical variables in dummy variables)
x <- model.matrix ( price ~ . +(length : width : depth) , 
                    Diamonds )[,-1]  #tutte le righe - la prima colonna (intercetta)
y <- Diamonds$price

#here we have chosen to implement
#the function over a grid of values ranging from lamba = 10^7 to lambda = 10^-3, 
#essentially covering the full range of scenarios from the null model containing
#only the intercept, to the least squares fit
#lambda_grid
lambda_grid <- 10^seq(-3,4,length = 100); #generating 100 lambdas

#Note that by default, the glmnet() function standardizes the
#variables so that they are on the same scale. To turn off this default setting,
#use the argument standardize = FALSE
ridge_model_1 <- glmnet(x[train , ], y[train], alpha = 0, 
                    lambda = NULL, 
                    standardize = TRUE)
dim(coef(ridge_model_1))

#Esempi di valori di lambda
#Seleziono lambda 5 (grande) e calcolo l1 norm per lambda 5 (piccola)
ridge_model_1$lambda[5]
coef(ridge_model_1)[, 5]
sqrt(sum(coef(ridge_model_1)[-1,5]^2)) ##l1 norm (escludendo intercetta)

#Seleziono lambda 95(piccolo) e calcolo l2 norm per lambda 95 (grande)
ridge_model_1$lambda[95]
coef(ridge_model_1)[, 95]
sqrt(sum(coef(ridge_model_1)[-1,95]^2)) #l1 norm (escludendo intercetta)

### Andamento dei coefficienti al variare di lambda e l1 norm ###
plot(ridge_model_1, xvar = "lambda",xlab="Log(λ)")
plot(ridge_model_1, xvar = "norm",xlab="l1 norm")

####### Choosing the best lambda #######
cv_ridge_out <- cv.glmnet(x[train , ], y[train], alpha = 0,
                          lambda = NULL,
                          nfolds = 10)
plot(cv_ridge_out)
bestlam_ridge <- cv_ridge_out$lambda.min
bestlam_ridge  
# (idealmente il modello non ne metterebbe nessuna probabilmente)

### Test RMSE ###
ridge_model_2 <- glmnet(x[train , ], y[train], alpha = 0, 
                        lambda = bestlam_ridge, 
                        standardize = TRUE)

#Beta del modello trovato per il miglior lambda
coef(ridge_model_2)

fitt_value_ridge <- predict(ridge_model_2,newx = x[-train,])
test_RMSE_ridge = sqrt(mean((y[-train] - fitt_value_ridge)^2))
test_RMSE_ridge


################################################################################
############################### Lasso Regression
################################################################################

lambda_grid <- 10^seq(-3,1,length = 100);


lasso_model_1 <- glmnet(x[train , ], y[train], alpha = 1, 
                        lambda = NULL, 
                        standardize = TRUE)
dim(coef(lasso_model_1))

#Esempi di valori di lambda
#Seleziono lambda 5 (grande) e calcolo l1 norm per lambda 5 (piccola)
lasso_model_1$lambda[5]
coef(lasso_model_1)[, 5]
sqrt(sum(coef(lasso_model_1)[-1,5]^2)) ##l1 norm (escludendo intercetta)

#Seleziono lambda 95(piccolo) e calcolo l2 norm per lambda 95 (grande)
lasso_model_1$lambda[95]
coef(lasso_model_1)[, 95]
sqrt(sum(coef(lasso_model_1)[-1,95]^2)) #l1 norm (escludendo intercetta)

### Andamento dei coefficienti al variare di lambda e l1 norm ###
plot(lasso_model_1, xvar = "lambda",xlab="Log(λ)")
plot(lasso_model_1, xvar = "norm",xlab="l1 norm") 

####### Choosing the best lambda #######
cv_lasso_out <- cv.glmnet(x[train , ], y[train], alpha = 1,
                          lambda = NULL,
                          nfolds = 10)

plot(cv_lasso_out)
bestlam_lasso <- cv_lasso_out$lambda.min
bestlam_lasso #0.0001 

### Final model e Test RMSE ###
lasso_model_2 <- glmnet(x[train , ], y[train], alpha = 1, 
                        lambda = bestlam_lasso, 
                        standardize = TRUE)

#Beta del modello trovato per il miglior lambda
coef(lasso_model_2)

fitt_value_lasso <- predict(lasso_model_2,newx = x[-train,])
test_RMSE_lasso = sqrt(mean((y[-train] - fitt_value_lasso)^2))
test_RMSE_lasso

#Risultati molto simili alla regressione ridge e lineare normale, siccome
#il modello non penalizza i coefficienti grandi (necessari per spiegare il
#prezzo)

################################################################################
############################### Polynomials functions + K-fold Cross Validation; k = 10
################################################################################
library(boot)

kfold_RMSE <- rep (0 , 8)
k_fit_i <- vector("list", 8)

for(i in 1:8) {
  k_fit_i[[i]] <- glm(price ~ poly(length,i) + poly(width,i) + 
               poly(depth,i) + poly(table,i) + poly(depth_percentage,i)
               +poly(carat,i) + color + cut + clarity
               ,data = Diamonds ) #creo il modello di ordine i
  kfold_RMSE[ i ] <- sqrt(cv.glm( Diamonds , k_fit_i[[i]] , K = 10)$delta[1]) #prendo il test RMSE per quel modello
}

summary(k_fit_i[[1]])
kfold_RMSE

plot(1:8,kfold_RMSE,type = "b",col = "blue",
     ylab = "CV error",
     xlab = "Flexibility (poly degree)",
     main = "Test error estimation")

best_RMSE_poly <- kfold_RMSE[8]

#Si nota come ci sia un netto miglioramento passando dalla funzione lineare 
#quadratica, per le successive il miglioramento è costante ma ridotto fino al 
#polinomio di grado 5 da li in poi il comportamento è imprevedibile

#ANOVA Test
anova(k_fit_i[[1]], k_fit_i[[2]], k_fit_i[[3]], k_fit_i[[4]], 
      k_fit_i[[5]],k_fit_i[[6]],k_fit_i[[7]],k_fit_i[[8]],test = "F")
#P value sempre significativo ma però a scendere diminuisce l'importanza


################################################################################
############################### GAM
################################################################################

library(gam)

## carat -> smooth spline
## cut -> step function
## color -> step function  
## clarity -> step function 
## depth_percentage -> smooth spline
## table -> smoothing spline 
## lenght -> smooth spline 
## width -> smooth spline 
## depth -> smooth spline
## price -> output variable

####### GAM train and test #######
gam_model_1 <- gam(price~ s(carat,4) + cut + color + clarity +
                     s(depth_percentage,5) + s(table,5) + s(length,4) + 
                     s(width,4) + s(depth,4),data=Diamonds[train, ])


par(mfrow = c(1,1))
plot(gam_model_1,se=TRUE)
summary(gam_model_1)
#The “Anova for Parametric Effects” dimostra la significariva dei coefficienti
#p-value piccolo = variabile significativa.
#The “Anova for Nonparametric Effects” dimostra la se la relazione potrebbe
#esere lineare o no, p-valkue piccolo= relazione non lineare .

##### Analisi dei residui #####
gam_pred_value <- predict(gam_model_1,newdata = Diamonds[-train,])
gam_model_1_residuals = Diamonds$price[-train] - gam_pred_value
#Test Predicted values vs Residuals
plot(gam_pred_value,gam_model_1_residuals,xlab = "Predicted values",
ylab = "Residuals",
main = "Predicted values vs Residuals",
cex = 1, col = "black")
abline(a=0,b=0,lwd=1.5,col="red")
#Residuals
plot(gam_model_1_residuals)
#Test RMSE
gam_model_1_RMSE = sqrt(mean((Diamonds$price[-train] - gam_pred_value)^2))
gam_model_1_RMSE #Miglioramento significativo

################################################################################
############################### Regression Trees
################################################################################
library(tree)

####### Tre on train dataset ######
tree_model_1 <- tree(price ~ ., data = Diamonds, subset = train)
summary(tree_model_1)
plot(tree_model_1)
text(tree_model_1 , pretty = 0)

tree_model_1
##If we just type the name of the tree object, R prints output corresponding
#to each branch of the tree. R displays the split criterion ,
#the number of observations in that branch, the deviance, the overall prediction
#for the branch, and the fraction of observations in that
#branch that take on values of Yes and No. Branches that lead to terminal
#nodes are indicated using asterisk

yhat_tree_1 <- predict(tree_model_1 , newdata = Diamonds[-train,]) #predizioni
residuals_tree_1 <- Diamonds$price[-train] - yhat_tree_1
plot(yhat_tree_1 ,Diamonds$price[-train]) #Previsioni vs dati reali
tree_model_1_RMSE = sqrt(mean((residuals_tree_1)^2)) #Test RMSE
tree_model_1_RMSE

####### Pruning ######

#The function
#cv.tree() performs cross-validation in order to determine the optimal level of 
#tree complexity; cost complexity pruning is used in order to select a 
#sequence of trees for consideration.
cv_tree <- cv.tree(tree_model_1)
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
prune_model <- prune.tree(tree_model_1 , best = best)
plot(prune_model)
text(prune_model , pretty = 0)
#Oss: l'albero di pruning ha gli stessi terminal node dell'albero unpruned!!
#Quindi il miglior albero è quello senza pruning

##### Test MSE on the best sub-tree #####
yhat_prune <- predict(prune_model , newdata = Diamonds[-train , ]) #Predizioni
plot(yhat_prune, price_test) #Previsioni vs dati reali
prune_model_RMSE = sqrt(mean((yhat_prune - price_test)^2)) #Test RMSE
#Uguale a prima perchè non ho fatto pruning

################################################################################
############################### Bagging
################################################################################

library(randomForest)

#### Bagging ####
bag_model_1 <- randomForest(price ~ ., data = Diamonds , subset = train, 
                            mtry = ncol(Diamonds)-1, 
                            importance = TRUE,
                            replace=TRUE,
                            ntree=100)
bag_model_1
summary(bag_model_1)
plot(bag_model_1,main = "Error vs Number of Trees")
importance(bag_model_1)
#Questo restituirà un elenco delle variabili ordinate per importanza nel modello 

yhat_bag_1 <- predict(bag_model_1 , newdata = Diamonds[-train , ])
plot(yhat_bag_1 ,Diamonds$price[-train]) #Fiited value vs real value
plot(yhat_bag_1 ,yhat_bag_1 - Diamonds$price[-train],
     xlab = "Predicted values",
     ylab = "Residuals",
     main = "Predicted values vs Residuals",
     cex = 1, col = "black")
abline(a=0,b=0,lwd=1.5,col="red")
bag_RMSE=sqrt(mean((yhat_bag_1 - Diamonds$price[-train])^2)) #Test RMSE
bag_RMSE

################################################################################
############################### Random Forest
################################################################################

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
rf_RMSE = sqrt(mean((yhat_rf - Diamonds$price[-train])^2)) #Test RMSE
rf_RMSE

#### Confronto MSE Bagging e Random Forest ####
plot(rf_model_1,type = 'b',col="green",pch = "+",
     main = "Random Forest vs Bagging")
plot(bag_model_1,type = 'b',col="red",pch='o',add= TRUE)
legend("topright", legend = c("Random Forest", "Bagging"),
       col = c("green", "red"), pch = c("+", "o"))




################################################################################
############################### Boosting
################################################################################

library(gbm)

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
boost_RMSE_1 <- sqrt(mean((yhat_boost_1 - Diamonds$price[-train])^2))
boost_RMSE_1

plot(yhat_boost_1 ,yhat_boost_1 - Diamonds$price[-train],
     xlab = "Predicted values",
     ylab = "Residuals",
     main = "Predicted values vs Residuals",
     cex = 1, col = "black")
abline(a=0,b=0,lwd=1.5,col="red")

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
boost_RMSE_2 <- sqrt(mean((yhat_boost_2 - price_test)^2))
boost_RMSE_2
#Risultato migliore
#In media i valori predetti dal modello sono deviati di circa 379 dollari 
#rispetto ai valori osservati.

plot(yhat_boost_2 ,yhat_boost_2 - Diamonds$price[-train],
     xlab = "Predicted values",
     ylab = "Residuals",
     main = "Predicted values vs Residuals",
     cex = 1, col = "black")
abline(a=0,b=0,lwd=1.5,col="red")


################################################################################
############################### Confronto
################################################################################

models <- c("Tree","Ridge","LM 1","Lasso","LM 2",
            "BSS","GAM","Poly","Bagging","Boosting")
errors <- c(tree_model_1_RMSE,test_RMSE_ridge,lm_test_RMSE_1,test_RMSE_lasso,
            lm_test_RMSE_2,RMSE_subselection,
            gam_model_1_RMSE,best_RMSE_poly,
            bag_RMSE,boost_RMSE_2)

plot(1:length(models),errors, type = "b", col = "blue",
     ylab = "Test RMSE",
     xlab = "Models",
     main = "Test RMSE comparison",
     xaxt = "n")
axis(side = 1, at = 1:length(models), labels = models, las = 1)





