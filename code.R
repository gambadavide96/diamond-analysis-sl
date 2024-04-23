
###### clearing environment 
rm(list = ls())
graphics.off()

################################################################################
######################################### Setting Dataset 
################################################################################
Diamonds <- read.table("diamonds.csv", header = TRUE, 
                       sep = ",",quote = "\"", fileEncoding = "UTF-8")
Diamonds <- subset(Diamonds , select = - X)
colnames(Diamonds)[5] = "depth_percentage"
colnames(Diamonds)[8] = "length"
colnames(Diamonds)[9] = "width"
colnames(Diamonds)[10] = "depth"

Diamonds$cut <- factor(Diamonds$cut)
Diamonds$color <- factor(Diamonds$color)
Diamonds$clarity <- factor(Diamonds$clarity)

# no nan colums
colSums(is.na(Diamonds))

View(Diamonds)
summary(Diamonds)

################################################################################
#########################################  Histograms
################################################################################

par(mfrow = c(2,2))

hist(Diamonds$carat, 40 , 
     xlab = "Carat",  main = "Carat distribution") 

barplot(table(Diamonds$cut), xlab = "Cut",
      ylab = "Frequency", main = "Cut distribution")

barplot(table(Diamonds$color), xlab = "Color", 
      ylab = "Frequency", main = "Color Distribution")

barplot(table(Diamonds$clarity), xlab = "Clarity",
      ylab = "Frequency",main = "Clarity Distribution")

hist(Diamonds$depth_percentage, 50 , xlab = "Depth Percentage", 
      main = "Depth Percentage distribution")

hist(Diamonds$table, 40 , xlab = "Table",  main = "Table distribution")

hist(Diamonds$price, 40 , xlab = "Price",  main = "Price distribution")

hist(Diamonds$length, 40 , xlab = "Length",  main = "Length distribution")

hist(Diamonds$width, 50 , xlab = "Width",  main = "Width distribution")

hist(Diamonds$depth, 50 , xlab = "Depth",  main = "Depth distribution")

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







#Regressione Lineare
lm <- lm(price ~ . , data = Diamonds)
summary(lm)