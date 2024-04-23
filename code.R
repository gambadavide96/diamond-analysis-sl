
install.packages("MASS")
install.packages("ISLR2")
install.packages("data.table")    # excel
install.packages("corrplot")      # Correlations
install.packages("ggplot2")       # Pairs Plots Correlations
install.packages("GGally")
install.packages("pastecs")
install.packages("psych")
install.packages("dplyr")
install.packages("performance")
install.packages("lmtest")
################################################################################
######################################### Dataset 
################################################################################
Diamonds <- read.table("diamonds.csv", header = TRUE, sep = ",",quote = "\"", fileEncoding = "UTF-8")
Diamonds <- subset(Diamonds , select = - X)
colnames(Diamonds)[5] = "depth_percentage"
colnames(Diamonds)[8] = "length"
colnames(Diamonds)[9] = "width"
colnames(Diamonds)[10] = "depth"

# no nan colums
colSums(is.na(Diamonds))

View(Diamonds)
summary(Diamonds)

################################################################################
#########################################  Histograms
################################################################################

hist(Diamonds$carat, 40 , xlab = "Carat",  main = "Carat distribution") 

barplot(table(Diamonds$cut), xlab = "Cut", ylab = "Frequency",
        main = "Cut distribution")

barplot(table(Diamonds$color), xlab = "Color", ylab = "Frequency",
        main = "Color Distribution")

barplot(table(Diamonds$clarity), xlab = "Clarity", ylab = "Frequency",
        main = "Clarity Distribution")

hist(Diamonds$depth_percentage, 50 , xlab = "Depth Percentage",  
     main = "Depth Percentage distribution")

hist(Diamonds$table, 40 , xlab = "Table",  main = "Table distribution")

hist(Diamonds$price, 40 , xlab = "Price",  main = "Price distribution")

hist(Diamonds$length, 40 , xlab = "Length",  main = "Length distribution")

hist(Diamonds$width, 50 , xlab = "Width",  main = "Width distribution")

hist(Diamonds$depth, 50 , xlab = "Depth",  main = "Depth distribution")


#Regressione Lineare
lm <- lm(price ~ . , data = Diamonds)
summary(lm)