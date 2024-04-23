#install.packages("NHANES")
library(NHANES)
library(RcmdrMisc)
#summary(NHANES)
#help("NHANES")
# Seleccionar las variables de interés y filtrar según las condiciones necesarias
DataGroupF <- data.frame(Poverty = NHANES$Poverty, 
                         Depressed = NHANES$Depressed,
                         BMI = NHANES$BMI,
                         Marij = NHANES$Marijuana)

# Filtrar los datos según las condiciones deseadas (solo mayores de 18 años)
DataGroupF <- DataGroupF[complete.cases(DataGroupF) & NHANES$Age >= 18, ]

# Definir la muestra
set.seed(12345676)  # Fijar la semilla para reproducibilidad
DataGroupF_sample <- DataGroupF[sample(nrow(DataGroupF), 1000), ]

# Verificar que la muestra tenga el tamaño deseado
nrow(DataGroupF_sample)

summary(DataGroupF)

## ------------- Exercise 1
X1 = DataGroupF_sample$Poverty
summary(X1)

Povmean = mean(X1)
Povsd = sd(X1)
Povsd
dens = density(X1)

par(mfrow = c(1, 2))
hist(X1, breaks=15, probability=TRUE)
t = seq(min(X1),max(X1),length.out = 1000)
lines(t,dnorm(t,mean=Povmean,sd=Povsd),col='blue',lwd=3,lty=4)
lines(t,dgamma(t,rate=Povmean/Povsd,shape=Povmean^2/Povsd),col='darkgreen',lwd=3,lty=2)
lines(dens,col='red',lwd=3,lty=1)
legend('top',legend=c('Kernel','Normal','Gamma'),lty=c(1,4,2),lwd = 3,col=c('red','blue','darkgreen'))
rug(X1)  #adds ticks  at the values in the argument
Boxplot(X1, main = 'Boxplot of X1')

library(moments)
skewness(X1)
kurtosis(X1)
## ------------
X2 = DataGroupF_sample$BMI
summary(X2)

Povmean = mean(X2)
Povsd = sd(X2)
Povsd
skewness(X2)
dens = density(X2)

par(mfrow = c(1, 2))
hist(X2, breaks=15, probability=TRUE)
#t = seq(min(X2),max(X2),length.out = 1000)
#lines(t,dnorm(t,mean=Povmean,sd=Povsd),col='blue')
#lines(t,dgamma(t,rate=Povmean/Povsd,shape=Povmean^2/Povsd),col='darkgreen')
#lines(dens,col='red')
rug(X2)  #adds ticks  at the values in the argument
boxplot(X2, main = 'Boxplot of X2')
X2stats = boxplot.stats(X2)
length(X2stats$out) # Number of outlier points

## ------------
F1 = DataGroupF_sample$Depressed
summary(F1)
prop.table(table(F1))*100

F2 = DataGroupF_sample$Marij
summary(F2)
prop.table(table(F2))*100

par(mfrow = c(1, 2))
#barplot(table(F1), col = c("red", "blue","darkgreen"))
piechart(F1, col = 2:4, main = "Depressed-F1",clockwise=TRUE)
piechart(F2, col = 5:6, main = "Marijuana-F2",clockwise=TRUE)

## -------------
#Exercise 2
data <- data.frame(X1, F1)

# summary of the data frame
summary(data)

# now we compare some valours of x1 in function of f1
# mean and sd
mean = tapply (X1 ,F1 , FUN = mean , na.rm = TRUE )
sd = tapply (X1 ,F1 , FUN = sd , na.rm = TRUE )
# quantiles 
q0 = tapply (X1 ,F1 , FUN = function ( X1 ) quantile (X1 ,0 , na.rm = TRUE ) )
q25 = tapply (X1 ,F1 , FUN = function ( X1 ) quantile (X1 ,.25 , na.rm = TRUE ) )
q50 = tapply (X1 ,F1 , FUN = function ( X1 ) quantile (X1 ,.5 , na.rm = TRUE ) )
q75 = tapply (X1 ,F1 , FUN = function ( X1 ) quantile (X1 ,.75 , na.rm = TRUE ) )
q100 = tapply (X1 ,F1 , FUN = function ( X1 ) quantile (X1 ,1 , na.rm = TRUE ) )
sk = tapply (X1 ,F1 , FUN = function ( X1 ) skewness (X1) )
# We create a matrix with the results 
results <- rbind ( mean , sd , q0 , q25 , q50 , q75 , q100, sk )
print( results)


# Boxplot 
par(mfrow = c(1,1))
boxplot( X1 ~ F1 , xlab = " F1 " , ylab = " X1 ")
abline( h = median ( X1) , lty=2, lwd = 2)
# Histograms 
Hist ( X1 , groups = F1, scale = 'density')



#Exercise 3

#Barplots
library(RcmdrMisc)
Barplot(F1,F2, scale = "percent", conditional = TRUE, style = "parallel")

Barplot(F2,F1, scale = "percent", conditional = TRUE, style = "parallel")

#table with percentages
Tabla <- xtabs ( ~ F1 + F2 )
addmargins(Tabla)

prop.table(Tabla)*100
#conditional
prop.table(Tabla, margin=1)*100
prop.table(Tabla, margin=2)*100
#chi-square
test <- chisq.test(Tabla)
print(test)

# Exercise 4
cor(X1,X2)

lm1<-lm(X2~X1)
summary(lm1)

plot(X1,X2, xlab="Poverty", ylab="BMI",  pch=16, cex=0.7)
abline(a=lm1$coefficients[1], b=lm1$coefficients[2],col='red',lwd=3)


#Mental health

X1_none=X1[DataGroupF_sample$Depressed=='None']
X2_none = X2[DataGroupF_sample$Depressed=='None']

X1_sev=X1[DataGroupF_sample$Depressed=='Several']
X2_sev = X2[DataGroupF_sample$Depressed=='Several']

X1_most=X1[DataGroupF_sample$Depressed=='Most']
X2_most = X2[DataGroupF_sample$Depressed=='Most']

lm2<-lm(X2_none~X1_none)
summary(lm2)

lm3<-lm(X2_sev~X1_sev)
summary(lm3)

lm4<-lm(X2_most~X1_most)
summary(lm4)

cor(X1_none,X2_none)
cor(X1_sev,X2_sev)
cor(X1_most,X2_most)

plot(X1_none,X2_none, xlab="Poverty", ylab="BMI",  pch=16, cex=0.7,col='black')
abline(a=lm2$coefficients[1], b=lm2$coefficients[2],col='black',lwd=3)
points(X1_sev,X2_sev,col='blue',  pch=16, cex=0.7)
abline(a=lm3$coefficients[1], b=lm3$coefficients[2],col='blue',lwd=3)
points(X1_most,X2_most,col='red',  pch=16, cex=0.7)
abline(a=lm4$coefficients[1], b=lm4$coefficients[2],col='red',lwd=3)
legend(3.5, 80,c("None", "Several","Most"),lty=1,lwd = 3,col=c('black','blue','red'))
