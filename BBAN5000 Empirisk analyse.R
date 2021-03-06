suppressMessages(library(ggcorrplot))
library(ggplot2)  
library(rsample)   
library(knitr)
library(corrplot)
library(olsrr)
suppressMessages(library(tidyverse)) 
suppressMessages(library(caret)) 
suppressMessages(library(ggplot2))
suppressMessages(library(glmnet)) 
suppressMessages(library(plotly))
suppressMessages(library(reshape2))
suppressMessages(library(pROC))
suppressMessages(library(GGally))


library(jsonlite)
library(httr)
library(foreign)
library(rjstat)
library(plyr)
library(dplyr)
library(tidyverse)
library(VIM)
library(glmnet)
library(caret)

setwd("C:/Users/solme/OneDrive/Master")

options(max.print=100000000)

################################################################################
#LINE�R REGRESJON
#Predikere andel frafalt per studium per instiutt
#Importerer det endelige datasettet 
DF <- read.csv('Endelig datasett 333.csv', sep = ',', fileEncoding = 'UTF-8')
DF <- DF[,-c(1)]

#Pynte p� rekkef�lgen (og fjerner)
DF <- DF[,c(1:8, 14, 16, 17:27),]

summary(DF)

#Fjerner und�vendige kolonner og pynter p� rekkef�lgen 
DF <- DF[,-c(1,3,6)]
DF <- DF[,c(7, 1:6, 8:18)]
DF <- subset(DF[-c(2,3,4,8)])

#Beskrivende statistikk  
library('psych')
summary(DF)
describe(DF)



####kNN - Missing data imputation 
#Har pr�vd k=3, k=5, k=10 og k=sqrt(6055)
#k = 10 predikerer flest utveksling != 0 (sm� forskjeller) velger derfor k=10

#Setter startkull og kvinneandel som skal avgj�re om observasjonene ligner hverandre

DF_kNN <- kNN(DF, variable = c('Andel.ut.NORSK','Andel.ut.UTENL','Andel.UTENL','Antallstudperans',
                               'Karaktersnitt.H','Karaktersnitt.vgs',
                               'Publiseringspoeng','Snittalder','Strykandel'),
              k = 10, dist_var = c('Startkull','Kvinneandel'))


#Fjerner un�dvendige kolonner 
DF_kNN <- DF_kNN[,-c(15:25)]

#Bestemmer oss for � ikke inkludere antall doktorgrader og utveksling
DF_kNN <- DF_kNN[,-c(7, 12:13)]

describe(DF_kNN)


#Fjerner rare og ekstreme verdier
hist(DF_kNN$Frafallsandel)
hist(DF_kNN$Startkull)
hist(DF_kNN$Antallstudperans)
hist(DF_kNN$Snittalder)
hist(DF_kNN$Andel.UTENL)
hist(DF_kNN$Andel.ut.NORSK)
hist(DF_kNN$Andel.ut.UTENL)

#Frafallsandel 
DF_kNN <- DF_kNN[!(DF_kNN$Frafallsandel >= 0.8 & DF_kNN$Startkull>20),]
DF_kNN <- DF_kNN[!(DF_kNN$Frafallsandel >= 0.5 & DF_kNN$Startkull>100),]

#andre
DF_kNN <- DF_kNN[!(DF_kNN$Startkull > 500),]

DF_kNN <- DF_kNN[!(DF_kNN$Antallstudperans > 30),]

DF_kNN <- DF_kNN[!(DF_kNN$Snittalder > 35),]

DF_kNN <- DF_kNN[!(DF_kNN$Andel.UTENL > 0.6),]


################################################################################
#Korrelasjonsmatrise 
library(ggcorrplot)
r <- cor(DF_kNN, use="complete.obs")
round(r,2)
ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

korr <- cor(DF_kNN)
round(korr, 2)

library(reshape2)
melted_cormat <- melt(korr)
head(melted_cormat)


ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearsons \nkorrelasjonskoeffisient") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

cor.test(DF_kNN$Strykandel,DF_kNN$Karaktersnitt.H)

################################################################################
#Kj�rer line�r regresjon med alle variabler
original.lin <- lm(Frafallsandel ~ ., data = DF_kNN)
summary(original.lin) 

plot(original.lin, 1)

durbinWatsonTest(original.lin)

#Standardiserte betakoeffisienter 
library(lm.beta)
beta.lin <- lm.beta(original.lin)
summary(beta.lin)

###############################
#Residualanalyse 
library(lmtest)

#VIF-indexer for multikollinearitet 
library(car)
vif <- vif(original.lin)
vif

#Tester for heteroskedastisitet
BP <- bptest(original.lin)
BP

#Standardiserte residualer 
standard_res <- rstandard(original.lin)

final_data <- cbind(DF_kNN, standard_res)

#Scatterplot to visualize the values for the predictor variable vs. the standardized residuals:
plot(DF_kNN$Frafallsandel, standard_res, ylab='Standardiserte residualer', xlab='Frafallsandel') 
abline(3, 0, col = 'red', lty='dashed')

#Q_Q plot 
ols_plot_resid_qq(original.lin)

qqnorm(standard_res, ylab="Sample Quantiles", xlab="Theoretical Quantiles", main="Normal Q-Q Plot") 
qqline(standard_res, col = 'red')

#Residualplott
ols_plot_resid_fit(original.lin)

res <- resid(original.lin)

plot(fitted(original.lin), res, xlab = 'Fitted value', ylab='Residual')
abline(0,0, col = 'red')

plot(fitted(original.lin), standard_res, xlab = 'Fitted value', ylab='Residual')
abline(0,0, col = 'red')

ggplot(original.lin, aes(.fitted, .resid)) + geom_point(pch = 21) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, col = "red", size = 0.5, method = "loess") +
  labs(x = "Fitted values", y = "Residuals",
       title = "Fitted values vs. residuals",
       subtitle = deparse(original.lin$call))

#Robusthetssjekk
library(sandwich)

lmtest::coeftest(original.lin, vcov. = sandwich::vcovHC(original.lin, type = 'HC1'))

DF_kNN <- DF_kNN[,-c(8)]

################################################################################
#LASSO
lin_x <- model.matrix(Frafallsandel~.,DF_kNN)[,-c(1)]
lin_y <- DF_kNN$Frafallsandel

lasso.lin <- glmnet(lin_x,lin_y, alpha = 1)
plot(lasso.lin, xvar= 'lambda',label = TRUE, main = ' ')

#library(plotmo) # for plot_glmnet
#plot_glmnet(lasso.lin, label = 5)


#Bruker kryssvalidering for � finne lambda.min gir lambda med minst minimum mean cross-validated erro
lasso.lin.cv = cv.glmnet(lin_x,lin_y, alpha = 1)

lbs_fun <- function(fit, offset_x=1, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])+ offset_x
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
}


plot(lasso.lin.cv)
abline(v = log(lasso.lin.cv$lambda.min), col = 'red', lty='dashed')
#Bl� = korresponderende lambda for 1. standard feil
abline(v = log(lasso.lin.cv$lambda.1se), col = 'blue', lty='dashed')

bestlambda <- lasso.lin.cv$lambda.min

#Visualiserer: 
plot(lasso.lin, xvar= 'lambda', main = ' ')
#R�d = korresponderende lambda for minste MSE 
abline(v = log(lasso.lin.cv$lambda.min), col = 'red', lty='dashed')
#Bl� = korresponderende lambda for 1. standard feil
abline(v = log(lasso.lin.cv$lambda.1se), col = 'blue', lty='dashed')
#Den beste modellen vil ligge mellom den r�de og den bl� linjen 
lbs_fun(lasso.lin)

#Verkt�y for � zoome 
#library("zoom") 
#zm()

#LASSO-koeffisienter
coef(lasso.lin, c(lasso.lin.cv$lambda.min, lasso.lin.cv$lambda.1se))

#LASSO Influential Features 
coef(lasso.lin.cv, s = 'lambda.min') %>%
  tidy() %>%
  filter(row != '(Intercept)') %>%
  top_n(45, wt = abs(value))%>%
  ggplot(aes(value, reorder(row,value), color = value > 0)) +
  geom_point(show.legend = FALSE) + 
  ggtitle(' ') +
  xlab('Coefficient') + 
  ylab(NULL)

coef(lasso.lin.cv, s = 'lambda.1se') %>%
  tidy() %>%
  filter(row != '(Intercept)') %>%
  top_n(45, wt = abs(value))%>%
  ggplot(aes(value, reorder(row,value), color = value > 0)) +
  geom_point(show.legend = FALSE) + 
  ggtitle(' ') +
  xlab('Coefficient') + 
  ylab(NULL)


################################################################################
#LM med kun fire variabler 
lin.4 <- lm(Frafallsandel ~ �rstall + Snittalder  + Strykandel + Andel.UTENL, data = DF_kNN)
#summary(original.lin) 

#Standardiserte betakoeffisienter 
library(lm.beta)
beta.lin.4 <- lm.beta(lin.4)
summary(beta.lin.4)

