# Uncomment if packages not installed
## install.packages("psych")
## install.packages("dplyr")
## install.packages("ggpubr")
## install.packages("cowplot")
## install.packages("lavaan")
## install.packages("plotly")
## install.packages("tidySEM")



# Load data
#setwd('D:\\Yaxin\\HKBU BM\\Courses\\Sem 2\\ECON7860 Big Data Analytics for Business (S11)\\Group Project\\HR Analytics\\working')
rawData <- read.csv2("HR_comma_sep.csv", sep = ',')



# Check missing values
TRUE %in% is.na(rawData)



# Have a glance of the data
colnames(rawData)
str(rawData)
summary(rawData)
table(rawData$Work_accident)
table(rawData$promotion_last_5years)
table(rawData$sales)
table(rawData$salary)
table(rawData$left)



# Transform "satisfaction_level" and "last_evaluation" to numeric
rawData$satisfaction_level <- as.numeric(rawData$satisfaction_level)
rawData$last_evaluation <- as.numeric(rawData$last_evaluation)
summary(rawData)



# Separate numeric variables and target variable
X <- rawData[ , c(1, 2, 3, 4, 5, 7)]
tag <- colnames(X)
y <- rawData$left



# Check for outliers (of numeric variables) with boxplots
library(ggpubr)
library("cowplot")

bxp1 <- ggboxplot(X[, 1], xlab = tag[1], ylab = tag[1], fill = '#999999')
bxp2 <- ggboxplot(X[, 2], xlab = tag[2], ylab = tag[2], fill = '#999999')
bxp3 <- ggboxplot(X[, 3], xlab = tag[3], ylab = tag[3], fill = '#999999')
bxp4 <- ggboxplot(X[, 4], xlab = tag[4], ylab = tag[4], fill = '#999999')
bxp5 <- ggboxplot(X[, 5], xlab = tag[5], ylab = tag[5], fill = '#999999')

plot_grid(bxp1, bxp2, bxp3, bxp4, bxp5 + rremove("x.text"), 
          labels = tag, ncol = 3, nrow = 2, label_size = 8)



# Identify outliers for "time_spend_company"
out <- boxplot.stats(X[ , 5])$out
table(out)
length(out)



# Identify the subjects corresponding to the "extreme" values
out_ind <- which(X[ , 5] %in% out)



# Examine if the outlier dataset is a different population
X1 <- X[out_ind, ]; X2 <- X[-out_ind, ]
summary(X1)
summary(X2)

## Test if mean(left) are significantly lower for X1 than for X2
alpha <- 0.05
t.test(X1$left, X2$left, alternative = 'less')

## Test if distributions of features are different between X1 and X2
ind <- c()
### satisfaction_level
if (ks.test(X1$satisfaction_level, X2$satisfaction_level)$p.value < alpha / 2) {
  ind <- rbind(ind, TRUE);
  print("The distribution of 'satisfaction_level' is statistically different between X1 and X2 at 0.05 level of significance.")
} else {
  ind <- rbind(ind, FALSE);
  print("The distribution of 'satisfaction_level' is NOT statistically different between X1 and X2 at 0.05 level of significance.")
}

### last_evaluation
if (ks.test(X1$last_evaluation, X2$last_evaluation)$p.value < alpha / 2) {
  ind <- rbind(ind, TRUE);
  print("The distribution of 'last_evaluation' is statistically different between X1 and X2 at 0.05 level of significance.")
} else {
  ind <- rbind(ind, FALSE);
  print("The distribution of 'last_evaluation' is NOT statistically different between X1 and X2 at 0.05 level of significance.")
}

### number_project
if (ks.test(X1$number_project, X2$number_project)$p.value < alpha / 2) {
  ind <- rbind(ind, TRUE);
  print("The distribution of 'number_project' is statistically different between X1 and X2 at 0.05 level of significance.")
} else {
  ind <- rbind(ind, FALSE);
  print("The distribution of 'number_project' is NOT statistically different between X1 and X2 at 0.05 level of significance.")
}

### average_montly_hours
if (ks.test(X1$average_montly_hours, X2$average_montly_hours)$p.value < alpha / 2) {
  ind <- rbind(ind, TRUE);
  print("The distribution of 'average_montly_hours' is statistically different between X1 and X2 at 0.05 level of significance.")
} else {
  ind <- rbind(ind, FALSE);
  print("The distribution of 'average_montly_hours' is NOT statistically different between X1 and X2 at 0.05 level of significance.")
}

### time_spend_company
if (ks.test(X1$time_spend_company, X2$time_spend_company)$p.value < alpha / 2) {
  ind <- rbind(ind, TRUE);
  print("The distribution of 'time_spend_company' is statistically different between X1 and X2 at 0.05 level of significance.")
} else {
  ind <- rbind(ind, FALSE);
  print("The distribution of 'time_spend_company' is NOT statistically different between X1 and X2 at 0.05 level of significance.")
}

### left
if (ks.test(X1$left, X2$left)$p.value < alpha / 2) {
  ind <- rbind(ind, TRUE);
  print("The distribution of 'left' is statistically different between X1 and X2 at 0.05 level of significance.")
} else {
  ind <- rbind(ind, FALSE);
  print("The distribution of 'left' is NOT statistically different between X1 and X2 at 0.05 level of significance.")
}

ind

#### ks.test(X1$satisfaction_level, X2$satisfaction_level)
#### ks.test(X1$last_evaluation, X2$last_evaluation)
#### ks.test(X1$number_project, X2$number_project)
#### ks.test(X1$average_montly_hours, X2$average_montly_hours)
#### ks.test(X1$time_spend_company, X2$time_spend_company)
#### ks.test(X1$left, X2$left)


## Test if distributions of features are different between X1 and X
### ks.test(X1$satisfaction_level, X$satisfaction_level)
### ks.test(X1$last_evaluation, X$last_evaluation)
### ks.test(X1$number_project, X$number_project)
### ks.test(X1$average_montly_hours, X$average_montly_hours)
### ks.test(X1$time_spend_company, X$time_spend_company)
### ks.test(X1$left, X$left)

### All tests are significant at alpha = 0.05


## Test if distributions of features are different between X2 and X
### ks.test(X2$satisfaction_level, X$satisfaction_level)
### ks.test(X2$last_evaluation, X$last_evaluation)
### ks.test(X2$number_project, X$number_project)
### ks.test(X2$average_montly_hours, X$average_montly_hours)
### ks.test(X2$time_spend_company, X$time_spend_company)
### ks.test(X2$left, X$left)

### All tests are NOT significant at alpha = 0.05 except for "time_spend_company"



# Plot the scatter plot matrices for all variables
library(dplyr)
library(psych)

## Move target variable "left" to the end of the data frame
rawData <- rawData %>% relocate(left, .after = last_col())

pairs.panels(rawData, pch = '.')
pairs.panels(rawData[out_ind, ])
pairs.panels(rawData[-out_ind, ], pch = '.')
pairs.panels(rawData, pch = '.', ellipses = FALSE)
## pairs.panels(rawData)



# Exploratory factor (principal component, and cluster) analysis
## Standardization of numerical data
X_norm <- cbind(scale(X[ , -c(6)]), y)
X1_norm <- cbind(scale(X1[ , -c(6)]), y[out_ind])
X2_norm <- cbind(scale(X2[ , -c(6)]), y[-out_ind])


## On all observations
fa.parallel(X_norm[ , -c(6)])

fa1 <- fa(X_norm[ , -c(6)], 1)
fa1
fa.diagram(fa1)

fa2 <- fa(X_norm[ , -c(6)], 2)
fa2
fa.diagram(fa2)

pca1 <- pca(X_norm[ , -c(6)], 1)
pca1
fa.diagram(pca1)

pca2 <- pca(X_norm[ , -c(6)], 2)
pca2
fa.diagram(pca2)


### Existence of hierarchical latent factor
om <- omega(X_norm[ , -c(6)], 2, sl = FALSE)
om


### Item cluster analysis
ic <- iclust(X_norm[ , -c(6)])
ic


### Visualization
library(plotly)

#### 2-dim PCA
data_PCA <- as.data.frame(cbind(pca2$scores, y))
fig_pca <- plot_ly(data_PCA, x = ~RC1, y = ~RC2, z = ~y, type = 'scatter3d',
                   size = I(5), color = y, colors = c('gray50', 'red'))
fig_pca <- fig_pca %>% layout(
  title = 'Visualization with Numeric Variables and 2 PCs',
  scene = list(
    xaxis = list(title = "PC1"),
    yaxis = list(title = "PC2"),
    zaxis = list(title = "left")
  ))

fig_pca

#### 2-dim FA
data_FA <- as.data.frame(cbind(fa2$scores, y))
fig_fa <- plot_ly(data_FA, x = ~MR1, y = ~MR2, z = ~y, type = 'scatter3d',
                  size = I(5), color = y, colors = c('gray50', 'red'))
fig_fa <- fig_fa %>% layout(
  title = 'Visualization with Numeric Variables and 2 Factors',
  scene = list(
    xaxis = list(title = "Factor 1"),
    yaxis = list(title = "Factor 2"),
    zaxis = list(title = "left")
  ))

fig_fa


## On X1
fa.parallel(X1_norm[ , -c(6)])

pca1_X1 <- pca(X1_norm[ , -c(6)], 1)
pca1_X1
fa.diagram(pca1_X1)

pca2_X1 <- pca(X1_norm[ , -c(6)], 2)
pca2_X1
fa.diagram(pca2_X1)

### Existence of hierarchical latent factor
om_X1 <- omega(X1_norm[ , -c(6)], 2, sl = FALSE)
om_X1

### Item cluster analysis
ic_X1 <- iclust(X1_norm[ , -c(6)])
ic_X1

### Visualization
#### 2-dim PCA
y1 <- y[out_ind]
data_PCA_X1 <- as.data.frame(cbind(pca2_X1$scores, y1))
fig_pca_X1 <- plot_ly(data_PCA_X1, x = ~RC1, y = ~RC2, z = ~y1, 
                      type = 'scatter3d', size = I(8), 
                      color = y1, colors = c('gray50', 'red'))
fig_pca_X1 <- fig_pca_X1 %>% layout(
                      title = 'Visualization with Numeric Variables and 2 PCs (X1)',
                      scene = list(
                        xaxis = list(title = "PC1"),
                        yaxis = list(title = "PC2"),
                        zaxis = list(title = "left")
                      ))

fig_pca_X1


## On X2
fa.parallel(X2_norm[ , -c(6)])

fa1_X2 <- fa(X2_norm[ , -c(6)], 1)
fa1_X2
fa.diagram(fa1_X2)

fa2_X2 <- fa(X2_norm[ , -c(6)], 2)
fa2_X2
fa.diagram(fa2_X2)

pca1_X2 <- pca(X2_norm[ , -c(6)], 1)
pca1_X2
fa.diagram(pca1_X2)

pca2_X2 <- pca(X2_norm[ , -c(6)], 2)
pca2_X2
fa.diagram(pca2_X2)

### Existence of hierarchical latent factor
om_X2 <- omega(X2_norm[ , -c(6)], 2, sl = FALSE)
om_X2

### Item cluster analysis
ic_X2 <- iclust(X2_norm[ , -c(6)])
ic_X2

### Visualization
#### 2-dim PCA
y2 <- y[-out_ind]
data_PCA_X2 <- as.data.frame(cbind(pca2_X2$scores, y2))
fig_pca_X2 <- plot_ly(data_PCA_X2, x = ~RC1, y = ~RC2, z = ~y2, type = 'scatter3d',
                   size = I(5), color = y2, colors = c('gray50', 'red'))
fig_pca_X2 <- fig_pca_X2 %>% layout(
  title = 'Visualization with Numeric Variables and 2 PCs (X2)',
  scene = list(
    xaxis = list(title = "PC1"),
    yaxis = list(title = "PC2"),
    zaxis = list(title = "left")
  ))

fig_pca_X2

#### 2-dim FA
data_FA_X2 <- as.data.frame(cbind(fa2_X2$scores, y2))
fig_fa_X2 <- plot_ly(data_FA_X2, x = ~MR1, y = ~MR2, z = ~y2, type = 'scatter3d',
                  size = I(5), color = y2, colors = c('gray50', 'red'))
fig_fa_X2 <- fig_fa_X2 %>% layout(
  title = 'Visualization with Numeric Variables and 2 Factors (X2)',
  scene = list(
    xaxis = list(title = "Factor 1"),
    yaxis = list(title = "Factor 2"),
    zaxis = list(title = "left")
  ))

fig_fa_X2



# Confirmatory factor analysis
library(lavaan)

## One-factor model
m1 <- 'f1 =~  number_project + average_montly_hours + last_evaluation'
CFA1 <- cfa(m1, X_norm[ , -c(6)])
summary(CFA1, fit.measures=TRUE,standardized=TRUE)


## Two-factor model
m2 <- 'f1 =~ number_project + average_montly_hours + last_evaluation
        f2 =~ time_spend_company' 
CFA2 <- cfa(m2, X_norm[ , -c(6)]) 
summary(CFA2,fit.measures=TRUE,standardized=TRUE)


## Visualization of CFA
library(tidySEM)
graph_sem(CFA1)
graph_sem(CFA2)




#save.image("D:/Yaxin/HKBU BM/Courses/Sem 2/ECON7860 Big Data Analytics for Business (S11)/Group Project/HR Analytics/working/EDA.RData")




