library('ggplot2')
library('stats')
library('tidyr')
library('dplyr')
library('readr')

#adding timepoint and treatment group columns

pca_weight_data <- read.csv('weighted.csv') %>%
  mutate(day = case_when(
    endsWith(X, '0') ~ 0,
    endsWith(X, '3') ~ 3,
    endsWith(X, '5') ~ 5,
    endsWith(X, '7') ~ 7
  )) %>%
  mutate(group = case_when(
    startsWith(X, 'HC') ~ 'control',
    startsWith(X, 'HT') ~ 'treatment',
    
  ))

pca_weight_data


#distance calculations
distPCA_matrix <- as.dist(pca_weight_data[,c(2:126)])

#PCOA calculation
my_weight_pcoa <- cmdscale(distPCA_matrix, eig=TRUE) 


#For calculating variance explained
Eigenvalues <- eigenvals(my_weight_pcoa)
Variance <- Eigenvalues[1:54] / sum(Eigenvalues[1:54]) 
Variance1 <- 100 * signif(Variance[1])
Variance2 <- 100 * signif(Variance[2])

variances <- data.frame(matrix(ncol=2, nrow=0))
variances[1,] <- c(Variance1, Variance2)
write.csv(variances, 'weight_variances.csv')


my_weight_pcoa$group <- pca_weight_data$group
my_weight_pcoa$day <- pca_weight_data$day

write.csv(my_weight_pcoa, file = 'weight_pcoa.csv')


