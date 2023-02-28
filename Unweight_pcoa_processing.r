library('ggplot2')
library('stats')
library('tidyr')
library('dplyr')
library('readr')

#adding timepoint and treatment group columns

pca_unweight_data <- read.csv('unweighted.csv') %>%
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

pca_unweight_data


#distance calculations
distPCA_matrix <- as.dist(pca_unweight_data[,c(2:126)])

#PCOA calculation
my_unweight_pcoa <- cmdscale(distPCA_matrix, eig=TRUE) 

#For calculating variance explained
Eigenvalues <- eigenvals(my_unweight_pcoa)
Variance <- Eigenvalues[1:60] / sum(Eigenvalues[1:60]) 
Variance1 <- 100 * signif(Variance[1])
Variance2 <- 100 * signif(Variance[2])

variances <- data.frame(matrix(ncol=2, nrow=0))
variances[1,] <- c(Variance1, Variance2)
write.csv(variances, 'unweight_variances.csv')


my_unweight_pcoa$group <- pca_unweight_data$group
my_unweight_pcoa$day <- pca_unweight_data$day

write.csv(my_unweight_pcoa, file = 'unweight_pcoa.csv')

