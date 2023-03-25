
#read data
#-------------------------------------
data <- read_dta("Data/May6Survey.dta")

data$imp_global <- data$Q2_1r1 
data$imp_national<-data$Q2_1r2 
data$imp_indiv  <- data$Q2_1r3
data$anxiety <- data$Q2_1r5
data$imp_others <- data$Q2_1r4

#subset dataframe

aux<- data[,c('imp_global','imp_national','imp_indiv',
            'imp_others', 'anxiety')]

#check correlation between variables
corrplot(cor(aux))



#select number of factors
eig <- eigen(cor(aux))
sum(eig$values >1)

plot(eig$values, type='b', ylab='Eigenvalues', xlab='Factor')


#factor analysis


concern.fa <- fa(aux, nfactors = 2, rotate = 'none', fm='gls')

concern.fa

#the null hypothesis is rejected, so we need more factors


concern.fa <- fa(aux, nfactors = 3, rotate = 'none', fm='gls')


concern.fa


#with rotation

concern.fa <- fa(aux, nfactors = 3, rotate = 'promax', fm='gls')


concern.fa


concern.fa <- fa(aux, nfactors = 3, rotate = 'oblimin', fm='gls')


concern.fa
