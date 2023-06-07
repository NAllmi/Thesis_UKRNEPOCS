
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
             'anxiety', 'imp_others')]


colnames(aux)<- c('Importance Global', "Importance National",
                  "Importance Individual", "Anxiety", 'Importance Others')
#check correlation between variables
corrplot(cor(aux))

#check eigenvalues
eig <- eigen(cor(aux))
print(eig$values)

#redefine dataframe
aux<- data[,c('imp_global','imp_national','imp_indiv',
              'anxiety')]


colnames(aux)<- c('Importance Global', "Importance National",
                  "Importance Individual", "Anxiety")


#select number of factors
eig <- eigen(cor(aux))
print(eig$values)

png(file="output/efa_factors.png",
    width=400, height=250)
plot(eig$values, type='b', ylab='Eigenvalues', xlab='Factor')
dev.off()

#factor analysis

concern.fa <- fa(aux, nfactors = 2, rotate = 'none', fm='gls')

concern.fa


#with rotation


concern.fa <- fa(aux, nfactors = 2, rotate = 'oblimin', fm='gls')


concern.fa



concern.fa <- fa(aux, nfactors = 2, rotate = 'promax', fm='gls')


concern.fa


#fa.diagram(concern.fa$loadings)


p <- pheatmap(concern.fa$loadings)
ggsave(p, file='output/efa_result.png',width=6, height=4) #save plot to output file
