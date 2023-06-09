#--------------------------------------
#OUTPUT PLOTS FOR DATA COLLECTION SECTION
#--------------------------------------

#Plot distribution of response variable by country
p<- ggplot(df, aes(x=country2, y=fact_checks, color=country2)) + 
  geom_boxplot()+ coord_flip()+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 0),legend.position="none", 
        plot.title = element_text(hjust = 0.5, size=15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))+ 
  xlab('')+ 
  ylab('Fact-checked Statements')
  #+ggtitle('Distribution of Fact-Checks by Country')

ggsave(p, file='output/factchecks_bycountry.png',width=6, height=7) #save plot to output file



#Plot distribution of response variable with pooled data
p <- ggplot(df, aes(x=fact_checks)) + 
  geom_histogram(binwidth=1, color="darkblue", fill="lightblue")+ 
  theme(axis.text.x = element_text(angle = 0),legend.position="none", 
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))+ 
  xlab('Fact-checked statements')+ 
  ylab('Frequency')+
  scale_x_continuous(breaks=seq(0,10,1))+
  geom_vline(aes(xintercept=mean(fact_checks)),
             color="blue", linetype="dashed", size=1)
  #+ggtitle('Fact-Checks Distribution')


ggsave(p, file='output/factchecks_pooled.png',width=6, height=4) #save plot to output file


#--------------------------------------
#OUTPUT TABLES FOR DATA COLLECTION SECTION
#--------------------------------------


#Summary of sociodemographic characteristics of the surveyed participants by country
#-----------------------------------------------
tbl1 <- df %>% group_by(country2) %>%  #create table with gender statistic
  dplyr::reframe(NWoman = count(woman),
                 N= count(country2)) %>%
  filter(NWoman$x==1) %>%
  mutate(Women=round(NWoman$freq*100/N$freq,1)) %>%
  dplyr::select(-c(2)) 

#create auxiliary df to calculate new variables
aux <- df
aux <- dummy_cols(aux, select_columns = c('AgeCat', 'college_grad'))  #create dummycols for the categorical variables
colnames(aux)[33:37] <- c("Age_25_74", "Age_18_24", "Age_75_older", 
                          "No_College", "College_Grad")

tbl2 <- aux %>% group_by(country2) %>%  #create table with education and age statistic
  dplyr::reframe(NColl=count(College_Grad),
                 NAge1=count(Age_18_24),
                 NAge2=count(Age_25_74),
                 NAge3=count(Age_75_older),
                 N= count(country2))  %>%
  filter(NColl$x==1) %>%
  mutate(College_Grad=round(NColl$freq*100/N$freq,1),
         Age_18_24=round(NAge1$freq*100/N$freq,1),
         Age_25_74=round(NAge2$freq*100/N$freq,1),
         Age_75_Older= round(NAge3$freq*100/N$freq,1)) %>%
  dplyr::select(-c(2:6)) 

tbl <- left_join(tbl1,tbl2) #join tables

tbl$Ncountry <- tbl$N$freq #create new column to delete this one with two components
tbl <- dplyr::select(tbl, -c('N'))

colnames(tbl) <- c('Country',  'Women (%)', 'College Graduates (%)', #rename cols
                   'Age 18-24 (%)', 'Age 25-74 (%)', 'Age 75 or Older (%)', 'N')


print(xtable(tbl,
             digits = c( 0, 0, 0, 0, 0, 0,0,0 ),
             label='sociodemsum::country',
             caption = 'Summary of sociodemographic characteristics of the surveyed participants by country'), 
      digits= 0,
      include.rownames=FALSE,
      file = "output/demographics_table.tex")


#main variables
#--------------------------------------------------
df$anxiety <- as.numeric(df$anxiety)
tbl <- df %>% group_by(country2) %>% 
  dplyr::summarise(mean_fc= mean(fact_checks),
                   sd_fc=sd(fact_checks),
                   quant_25_fc=quantile(fact_checks, probs=c(0.25)),
                   quant_75_fc=quantile(fact_checks, probs=c(0.75)),
                   mean_anx = mean(anxiety),
                   sd_anx=sd(anxiety),
                   quant_25_anx=quantile(anxiety, probs=c(0.25)),
                   quant_75_anx=quantile(anxiety, probs=c(0.75)),
                   mean_imp = mean(importance_scale),
                   sd_imp=sd(importance_scale),
                   quant_25_imp=quantile(importance_scale, probs=c(0.25)),
                   quant_75_imp=quantile(importance_scale, probs=c(0.75))
                   #,mean_aff = mean(imp_others),
                   #sd_aff=sd(imp_others),
                   #quant_25_aff=quantile(imp_others, probs=c(0.25)),
                   #quant_75_aff=quantile(imp_others, probs=c(0.75))
  )  

var <- c('Fact-Checks', 'Anxiety', 'Importance') #, 'Affect')
addtorow <- list()
addtorow$pos <- list(0,0)
addtorow$command <- addtorow$command <- c(paste0(paste0('& \\multicolumn{4}{c}{', var, '}', collapse=''), '\\\\'),
                                          paste("&", paste(replicate(length(var), "Mean & Sd. & Q1 & Q3"), collapse = ' & '),'\\\\')
)


print(xtable(tbl,
             digits = c(0,0,rep(c(1,1,0,0), length(var))),
             label='sumstats::vars',
             caption = 'Summary statistics of main variables by country'),
      add.to.row = addtorow, include.colnames = FALSE,
      include.rownames = FALSE,
      file = "output/mainvars_table.tex")


rm(aux, tbl, tbl1, tbl2, p, addtorow)