#----------------
#   MODEL FIT
#----------------


#CHECK DIAGNOSTICS FOR A COUNTRY MODEL
#-----------------------------------

#country <- 'Denmark'
#  
#aux <- df[df$country2 == country,]
#  
#nb <- glm.nb(fact_checks ~ AgeCat + woman + college_grad +
#                 pol_cynisism_scale + trust_gov + Political_Interest + 
#                 political_orient + political_orient_sq +
#                 satisfaction_dem + 
#                 media_trust_scale + sp_media_lit_scale +
#                 TV + newspapers + social_media +
#                 radio +      
#                 aggregators+  messaging +  
#                 left_alt + right_alt +
#                 anxiety + 
#                 importance_scale
#               #+ important_others
#               , data = aux)
#summary(nb)
#  
#  
#simulationOutput <- simulateResiduals(fittedModel = nb, plot = FALSE) #The function creates scaled residuals by simulating from the fitted model.
#  
#print(testZeroInflation(nb, plot=FALSE))
#  
#print(check_collinearity(nb))
#  
#plot(simulationOutput, quantreg = TRUE)
#  
#  
#plot(simulationOutput)
#plotResiduals(simulationOutput, aux$anxiety)
#plotResiduals(simulationOutput, aux$importance_scale)
##plotResiduals(simulationOutput, aux$important_others)
#  


#PRINT STATISTICS FOR EACH COUNTRY MODEL
#--------------------------------------------

country_list <- unique(df$country2)


RMSE_nb  <-  c()
R_sq_nb  <-  c()
AIC_nb   <-  c()
RMSE_ols <-  c()
R_sq_ols <-  c()
AIC_ols  <- c()
anxiety_coef <- c()
anxiety_pval <- c()
imp_sc_coef <- c()
imp_sc_pval <- c()
imp_others_coef <- c()
imp_others_pval <- c()

for (i in country_list){
  
  aux <- df[df$country2 == i,]
  
  nb <- glm.nb(fact_checks ~ AgeCat + woman + college_grad +
                 pol_cynisism_scale + trust_gov + Political_Interest + 
                 political_orient + political_orient_sq +
                 satisfaction_dem + 
                 media_trust_scale + sp_media_lit_scale +
                 TV + newspapers + social_media +
                 radio +      
                 aggregators+  messaging +  
                 left_alt + right_alt +
                 anxiety + 
                 importance_scale
                 #+ important_others
               , data = aux)
  
  anxiety_coef    <- append(anxiety_coef, summary(nb)$coefficients[22,1])
  anxiety_pval    <- append(anxiety_pval, summary(nb)$coefficients[22,4])
  imp_sc_coef     <- append(imp_sc_coef,  summary(nb)$coefficients[23,1])
  imp_sc_pval     <- append(imp_sc_pval,  summary(nb)$coefficients[23,4])
  #imp_others_coef <- append(imp_others_coef, summary(nb)$coefficients[24,1])
  #imp_others_pval <- append(imp_others_pval, summary(nb)$coefficients[24,4])

  RMSE_nb <- append(RMSE_nb, sqrt(mean((aux$fact_checks - nb$fitted.values)^2)))
  
  R_sq_nb <- append(R_sq_nb,r.squaredGLMM(nb, conditional = TRUE)[6]) #goodness of fit
  
  AIC_nb <- append(AIC_nb, nb$aic)
  
  ols <- lm(fact_checks ~ AgeCat + woman + college_grad +
              pol_cynisism_scale + trust_gov + Political_Interest + 
              political_orient + political_orient_sq +
              satisfaction_dem + 
              media_trust_scale + sp_media_lit_scale +
              TV + newspapers + social_media +
              radio +      
              aggregators+  messaging +  
              left_alt + right_alt +
              anxiety + 
              importance_scale
            #+ important_others
            , data = aux)
  
  RMSE_ols <- append(RMSE_ols, sqrt(mean((aux$fact_checks - ols$fitted.values)^2)))
  
  R_sq_ols <- append(R_sq_ols, summary(ols)$adj.r.squared)
  
  AIC_ols <- append(AIC_ols, AIC(ols))
  
  
}


df2 <- data.frame(country = as.character(country_list) ,
                 RMSE_nb = RMSE_nb,
                 R_sq_nb = R_sq_nb, 
                 AIC_nb  = AIC_nb ,
                 RMSE_ols= RMSE_ols,
                 R_sq_ols= R_sq_ols ,
                 AIC_ols = AIC_ols,
                 anxiety_coef = anxiety_coef,
                 anxiety_pval = anxiety_pval,
                 imp_sc_coef = imp_sc_coef, 
                 imp_sc_pval = imp_sc_pval 
                 #imp_others_coef =imp_others_coef,
                 #imp_others_pval =imp_others_pval
                 )

#df2 <- df2  %>% dplyr::select(-c(8:11))

#print(xtable(df2,
#             digits = c( 0, 0, 2, 2, 0, 2,2,0 )),
#      digits= 0,
#      include.rownames=FALSE,
#      file = "output/metrics_nb_ols.tex")


#PLOT MAIN COEFFICIENTS 
#-------------------------------------

country_list <- unique(df$country2)

models <- c()


for (i in country_list){
  
  aux <- df[df$country2 == i,]
  
  nb <- glm.nb(fact_checks ~ AgeCat + woman + college_grad +
                 pol_cynisism_scale + trust_gov + Political_Interest + 
                 political_orient + political_orient_sq +
                 satisfaction_dem + 
                 media_trust_scale + sp_media_lit_scale +
                 TV + newspapers + social_media +
                 radio +      
                 aggregators+  messaging +  
                 left_alt + right_alt +
                 anxiety + 
                 importance_scale
               #+ important_others
               , data = aux)
  
  models[[i]] <- nb
  
}

#sample figure for main text


p <- plot_summs(models[c(3,5,6,11,14,15,16,17,18,19)], coefs = c('Anxiety' = 'anxiety', 'Importance' = 'importance_scale',
                                  'Social Media' = 'social_media', 'TV' = 'TV', 
                                  'Newspapers' = "newspapers"),
                colors = c(  'red','orange',
                           'hotpink', 
                           'plum', 'darkblue', 'brown', 'magenta',
                           'darkgray','darkcyan', 'green'),
                point.shape = FALSE,
                point.size = 3,
                line.size = c(0.8, 2),
                facet.label.pos = "top",
                facet.cols = 2,
                groups = list("A"= c("Anxiety", "Importance"),
                              "B" =c("Social Media", "TV", "Newspapers"))
)  +
  theme(legend.title=element_blank(),
        legend.text =  element_text(size=10),
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'),
        axis.text = element_text(size = 10))+ #change legend key width)+
  guides(shape = guide_legend(override.aes = list(size = 0.1)))+
  xlab("Coefficient Estimates")

ggsave(p, file='output/coefs_rq2_sample.png',width=10, height=7) #save plot to output file




#complete figure for appendix

p <- plot_summs(models, coefs = c('Anxiety' = 'anxiety', 'Importance' = 'importance_scale',
                                  'Social Media' = 'social_media', 'TV' = 'TV', 
                                  'Newspapers' = "newspapers", 'Radio'='radio', 
                                  'Aggregators' = 'aggregators', 'Messaging' = 'messaging',
                                  'Left Alt.' = 'left_alt', 'Right Alt.' = "right_alt"),
           colors = c('blue', 'pink', 'red', 'gray','orange',
                      'hotpink', 'lavender', 'lightgreen', 'lightblue','yellow',
                      'plum', 'tan', 'darkred', 'darkblue', 'brown', 
                      'magenta', 'darkgray','darkcyan', 'green'),
           point.shape = FALSE,
           point.size = 3,
           line.size = c(0.8, 2),
           facet.label.pos = "top",
           facet.cols = 2,
           groups = list("A"= c("Anxiety", "Importance"),
                      "B" =c("Social Media", "TV", "Newspapers", "Radio", 
                                  "Aggregators","Messaging", "Left Alt.", "Right Alt."))
           )  +
          theme(legend.title=element_blank(),
                legend.text =  element_text(size=12),
                legend.key.height = unit(0.5, 'cm'), #change legend key height
                legend.key.width = unit(1, 'cm'),
                axis.text = element_text(size = 10))+ #change legend key width)+
          guides(shape = guide_legend(override.aes = list(size = 0.1)))+
        xlab("Coefficient Estimates")

ggsave(p, file='output/coefs_rq2.png',width=10, height=13) #save plot to output file


#prediction vs predictor plots
#------------------------------
country_list <- unique(df$country2)
#country_list<- c('Brazil', 'Romania', 'Belgium', 'Netherlands', 'Spain', 
#                 'Poland', 'Czechia', 'United States', 'United Kingdom',
#                 'Serbia')

full_aux <- data.frame()

for (i in country_list){
  
  aux <- df[df$country2 == i,]
  
  nb <- glm.nb(fact_checks ~ AgeCat + woman + college_grad +
                 pol_cynisism_scale + trust_gov + Political_Interest + 
                 political_orient + political_orient_sq +
                 satisfaction_dem + 
                 media_trust_scale + sp_media_lit_scale +
                 TV + newspapers + social_media +
                 radio +      
                 aggregators+  messaging +  
                 left_alt + right_alt +
                 anxiety + 
                 importance_scale
               #+ important_others
               , data = aux)
  
  aux$prediction <- nb$fitted.values

  full_aux <- rbind(full_aux, aux)
}

p<- ggplot(full_aux, aes(x=anxiety, y=prediction)) +#visualize the data to decide how to code the variable
  geom_point(alpha=0.5)+
  geom_smooth(method=lm)+
  xlab("Anxiety") + ylab("Prediction")+
  facet_wrap(country2 ~. ,nrow=4)

ggsave(p, file='output/anx_pred_rq2.png',width=10, height=6) #save plot to output file


p <- ggplot(full_aux, aes(x=importance_scale, y=prediction)) +#visualize the data to decide how to code the variable
  geom_point(alpha=0.5)+
  geom_smooth(method=lm)+
  xlab("Importance") + ylab("Prediction")+
  facet_wrap(country2 ~. ,nrow=4)

ggsave(p, file='output/imp_pred_rq2.png',width=10, height=6) #save plot to output file





#model summary tables

#part1
tbl <- export_summs(models[1:10],digits=2,
                    statistics = c(N = "nobs"))
tbl <- insert_row(tbl,c("Pseudo R2",R_sq_nb[1:10] ), after=48)
tbl <- set_font_size(tbl, 7)
tbl <- set_caption(tbl, 'Results from country-by-country negative binomial regressions. 
             Cell entries are coefficients and standard
              errors are in parentheses. The outcome variable is fact-checks. 
              All continuous variables are scaled to range from 0 to 1.')
tbl <- set_label(tbl, "summodel::rq2A")
tbl<- set_all_padding(tbl,0)
tbl<-set_width(tbl,"200mm")
tbl <- set_col_width(tbl, c("40mm", "15mm", "15mm","15mm",
                            "15mm","15mm","15mm","15mm",
                            "15mm","15mm", "15mm"))
tbl <- set_number_format(tbl, col=c(2:11) ,row=49,value="%.2f")
quick_latex(tbl, file="output/rq2_sum_A.tex", open = FALSE)


#part2
tbl <- export_summs(models[11:19],digits=2,
                    statistics = c(N = "nobs"))
tbl <- insert_row(tbl,c("Pseudo R2",R_sq_nb[11:19]), after=48, copy_cell_props = TRUE)
tbl <- set_font_size(tbl, 7)
tbl <- set_caption(tbl, "Table \ref{summodel::rq2A} (Continued)")
tbl<- set_all_padding(tbl,0)
tbl <- set_number_format(tbl, col=c(2:10) ,row=49,value="%.2f")
tbl<-set_width(tbl,"200mm")
tbl <- set_col_width(tbl, c("40mm", "15mm", "15mm","15mm",
                            "15mm","15mm","15mm","15mm",
                            "15mm","15mm"))
quick_latex(tbl, file="output/rq2_sum_B.tex", open = FALSE)
