#-----------------------------------------
#MODELS FOR RESEARCH QUESTION 1
#-----------------------------------------

#Anxiety as DV
#---------------------

#one model per country
#print statistics
country_list <- unique(df$country2)

RMSE_ols <-  c()
R_sq_ols <-  c()


for (i in country_list){
  
  aux <- df[df$country2 == i,]
  
  ols <- lm_robust(anxiety ~ AgeCat + woman + college_grad +
                 pol_cynisism_scale + trust_gov + Political_Interest + 
                 political_orient + political_orient_sq +
                 satisfaction_dem + 
                 media_trust_scale + sp_media_lit_scale +
                 TV + newspapers + #social_media +
                 radio +      
                 aggregators+  messaging +  
                 left_alt + right_alt +
                 twitter + instagram + tiktok + gab + reddit + 
                 facebook + youtube+ other,
                data = aux,
                se_type = 'HC1')
  
  RMSE_ols <- append(RMSE_ols, sqrt(mean((aux$anxiety - ols$fitted.values)^2)))
  
  R_sq_ols <- append(R_sq_ols, summary(ols)$adj.r.squared)

  
  
}


df_summ_anx <- data.frame(country = as.character(country_list) ,
                  RMSE_ols= RMSE_ols,
                  R_sq_ols= R_sq_ols 
                  )

print(xtable(df_summ_anx,
             digits = c( 0, 0, 2, 2 )),
      digits= 0,
      include.rownames=FALSE,
      file = "output/metrics_ols_anx.tex")


#plot coefficients for each model
country_list <- unique(df$country2)

models <- c()


for (i in country_list){
  
  aux <- df[df$country2 == i,]
  
  ols <- lm_robust(anxiety ~ AgeCat + woman + college_grad +
              pol_cynisism_scale + trust_gov + Political_Interest + 
              political_orient + political_orient_sq +
              satisfaction_dem + 
              media_trust_scale + sp_media_lit_scale +
              TV + newspapers + #social_media +
              radio +      
              aggregators+  messaging +  
              left_alt + right_alt +
              twitter + instagram + tiktok + gab + reddit + 
              facebook + youtube+ other,
            data = aux,
            se_type = "HC1")
  
  models[[i]] <- ols
  
}

#sample figure for main text
p <-plot_summs(models[c(3,5,6,11,14,15,16,17,18,19)], 
               coefs = c('Twitter' = 'twitter','Instagram' = 'instagram',
                                 'Facebook' = 'facebook' ,
                                 'Gab' ='gab', 
                                 'TV' = 'TV', 'Newspapers' = "newspapers", 'Radio'='radio'
                                   ),
               colors = c(  'red','orange',
                           'hotpink', 
                           'plum', 'darkblue','brown', 'magenta',
                           'darkgray','darkcyan', 'green'),
               point.shape = FALSE,
               point.size = 3,
               line.size = c(0.8, 1),
               facet.label.pos = "top",
               facet.cols = 2,
               groups = list("B"= c("Twitter", "Instagram", 
                                    'Facebook','Gab'),
                             "A" =c("Social Media", "TV", "Newspapers", "Radio" 
                             ))
)+
  theme(legend.title=element_blank(),
        legend.text =  element_text(size=10),
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'),
        axis.text = element_text(size = 10))+ #change legend key width)+
  guides(shape = guide_legend(override.aes = list(size = 0.1)))+
  xlab("Coefficient Estimates")

ggsave(p, file='output/coefs_rq1_anx_sample.png',width=10, height=7) #save plot to output file




#full figure for appendix
p <-plot_summs(models, coefs = c('Twitter' = 'twitter','Instagram' = 'instagram',
                             'TikTok' = 'tiktok', 'Facebook' = 'facebook' ,
                             'Reddit' = 'reddit','Gab' ='gab', 
                             'YouTube' ='youtube', 'Other' ='other',
                             'TV' = 'TV', 'Newspapers' = "newspapers", 'Radio'='radio', 
                             'Aggregators' = 'aggregators', 'Messaging' = 'messaging',
                             'Left Alt.' = 'left_alt', 'Right Alt.' = "right_alt"),
               colors = c('blue', 'pink', 'red', 'gray','orange',
                          'hotpink', 'lavender', 'lightgreen', 'lightblue','yellow',
                          'plum', 'tan', 'darkred', 'darkblue', 'brown', 
                          'magenta', 'darkgray','darkcyan', 'green'),
           point.shape = FALSE,
           point.size = 3,
           line.size = c(0.8, 1),
           facet.label.pos = "top",
           facet.cols = 2,
           groups = list("B"= c("Twitter", "Instagram", 'TikTok',
                                                     'Facebook', 'Reddit', 'Gab',
                                                     'YouTube', 'Other'),
                         "A" =c("Social Media", "TV", "Newspapers", "Radio", 
                                    "Aggregators","Messaging", "Left Alt.", "Right Alt."))
           )  +
  theme(legend.title=element_blank(),
        legend.text =  element_text(size=12),
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'),
        axis.text = element_text(size = 10))+ #change legend key width)+
  guides(shape = guide_legend(override.aes = list(size = 0.1)))+
  xlab("Coefficient Estimates")
           
ggsave(p, file='output/coefs_rq1_anx.png',width=10, height=13) #save plot to output file


#model summary table

#part 1
tbl <- export_summs(models[1:10],digits=2,
                    statistics = c(N = "nobs", R2adj = "adj.r.squared"))
tbl <- set_font_size(tbl, 7)
tbl <- set_caption(tbl, 'Results from country-by-country OLS models. 
             Cell entries are OLS coefficients and robust standard
              errors are in parentheses. The outcome variable (anxiety) 
              and all continuous variables are scaled to range from 0 to 1.')
tbl <- set_label(tbl, "summodel::rq1anxA")
tbl<- set_all_padding(tbl,0)
tbl <- set_col_width(tbl, c("40mm", "15mm", "15mm","15mm",
                            "15mm","15mm","15mm","15mm",
                            "15mm","15mm","15mm"))
tbl<-set_width(tbl,"200mm")
quick_latex(tbl, file="output/rq1_anx_sum_A.tex", open = FALSE)


#part 2
tbl <- export_summs(models[11:19],digits=2,
                    statistics = c(N = "nobs", R2adj = "adj.r.squared"))
tbl <- set_font_size(tbl, 7)
tbl <- set_caption(tbl, "Table \ref{summodel::rq1anxA} (Continued)")
tbl<- set_all_padding(tbl,0)
tbl <- set_col_width(tbl, c("40mm", "15mm", "15mm","15mm",
                            "15mm","15mm","15mm","15mm",
                            "15mm","15mm"))
tbl<-set_width(tbl,"200mm")
quick_latex(tbl, file="output/rq1_anx_sum_B.tex", open = FALSE)


#importance scale as DV
#---------------------------------------------
#one model per country
#print statistics
country_list <- unique(df$country2)

RMSE_ols <-  c()
R_sq_ols <-  c()

for (i in country_list){
  
  aux <- df[df$country2 == i,]
  
  ols <- lm_robust(importance_scale ~ AgeCat + woman + college_grad +
              pol_cynisism_scale + trust_gov + Political_Interest + 
              political_orient + political_orient_sq +
              satisfaction_dem + 
              media_trust_scale + sp_media_lit_scale +
              TV + newspapers + #social_media +
              radio +      
              aggregators+  messaging +  
              left_alt + right_alt +
              twitter + instagram + tiktok + gab + reddit + 
              facebook + youtube+ other,
            data = aux,
            se_type='HC1')
  
  RMSE_ols <- append(RMSE_ols, sqrt(mean((aux$importance_scale - ols$fitted.values)^2)))
  
  R_sq_ols <- append(R_sq_ols, summary(ols)$adj.r.squared)

  
  
}


df_summ_imp <- data.frame(country = as.character(country_list) ,
                          RMSE_ols= RMSE_ols,
                          R_sq_ols= R_sq_ols
                          )


print(xtable(df_summ_imp,
             digits = c( 0, 0, 2, 2 )),
      digits= 0,
      include.rownames=FALSE,
      file = "output/metrics_ols_imp.tex")

#plot coefficients for each model
country_list <- unique(df$country2)

models <- c()


for (i in country_list){
  
  aux <- df[df$country2 == i,]
  
  ols <- lm_robust(importance_scale ~ AgeCat + woman + college_grad +
                     pol_cynisism_scale + trust_gov + Political_Interest + 
                     political_orient + political_orient_sq +
                     satisfaction_dem + 
                     media_trust_scale + sp_media_lit_scale +
                     TV + newspapers + #social_media +
                     radio +      
                     aggregators+  messaging +  
                     left_alt + right_alt +
                     twitter + instagram + tiktok + gab + reddit + 
                     facebook + youtube+ other,
                   data = aux,
                   se_type = "HC1")
  
  models[[i]] <- ols
  
}

#sample figure for main text
p <-plot_summs(models[c(3,5,6,11,14,15,16,17,18,19)],            
               coefs = c('Twitter' = 'twitter','Instagram' = 'instagram',
                            'Facebook' = 'facebook' ,
                           'Gab' ='gab', 
                          'TV' = 'TV', 'Newspapers' = "newspapers", 'Radio'='radio'),
              colors = c( 'red','orange',
             'hotpink', 
             'plum', 'darkblue','brown', 'magenta',
              'darkgray','darkcyan', 'green'),
               point.shape = FALSE,
               point.size = 3,
               line.size = c(0.8, 1),
               facet.label.pos = "top",
               facet.cols = 2,
               groups = list("B"= c("Twitter", "Instagram", 
                                   'Facebook','Gab'),
                             "A" =c("Social Media", "TV", "Newspapers", "Radio" 
                                        ))
)  +
  theme(legend.title=element_blank(),
        legend.text =  element_text(size=10),
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'),
        axis.text = element_text(size = 10))+ #change legend key width)+
  guides(shape = guide_legend(override.aes = list(size = 0.1)))+
  xlab("Coefficient Estimates")

ggsave(p, file='output/coefs_rq1_imp_sample.png',width=10, height=7) #save plot to output file

#full figure for appendix
p <-plot_summs(models, coefs = c('Twitter' = 'twitter','Instagram' = 'instagram',
                                 'TikTok' = 'tiktok', 'Facebook' = 'facebook' ,
                                 'Reddit' = 'reddit','Gab' ='gab', 
                                 'YouTube' ='youtube', 'Other' ='other',
                                 'TV' = 'TV', 'Newspapers' = "newspapers", 'Radio'='radio', 
                                 'Aggregators' = 'aggregators', 'Messaging' = 'messaging',
                                 'Left Alt.' = 'left_alt', 'Right Alt.' = "right_alt"),
               colors = c('blue', 'pink', 'red', 'gray','orange',
                          'hotpink', 'lavender', 'lightgreen', 'lightblue','yellow',
                          'plum', 'tan', 'darkred', 'darkblue', 'brown', 
                          'magenta', 'darkgray','darkcyan', 'green'),
               point.shape = FALSE,
               point.size = 3,
               line.size = c(0.8, 1),
               facet.label.pos = "top",
               facet.cols = 2,
               groups = list("B"= c("Twitter", "Instagram", 'TikTok',
                                                         'Facebook', 'Reddit', 'Gab',
                                                         'YouTube', 'Other'),
                             "A" =c("Social Media", "TV", "Newspapers", "Radio", 
                                        "Aggregators","Messaging", "Left Alt.", "Right Alt."))
)  +
  theme(legend.title=element_blank(),
        legend.text =  element_text(size=12),
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'),
        axis.text = element_text(size = 10))+ #change legend key width)+
  guides(shape = guide_legend(override.aes = list(size = 0.1)))+
  xlab("Coefficient Estimates")

ggsave(p, file='output/coefs_rq1_imp.png',width=10, height=13) #save plot to output file


#model summary table

#part 1
tbl <- export_summs(models[1:10],digits=2,
                    statistics = c(N = "nobs", R2adj = "adj.r.squared"))
tbl <- set_font_size(tbl, 7)
tbl <- set_caption(tbl, 'Results from country-by-country OLS models. 
             Cell entries are OLS coefficients and robust standard
              errors are in parentheses. The outcome variable (importance) 
              and all continuous variables are scaled to range from 0 to 1.')
tbl <- set_label(tbl, "summodel::rq1impA")
tbl<- set_all_padding(tbl,0)
tbl <- set_col_width(tbl, c("40mm", "15mm", "15mm","15mm",
                            "15mm","15mm","15mm","15mm",
                            "15mm","15mm","15mm"))
tbl<-set_width(tbl,"200mm")
quick_latex(tbl, file="output/rq1_imp_sum_A.tex", open = FALSE)

#part 2
tbl <- export_summs(models[11:19],digits=2,
                    statistics = c(N = "nobs", R2adj = "adj.r.squared"))
tbl <- set_font_size(tbl, 7)
tbl <- set_caption(tbl, "Table \ref{summodel::rq1impA} (Continued)")
tbl<- set_all_padding(tbl,0)
tbl <- set_col_width(tbl, c("40mm", "15mm", "15mm","15mm",
                            "15mm","15mm","15mm","15mm",
                            "15mm","15mm"))
tbl<-set_width(tbl,"200mm")
quick_latex(tbl, file="output/rq1_imp_sum_B.tex", open = FALSE)
