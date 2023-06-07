
#save array of models and plot coeffs

country_list <- unique(df$country2)

models <- c()
tib <- c()

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
  
  tib[[i]] <- tail(broom::tidy(nb, conf.int = TRUE),2)
  
}
  
  
  #ggcoef(tail(broom::tidy(coeftest(nb, vcovHAC(nb, type ='HC0'), conf.int = TRUE)),2))



plot_summs(models, coefs = c('Anxiety' = 'anxiety'),
           colors = c('blue', 'green', 'red', 'orange','gray',
                      'lightblue', 'lavender', 'lightgreen', 'pink','plum',
                       'tan', 'yellow', 'darkred', 'darkblue', 'brown', 
                      'magenta', 'darkgray','darkcyan', 'hotpink'),
           point.shape = FALSE,
           point.size = 3,
           line.size = c(0.8, 2),
           legend.title = "Model")  


plot_summs(models, coefs = c('Importance Scale' = 'importance_scale'),
           colors = c('blue', 'green', 'red', 'orange','gray',
                      'lightblue', 'lavender', 'lightgreen', 'pink','plum',
                      'tan', 'yellow', 'darkred', 'darkblue', 'brown', 
                      'magenta', 'darkgray','darkcyan', 'hotpink'),
           point.shape = FALSE,
           point.size = 3,
           line.size = c(0.8, 2),
           legend.title = "Model")  
