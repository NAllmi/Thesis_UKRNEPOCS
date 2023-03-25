#----------------
#   MODEL FIT
#----------------

library(MASS)

aux <- df[df$country2 == 'Germany',]

ggplot(aux, aes(fact_checks)) + 
  geom_histogram(binwidth = 1) 


nb <- glm.nb(fact_checks ~ AgeCat + woman + college_grad +
               pol_cynisism_scale + trust_gov + Political_Interest + 
               political_orient_sq +
               satisfaction_dem + 
               media_trust_scale + sp_media_lit_scale +
               TV + newspapers + social_media +
               radio +      
               aggregators+  messaging +  
               left_alt + right_alt +
               anxiety + 
               importance_scale +
               important_others
             , data = aux)


summary(nb)
simulationOutput <- simulateResiduals(fittedModel = nb, plot = F) #The function creates scaled residuals by simulating from the fitted model.

testDispersion(simulationOutput)

testZeroInflation(nb)

plot(simulationOutput, quantreg = TRUE)



plotResiduals(simulationOutput, aux$anxiety)
plotResiduals(simulationOutput, aux$importance_scale)
plotResiduals(simulationOutput, aux$imp_others_0_1)


