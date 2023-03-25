#----------------
#   MODEL FIT
#----------------

#plot histogram of dependent variable
ggplot(df, aes(fact_checks)) + 
  geom_histogram(binwidth = 1) 


# POISSON

mp1 <- glmmTMB(fact_checks ~ AgeCat + woman + college_grad +
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
                 important_others+ 
                 (1|country2), 
               data = df, poisson(link = "log"))
#TESTS

#check for overdispersion
#overdispersion is the presence of greater variability in a data set 
#than would be expected based on a given statistical model

#
simulationOutput <- simulateResiduals(fittedModel = mp1, plot = F) #The function creates scaled residuals by simulating from the fitted model.

testDispersion(simulationOutput, plot=FALSE)
#ratio is significantly >1 indicates overdispersion, so I use a negative binomial


#NEGATIVE BINOMIAL
mnb1 <- glmmTMB(fact_checks ~ AgeCat + woman + college_grad+
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
                  important_others+ 
                  (1|country2), 
                data = df, nbinom2(link = "log"))


summary(mnb1)

#TESTS
#------------------------
#check for overdispersion

simulationOutput <- simulateResiduals(fittedModel = mnb1, plot = F,n =1000)
testDispersion(simulationOutput, plot=FALSE)
#ratio is insignificantly <1 doesnt indicate under or overdispersion

#test dispersion by group
simulationOutput2 <- recalculateResiduals(simulationOutput, group=df$country2) #The purpose of this function is to recalculate scaled residuals per group, based on the simulations done by simulateResiduals

testDispersion(simulationOutput2, plot=FALSE)
#result by group doesn't indicate over dispersion
#--------------------------------------------------


#test for zero inflation

testZeroInflation(mnb1, plot=FALSE)
#because ratioObsSim is <1 the data has less zeros than expected
#(I think a potential reason for this is that people choose to 
#do one fact-check rather than zero "for-show")

#--------------------------------
# Test for multicollinearity using VIF
check_collinearity(mnb1)
#------------------------------------------------------

testCategorical(simulationOutput, df$country2, quantiles = c(0.25, 0.5, 0.75),
                plot = T)
#The function tests for two common problems: are residuals within each 
#group distributed according to model assumptions, and is the variance 
#between group heterogeneous.

#--------------------------------------------

#residual plots
plot(simulationOutput, quantreg = TRUE) #creates standard plots for the simulated residuals

#testOutliers(simulationOutput, margin = c("both"), type = "bootstrap",
#             nBoot = 1000)                                                                                                      

plot(simulationOutput2, quantreg=TRUE) #same plot but for the residuals per group

plotResiduals(simulationOutput, rank = FALSE) #generic residual plot
hist(simulationOutput$fittedResiduals) #histogram of the fitted residuals


#-------------------------------------------------
#plot residuals against different predictors (the default plots against predicted values)

plotResiduals(simulationOutput, df$anxiety, quantreg = TRUE)
plotResiduals(simulationOutput, df$importance_scale, quantreg = TRUE)
plotResiduals(simulationOutput, df$important_others, quantreg = TRUE)

plotResiduals(simulationOutput, df$woman, quantreg = TRUE)
plotResiduals(simulationOutput, df$AgeCat, quantreg = TRUE)
plotResiduals(simulationOutput, df$college_grad, quantreg = TRUE)
plotResiduals(simulationOutput, df$pol_cynisism_scale, quantreg = TRUE)
plotResiduals(simulationOutput, df$trust_gov, quantreg = TRUE)
plotResiduals(simulationOutput, df$Political_Interest, quantreg = TRUE)
plotResiduals(simulationOutput, df$political_orient_sq, quantreg = TRUE)
plotResiduals(simulationOutput, df$media_trust_scale, quantreg = TRUE)
plotResiduals(simulationOutput, df$sp_media_lit_scale, quantreg = TRUE)
plotResiduals(simulationOutput, df$satisfaction_dem, quantreg = TRUE)



plotResiduals(simulationOutput, df$social_media, quantreg = TRUE)
plotResiduals(simulationOutput, df$TV, quantreg = TRUE)
plotResiduals(simulationOutput, df$newspapers, quantreg = TRUE)
plotResiduals(simulationOutput, df$radio, quantreg = TRUE)
plotResiduals(simulationOutput, df$aggregators, quantreg = TRUE)
plotResiduals(simulationOutput, df$messaging, quantreg = TRUE)
plotResiduals(simulationOutput, df$left_alt_, quantreg = TRUE)
plotResiduals(simulationOutput, df$right_alt, quantreg = TRUE)


