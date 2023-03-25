#----------------
#   MODEL FIT  - FIXED EFFECTS
#----------------

#read data
#-------------------------------------
data <- read_dta("Data/May6Survey.dta")

df$anxiety_or <- data$Q2_1r5

#set dv as numeric for computation
df$anxiety_or <- as.numeric(df$anxiety_or)


#ols with fixed effects

m <- lm(anxiety_or ~ AgeCat + woman + college_grad +
          pol_cynisism_scale + trust_gov + Political_Interest + 
          political_orient_sq +
          satisfaction_dem + 
          media_trust_scale + sp_media_lit_scale +
          TV + newspapers + radio +  
          twitter + instagram + tiktok + gab + reddit + facebook + youtube+ other +
          aggregators+  messaging +  
          left_alt + right_alt +
          country2 - 1,   #fixed effects
        data=df) 

summary(m)      


#----------------------------------------------
#Assumptions
#1.Linearity of the data. The relationship between the predictor (x) and the outcome (y) is assumed to be linear.
#2.Normality of residuals. The residual errors are assumed to be normally distributed.
#3.Homogeneity of residuals variance. The residuals are assumed to have a constant variance (homoscedasticity)
#4.Independence of residuals error terms.

#checks
#####1. Linearity #######

#plot observed vs predicted values
df$pred <- m$fitted
ggplot(df, aes(x = anxiety, y = pred))+
  geom_point() +
  geom_smooth(method=lm)

#plot residuals vs fitted values
res<- df$pred-df$anxiety
plot(df$pred, res)
abline(0,0)


####2 Normality of residuals####

#histogram of residuals
hist(res)

#qqplot
qqnorm(res)
qqline(res) 

#plot residuals hist by country

df$residuals <- res

ggplot(df, aes(x = residuals)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_wrap(country2 ~. ,nrow=3 , scales = "free")


#plot residuals qqplot by country
ggplot(df, aes(sample = residuals)) +
  stat_qq() + 
  stat_qq_line() +
  facet_wrap(country2 ~. ,nrow=3 , scales = "free")


#print shapiro wilk normality test for each country
sw_list <- c()
for (i in unique(df$country2)){
  aux <- df[df$country2 == i,]
  sw <- shapiro.test(aux$residuals) 
  sw_list <- append(sw_list, sw$p.value)
  
}

sw_df <- data.frame(country = unique(df$country2),
                    shapiro_test_pvalue = sw_list)

#null hipothesis: normality. We reject the null hypothesis for p<0.05

#guide https://sscc.wisc.edu/sscc/pubs/RegDiag-R/normality.html

####3. Homogeneity of residuals variance (Homoscedasticity Assumption) #####

#plotting residuals
plot(m, 1) #should be a horizontal line

# Breusch-Pagan test
lmtest::bptest(m)
#p-values less than 0.05 suggest variances are significantly different and the homogeneity of variance assumption has been violated.
#there is heteroscedasticity

####4.Independence of residuals error terms######
durbinWatsonTest(m)
#p-value is less than 0.05, we can reject the null hypothesis and conclude that the residuals in this regression model are autocorrelated.

####Multicollinearity#####

car::vif(m)


#run model with robust std errors and compare
mrob <- lm_robust(anxiety_or ~ AgeCat + woman + college_grad +
                    pol_cynisism_scale + trust_gov + Political_Interest + 
                    political_orient_sq +
                    satisfaction_dem + 
                    media_trust_scale + sp_media_lit_scale +
                    TV + newspapers + radio +  
                    twitter + instagram + tiktok + gab + reddit + facebook + youtube+ other +
                    aggregators+  messaging +  
                    left_alt + right_alt , 
                  fixed_effects = ~country2,
                  data=df,
                  se_type = "stata")


summary(mrob)
