#--------------------------------------
#Data Pre-processing
#--------------------------------------

#read data
#-------------------------------------
data <- read_dta("Data/May6Survey.dta")

#-------------------------------------
#MAIN INDEPENDENT VARIABLE

#2.1 Concerns about the war
#scale from 1 (completely disagree) to 7 (completely agree)

#we want all these to go from LC to HC

#1. The war in Ukraine is an important global issue (data$Q2_1r1)
#2. The war in Ukraine is an important issue for my country (data$Q2_1r2)
#3. The war in Ukraine is an important issue to me personally (data$Q2_1r3)

#compute sum of the previous 3
data$importance_scale <- (data$Q2_1r1 + data$Q2_1r2 + data$Q2_1r3)/3

#the following I leave separate (see EFA analysis)
#4. The war in Ukraine mostly affects other people (data$Q2_1r4)
#5. The war in Ukraine makes me anxious (data$Q2_1r5)
#----------------------------------------------------------
#DEPENDENT VARIABLE

#3.4 Fact-checking option
#The statements you read earlier were subject to recent fact-checks. This means a journalist or
#independent fact-checker investigated whether the statements were true or false.
#Would you like to see the fact-checks for any of these statements? Please select all the statements for
#which you would like to see the fact-checks:
#Routing to fact-check of the selected statements →
#How would you rate the truthfulness of this statement now?

#for each individual I calculate the sum of number of statements they fact check

data['fact_checks'] <- rowSums(data[,c('Q3_4r1','Q3_4r2','Q3_4r3','Q3_4r4',
                                       'Q3_4r5','Q3_4r6','Q3_4r7','Q3_4r8',
                                       'Q3_4r9','Q3_4r10')])

#fact checks How to address this variable? fact checks or doesnt? or continous?

#-------------------------------------------------------
#CONTROL VARIABLES - DEMOGRAPHICS

#gender
data$woman <- ifelse(data$Gender==1,1,0)

#education
data$college_grad <- ifelse(data$Education==8 | data$Education==9 | data$Education==10 | data$Education==11,1,0)
data$college_grad <- factor(data$college_grad,
                         levels = c(0,1),
                         labels = c("Non-college","College graduate"))

#age

data$hAge <- as_factor(data$hAge)
data$hAge <- relevel(data$hAge, ref="45-54")
data$AgeCat <- fct_other(data$hAge,keep = c("18-24","75 or older"))
data$AgeCat <- relevel(data$AgeCat,ref="Other")


#country variable
data$country <- countrycode(
  as_factor(data$hCountry),
  origin='iso2c',
  destination='country.name')

data$country2 <- factor(data$country, levels = c( "Germany",
                                            "United States",
                                            "France",
                                            "Poland",
                                            "Austria",
                                            "Belgium",
                                            "Brazil",
                                            "Czechia",
                                            "Denmark",
                                            "Greece",
                                            "Hungary",
                                            "Italy",
                                            "Netherlands",
                                            "Romania",
                                            "Serbia",
                                            "Spain",
                                            "Sweden",
                                            "Switzerland",
                                            "United Kingdom"))




#aggregate political cynicism 
#order all from LC to HC and then compute sum and divide by number of cols

data['pol_cynisism_scale'] <- rowSums(data[,c('Political_Cynicismr1',
                                            'Political_Cynicismr2',
                                            'Political_Cynicismr3', 
                                            'Political_Cynicismr4',
                                            'Political_Cynicismr5', 
                                            'Political_Cynicismr6',
                                            'Political_Cynicismr7',
                                            'Political_Cynicismr8')])/8

#aggregate media trust
#order all from Low Trust to High Trust and then compute sum
#the last two have to be remapped
data$Q1_3r4 <-mapvalues(data$Q1_3r4, from = c(1,2,3,4,5,6,7), to = c(7,6,5,4,3,2,1))
data$Q1_3r5 <-mapvalues(data$Q1_3r5, from = c(1,2,3,4,5,6,7), to = c(7,6,5,4,3,2,1))

data['media_trust_scale'] <- rowSums(data[,c('Q1_3r1','Q1_3r2','Q1_3r3',
                                      'Q1_3r4','Q1_3r5')])/8

#aggregate self-perceived media literacy
#order from low perceived media literacy to high perceived media literacy
data$Q1_6r2 <-mapvalues(data$Q1_6r2, from = c(1,2,3,4,5,6,7), to = c(7,6,5,4,3,2,1))

data['sp_media_lit_scale'] <- rowSums(data[,c('Q1_6r1','Q1_6r2','Q1_6r3',
                                               'Q1_6r4','Q1_6r5')])/5

#Media use

#solve problem of social media use = 8 being routed to platforms.
#those who selected 8 should not have been routed, so I substitute
#their responses in platforms to all 1

data[data$Q1_2r5 == 8, c('Q1_2ar1','Q1_2ar2','Q1_2ar3','Q1_2ar4', 'Q1_2ar5','Q1_2ar6','Q1_2ar7', 'Q1_2ar8')] <- 1

# Classify people who "don't use this source" as "never" [this ensures higher values -> greater consumption]
data$TV           <-ifelse(data$Q1_2r1<=7,data$Q1_2r1,1)
data$radio        <-ifelse(data$Q1_2r2<=7,data$Q1_2r3,1)
data$newspapers   <-ifelse(data$Q1_2r3<=7,data$Q1_2r3,1)
data$aggregators  <-ifelse(data$Q1_2r4<=7,data$Q1_2r3,1)
data$social_media <-ifelse(data$Q1_2r5<=7,data$Q1_2r5,1)
data$messaging    <-ifelse(data$Q1_2r6<=7,data$Q1_2r1,1)
data$left_alt     <-ifelse(data$Q1_2r7<=7,data$Q1_2r5,1)
data$right_alt    <-ifelse(data$Q1_2r8<=7,data$Q1_2r1,1)


#social media platforms
#if theres is an NA change value to 1 (never)
data$facebook <- ifelse(is.na(data$Q1_2ar1), 1, data$Q1_2ar1)
data$twitter <- ifelse(is.na(data$Q1_2ar2), 1, data$Q1_2ar2)
data$instagram <- ifelse(is.na(data$Q1_2ar3), 1, data$Q1_2ar3)
data$youtube <- ifelse(is.na(data$Q1_2ar4), 1, data$Q1_2ar4)
data$tiktok <- ifelse(is.na(data$Q1_2ar5), 1, data$Q1_2ar5)
data$gab <- ifelse(is.na(data$Q1_2ar6), 1, data$Q1_2ar6)
data$reddit <- ifelse(is.na(data$Q1_2ar7), 1, data$Q1_2ar7)
data$other <- ifelse(is.na(data$Q1_2ar8), 1, data$Q1_2ar8)




#---------------------------------------------------------------
#build dataframe with the variables I will use for the model

df<- data[, c('fact_checks', 
              'woman', 'AgeCat', 'college_grad', 'country2',
              'pol_cynisism_scale', 'media_trust_scale', 'sp_media_lit_scale',
              'TV','radio','newspapers','aggregators', 
              'social_media', 'messaging', 'left_alt','right_alt',   
              'facebook', 'twitter','instagram','youtube',
              'tiktok','gab','reddit','other',
              'importance_scale')]

#add some columns that didn't need preprocessing with clear names

df$anxiety <- data$Q2_1r5
df$important_others <- data$Q2_1r4

df$satisfaction_dem <- data$Q1_4 
df$trust_gov <- data$Q1_5 
df$Political_Interest <- data$Political_Interest 


df$political_orient <- data$Political_Orientation
# there is some research suggesting that relationships could be U-shapred
#with extremists being different from people in the middle - so then we'd want a squared (or any non-linear) term in a regression to account for that
df$political_orient_sq <- data$Political_Orientation -5 #center around 0
df$political_orient_sq <- df$political_orient_sq^3 #elevate to cube so it will have a u shape 

#Print plots and tables for data collection section before normalizing

source("data_collection_output.R")

#--------------------------------------------
#Normalize  data


range01 <- function(x){(x-min(x))/(max(x)-min(x))} #define function to standardize

df[,6:32] <- lapply(df[,6:32], range01) #apply function to all continuous data

#------------------------------------
#remove original data
rm(data)


