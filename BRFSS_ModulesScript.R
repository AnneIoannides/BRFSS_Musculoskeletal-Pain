#Anne Elizabeth Ioannides

#For the qualification of PhD (Physiology), University of the Witwatersrand, South Africa

#Behavioural Risk Factor Surveillance System (BRFSS)

#Due to very large file size, the workflow for BRFSS analyses is slightly different compared to the other surveys used in my PhD. Each year (data download, cleaning, and analysis) takes place in one script. 
#Each year (except module years) has one R script dedicated to it. Module years done collectively, and BRFSS figures that are constructed across time can be found in the Module years scripts

#Note - "Arthritis" is often used in this script as a synonym for Rheumatic Disorder Diagnosis (RDD). This is because the BRFSS variable for RDD is called "HAVARTH", and therefore "ARTHritis" was an easy-navigation term.

#Packages
library(haven)
library(foreign)
library(tidyverse)
library(survey)
library(gdata)
library(ggplot2)
library(ggthemes)
library(sf)
library(sp)
library(spdep)

options(survey.lonely.psu = "adjust")

#Module years: 1996, 1997, 1998, 1999, 2000, 2002, 2004, 2010
#Note that there are no geospatial data analysed for module years. Therefore, these analyses consist of overall prevalence, demographic, and lifestyle analyses


# == 1996 == #

#DOWNLOAD

B96_url <- "http://www.cdc.gov/brfss/annual_data/1996/files/CDBRFS96XPT.zip"

tempB96 <- tempfile()
tempB96b <- tempfile()

download.file(B96_url, tempB96, mode = "wb")
unzip(zipfile = tempB96, exdir = tempB96b)
BRFSS96_raw <- read_xpt(file.path(tempB96b, "CDBRFS96.XPT"))

saveRDS(object = BRFSS96_raw,
        file = "BRFSS_1996.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

unlink(c(tempB96, tempB96b))

#CLEANING

#Observe the dataset
str(BRFSS96_raw) 
tail(BRFSS96_raw) 
glimpse(BRFSS96_raw) 
colnames(BRFSS96_raw)

BRFSS96 <- select(BRFSS96_raw,
                  "_PSU", "_FINALWT", "_STSTR",
                  "AGE", "SEX",
                  "EDUCA", "EMPLOY", "INCOME2",
                  "HAVARTH",
                  "_BMI") 

#Observations
str(BRFSS96)
tail(BRFSS96)
glimpse(BRFSS96)
colnames(BRFSS96)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS96))
#Lots of missing arthritis data, which makes sense due to being a module
which(colSums(is.na(BRFSS96)) == nrow(BRFSS96))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Now have a look at the arthritis variable (HAVARTH)
#Observe how many people answered yes, no, don't know, & refused
table(BRFSS96$HAVARTH)
#   1    2    7    9 
#3311 8987   62  582

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS96$HAVARTH <- recode(BRFSS96$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS96$HAVARTH <- unknownToNA(BRFSS96$HAVARTH, unknown = c("7", "9")))
table(BRFSS96$HAVARTH)

BRFSS96$HAVARTH <- as.factor(BRFSS96$HAVARTH)
class(BRFSS96$HAVARTH)

table(BRFSS96$AGE)
#From the above command, I saw that this data is continuous. However, in order for us to make meaningful comparisons with other years, I will alter the variables into categorical ranges in the same way as later BRFSS datasets
#Recode age data into categories corresponding to BRFSS codebook
BRFSS96$AGE<- recode(BRFSS96$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS96$AGE <- unknownToNA(BRFSS96$AGE, unknown = c("7", "9")))
table(BRFSS96$AGE)

BRFSS96$AGE <- as.factor(BRFSS96$AGE)
class(BRFSS96$AGE)

#Recode sex data into categories corresponding to BRFSS codebook
BRFSS96$SEX <- recode(BRFSS96$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS96$SEX)

BRFSS96$SEX <- as.factor(BRFSS96$SEX)
class(BRFSS96$SEX)

BRFSS96$EDUCA <- recode(BRFSS96$EDUCA,
                        "1" = "Never attended school/only kindergarten",
                        "2" = "Elementary school",
                        "3" = "Some high school",
                        "4" = "High school graduate",
                        "5" = "Some college/some technical school",
                        "6" = "College graduate",
                        "9" = "9")
#Change refused (9) values to NA
(BRFSS96$EDUCA <- unknownToNA(BRFSS96$EDUCA, unknown = "9"))
table(BRFSS96$EDUCA)

BRFSS96$EDUCA <- as.factor(BRFSS96$EDUCA)
class(BRFSS96$EDUCA)

#Recode income into categories corresponding to BRFSS codebook
BRFSS96$INCOME2 <- recode(BRFSS96$INCOME2,
                          "1" = "$10 000 or less",
                          "2" = "$10 000 to $14 999",
                          "3" = "$15 000 to $19 999",
                          "4" = "$20 000 to $24 999",
                          "5" = "$25 000 to $34 999",
                          "6" = "$35 000 to $49 999",
                          "7" = "$50 000 to $74 999",
                          "8" = "$75 000 or more",
                          "77" = "77",
                          "99" = "99")
#Change don't know (77) and (99) values to NA
(BRFSS96$INCOME2 <- unknownToNA(BRFSS96$INCOME2, unknown = c("77", "99")))
table(BRFSS96$INCOME2)

BRFSS96$INCOME2 <- as.factor(BRFSS96$INCOME2)
class(BRFSS96$INCOME2)

BRFSS96$EMPLOY <- recode(BRFSS96$EMPLOY,
                         "1" = "Employed for wages",
                         "2" = "Self-employed",
                         "3" = "Unemployed",
                         "4" = "Unemployed",
                         "5" = "Homemaker",
                         "6" = "Student",
                         "7" = "Retired",
                         "8" = "Unable to work",
                         "9" = "9")
#Change refused (9) to NA
(BRFSS96$EMPLOY <- unknownToNA(BRFSS96$EMPLOY, unknown = "9"))
table(BRFSS96$EMPLOY)

BRFSS96$EMPLOY <- as.factor(BRFSS96$EMPLOY)
class(BRFSS96$EMPLOY)

#BMI
#one implied decimal place
BRFSS96 <- BRFSS96 %>%
  mutate(`_BMI` = `_BMI`/10)

BRFSS96$`_BMI` <- unknownToNA(BRFSS96$`_BMI`, unknown = "99.9")
BRFSS96 %>% top_n(10, `_BMI`)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS96_dataset <- subset(BRFSS96,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS96_dataset$`_FINALWT`)
#The sum of the weights is 17 638 547, MODULE

#Check the number of people (unique PSU's)
length(unique(BRFSS96_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 7 926

#Check the number of unique strata
length(unique(BRFSS96_dataset[["_STSTR"]]))
#The number of unique strata is 15

#Used to generate frequency tables (unweighted)

table(subset(BRFSS96_dataset, AGE == "Age 18 to 24")$SEX)
table(subset(BRFSS96_dataset, AGE == "Age 25 to 29")$SEX)
table(subset(BRFSS96_dataset, AGE == "Age 30 to 34")$SEX)
table(subset(BRFSS96_dataset, AGE == "Age 35 to 39")$SEX)
table(subset(BRFSS96_dataset, AGE == "Age 40 to 44")$SEX)
table(subset(BRFSS96_dataset, AGE == "Age 45 to 49")$SEX)
table(subset(BRFSS96_dataset, AGE == "Age 50 to 54")$SEX)
table(subset(BRFSS96_dataset, AGE == "Age 55 to 59")$SEX)
table(subset(BRFSS96_dataset, AGE == "Age 60 to 64")$SEX)
table(subset(BRFSS96_dataset, AGE == "Age 65 to 69")$SEX)
table(subset(BRFSS96_dataset, AGE == "Age 70 and above")$SEX)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS96_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS96_dataset)
#Observe the design oject
BRFSS96_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(BRFSS96_DO,
        file = "BRFSS96_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_______________________________________________________________________________________________

#ANALYSIS

#Overall prevalence
B96_overall <- svymean(~factor(HAVARTH),
                       BRFSS96_DO,
                       na.rm = TRUE)
B96_overall.c <- B96_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B96_overall_ci <- confint(B96_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B96 <- bind_cols(B96_overall.c, B96_overall_ci)
#remove havarth = 0
B96 <- B96[-c(1), ] #final proportion, se and 95% ci
#backup save
#write.csv(B96, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B96_Arthritis.csv")

#Demographic analysis

#1. Arthritis (RDD) prevalence by age 
B96_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS96_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B96_Arthtitis_age <- B96_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B96_arth_age_ci <- confint(B96_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B96_arth_age_ci <- B96_arth_age_ci[-c(1:11), ]
#join ci and proportions
B96_Arthritis.Age <- bind_cols(B96_Arthtitis_age, B96_arth_age_ci)
#Save
#write.csv(B96_Arthritis.Age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B96_Arthritis.age.csv")

#Age logistic regression
B96_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS96_DO)
summary(B96_age_glm)
exp(cbind(OR=coef(B96_age_glm), confint(B96_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B96_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS96_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B96_Arthritis_sex <- B96_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B96_arth_sex_ci <- confint(B96_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B96_arth_sex_ci <- B96_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B96_Arthritis.Sex <- bind_cols(B96_Arthritis_sex, B96_arth_sex_ci)
#save
#write.csv(B96_Arthritis.Sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B96_Arthritis.sex.csv")

#Sex logistic regression
B96_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS96_DO)
summary(B96_sex_glm)
exp(cbind(OR=coef(B96_sex_glm), confint(B96_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B96_Arth_emp <- svyby(formula = ~HAVARTH,
                      by = ~EMPLOY,
                      design = BRFSS96_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B96_Arthritis_emp <- B96_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B96_arth_emp_ci <- confint(B96_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B96_arth_emp_ci <- B96_arth_emp_ci[-c(1:7), ]
#join ci and proportion
B96_Arthritis.Employ <- bind_cols(B96_Arthritis_emp, B96_arth_emp_ci)
#save
#write.csv(B96_Arthritis.Employ, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B96_Arthritis.employment.csv")

#Employment status logistic regression
B96_emp_glm <- svyglm(HAVARTH~EMPLOY + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS96_DO)
summary(B96_emp_glm)
exp(cbind(OR=coef(B96_emp_glm), confint(B96_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B96_inc_glm <- svyglm(HAVARTH~relevel(INCOME2, ref = "$75 000 or more") + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS96_DO)
summary(B96_inc_glm)
exp(cbind(OR=coef(B96_inc_glm), confint(B96_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B96_edu_glm <- svyglm(HAVARTH~EDUCA + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS96_DO)
summary(B96_edu_glm)
exp(cbind(OR=coef(B96_edu_glm), confint(B96_edu_glm)))

#------------------------------------------------------------------------------------------------------------------------

#Lifestyle analysis

#1. Arthritis (RDD) by BMI (logistic regression)
B96.BMI.glm <- svyglm(HAVARTH ~ `_BMI` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS96_DO)
summary(B96.BMI.glm)
exp(cbind(OR=coef(B96.BMI.glm), confint(B96.BMI.glm)))

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


# == 1997 == #

B97_url <- "http://www.cdc.gov/brfss/annual_data/1997/files/CDBRFS97XPT.zip"

tempB97 <- tempfile()
tempB97b <- tempfile()

download.file(B97_url, tempB97, mode = "wb")
unzip(zipfile = tempB97, exdir = tempB97b)
BRFSS97_raw <- read_xpt(file.path(tempB97b, "CDBRFS97.XPT"))

saveRDS(object = BRFSS97_raw,
        file = "BRFSS_1997.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

unlink(c(tempB97, tempB97b))

#Observe the dataset
str(BRFSS97_raw) 
tail(BRFSS97_raw) 
glimpse(BRFSS97_raw) 
colnames(BRFSS97_raw)

BRFSS97 <- select(BRFSS97_raw,
                 "_PSU", "_FINALWT", "_STSTR",
                  "AGE", "SEX",
                  "EDUCA", "EMPLOY", "INCOME2",
                  "HAVARTH",
                  "_BMI") 

#Observations
str(BRFSS97)
tail(BRFSS97)
glimpse(BRFSS97)
colnames(BRFSS97)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS97))
#Lots missing for arthritis due to being module year
which(colSums(is.na(BRFSS97)) == nrow(BRFSS97))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Now have a look at the arthritis variable (HAVARTH)
#Observe how many people answered yes, no, don't know, & refused
table(BRFSS97$HAVARTH)

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS97$HAVARTH <- recode(BRFSS97$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS97$HAVARTH <- unknownToNA(BRFSS97$HAVARTH, unknown = c("7", "9")))
table(BRFSS97$HAVARTH)

BRFSS97$HAVARTH <- as.factor(BRFSS97$HAVARTH)
class(BRFSS97$HAVARTH)

table(BRFSS97$AGE)
#From the above command, I saw that this data is continuous. However, in order for us to make meaningful comparisons with other years, I will alter the variables into categorical ranges in the same way as later BRFSS datasets
#Recode age data into categories corresponding to BRFSS codebook
BRFSS97$AGE<- recode(BRFSS97$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "97" = "Age 70 and above",
                     "97" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS97$AGE <- unknownToNA(BRFSS97$AGE, unknown = c("7", "9")))
table(BRFSS97$AGE)

BRFSS97$AGE <- as.factor(BRFSS97$AGE)
class(BRFSS97$AGE)

#Recode sex data into categories corresponding to BRFSS codebook
BRFSS97$SEX <- recode(BRFSS97$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS97$SEX)

BRFSS97$SEX <- as.factor(BRFSS97$SEX)
class(BRFSS97$SEX)

BRFSS97$EDUCA <- recode(BRFSS97$EDUCA,
                        "1" = "Never attended school/only kindergarten",
                        "2" = "Elementary school",
                        "3" = "Some high school",
                        "4" = "High school graduate",
                        "5" = "Some college/some technical school",
                        "6" = "College graduate",
                        "9" = "9")
#Change refused (9) values to NA
(BRFSS97$EDUCA <- unknownToNA(BRFSS97$EDUCA, unknown = "9"))
table(BRFSS97$EDUCA)

BRFSS97$EDUCA <- as.factor(BRFSS97$EDUCA)
class(BRFSS97$EDUCA)

#Recode income into categories corresponding to BRFSS codebook
BRFSS97$INCOME2 <- recode(BRFSS97$INCOME2,
                          "01" = "$10 000 or less",
                          "02" = "$10 000 to $14 999",
                          "03" = "$15 000 to $19 999",
                          "04" = "$20 000 to $24 999",
                          "05" = "$25 000 to $34 999",
                          "06" = "$35 000 to $49 999",
                          "07" = "$50 000 to $74 999",
                          "08" = "$75 000 or more",
                          "77" = "77",
                          "99" = "99")
#Change don't know (77) and (99) values to NA
(BRFSS97$INCOME2 <- unknownToNA(BRFSS97$INCOME2, unknown = c("77", "99")))
table(BRFSS97$INCOME2)

BRFSS97$INCOME2 <- as.factor(BRFSS97$INCOME2)
class(BRFSS97$INCOME2)

BRFSS97$EMPLOY <- recode(BRFSS97$EMPLOY,
                         "1" = "Employed for wages",
                         "2" = "Self-employed",
                         "3" = "Unemployed",
                         "4" = "Unemployed",
                         "5" = "Homemaker",
                         "6" = "Student",
                         "7" = "Retired",
                         "8" = "Unable to work",
                         "9" = "9")
#Change refused (9) to NA
(BRFSS97$EMPLOY <- unknownToNA(BRFSS97$EMPLOY, unknown = "9"))
table(BRFSS97$EMPLOY)

BRFSS97$EMPLOY <- as.factor(BRFSS97$EMPLOY)
class(BRFSS97$EMPLOY)

#BMI
#one implied decimal place
BRFSS97 <- BRFSS97 %>%
  mutate(`_BMI` = `_BMI`/10)

BRFSS97$`_BMI` <- unknownToNA(BRFSS97$`_BMI`, unknown = "99.9")
BRFSS97 %>% top_n(10, `_BMI`)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS97_dataset <- subset(BRFSS97,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS97_dataset$`_FINALWT`)
#The sum of the weights is 13 483 408, MODULE

#Check the number of people (unique PSU's)
length(unique(BRFSS97_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 6516

#Check the number of unique strata
length(unique(BRFSS97_dataset[["_STSTR"]]))
#The number of unique strata is 12

#Used to generate frequency tables (unweighted)

table(subset(BRFSS97_dataset, AGE == "Age 18 to 24")$SEX)
table(subset(BRFSS97_dataset, AGE == "Age 25 to 29")$SEX)
table(subset(BRFSS97_dataset, AGE == "Age 30 to 34")$SEX)
table(subset(BRFSS97_dataset, AGE == "Age 35 to 39")$SEX)
table(subset(BRFSS97_dataset, AGE == "Age 40 to 44")$SEX)
table(subset(BRFSS97_dataset, AGE == "Age 45 to 49")$SEX)
table(subset(BRFSS97_dataset, AGE == "Age 50 to 54")$SEX)
table(subset(BRFSS97_dataset, AGE == "Age 55 to 59")$SEX)
table(subset(BRFSS97_dataset, AGE == "Age 60 to 64")$SEX)
table(subset(BRFSS97_dataset, AGE == "Age 65 to 69")$SEX)
table(subset(BRFSS97_dataset, AGE == "Age 70 and above")$SEX)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS97_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS97_dataset)
#Observe the design oject
BRFSS97_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(BRFSS97_DO,
        file = "BRFSS97_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)
#_____________________________________________________________________________________________________________________________________________________

#ANALYSIS
B97_overall <- svymean(~factor(HAVARTH),
                       BRFSS97_DO,
                       na.rm = TRUE)
B97_overall.c <- B97_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B97_overall_ci <- confint(B97_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B97 <- bind_cols(B97_overall.c, B97_overall_ci)
#remove havarth = 0
B97 <- B97[-c(1), ] #final proportion, se and 95% ci
#backup save
#write.csv(B97, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B97_Arthritis.csv")

#Demographic analysis

#1. Arthritis (RDD) prevalence by age 
B97_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS97_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B97_Arthtitis_age <- B97_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B97_arth_age_ci <- confint(B97_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B97_arth_age_ci <- B97_arth_age_ci[-c(1:11), ]
#join ci and proportions
B97_Arthritis.Age <- bind_cols(B97_Arthtitis_age, B97_arth_age_ci)
#Save
#write.csv(B97_Arthritis.Age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B97_Arthritis.age.csv")

#Age logistic regression
B97_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS97_DO)
summary(B97_age_glm)
exp(cbind(OR=coef(B97_age_glm), confint(B97_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B97_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS97_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B97_Arthritis_sex <- B97_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B97_arth_sex_ci <- confint(B97_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B97_arth_sex_ci <- B97_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B97_Arthritis.Sex <- bind_cols(B97_Arthritis_sex, B97_arth_sex_ci)
#save
#write.csv(B97_Arthritis.Sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B97_Arthritis.sex.csv")

#Sex logistic regression
B97_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS97_DO)
summary(B97_sex_glm)
exp(cbind(OR=coef(B97_sex_glm), confint(B97_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B97_Arth_emp <- svyby(formula = ~HAVARTH,
                      by = ~EMPLOY,
                      design = BRFSS97_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B97_Arthritis_emp <- B97_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B97_arth_emp_ci <- confint(B97_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B97_arth_emp_ci <- B97_arth_emp_ci[-c(1:7), ]
#join ci and proportion
B97_Arthritis.Employ <- bind_cols(B97_Arthritis_emp, B97_arth_emp_ci)
#save
#write.csv(B97_Arthritis.Employ, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B97_Arthritis.employment.csv")

#Employment status logistic regression
B97_emp_glm <- svyglm(HAVARTH~EMPLOY + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS97_DO)
summary(B97_emp_glm)
exp(cbind(OR=coef(B97_emp_glm), confint(B97_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B97_inc_glm <- svyglm(HAVARTH~relevel(INCOME2, ref = "$75 000 or more") + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS97_DO)
summary(B97_inc_glm)
exp(cbind(OR=coef(B97_inc_glm), confint(B97_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B97_edu_glm <- svyglm(HAVARTH~EDUCA + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS97_DO)
summary(B97_edu_glm)
exp(cbind(OR=coef(B97_edu_glm), confint(B97_edu_glm)))

#------------------------------------------------------------------------------------------------------------------------

#Lifestyle analysis

#1. Arthritis (RDD) by BMI (logistic regression)
B97.BMI.glm <- svyglm(HAVARTH ~ `_BMI` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS97_DO)
summary(B97.BMI.glm)
exp(cbind(OR=coef(B97.BMI.glm), confint(B97.BMI.glm)))

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


# == 1998 == #

#DOWNLOAD

B98_url <- "http://www.cdc.gov/brfss/annual_data/1998/files/CDBRFS98XPT.zip"

tempB98 <- tempfile()
tempB98b <- tempfile()

download.file(B98_url, tempB98, mode = "wb")
unzip(zipfile = tempB98, exdir = tempB98b)
BRFSS98_raw <- read_xpt(file.path(tempB98b, "CDBRFS98.XPT"))

saveRDS(object = BRFSS98_raw,
        file = "BRFSS_1998.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

unlink(c(tempB98, tempB98b))

#_____________________________________________________________________________________________________________________________________

#CLEANING

#Observe the dataset
str(BRFSS98_raw) 
tail(BRFSS98_raw) 
glimpse(BRFSS98_raw) 
colnames(BRFSS98_raw)

#Units and sampling factors; states; demographic information; socio-economic status; mental health; arthritis diagnosis; exercise; sleep; quality of life variables; healthcare-seeking behaviour; substance habits; confounders
BRFSS98 <- select(BRFSS98_raw,
                  "_PSU", "_FINALWT", "_STSTR",
                  "AGE", "SEX",
                  "EDUCA", "EMPLOY", "INCOME2",
                  "HAVARTH",
                  "_BMI") 

#Observations
str(BRFSS98)
tail(BRFSS98)
glimpse(BRFSS98)
colnames(BRFSS98)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS98))
#Lots of missing arthritis data due to module
which(colSums(is.na(BRFSS98)) == nrow(BRFSS98))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Now have a look at the arthritis variable (HAVARTH)
#Observe how many people answered yes, no, don't know, & refused
table(BRFSS98$HAVARTH)

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS98$HAVARTH <- recode(BRFSS98$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS98$HAVARTH <- unknownToNA(BRFSS98$HAVARTH, unknown = c("7", "9")))
table(BRFSS98$HAVARTH)

BRFSS98$HAVARTH <- as.factor(BRFSS98$HAVARTH)
class(BRFSS98$HAVARTH)

table(BRFSS98$AGE)
#From the above command, I saw that this data is continuous. However, in order for us to make meaningful comparisons with other years, I will alter the variables into categorical ranges in the same way as later BRFSS datasets
#Recode age data into categories corresponding to BRFSS codebook
BRFSS98$AGE<- recode(BRFSS98$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS98$AGE <- unknownToNA(BRFSS98$AGE, unknown = c("7", "9")))
table(BRFSS98$AGE)

BRFSS98$AGE <- as.factor(BRFSS98$AGE)
class(BRFSS98$AGE)

#Recode sex data into categories corresponding to BRFSS codebook
BRFSS98$SEX <- recode(BRFSS98$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS98$SEX)

BRFSS98$SEX <- as.factor(BRFSS98$SEX)
class(BRFSS98$SEX)

BRFSS98$EDUCA <- recode(BRFSS98$EDUCA,
                        "1" = "Never attended school/only kindergarten",
                        "2" = "Elementary school",
                        "3" = "Some high school",
                        "4" = "High school graduate",
                        "5" = "Some college/some technical school",
                        "6" = "College graduate",
                        "9" = "9")
#Change refused (9) values to NA
(BRFSS98$EDUCA <- unknownToNA(BRFSS98$EDUCA, unknown = "9"))
table(BRFSS98$EDUCA)

BRFSS98$EDUCA <- as.factor(BRFSS98$EDUCA)
class(BRFSS98$EDUCA)

#Recode income into categories corresponding to BRFSS codebook
BRFSS98$INCOME2 <- recode(BRFSS98$INCOME2,
                          "1" = "$10 000 or less",
                          "2" = "$10 000 to $14 999",
                          "3" = "$15 000 to $19 999",
                          "4" = "$20 000 to $24 999",
                          "5" = "$25 000 to $34 999",
                          "6" = "$35 000 to $49 999",
                          "7" = "$50 000 to $74 999",
                          "8" = "$75 000 or more",
                          "77" = "77",
                          "99" = "99")
#Change don't know (77) and (99) values to NA
(BRFSS98$INCOME2 <- unknownToNA(BRFSS98$INCOME2, unknown = c("77", "99")))
table(BRFSS98$INCOME2)

BRFSS98$INCOME2 <- as.factor(BRFSS98$INCOME2)
class(BRFSS98$INCOME2)

BRFSS98$EMPLOY <- recode(BRFSS98$EMPLOY,
                         "1" = "Employed for wages",
                         "2" = "Self-employed",
                         "3" = "Unemployed",
                         "4" = "Unemployed",
                         "5" = "Homemaker",
                         "6" = "Student",
                         "7" = "Retired",
                         "8" = "Unable to work",
                         "9" = "9")
#Change refused (9) to NA
(BRFSS98$EMPLOY <- unknownToNA(BRFSS98$EMPLOY, unknown = "9"))
table(BRFSS98$EMPLOY)

BRFSS98$EMPLOY <- as.factor(BRFSS98$EMPLOY)
class(BRFSS98$EMPLOY)

#BMI
#one implied decimal place
BRFSS98 <- BRFSS98 %>%
  mutate(`_BMI` = `_BMI`/10)

BRFSS98$`_BMI` <- unknownToNA(BRFSS98$`_BMI`, unknown = "99.9")
BRFSS98 %>% top_n(10, `_BMI`)

#______________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS98_dataset <- subset(BRFSS98,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS98_dataset$`_FINALWT`)
#The sum of the weights is 22 910 818, MODULE

#Check the number of people (unique PSU's)
length(unique(BRFSS98_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 8483

#Check the number of unique strata
length(unique(BRFSS98_dataset[["_STSTR"]]))
#The number of unique strata is 24

#Used to generate frequency tables (unweighted)

table(subset(BRFSS98_dataset, AGE == "Age 18 to 24")$SEX)
table(subset(BRFSS98_dataset, AGE == "Age 25 to 29")$SEX)
table(subset(BRFSS98_dataset, AGE == "Age 30 to 34")$SEX)
table(subset(BRFSS98_dataset, AGE == "Age 35 to 39")$SEX)
table(subset(BRFSS98_dataset, AGE == "Age 40 to 44")$SEX)
table(subset(BRFSS98_dataset, AGE == "Age 45 to 49")$SEX)
table(subset(BRFSS98_dataset, AGE == "Age 50 to 54")$SEX)
table(subset(BRFSS98_dataset, AGE == "Age 55 to 59")$SEX)
table(subset(BRFSS98_dataset, AGE == "Age 60 to 64")$SEX)
table(subset(BRFSS98_dataset, AGE == "Age 65 to 69")$SEX)
table(subset(BRFSS98_dataset, AGE == "Age 70 and above")$SEX)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS98_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS98_dataset)
#Observe the design oject
BRFSS98_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(BRFSS98_DO,
        file = "BRFSS98_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_____________________________________________________________________________________________________________________________________

#ANALYSIS
B98_overall <- svymean(~factor(HAVARTH),
                       BRFSS98_DO,
                       na.rm = TRUE)
B98_overall.c <- B98_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B98_overall_ci <- confint(B98_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B98 <- bind_cols(B98_overall.c, B98_overall_ci)
#remove havarth = 0
B98 <- B98[-c(1), ] #final proportion, se and 95% ci
#backup save
#write.csv(B98, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B98_Arthritis.csv")

#Demographic analysis

#1. Arthritis (RDD) prevalence by age 
B98_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS98_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B98_Arthtitis_age <- B98_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B98_arth_age_ci <- confint(B98_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B98_arth_age_ci <- B98_arth_age_ci[-c(1:11), ]
#join ci and proportions
B98_Arthritis.Age <- bind_cols(B98_Arthtitis_age, B98_arth_age_ci)
#Save
#write.csv(B98_Arthritis.Age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B98_Arthritis.age.csv")

#Age logistic regression
B98_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS98_DO)
summary(B98_age_glm)
exp(cbind(OR=coef(B98_age_glm), confint(B98_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B98_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS98_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B98_Arthritis_sex <- B98_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B98_arth_sex_ci <- confint(B98_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B98_arth_sex_ci <- B98_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B98_Arthritis.Sex <- bind_cols(B98_Arthritis_sex, B98_arth_sex_ci)
#save
#write.csv(B98_Arthritis.Sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B98_Arthritis.sex.csv")

#Sex logistic regression
B98_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS98_DO)
summary(B98_sex_glm)
exp(cbind(OR=coef(B98_sex_glm), confint(B98_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B98_Arth_emp <- svyby(formula = ~HAVARTH,
                      by = ~EMPLOY,
                      design = BRFSS98_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B98_Arthritis_emp <- B98_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B98_arth_emp_ci <- confint(B98_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B98_arth_emp_ci <- B98_arth_emp_ci[-c(1:7), ]
#join ci and proportion
B98_Arthritis.Employ <- bind_cols(B98_Arthritis_emp, B98_arth_emp_ci)
#save
#write.csv(B98_Arthritis.Employ, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B98_Arthritis.employment.csv")

#Employment status logistic regression
B98_emp_glm <- svyglm(HAVARTH~EMPLOY + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS98_DO)
summary(B98_emp_glm)
exp(cbind(OR=coef(B98_emp_glm), confint(B98_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B98_inc_glm <- svyglm(HAVARTH~relevel(INCOME2, ref = "$75 000 or more") + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS98_DO)
summary(B98_inc_glm)
exp(cbind(OR=coef(B98_inc_glm), confint(B98_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B98_edu_glm <- svyglm(HAVARTH~EDUCA + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS98_DO)
summary(B98_edu_glm)
exp(cbind(OR=coef(B98_edu_glm), confint(B98_edu_glm)))

#------------------------------------------------------------------------------------------------------------------------

#Lifestyle analysis

#1. Arthritis (RDD) by BMI (logistic regression)
B98.BMI.glm <- svyglm(HAVARTH ~ `_BMI` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS98_DO)
summary(B98.BMI.glm)
exp(cbind(OR=coef(B98.BMI.glm), confint(B98.BMI.glm)))

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


# == 1999 == #

#DOWNLOAD

B99_url <- "http://www.cdc.gov/brfss/annual_data/1999/files/CDBRFS99XPT.zip"

tempB99 <- tempfile()
tempB99b <- tempfile()

download.file(B99_url, tempB99, mode = "wb")
unzip(zipfile = tempB99, exdir = tempB99b)
BRFSS99_raw <- read_xpt(file.path(tempB99b, "CDBRFS99.XPT"))

saveRDS(object = BRFSS99_raw,
        file = "BRFSS_1999.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

unlink(c(tempB99, tempB99b))

#____________________________________________________________________

#CLEANING

#Observe the dataset
str(BRFSS99_raw) 
tail(BRFSS99_raw) 
glimpse(BRFSS99_raw) 
colnames(BRFSS99_raw)

#Units and sampling factors; states; demographic information; socio-economic status; mental health; arthritis diagnosis; exercise; sleep; quality of life variables; healthcare-seeking behaviour; substance habits; confounders
BRFSS99 <- select(BRFSS99_raw,
                  "_PSU", "_FINALWT", "_STSTR",
                  "AGE", "SEX",
                  "EDUCA", "EMPLOY", "INCOME2",
                  "HAVARTH",
                  "_BMI")


#Observations
str(BRFSS99)
tail(BRFSS99)
glimpse(BRFSS99)
colnames(BRFSS99)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS99))
#Lots of arthritis data missing due to module
which(colSums(is.na(BRFSS99)) == nrow(BRFSS99))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Now have a look at the arthritis variable (HAVARTH3)
#Observe how many people answered yes, no, don't know, & refused
table(BRFSS99$HAVARTH)
#Module, so data looks as follows:     
#1     2     7     9 
#5870 15978    66   309
#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS99$HAVARTH <- recode(BRFSS99$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS99$HAVARTH <- unknownToNA(BRFSS99$HAVARTH, unknown = c("7", "9")))
table(BRFSS99$HAVARTH)

BRFSS99$HAVARTH <- as.factor(BRFSS99$HAVARTH)
class(BRFSS99$HAVARTH)

table(BRFSS99$AGE)
#From the above command, I saw that this data is continuous. However, in order for us to make meaningful comparisons with other years, I will alter the variables into categorical ranges in the same way as later BRFSS datasets
#Recode age data into categories corresponding to BRFSS codebook
BRFSS99$AGE<- recode(BRFSS99$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS99$AGE <- unknownToNA(BRFSS99$AGE, unknown = c("7", "9")))
table(BRFSS99$AGE)

BRFSS99$AGE <- as.factor(BRFSS99$AGE)
class(BRFSS99$AGE)

#Recode sex data into categories corresponding to BRFSS codebook
BRFSS99$SEX <- recode(BRFSS99$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS99$SEX)

BRFSS99$SEX <- as.factor(BRFSS99$SEX)
class(BRFSS99$SEX)

BRFSS99$EDUCA <- recode(BRFSS99$EDUCA,
                        "1" = "Never attended school/only kindergarten",
                        "2" = "Elementary school",
                        "3" = "Some high school",
                        "4" = "High school graduate",
                        "5" = "Some college/some technical school",
                        "6" = "College graduate",
                        "9" = "9")
#Change refused (9) values to NA
(BRFSS99$EDUCA <- unknownToNA(BRFSS99$EDUCA, unknown = "9"))
table(BRFSS99$EDUCA)

BRFSS99$EDUCA <- as.factor(BRFSS99$EDUCA)
class(BRFSS99$EDUCA)

#Recode income into categories corresponding to BRFSS codebook
BRFSS99$INCOME2 <- recode(BRFSS99$INCOME2,
                          "1" = "$10 000 or less",
                          "2" = "$10 000 to $14 999",
                          "3" = "$15 000 to $19 999",
                          "4" = "$20 000 to $24 999",
                          "5" = "$25 000 to $34 999",
                          "6" = "$35 000 to $49 999",
                          "7" = "$50 000 to $74 999",
                          "8" = "$75 000 or more",
                          "77" = "77",
                          "99" = "99")
#Change don't know (77) and (99) values to NA
(BRFSS99$INCOME2 <- unknownToNA(BRFSS99$INCOME2, unknown = c("77", "99")))
table(BRFSS99$INCOME2)

BRFSS99$INCOME2 <- as.factor(BRFSS99$INCOME2)
class(BRFSS99$INCOME2)

BRFSS99$EMPLOY <- recode(BRFSS99$EMPLOY,
                         "1" = "Employed for wages",
                         "2" = "Self-employed",
                         "3" = "Unemployed",
                         "4" = "Unemployed",
                         "5" = "Homemaker",
                         "6" = "Student",
                         "7" = "Retired",
                         "8" = "Unable to work",
                         "9" = "9")
#Change refused (9) to NA
(BRFSS99$EMPLOY <- unknownToNA(BRFSS99$EMPLOY, unknown = "9"))
table(BRFSS99$EMPLOY)

BRFSS99$EMPLOY <- as.factor(BRFSS99$EMPLOY)
class(BRFSS99$EMPLOY)

#BMI
#one implied decimal place
BRFSS99 <- BRFSS99 %>%
  mutate(`_BMI` = `_BMI`/10)

BRFSS99$`_BMI` <- unknownToNA(BRFSS99$`_BMI`, unknown = "99.9")
BRFSS99 %>% top_n(10, `_BMI`)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS99_dataset <- subset(BRFSS99,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS99_dataset$`_FINALWT`)
#The sum of the weights is 23 533 065, MODULE

#Check the number of people (unique PSU's)
length(unique(BRFSS99_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 14 057

#Check the number of unique strata
length(unique(BRFSS99_dataset[["_STSTR"]]))
#The number of unique strata is 28

#Used to generate frequency tables (unweighted)

table(subset(BRFSS99_dataset, AGE == "Age 18 to 24")$SEX)
table(subset(BRFSS99_dataset, AGE == "Age 25 to 29")$SEX)
table(subset(BRFSS99_dataset, AGE == "Age 30 to 34")$SEX)
table(subset(BRFSS99_dataset, AGE == "Age 35 to 39")$SEX)
table(subset(BRFSS99_dataset, AGE == "Age 40 to 44")$SEX)
table(subset(BRFSS99_dataset, AGE == "Age 45 to 49")$SEX)
table(subset(BRFSS99_dataset, AGE == "Age 50 to 54")$SEX)
table(subset(BRFSS99_dataset, AGE == "Age 55 to 59")$SEX)
table(subset(BRFSS99_dataset, AGE == "Age 60 to 64")$SEX)
table(subset(BRFSS99_dataset, AGE == "Age 65 to 69")$SEX)
table(subset(BRFSS99_dataset, AGE == "Age 70 and above")$SEX)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS99_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS99_dataset)
#Observe the design oject
BRFSS99_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(BRFSS99_DO,
        file = "BRFSS99_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#____________________________________________________________________________________________

#ANALYSIS
B99_overall <- svymean(~factor(HAVARTH),
                       BRFSS99_DO,
                       na.rm = TRUE)
B99_overall.c <- B99_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B99_overall_ci <- confint(B99_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B99 <- bind_cols(B99_overall.c, B99_overall_ci)
#remove havarth = 0
B99 <- B99[-c(1), ] #final proportion, se and 95% ci
#backup save
#write.csv(B99, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B99_Arthritis.csv")

#Demographic analysis

#1. Arthritis (RDD) prevalence by age 
B99_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS99_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B99_Arthtitis_age <- B99_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B99_arth_age_ci <- confint(B99_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B99_arth_age_ci <- B99_arth_age_ci[-c(1:11), ]
#join ci and proportions
B99_Arthritis.Age <- bind_cols(B99_Arthtitis_age, B99_arth_age_ci)
#Save
#write.csv(B99_Arthritis.Age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B99_Arthritis.age.csv")

#Age logistic regression
B99_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS99_DO)
summary(B99_age_glm)
exp(cbind(OR=coef(B99_age_glm), confint(B99_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B99_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS99_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B99_Arthritis_sex <- B99_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B99_arth_sex_ci <- confint(B99_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B99_arth_sex_ci <- B99_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B99_Arthritis.Sex <- bind_cols(B99_Arthritis_sex, B99_arth_sex_ci)
#save
#write.csv(B99_Arthritis.Sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B99_Arthritis.sex.csv")

#Sex logistic regression
B99_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS99_DO)
summary(B99_sex_glm)
exp(cbind(OR=coef(B99_sex_glm), confint(B99_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B99_Arth_emp <- svyby(formula = ~HAVARTH,
                      by = ~EMPLOY,
                      design = BRFSS99_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B99_Arthritis_emp <- B99_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B99_arth_emp_ci <- confint(B99_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B99_arth_emp_ci <- B99_arth_emp_ci[-c(1:7), ]
#join ci and proportion
B99_Arthritis.Employ <- bind_cols(B99_Arthritis_emp, B99_arth_emp_ci)
#save
#write.csv(B99_Arthritis.Employ, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B99_Arthritis.employment.csv")

#Employment status logistic regression
B99_emp_glm <- svyglm(HAVARTH~EMPLOY + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS99_DO)
summary(B99_emp_glm)
exp(cbind(OR=coef(B99_emp_glm), confint(B99_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B99_inc_glm <- svyglm(HAVARTH~relevel(INCOME2, ref = "$75 000 or more") + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS99_DO)
summary(B99_inc_glm)
exp(cbind(OR=coef(B99_inc_glm), confint(B99_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B99_edu_glm <- svyglm(HAVARTH~EDUCA + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS99_DO)
summary(B99_edu_glm)
exp(cbind(OR=coef(B99_edu_glm), confint(B99_edu_glm)))

#------------------------------------------------------------------------------------------------------------------------

#Lifestyle analysis

#1. Arthritis (RDD) by BMI (logistic regression)
B99.BMI.glm <- svyglm(HAVARTH ~ `_BMI` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS99_DO)
summary(B99.BMI.glm)
exp(cbind(OR=coef(B99.BMI.glm), confint(B99.BMI.glm)))


#____________________________________________________________________________________________
#____________________________________________________________________________________________

# == 2000 == #

#DOWNLOAD

B00_url <- "http://www.cdc.gov/brfss/annual_data/2000/files/CDBRFS00XPT.ZIP"

tempB00 <- tempfile()
tempB00b <- tempfile()

download.file(B00_url, tempB00, mode = "wb")
unzip(zipfile = tempB00, exdir = tempB00b)
BRFSS00_raw <- read_xpt(file.path(tempB00b, "CDBRFS00.XPT"))

saveRDS(object = BRFSS00_raw,
        file = "BRFSS_2000.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

unlink(c(tempB00, tempB00b))

#_____________________________________________________________________________________________

#CLEANING

#Observe the dataset
str(BRFSS00_raw) 
tail(BRFSS00_raw) 
glimpse(BRFSS00_raw) 
colnames(BRFSS00_raw)

BRFSS00 <- select(BRFSS00_raw,
                  "_PSU", "_FINALWT", "_STSTR",
                  "AGE", "SEX",
                  "EDUCA", "EMPLOY", "INCOME2",
                  "HAVARTH",
                  "_BMI2") 

#Observations
str(BRFSS00)
tail(BRFSS00)
glimpse(BRFSS00)
colnames(BRFSS00)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS00))
#Fair amount of arthritis data missing due to module
which(colSums(is.na(BRFSS00)) == nrow(BRFSS00))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Now have a look at the arthritis variable (HAVARTH)
#Observe how many people answered yes, no, don't know, & refused
table(BRFSS00$HAVARTH)
#This is a module so original data looks as follows:
#1     2     7     9 
#29776 89954   371  1436 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS00$HAVARTH <- recode(BRFSS00$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS00$HAVARTH <- unknownToNA(BRFSS00$HAVARTH, unknown = c("7", "9")))
table(BRFSS00$HAVARTH)

BRFSS00$HAVARTH <- as.factor(BRFSS00$HAVARTH)
class(BRFSS00$HAVARTH)

table(BRFSS00$AGE)
#From the above command, I saw that this data is continuous. However, in order for us to make meaningful comparisons with other years, I will alter the variables into categorical ranges in the same way as later BRFSS datasets
#Recode age data into categories corresponding to BRFSS codebook
BRFSS00$AGE<- recode(BRFSS00$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "97" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS00$AGE <- unknownToNA(BRFSS00$AGE, unknown = c("7", "9")))
table(BRFSS00$AGE)

BRFSS00$AGE <- as.factor(BRFSS00$AGE)
class(BRFSS00$AGE)

#Recode sex data into categories corresponding to BRFSS codebook
BRFSS00$SEX <- recode(BRFSS00$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS00$SEX)

BRFSS00$SEX <- as.factor(BRFSS00$SEX)
class(BRFSS00$SEX)

BRFSS00$EDUCA <- recode(BRFSS00$EDUCA,
                        "1" = "Never attended school/only kindergarten",
                        "2" = "Elementary school",
                        "3" = "Some high school",
                        "4" = "High school graduate",
                        "5" = "Some college/some technical school",
                        "6" = "College graduate",
                        "9" = "9")
#Change refused (9) values to NA
(BRFSS00$EDUCA <- unknownToNA(BRFSS00$EDUCA, unknown = "9"))
table(BRFSS00$EDUCA)

BRFSS00$EDUCA <- as.factor(BRFSS00$EDUCA)
class(BRFSS00$EDUCA)

#Recode income into categories corresponding to BRFSS codebook
BRFSS00$INCOME2 <- recode(BRFSS00$INCOME2,
                          "01" = "< $10 000",
                          "02" = "$10 000 to $14 999",
                          "03" = "$15 000 to $19 999",
                          "04" = "$20 000 to $24 999",
                          "05" = "$25 000 to $34 999",
                          "06" = "$35 000 to $49 999",
                          "07" = "$50 000 to $74 999",
                          "08" = ">= $75 0000",
                          "77" = "77",
                          "99" = "99")
#Change don't know (77) and (99) values to NA
(BRFSS00$INCOME2 <- unknownToNA(BRFSS00$INCOME2, unknown = c("77", "99")))
table(BRFSS00$INCOME2)

BRFSS00$INCOME2 <- as.factor(BRFSS00$INCOME2)
class(BRFSS00$INCOME2)

BRFSS00$EMPLOY <- recode(BRFSS00$EMPLOY,
                         "1" = "Employed for wages",
                         "2" = "Self-employed",
                         "3" = "Unemployed",
                         "4" = "Unemployed",
                         "5" = "Homemaker",
                         "6" = "Student",
                         "7" = "Retired",
                         "8" = "Unable to work",
                         "9" = "9")
#Change refused (9) to NA
(BRFSS00$EMPLOY <- unknownToNA(BRFSS00$EMPLOY, unknown = "9"))
table(BRFSS00$EMPLOY)

BRFSS00$EMPLOY <- as.factor(BRFSS00$EMPLOY)
class(BRFSS00$EMPLOY)

#BMI
#one implied decimal place
BRFSS00 <- BRFSS00 %>%
  mutate(`_BMI2` = `_BMI2`/10)

BRFSS00$`_BMI2` <- unknownToNA(BRFSS00$`_BMI2`, unknown = "99.9")
BRFSS00 %>% top_n(10, `_BMI2`)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS00_dataset <- subset(BRFSS00,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS00_dataset$`_FINALWT`)
#The sum of the weights is 142 187 598, which is acceptable FOR A MODULE

#Check the number of people (unique PSU's)
length(unique(BRFSS00_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 43 744

#Check the number of unique strata
length(unique(BRFSS00_dataset[["_STSTR"]]))
#The number of unique strata is 213

#Used to generate frequency tables (unweighted)

table(subset(BRFSS00_dataset, AGE == "Age 18 to 24")$SEX)
table(subset(BRFSS00_dataset, AGE == "Age 25 to 29")$SEX)
table(subset(BRFSS00_dataset, AGE == "Age 30 to 34")$SEX)
table(subset(BRFSS00_dataset, AGE == "Age 35 to 39")$SEX)
table(subset(BRFSS00_dataset, AGE == "Age 40 to 44")$SEX)
table(subset(BRFSS00_dataset, AGE == "Age 45 to 49")$SEX)
table(subset(BRFSS00_dataset, AGE == "Age 50 to 54")$SEX)
table(subset(BRFSS00_dataset, AGE == "Age 55 to 59")$SEX)
table(subset(BRFSS00_dataset, AGE == "Age 60 to 64")$SEX)
table(subset(BRFSS00_dataset, AGE == "Age 65 to 69")$SEX)
table(subset(BRFSS00_dataset, AGE == "Age 70 and above")$SEX)


#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS00_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS00_dataset)
#Observe the design oject
BRFSS00_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(BRFSS00_DO,
        file = "BRFSS00_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#____________________________________________________________________________________________

#ANALYSIS

B00_overall <- svymean(~factor(HAVARTH),
                       BRFSS00_DO,
                       na.rm = TRUE)
B00_overall.c <- B00_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B00_overall_ci <- confint(B00_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B00 <- bind_cols(B00_overall.c, B00_overall_ci)
#remove havarth = 0
B00 <- B00[-c(1), ] #final proportion, se and 95% ci
#backup save
#write.csv(B00, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B00_Arthritis.csv")

#Demographic analysis

#1. Arthritis (RDD) prevalence by age 
B00_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS00_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B00_Arthtitis_age <- B00_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B00_arth_age_ci <- confint(B00_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B00_arth_age_ci <- B00_arth_age_ci[-c(1:11), ]
#join ci and proportions
B00_Arthritis.Age <- bind_cols(B00_Arthtitis_age, B00_arth_age_ci)
#Save
#write.csv(B00_Arthritis.Age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B00_Arthritis.age.csv")

#Age logistic regression
B00_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS00_DO)
summary(B00_age_glm)
exp(cbind(OR=coef(B00_age_glm), confint(B00_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B00_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS00_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B00_Arthritis_sex <- B00_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B00_arth_sex_ci <- confint(B00_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B00_arth_sex_ci <- B00_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B00_Arthritis.Sex <- bind_cols(B00_Arthritis_sex, B00_arth_sex_ci)
#save
#write.csv(B00_Arthritis.Sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B00_Arthritis.sex.csv")

#Sex logistic regression
B00_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS00_DO)
summary(B00_sex_glm)
exp(cbind(OR=coef(B00_sex_glm), confint(B00_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B00_Arth_emp <- svyby(formula = ~HAVARTH,
                      by = ~EMPLOY,
                      design = BRFSS00_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B00_Arthritis_emp <- B00_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B00_arth_emp_ci <- confint(B00_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B00_arth_emp_ci <- B00_arth_emp_ci[-c(1:7), ]
#join ci and proportion
B00_Arthritis.Employ <- bind_cols(B00_Arthritis_emp, B00_arth_emp_ci)
#save
#write.csv(B00_Arthritis.Employ, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B00_Arthritis.employment.csv")

#Employment status logistic regression
B00_emp_glm <- svyglm(HAVARTH~EMPLOY + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS00_DO)
summary(B00_emp_glm)
exp(cbind(OR=coef(B00_emp_glm), confint(B00_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B00_inc_glm <- svyglm(HAVARTH~relevel(INCOME2, ref = ">= $75 0000") + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS00_DO)
summary(B00_inc_glm)
exp(cbind(OR=coef(B00_inc_glm), confint(B00_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B00_edu_glm <- svyglm(HAVARTH~EDUCA + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS00_DO)
summary(B00_edu_glm)
exp(cbind(OR=coef(B00_edu_glm), confint(B00_edu_glm)))

#------------------------------------------------------------------------------------------------------------------------

#Lifestyle analysis

#1. Arthritis (RDD) by BMI (logistic regression)
B00.BMI.glm <- svyglm(HAVARTH ~ `_BMI2` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS00_DO)
summary(B00.BMI.glm)
exp(cbind(OR=coef(B00.BMI.glm), confint(B00.BMI.glm)))

#_____________________________________________________________________________________________
#_____________________________________________________________________________________________

# == 2002 == #

#DOWNLOAD

B02_url <- "http://www.cdc.gov/brfss/annual_data/2002/files/CDBRFS02XPT.ZIP"

tempB02 <- tempfile()
tempB02b <- tempfile()

download.file(B02_url, tempB02, mode = "wb")
unzip(zipfile = tempB02, exdir = tempB02b)
BRFSS02_raw <- read_xpt(file.path(tempB02b, "CDBRFS02.XPT"))

saveRDS(object = BRFSS02_raw,
        file = "BRFSS_2002.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

unlink(c(tempB02, tempB02b))

#_____________________________________________________________________________________________________________________

#CLEANING

#Observe the dataset
str(BRFSS02_raw) 
tail(BRFSS02_raw) 
glimpse(BRFSS02_raw) 
colnames(BRFSS02_raw)

BRFSS02 <- select(BRFSS02_raw,
                  "_PSU", "_FINALWT", "_STSTR",
                  "AGE", "SEX",
                  "EDUCA", "EMPLOY", "INCOME2",
                  "HAVARTH2", 
                  "_BMI2")

#Observations
str(BRFSS02)
tail(BRFSS02)
glimpse(BRFSS02)
colnames(BRFSS02)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS02))
#Lots of missing arthritis due to module year
which(colSums(is.na(BRFSS02)) == nrow(BRFSS02))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Now have a look at the arthritis variable (HAVARTH3)
#Observe how many people answered yes, no, don't know, & refused
table(BRFSS02$HAVARTH2)
#Module, so data looks as follows: 1 = 41 067 || 2 = 89 878 || 7 =  489 || 9 = 2 897

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS02$HAVARTH2 <- recode(BRFSS02$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS02$HAVARTH2 <- unknownToNA(BRFSS02$HAVARTH2, unknown = c("7", "9")))
table(BRFSS02$HAVARTH2)

BRFSS02$HAVARTH2 <- as.factor(BRFSS02$HAVARTH2)
class(BRFSS02$HAVARTH2)

table(BRFSS02$AGE)
#From the above command, I saw that this data is continuous. However, in order for us to make meaningful comparisons with other years, I will alter the variables into categorical ranges in the same way as later BRFSS datasets
#Recode age data into categories corresponding to BRFSS codebook
BRFSS02$AGE<- recode(BRFSS02$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "97" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS02$AGE <- unknownToNA(BRFSS02$AGE, unknown = c("7", "9")))
table(BRFSS02$AGE)

BRFSS02$AGE <- as.factor(BRFSS02$AGE)
class(BRFSS02$AGE)

#Recode sex data into categories corresponding to BRFSS codebook
BRFSS02$SEX <- recode(BRFSS02$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS02$SEX)

BRFSS02$SEX <- as.factor(BRFSS02$SEX)
class(BRFSS02$SEX)

#Recode education data into categories corresponding to BRFSS codebook;
# Note that for the purpose of comparisons across surveys, all education categories have been grouped simiarly to the BRFSS codebook categories
BRFSS02$EDUCA <- recode(BRFSS02$EDUCA,
                        "1" = "Never attended school/only kindergarten",
                        "2" = "Elementary school",
                        "3" = "Some high school",
                        "4" = "High school graduate",
                        "5" = "Some college/some technical school",
                        "6" = "College graduate",
                        "9" = "9")
#Change refused (9) values to NA
(BRFSS02$EDUCA <- unknownToNA(BRFSS02$EDUCA, unknown = "9"))
table(BRFSS02$EDUCA)

BRFSS02$EDUCA <- as.factor(BRFSS02$EDUCA)
class(BRFSS02$EDUCA)

#Recode income into categories corresponding to BRFSS codebook
BRFSS02$INCOME2 <- recode(BRFSS02$INCOME2,
                          "1" = "< $10 000",
                          "2" = "$10 000 to $14 999",
                          "3" = "$15 000 to $19 999",
                          "4" = "$20 000 to $24 999",
                          "5" = "$25 000 to $34 999",
                          "6" = "$35 000 to $49 999",
                          "7" = "$50 000 to $74 999",
                          "8" = ">= $75 0000",
                          "77" = "77",
                          "99" = "99")
#Change don't know (77) and (99) values to NA
(BRFSS02$INCOME2 <- unknownToNA(BRFSS02$INCOME2, unknown = c("77", "99")))
table(BRFSS02$INCOME2)

BRFSS02$INCOME2 <- as.factor(BRFSS02$INCOME2)
class(BRFSS02$INCOME2)

#Recode employment into categories corresponding to BRFSS codebook
# The BRFSS employment data is more comprehensive than the other surveys (ie offers options such as 'student' and 'retired'). Thus, this employment status data is labelled according to the BRFSS codebook precisely
BRFSS02$EMPLOY <- recode(BRFSS02$EMPLOY,
                         "1" = "Employed for wages",
                         "2" = "Self-employed",
                         "3" = "Unemployed",
                         "4" = "Unemployed",
                         "5" = "Homemaker",
                         "6" = "Student",
                         "7" = "Retired",
                         "8" = "Unable to work",
                         "9" = "9")
#Change refused (9) to NA
(BRFSS02$EMPLOY <- unknownToNA(BRFSS02$EMPLOY, unknown = "9"))
table(BRFSS02$EMPLOY)

BRFSS02$EMPLOY <- as.factor(BRFSS02$EMPLOY)
class(BRFSS02$EMPLOY)

#BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS02 <- BRFSS02 %>%
  mutate(`_BMI2` = `_BMI2`/100)
BRFSS02$`_BMI2` <- unknownToNA(BRFSS02$`_BMI2`, unknown = "99.99")
BRFSS02 %>% top_n(10, `_BMI2`)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS02_dataset <- subset(BRFSS02,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH2))

#Check that the sum of the weights is equal to the US population
sum(BRFSS02_dataset$`_FINALWT`)
#The sum of the weights is 126 670 296, which is acceptable FOR A MODULE

#Check the number of people (unique PSU's)
length(unique(BRFSS02_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 51 252

#Check the number of unique strata
length(unique(BRFSS02_dataset[["_STSTR"]]))
#The number of unique strata is 500

#Used to generate frequency tables (unweighted)

table(subset(BRFSS02_dataset, AGE == "Age 18 to 24")$SEX)
table(subset(BRFSS02_dataset, AGE == "Age 25 to 29")$SEX)
table(subset(BRFSS02_dataset, AGE == "Age 30 to 34")$SEX)
table(subset(BRFSS02_dataset, AGE == "Age 35 to 39")$SEX)
table(subset(BRFSS02_dataset, AGE == "Age 40 to 44")$SEX)
table(subset(BRFSS02_dataset, AGE == "Age 45 to 49")$SEX)
table(subset(BRFSS02_dataset, AGE == "Age 50 to 54")$SEX)
table(subset(BRFSS02_dataset, AGE == "Age 55 to 59")$SEX)
table(subset(BRFSS02_dataset, AGE == "Age 60 to 64")$SEX)
table(subset(BRFSS02_dataset, AGE == "Age 65 to 69")$SEX)
table(subset(BRFSS02_dataset, AGE == "Age 70 and above")$SEX)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS02_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS02_dataset)
#Observe the design oject
BRFSS02_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(BRFSS02_DO,
        file = "BRFSS02_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_____________________________________________________________________________________________________________________

#ANALYSIS

B02_overall <- svymean(~factor(HAVARTH2),
                       BRFSS02_DO,
                       na.rm = TRUE)
B02_overall.c <- B02_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B02_overall_ci <- confint(B02_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B02 <- bind_cols(B02_overall.c, B02_overall_ci)
#remove havarth = 0
B02 <- B02[-c(1), ] #final proportion, se and 95% ci
#backup save
#write.csv(B02, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B02_Arthritis.csv")

#Demographic analysis

#1. Arthritis (RDD) prevalence by age 
B02_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B02_Arthtitis_age <- B02_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B02_arth_age_ci <- confint(B02_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B02_arth_age_ci <- B02_arth_age_ci[-c(1:11), ]
#join ci and proportions
B02_Arthritis.Age <- bind_cols(B02_Arthtitis_age, B02_arth_age_ci)
#Save
#write.csv(B02_Arthritis.Age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B02_Arthritis.age.csv")

#Age logistic regression
B02_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS02_DO)
summary(B02_age_glm)
exp(cbind(OR=coef(B02_age_glm), confint(B02_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B02_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B02_Arthritis_sex <- B02_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B02_arth_sex_ci <- confint(B02_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B02_arth_sex_ci <- B02_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B02_Arthritis.Sex <- bind_cols(B02_Arthritis_sex, B02_arth_sex_ci)
#save
#write.csv(B02_Arthritis.Sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B02_Arthritis.sex.csv")

#Sex logistic regression
B02_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS02_DO)
summary(B02_sex_glm)
exp(cbind(OR=coef(B02_sex_glm), confint(B02_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B02_Arth_emp <- svyby(formula = ~HAVARTH2,
                      by = ~EMPLOY,
                      design = BRFSS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B02_Arthritis_emp <- B02_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B02_arth_emp_ci <- confint(B02_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B02_arth_emp_ci <- B02_arth_emp_ci[-c(1:7), ]
#join ci and proportion
B02_Arthritis.Employ <- bind_cols(B02_Arthritis_emp, B02_arth_emp_ci)
#save
#write.csv(B02_Arthritis.Employ, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B02_Arthritis.employment.csv")

#Employment status logistic regression
B02_emp_glm <- svyglm(HAVARTH2~EMPLOY + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS02_DO)
summary(B02_emp_glm)
exp(cbind(OR=coef(B02_emp_glm), confint(B02_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B02_inc_glm <- svyglm(HAVARTH2~relevel(INCOME2, ref = ">= $75 0000") + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS02_DO)
summary(B02_inc_glm)
exp(cbind(OR=coef(B02_inc_glm), confint(B02_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B02_edu_glm <- svyglm(HAVARTH2~EDUCA + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS02_DO)
summary(B02_edu_glm)
exp(cbind(OR=coef(B02_edu_glm), confint(B02_edu_glm)))

#------------------------------------------------------------------------------------------------------------------------

#Lifestyle analysis

#1. Arthritis (RDD) by BMI (logistic regression)
B02.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI2` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS02_DO)
summary(B02.BMI.glm)
exp(cbind(OR=coef(B02.BMI.glm), confint(B02.BMI.glm)))

#_____________________________________________________________________________________________
#_____________________________________________________________________________________________


# == 2004 == #

#DOWNLOAD

B04_url <- "http://www.cdc.gov/brfss/annual_data/2004/files/CDBRFS04XPT.zip"

tempB04 <- tempfile()
tempB04b <- tempfile()

download.file(B04_url, tempB04)
unzip(zipfile = tempB04, exdir = tempB04b)
BRFSS04_raw <- read_xpt(file.path(tempB04b, "CDBRFS04.XPT"))

saveRDS(object = BRFSS04_raw,
        file = "BRFSS_2004.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

unlink(c(tempB04, tempB04b))

#CLEANING

#Observe the dataset
str(BRFSS04_raw) 
tail(BRFSS04_raw) 
glimpse(BRFSS04_raw) 
colnames(BRFSS04_raw)

#Select relevant variables
BRFSS04 <- select(BRFSS04_raw,
                  "_PSU", "_FINALWT", "_STSTR",
                  "AGE", "SEX",
                  "EDUCA", "EMPLOY", "INCOME2",
                  "HAVARTH2",
                  "_BMI4") 

#Observations
str(BRFSS04)
tail(BRFSS04)
glimpse(BRFSS04)
colnames(BRFSS04)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS04))
#Lots of arthritis data missing due to module year
which(colSums(is.na(BRFSS04)) == nrow(BRFSS04))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Now have a look at the arthritis variable (HAVARTH2)
#Observe how many people answered yes, no, don't know, & refused
table(BRFSS04$HAVARTH2)
#This is a module section, therefore numbers look like this:
# 1 = 17 763  ||   2 = 33 223 || 7 = 172 ||   9  = 12

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS04$HAVARTH2 <- recode(BRFSS04$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS04$HAVARTH2 <- unknownToNA(BRFSS04$HAVARTH2, unknown = c("7", "9")))
table(BRFSS04$HAVARTH2)

BRFSS04$HAVARTH2 <- as.factor(BRFSS04$HAVARTH2)
class(BRFSS04$HAVARTH2)

table(BRFSS04$AGE)
#From the above command, I saw that this data is continuous. However, in order for us to make meaningful comparisons with other years, I will alter the variables into categorical ranges in the same way as later BRFSS datasets
#Recode age data into categories corresponding to BRFSS codebook
BRFSS04$AGE<- recode(BRFSS04$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "97" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS04$AGE <- unknownToNA(BRFSS04$AGE, unknown = c("7", "9")))
table(BRFSS04$AGE)

BRFSS04$AGE <- as.factor(BRFSS04$AGE)
class(BRFSS04$AGE)

#Recode sex data into categories corresponding to BRFSS codebook
BRFSS04$SEX <- recode(BRFSS04$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS04$SEX)

BRFSS04$SEX <- as.factor(BRFSS04$SEX)
class(BRFSS04$SEX)

BRFSS04$EDUCA <- recode(BRFSS04$EDUCA,
                        "1" = "Never attended school/only kindergarten",
                        "2" = "Elementary school",
                        "3" = "Some high school",
                        "4" = "High school graduate",
                        "5" = "Some college/some technical school",
                        "6" = "College graduate",
                        "9" = "9")
#Change refused (9) values to NA
(BRFSS04$EDUCA <- unknownToNA(BRFSS04$EDUCA, unknown = "9"))
table(BRFSS04$EDUCA)

BRFSS04$EDUCA <- as.factor(BRFSS04$EDUCA)
class(BRFSS04$EDUCA)

#Recode income into categories corresponding to BRFSS codebook
BRFSS04$INCOME2 <- recode(BRFSS04$INCOME2,
                          "1" = "< $10 000",
                          "2" = "$10 000 to $14 999",
                          "3" = "$15 000 to $19 999",
                          "4" = "$20 000 to $24 999",
                          "5" = "$25 000 to $34 999",
                          "6" = "$35 000 to $49 999",
                          "7" = "$50 000 to $74 999",
                          "8" = ">= $75 0000",
                          "77" = "77",
                          "99" = "99")
#Change don't know (77) and (99) values to NA
(BRFSS04$INCOME2 <- unknownToNA(BRFSS04$INCOME2, unknown = c("77", "99")))
table(BRFSS04$INCOME2)

BRFSS04$INCOME2 <- as.factor(BRFSS04$INCOME2)
class(BRFSS04$INCOME2)

BRFSS04$EMPLOY <- recode(BRFSS04$EMPLOY,
                         "1" = "Employed for wages",
                         "2" = "Self-employed",
                         "3" = "Unemployed",
                         "4" = "Unemployed",
                         "5" = "Homemaker",
                         "6" = "Student",
                         "7" = "Retired",
                         "8" = "Unable to work",
                         "9" = "9")
#Change refused (9) to NA
(BRFSS04$EMPLOY <- unknownToNA(BRFSS04$EMPLOY, unknown = "9"))
table(BRFSS04$EMPLOY)

BRFSS04$EMPLOY <- as.factor(BRFSS04$EMPLOY)
class(BRFSS04$EMPLOY)

#BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS04$`_BMI4` <- unknownToNA(BRFSS04$`_BMI4`, unknown = "9999")
BRFSS04 %>% top_n(20, `_BMI4`)
BRFSS04 <- BRFSS04 %>%
  mutate(`_BMI4` = `_BMI4`/100)
BRFSS04$`_BMI4`
#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS04_dataset <- subset(BRFSS04,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH2))

#Check that the sum of the weights is equal to the US population
sum(BRFSS04_dataset$`_FINALWT`)
#The sum of the weights is 45 088 268, which is acceptable FOR A MODULE

#Check the number of people (unique PSU's)
length(unique(BRFSS04_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 34 156

#Check the number of unique strata
length(unique(BRFSS04_dataset[["_STSTR"]]))
#The number of unique strata is 144

#Used to generate frequency tables (unweighted)

table(subset(BRFSS04_dataset, AGE == "Age 18 to 24")$SEX)
table(subset(BRFSS04_dataset, AGE == "Age 25 to 29")$SEX)
table(subset(BRFSS04_dataset, AGE == "Age 30 to 34")$SEX)
table(subset(BRFSS04_dataset, AGE == "Age 35 to 39")$SEX)
table(subset(BRFSS04_dataset, AGE == "Age 40 to 44")$SEX)
table(subset(BRFSS04_dataset, AGE == "Age 45 to 49")$SEX)
table(subset(BRFSS04_dataset, AGE == "Age 50 to 54")$SEX)
table(subset(BRFSS04_dataset, AGE == "Age 55 to 59")$SEX)
table(subset(BRFSS04_dataset, AGE == "Age 60 to 64")$SEX)
table(subset(BRFSS04_dataset, AGE == "Age 65 to 69")$SEX)
table(subset(BRFSS04_dataset, AGE == "Age 70 and above")$SEX)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS04_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS04_dataset)
#Observe the design oject
BRFSS04_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(BRFSS04_DO,
        file = "BRFSS04_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#____________________________________________________________________________________________

#ANALYSIS

B04_overall <- svymean(~factor(HAVARTH2),
                       BRFSS04_DO,
                       na.rm = TRUE)
B04_overall.c <- B04_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B04_overall_ci <- confint(B04_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B04 <- bind_cols(B04_overall.c, B04_overall_ci)
#remove havarth = 0
B04 <- B04[-c(1), ] #final proportion, se and 95% ci
#backup save
#write.csv(B04, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B04_Arthritis.csv")

#Demographic analysis

#1. Arthritis (RDD) prevalence by age 
B04_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS04_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B04_Arthtitis_age <- B04_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B04_arth_age_ci <- confint(B04_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B04_arth_age_ci <- B04_arth_age_ci[-c(1:11), ]
#join ci and proportions
B04_Arthritis.Age <- bind_cols(B04_Arthtitis_age, B04_arth_age_ci)
#Save
#write.csv(B04_Arthritis.Age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B04_Arthritis.age.csv")

#Age logistic regression
B04_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS04_DO)
summary(B04_age_glm)
exp(cbind(OR=coef(B04_age_glm), confint(B04_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B04_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS04_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B04_Arthritis_sex <- B04_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B04_arth_sex_ci <- confint(B04_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B04_arth_sex_ci <- B04_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B04_Arthritis.Sex <- bind_cols(B04_Arthritis_sex, B04_arth_sex_ci)
#save
#write.csv(B04_Arthritis.Sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B04_Arthritis.sex.csv")

#Sex logistic regression
B04_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS04_DO)
summary(B04_sex_glm)
exp(cbind(OR=coef(B04_sex_glm), confint(B04_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B04_Arth_emp <- svyby(formula = ~HAVARTH2,
                      by = ~EMPLOY,
                      design = BRFSS04_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B04_Arthritis_emp <- B04_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B04_arth_emp_ci <- confint(B04_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B04_arth_emp_ci <- B04_arth_emp_ci[-c(1:7), ]
#join ci and proportion
B04_Arthritis.Employ <- bind_cols(B04_Arthritis_emp, B04_arth_emp_ci)
#save
#write.csv(B04_Arthritis.Employ, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B04_Arthritis.employment.csv")

#Employment status logistic regression
B04_emp_glm <- svyglm(HAVARTH2~EMPLOY + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS04_DO)
summary(B04_emp_glm)
exp(cbind(OR=coef(B04_emp_glm), confint(B04_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B04_inc_glm <- svyglm(HAVARTH2~relevel(INCOME2, ref = ">= $75 0000") + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS04_DO)
summary(B04_inc_glm)
exp(cbind(OR=coef(B04_inc_glm), confint(B04_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B04_edu_glm <- svyglm(HAVARTH2~EDUCA + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS04_DO)
summary(B04_edu_glm)
exp(cbind(OR=coef(B04_edu_glm), confint(B04_edu_glm)))

#------------------------------------------------------------------------------------------------------------------------

#Lifestyle analysis

#1. Arthritis (RDD) by BMI (logistic regression)
B04.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI4` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS04_DO)
summary(B04.BMI.glm)
exp(cbind(OR=coef(B04.BMI.glm), confint(B04.BMI.glm)))

#_____________________________________________________________________________________________
#_____________________________________________________________________________________________


# == 2010 == #

#DOWNLOAD

B10_url <- "http://www.cdc.gov/brfss/annual_data/2010/files/CDBRFS10XPT.zip"

tempB10 <- tempfile()
tempB10b <- tempfile()

download.file(B10_url, tempB10)
unzip(zipfile = tempB10, exdir = tempB10b)
BRFSS10_raw <- read_xpt(file.path(tempB10b, "CDBRFS10.XPT"))

saveRDS(object = BRFSS10_raw,
        file = "BRFSS_2010.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

unlink(c(tempB10, tempB10b))

#_____________________________________________________________________


#CLEANING

#Observe the dataset
str(BRFSS10_raw) 
tail(BRFSS10_raw) 
glimpse(BRFSS10_raw) 
colnames(BRFSS10_raw)

BRFSS10 <- select(BRFSS10_raw,
                  "_PSU", "_FINALWT", "_STSTR",
                  "AGE", "SEX",
                  "EDUCA", "EMPLOY", "INCOME2",
                  "HAVARTH2", 
                  "_BMI4")

#Observations
str(BRFSS10)
tail(BRFSS10)
glimpse(BRFSS10)
colnames(BRFSS10)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS10))
#Arthritis is only a module so there are many missing values
which(colSums(is.na(BRFSS10)) == nrow(BRFSS10))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Now have a look at the arthritis variable (HAVARTH3)
#Observe how many people answered yes, no, don't know, & refused
table(BRFSS10$HAVARTH2)
#This is a module, therefore values as follows:
# 1 = 15 538 || 2 = 19 810 || 7 = 117 || 9 = 13

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS10$HAVARTH2 <- recode(BRFSS10$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS10$HAVARTH2 <- unknownToNA(BRFSS10$HAVARTH2, unknown = c("7", "9")))
table(BRFSS10$HAVARTH2)
BRFSS10$HAVARTH2 <- as.factor(BRFSS10$HAVARTH2)
class(BRFSS10$HAVARTH2)

table(BRFSS10$AGE)
#From the above command, I saw that this data is continuous. However, in order for us to make meaningful comparisons with other years, I will alter the variables into categorical ranges in the same way as later BRFSS datasets
#Recode age data into categories corresponding to BRFSS codebook
BRFSS10$AGE<- recode(BRFSS10$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "97" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS10$AGE <- unknownToNA(BRFSS10$AGE, unknown = c("7", "9")))
table(BRFSS10$AGE)

BRFSS10$AGE <- as.factor(BRFSS10$AGE)
class(BRFSS10$AGE)

#Recode sex data into categories corresponding to BRFSS codebook
BRFSS10$SEX <- recode(BRFSS10$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS10$SEX)

BRFSS10$SEX <- as.factor(BRFSS10$SEX)
class(BRFSS10$SEX)

BRFSS10$EDUCA <- recode(BRFSS10$EDUCA,
                        "1" = "Never attended school/only kindergarten",
                        "2" = "Elementary school",
                        "3" = "Some high school",
                        "4" = "High school graduate",
                        "5" = "Some college/some technical school",
                        "6" = "College graduate",
                        "9" = "9")
#Change refused (9) values to NA
(BRFSS10$EDUCA <- unknownToNA(BRFSS10$EDUCA, unknown = "9"))
table(BRFSS10$EDUCA)

BRFSS10$EDUCA <- as.factor(BRFSS10$EDUCA)
class(BRFSS10$EDUCA)

#Recode income into categories corresponding to BRFSS codebook
BRFSS10$INCOME2 <- recode(BRFSS10$INCOME2,
                          "1" = "< $10 000",
                          "2" = "$10 000 to $14 999",
                          "3" = "$15 000 to $19 999",
                          "4" = "$20 000 to $24 999",
                          "5" = "$25 000 to $34 999",
                          "6" = "$35 000 to $49 999",
                          "7" = "$50 000 to $74 999",
                          "8" = ">= $75 0000",
                          "77" = "77",
                          "99" = "99")
#Change don't know (77) and (99) values to NA
(BRFSS10$INCOME2 <- unknownToNA(BRFSS10$INCOME2, unknown = c("77", "99")))
table(BRFSS10$INCOME2)

BRFSS10$INCOME2 <- as.factor(BRFSS10$INCOME2)
class(BRFSS10$INCOME2)

#Recode employment into categories corresponding to BRFSS codebook
# The BRFSS employment data is more comprehensive than the other surveys (ie offers options such as 'student' and 'retired'). Thus, this employment status data is labelled according to the BRFSS codebook precisely
BRFSS10$EMPLOY <- recode(BRFSS10$EMPLOY,
                         "1" = "Employed for wages",
                         "2" = "Self-employed",
                         "3" = "Unemployed",
                         "4" = "Unemployed",
                         "5" = "Homemaker",
                         "6" = "Student",
                         "7" = "Retired",
                         "8" = "Unable to work",
                         "9" = "9")
#Change refused (9) to NA
(BRFSS10$EMPLOY <- unknownToNA(BRFSS10$EMPLOY, unknown = "9"))
table(BRFSS10$EMPLOY)

BRFSS10$EMPLOY <- as.factor(BRFSS10$EMPLOY)
class(BRFSS10$EMPLOY)

#BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS10 %>% top_n(20, `_BMI4`)
BRFSS10$`_BMI4` <- unknownToNA(BRFSS10$`_BMI4`, unknown = "9999")
BRFSS10 <- BRFSS10 %>%
  mutate(`_BMI4` = `_BMI4`/100)
BRFSS10$`_BMI4`
#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS10_dataset <- subset(BRFSS10,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH2)) 

#Check that the sum of the weights is equal to the US population
sum(BRFSS10_dataset$`_FINALWT`)
#The sum of the weights is 15 548 620, MODULE YEAR

#Check the number of people (unique PSU's)
length(unique(BRFSS10_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 30 283

#Check the number of unique strata
length(unique(BRFSS10_dataset[["_STSTR"]]))
#The number of unique strata is 94

#Used to generate frequency tables (unweighted)
table(subset(BRFSS10_dataset, AGE == "Age 18 to 24")$SEX)
table(subset(BRFSS10_dataset, AGE == "Age 25 to 29")$SEX)
table(subset(BRFSS10_dataset, AGE == "Age 30 to 34")$SEX)
table(subset(BRFSS10_dataset, AGE == "Age 35 to 39")$SEX)
table(subset(BRFSS10_dataset, AGE == "Age 40 to 44")$SEX)
table(subset(BRFSS10_dataset, AGE == "Age 45 to 49")$SEX)
table(subset(BRFSS10_dataset, AGE == "Age 50 to 54")$SEX)
table(subset(BRFSS10_dataset, AGE == "Age 55 to 59")$SEX)
table(subset(BRFSS10_dataset, AGE == "Age 60 to 64")$SEX)
table(subset(BRFSS10_dataset, AGE == "Age 65 to 69")$SEX)
table(subset(BRFSS10_dataset, AGE == "Age 70 and above")$SEX)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS10_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS10_dataset)
#Observe the design oject
BRFSS10_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(BRFSS10_DO,
        file = "BRFSS10_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_______________________________________________________________________________________________________

#ANALYSIS

B10_overall <- svymean(~factor(HAVARTH2),
                       BRFSS10_DO,
                       na.rm = TRUE)
B10_overall.c <- B10_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B10_overall_ci <- confint(B10_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B10 <- bind_cols(B10_overall.c, B10_overall_ci)
#remove havarth = 0
B10 <- B10[-c(1), ] #final proportion, se and 95% ci
#backup save
#write.csv(B10, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B10_Arthritis.csv")

#Demographic analysis

#1. Arthritis (RDD) prevalence by age 
B10_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B10_Arthtitis_age <- B10_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B10_arth_age_ci <- confint(B10_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B10_arth_age_ci <- B10_arth_age_ci[-c(1:11), ]
#join ci and proportions
B10_Arthritis.Age <- bind_cols(B10_Arthtitis_age, B10_arth_age_ci)
#Save
#write.csv(B10_Arthritis.Age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B10_Arthritis.age.csv")

#Age logistic regression
B10_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS10_DO)
summary(B10_age_glm)
exp(cbind(OR=coef(B10_age_glm), confint(B10_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B10_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B10_Arthritis_sex <- B10_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B10_arth_sex_ci <- confint(B10_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B10_arth_sex_ci <- B10_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B10_Arthritis.Sex <- bind_cols(B10_Arthritis_sex, B10_arth_sex_ci)
#save
#write.csv(B10_Arthritis.Sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B10_Arthritis.sex.csv")

#Sex logistic regression
B10_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS10_DO)
summary(B10_sex_glm)
exp(cbind(OR=coef(B10_sex_glm), confint(B10_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B10_Arth_emp <- svyby(formula = ~HAVARTH2,
                      by = ~EMPLOY,
                      design = BRFSS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B10_Arthritis_emp <- B10_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B10_arth_emp_ci <- confint(B10_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0
B10_arth_emp_ci <- B10_arth_emp_ci[-c(1:7), ]
#join ci and proportion
B10_Arthritis.Employ <- bind_cols(B10_Arthritis_emp, B10_arth_emp_ci)
#save
#write.csv(B10_Arthritis.Employ, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B10_Arthritis.employment.csv")

#Employment status logistic regression
B10_emp_glm <- svyglm(HAVARTH2~EMPLOY + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS10_DO)
summary(B10_emp_glm)
exp(cbind(OR=coef(B10_emp_glm), confint(B10_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B10_inc_glm <- svyglm(HAVARTH2~relevel(INCOME2, ref = ">= $75 0000") + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS10_DO)
summary(B10_inc_glm)
exp(cbind(OR=coef(B10_inc_glm), confint(B10_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B10_edu_glm <- svyglm(HAVARTH2~EDUCA + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS10_DO)
summary(B10_edu_glm)
exp(cbind(OR=coef(B10_edu_glm), confint(B10_edu_glm)))

#------------------------------------------------------------------------------------------------------------------------

#Lifestyle analysis

#1. Arthritis (RDD) by BMI (logistic regression)
B10.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI4` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS10_DO)
summary(B10.BMI.glm)
exp(cbind(OR=coef(B10.BMI.glm), confint(B10.BMI.glm)))

#_____________________________________________________________________________________________
#_____________________________________________________________________________________________
