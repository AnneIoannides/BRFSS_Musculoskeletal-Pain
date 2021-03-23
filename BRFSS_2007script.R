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


# == 2007 script == #

#DOWNLOAD

B07_url <- "http://www.cdc.gov/brfss/annual_data/2007/files/CDBRFS07XPT.ZIP"

tempB07 <- tempfile()
tempB07b <- tempfile()

download.file(B07_url, tempB07, mode = "wb")
unzip(zipfile = tempB07, exdir = tempB07b)
BRFSS07_raw <- read_xpt(file.path(tempB07b, "CDBRFS07.XPT"))

saveRDS(object = BRFSS07_raw,
        file = "BRFSS_2007.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

unlink(c(tempB07, tempB07b))


#CLEANING

# == 2009 == #

#Observe the dataset
str(BRFSS07_raw) 
tail(BRFSS07_raw) 
glimpse(BRFSS07_raw) 
colnames(BRFSS07_raw)

BRFSS07 <- select(BRFSS07_raw,
                  "_STATE",
                  "_PSU", "_FINALWT", "_STSTR",
                  "AGE", "SEX",
                  "EDUCA", "EMPLOY", "INCOME2",
                  "HAVARTH2",
                  "_BMI4") 

#Observations
str(BRFSS07)
tail(BRFSS07)
glimpse(BRFSS07)
colnames(BRFSS07)

table(BRFSS07$`_STATE`)
BRFSS07$`_STATE` <- as.factor(BRFSS07$`_STATE`)
class(BRFSS07$`_STATE`)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS07))
which(colSums(is.na(BRFSS07)) == nrow(BRFSS07))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Now have a look at the arthritis variable (HAVARTH3)
#Observe how many people answered yes, no, don't know, & refused
table(BRFSS07$HAVARTH2)

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS07$HAVARTH2 <- recode(BRFSS07$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS07$HAVARTH2 <- unknownToNA(BRFSS07$HAVARTH2, unknown = c("7", "9")))
table(BRFSS07$HAVARTH2)

BRFSS07$HAVARTH2 <- as.factor(BRFSS07$HAVARTH2)
class(BRFSS07$HAVARTH2)

table(BRFSS07$AGE)
#From the above command, I saw that this data is continuous. However, in order for us to make meaningful comparisons with other years, I will alter the variables into categorical ranges in the same way as later BRFSS datasets
#Recode age data into categories corresponding to BRFSS codebook
BRFSS07$AGE<- recode(BRFSS07$AGE,
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
(BRFSS07$AGE <- unknownToNA(BRFSS07$AGE, unknown = c("7", "9")))
table(BRFSS07$AGE)

BRFSS07$AGE <- as.factor(BRFSS07$AGE)
class(BRFSS07$AGE)

#Recode sex data into categories corresponding to BRFSS codebook
BRFSS07$SEX <- recode(BRFSS07$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS07$SEX)

BRFSS07$SEX <- as.factor(BRFSS07$SEX)
class(BRFSS07$SEX)

BRFSS07$EDUCA <- recode(BRFSS07$EDUCA,
                        "1" = "Never attended school/only kindergarten",
                        "2" = "Elementary school",
                        "3" = "Some high school",
                        "4" = "High school graduate",
                        "5" = "Some college/some technical school",
                        "6" = "College graduate",
                        "9" = "9")
#Change refused (9) values to NA
(BRFSS07$EDUCA <- unknownToNA(BRFSS07$EDUCA, unknown = "9"))
table(BRFSS07$EDUCA)

BRFSS07$EDUCA <- as.factor(BRFSS07$EDUCA)
class(BRFSS07$EDUCA)

#Recode income into categories corresponding to BRFSS codebook
BRFSS07$INCOME2 <- recode(BRFSS07$INCOME2,
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
(BRFSS07$INCOME2 <- unknownToNA(BRFSS07$INCOME2, unknown = c("77", "99")))
table(BRFSS07$INCOME2)

BRFSS07$INCOME2 <- as.factor(BRFSS07$INCOME2)
class(BRFSS07$INCOME2)

BRFSS07$EMPLOY <- recode(BRFSS07$EMPLOY,
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
(BRFSS07$EMPLOY <- unknownToNA(BRFSS07$EMPLOY, unknown = "9"))
table(BRFSS07$EMPLOY)

BRFSS07$EMPLOY <- as.factor(BRFSS07$EMPLOY)
class(BRFSS07$EMPLOY)

#BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS07$`_BMI4` <- unknownToNA(BRFSS07$`_BMI4`, unknown = "9999")
BRFSS07 %>% top_n(20, `_BMI4`)
BRFSS07 <- BRFSS07 %>%
  mutate(`_BMI4` = `_BMI4`/100)
BRFSS07$`_BMI4`

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS07_dataset <- subset(BRFSS07,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH2))

#Check that the sum of the weights is equal to the US population
sum(BRFSS07_dataset$`_FINALWT`)
#The sum of the weights is 224 165 210, which is acceptable

#Check the number of people (unique PSU's)
length(unique(BRFSS07_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 158 471
#Check the number of unique strata
length(unique(BRFSS07_dataset[["_STSTR"]]))
#The number of unique strata is 1 006

#Used to generate frequency tables (unweighted)

table(subset(BRFSS07_dataset, AGE == "Age 18 to 24")$SEX)
table(subset(BRFSS07_dataset, AGE == "Age 25 to 29")$SEX)
table(subset(BRFSS07_dataset, AGE == "Age 30 to 34")$SEX)
table(subset(BRFSS07_dataset, AGE == "Age 35 to 39")$SEX)
table(subset(BRFSS07_dataset, AGE == "Age 40 to 44")$SEX)
table(subset(BRFSS07_dataset, AGE == "Age 45 to 49")$SEX)
table(subset(BRFSS07_dataset, AGE == "Age 50 to 54")$SEX)
table(subset(BRFSS07_dataset, AGE == "Age 55 to 59")$SEX)
table(subset(BRFSS07_dataset, AGE == "Age 60 to 64")$SEX)
table(subset(BRFSS07_dataset, AGE == "Age 65 to 69")$SEX)
table(subset(BRFSS07_dataset, AGE == "Age 70 and above")$SEX)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS07_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS07_dataset)
#Observe the design oject
BRFSS07_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(BRFSS07_DO,
        file = "BRFSS07_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_______________________________________________________________________________________________
#RUN
B07_overall <- svymean(~factor(HAVARTH2),
                       BRFSS07_DO,
                       na.rm = TRUE)

B07_Arth_state <- svyby(formula = ~HAVARTH2,
                        by = ~`_STATE`,
                        design = BRFSS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)

B07_Arth_state_age <- svyby(formula = ~HAVARTH2,
                            by = ~`_STATE` + AGE,
                            design = BRFSS07_DO,
                            FUN = svymean,
                            na.rm = TRUE)

B07_Arth_state_sex <- svyby(formula = ~HAVARTH2,
                            by = ~`_STATE` + SEX,
                            design = BRFSS07_DO,
                            FUN = svymean,
                            na.rm = TRUE)

#1. Arthritis (RDD) prevalence by age 
B07_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)

#Age logistic regression
B07_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_age_glm)
exp(cbind(OR=coef(B07_age_glm), confint(B07_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B07_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)

#Sex logistic regression
B07_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_sex_glm)
exp(cbind(OR=coef(B07_sex_glm), confint(B07_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B07_Arth_emp <- svyby(formula = ~HAVARTH2,
                      by = ~EMPLOY,
                      design = BRFSS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)


#Employment status logistic regression
B07_emp_glm <- svyglm(HAVARTH2~EMPLOY + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_emp_glm)
exp(cbind(OR=coef(B07_emp_glm), confint(B07_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B07_inc_glm <- svyglm(HAVARTH2~relevel(INCOME2, ref = ">= $75 0000") + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_inc_glm)
exp(cbind(OR=coef(B07_inc_glm), confint(B07_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B07_edu_glm <- svyglm(HAVARTH2~EDUCA + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_edu_glm)
exp(cbind(OR=coef(B07_edu_glm), confint(B07_edu_glm)))


#1. Arthritis (RDD) by BMI (logistic regression)
B07.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI4` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07.BMI.glm)
exp(cbind(OR=coef(B07.BMI.glm), confint(B07.BMI.glm)))


#_______________________________________________________________
#ANALYSIS

#Overall prevalence
B07_overall <- svymean(~factor(HAVARTH2),
                       BRFSS07_DO,
                       na.rm = TRUE)
B07_overall.c <- B07_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_overall_ci <- confint(B07_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B07 <- bind_cols(B07_overall.c, B07_overall_ci)
#remove havarth = 0
B07 <- B07[-c(1), ] #final proportion, se and 95% ci
#backup save
#write.csv(B07, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B07_Arthritis.csv")

#------------------------------------------------------------------------------------------------------------------------

#Spatial analysis

#Arthritis prevalence (overall) by state
B07_Arth_state <- svyby(formula = ~HAVARTH2,
                        by = ~`_STATE`,
                        design = BRFSS07_DO,
                        FUN = svymean,
                        na.rm = TRUE)
B07_Arthtitis_state <- B07_Arth_state %>%
  select(1, 3, 5) %>%
  setNames(c("State", "Proportion", "SE")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_arth_state_ci <- confint(B07_Arth_state) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Rows 1-53 are the confidence intervals for the 0-values in the HAVARTH2 variable (ie those who have not been told they have arthritis), therefore I removed these rows
B07_arth_state_ci <- B07_arth_state_ci[-c(1:54), ]
#Join ci and proportion tables
B07_Arthritis.state <- bind_cols(B07_Arthtitis_state, B07_arth_state_ci)
#Save this table for convenience (will be saved to a separate folder therefore change working directory just for these saves)
#write.csv(B07_Arthritis.state, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B07_Arthritis.state.csv")

#Load information for mapping (lat and long for states)
us_states <- map_data("state")

# - Map~State(overall) - #

#Rename the states
B07_Arthritis.state$State[B07_Arthritis.state$State == "1"] <- "Alabama"
B07_Arthritis.state$State[B07_Arthritis.state$State == "2"] <- "Alaska"
B07_Arthritis.state$State[B07_Arthritis.state$State == "4"] <- "Arizona"
B07_Arthritis.state$State[B07_Arthritis.state$State == "5"] <- "Arkansas"
B07_Arthritis.state$State[B07_Arthritis.state$State == "6"] <- "California"
B07_Arthritis.state$State[B07_Arthritis.state$State == "8"] <- "Colorado"
B07_Arthritis.state$State[B07_Arthritis.state$State == "9"] <- "Connecticut"
B07_Arthritis.state$State[B07_Arthritis.state$State == "10"] <- "Delaware"
B07_Arthritis.state$State[B07_Arthritis.state$State == "11"] <- "District of Columbia"
B07_Arthritis.state$State[B07_Arthritis.state$State == "12"] <- "Florida"
B07_Arthritis.state$State[B07_Arthritis.state$State == "13"] <- "Georgia"
B07_Arthritis.state$State[B07_Arthritis.state$State == "15"] <- "Hawaii"
B07_Arthritis.state$State[B07_Arthritis.state$State == "16"] <- "Idaho"
B07_Arthritis.state$State[B07_Arthritis.state$State == "17"] <- "Illinois"
B07_Arthritis.state$State[B07_Arthritis.state$State == "18"] <- "Indiana"
B07_Arthritis.state$State[B07_Arthritis.state$State == "19"] <- "Iowa"
B07_Arthritis.state$State[B07_Arthritis.state$State == "20"] <- "Kansas"
B07_Arthritis.state$State[B07_Arthritis.state$State == "21"] <- "Kentucky"
B07_Arthritis.state$State[B07_Arthritis.state$State == "22"] <- "Louisiana"
B07_Arthritis.state$State[B07_Arthritis.state$State == "23"] <- "Maine"
B07_Arthritis.state$State[B07_Arthritis.state$State == "24"] <- "Maryland"
B07_Arthritis.state$State[B07_Arthritis.state$State == "25"] <- "Massachusetts"
B07_Arthritis.state$State[B07_Arthritis.state$State == "26"] <- "Michigan"
B07_Arthritis.state$State[B07_Arthritis.state$State == "27"] <- "Minnesota"
B07_Arthritis.state$State[B07_Arthritis.state$State == "28"] <- "Mississippi"
B07_Arthritis.state$State[B07_Arthritis.state$State == "29"] <- "Missouri"
B07_Arthritis.state$State[B07_Arthritis.state$State == "30"] <- "Montana"
B07_Arthritis.state$State[B07_Arthritis.state$State == "31"] <- "Nebraska"
B07_Arthritis.state$State[B07_Arthritis.state$State == "32"] <- "Nevada"
B07_Arthritis.state$State[B07_Arthritis.state$State == "33"] <- "New Hampshire"
B07_Arthritis.state$State[B07_Arthritis.state$State == "34"] <- "New Jersey"
B07_Arthritis.state$State[B07_Arthritis.state$State == "35"] <- "New Mexico"
B07_Arthritis.state$State[B07_Arthritis.state$State == "36"] <- "New York"
B07_Arthritis.state$State[B07_Arthritis.state$State == "37"] <- "North Carolina"
B07_Arthritis.state$State[B07_Arthritis.state$State == "38"] <- "North Dakota"
B07_Arthritis.state$State[B07_Arthritis.state$State == "39"] <- "Ohio"
B07_Arthritis.state$State[B07_Arthritis.state$State == "40"] <- "Oklahoma"
B07_Arthritis.state$State[B07_Arthritis.state$State == "41"] <- "Oregon"
B07_Arthritis.state$State[B07_Arthritis.state$State == "42"] <- "Pennsylvania"
B07_Arthritis.state$State[B07_Arthritis.state$State == "44"] <- "Rhode Island"
B07_Arthritis.state$State[B07_Arthritis.state$State == "45"] <- "South Carolina"
B07_Arthritis.state$State[B07_Arthritis.state$State == "46"] <- "South Dakota"
B07_Arthritis.state$State[B07_Arthritis.state$State == "47"] <- "Tennessee"
B07_Arthritis.state$State[B07_Arthritis.state$State == "48"] <- "Texas"
B07_Arthritis.state$State[B07_Arthritis.state$State == "49"] <- "Utah"
B07_Arthritis.state$State[B07_Arthritis.state$State == "50"] <- "Vermont"
B07_Arthritis.state$State[B07_Arthritis.state$State == "51"] <- "Virginia"
B07_Arthritis.state$State[B07_Arthritis.state$State == "53"] <- "Washington"
B07_Arthritis.state$State[B07_Arthritis.state$State == "54"] <- "West Virginia"
B07_Arthritis.state$State[B07_Arthritis.state$State == "55"] <- "Wisconsin"
B07_Arthritis.state$State[B07_Arthritis.state$State == "56"] <- "Wyoming"
B07_Arthritis.state$State[B07_Arthritis.state$State == "66"] <- "Guam"
B07_Arthritis.state$State[B07_Arthritis.state$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B07_Arthritis.state$region <- tolower(B07_Arthritis.state$State)
B07_Arthritis.state_join <- left_join(us_states, B07_Arthritis.state)

#Map
B07Arthritis.state.map <- ggplot(data = B07_Arthritis.state_join,
                                 mapping = aes(x = long, y = lat,
                                               group = group,
                                               fill = Proportion)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_viridis_c(option = "viridis", direction = -1, limits = c(10, 50), breaks = c(10, 30, 50)) +
  theme_map() +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  labs(fill = "Prevalence (%)")
ggsave("B07.Arthritis.state.map.png", width = 4, height = 3)


#Spatial analysis

B07_arth_SF <- st_as_sf(B07_Arthritis.state_join,
                        coords = c("long", "lat"),
                        crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_SF$geometry)
#assess neighbours
neighbours18 <- poly2nb(B07_arth_SF)
neighbours18
B07_arth_SF$Proportion <- as.numeric(B07_arth_SF$Proportion)
neighbours18.ii <- poly2nb(B07_arth_SF, queen = FALSE)
neighbours18.ii
plot(B07_arth_SF, border = "lightgrey")
listw18 <- nb2listw(neighbours18.ii)
listw18
globalMoran18 <- moran.test(B07_arth_SF$Proportion, listw18, na.action = na.exclude)
globalMoran18
globalMoran18[["estimate"]][["Moran I statistic"]]
globalMoran18[["p.value"]]

B07_Arthritis.state %>% top_n(-7, Proportion)
B07_Arthritis.state %>% top_n(5, Proportion)
summary(B07_Arthritis.state$Proportion)


#............................................


#Arthritis prevalence (by age) by state
B07_Arth_state_age <- svyby(formula = ~HAVARTH2,
                            by = ~`_STATE` + AGE,
                            design = BRFSS07_DO,
                            FUN = svymean,
                            na.rm = TRUE)
B07_Arthritis_state_age <- B07_Arth_state_age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("State", "Age", "Proportion", "SE")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_arth_state_age_ci <- confint(B07_Arth_state_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) 
#Rows 1 to 594 are confidence intervals for HAVARTH2 = 0 (No), therefore I will remove them
B07_arth_state_age_ci <- B07_arth_state_age_ci[-c(1:594), ]
#Join ci and proportion tables
B07_Arthritis.state.age <- bind_cols(B07_Arthritis_state_age, B07_arth_state_age_ci)
#Save this table for convenience
#write.csv(B07_Arthritis.state.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B07_Arthritis.state.age.csv")

# - Map~State(by age) - #

#Rename state values to state names
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "1"] <- "Alabama"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "2"] <- "Alaska"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "4"] <- "Arizona"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "5"] <- "Arkansas"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "6"] <- "California"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "8"] <- "Colorado"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "9"] <- "Connecticut"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "10"] <- "Delaware"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "11"] <- "District of Columbia"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "12"] <- "Florida"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "13"] <- "Georgia"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "15"] <- "Hawaii"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "16"] <- "Idaho"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "17"] <- "Illinois"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "18"] <- "Indiana"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "19"] <- "Iowa"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "20"] <- "Kansas"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "21"] <- "Kentucky"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "22"] <- "Louisiana"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "23"] <- "Maine"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "24"] <- "Maryland"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "25"] <- "Massachusetts"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "26"] <- "Michigan"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "27"] <- "Minnesota"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "28"] <- "Mississippi"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "29"] <- "Missouri"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "30"] <- "Montana"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "31"] <- "Nebraska"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "32"] <- "Nevada"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "33"] <- "New Hampshire"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "34"] <- "New Jersey"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "35"] <- "New Mexico"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "36"] <- "New York"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "37"] <- "North Carolina"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "38"] <- "North Dakota"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "39"] <- "Ohio"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "40"] <- "Oklahoma"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "41"] <- "Oregon"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "42"] <- "Pennsylvania"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "44"] <- "Rhode Island"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "45"] <- "South Carolina"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "46"] <- "South Dakota"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "47"] <- "Tennessee"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "48"] <- "Texas"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "49"] <- "Utah"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "50"] <- "Vermont"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "51"] <- "Virginia"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "53"] <- "Washington"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "54"] <- "West Virginia"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "55"] <- "Wisconsin"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "56"] <- "Wyoming"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "66"] <- "Guam"
B07_Arthritis.state.age$State[B07_Arthritis.state.age$State == "72"] <- "Puerto Rico"

B07_Arthritis.state.age$region <- tolower(B07_Arthritis.state.age$State)
B07_Arthritis.state.age.join <- left_join(us_states, B07_Arthritis.state.age)

#Map
B07_Arthritis.state.age.map <- ggplot(data = B07_Arthritis.state.age.join,
                                      mapping = aes(x = long, y = lat,
                                                    group = group,
                                                    fill = Proportion)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_viridis_c(option = "viridis", direction = -1, limits = c(0, 75), breaks = c(0, 25, 50, 75)) +
  theme_map() +
  facet_wrap(~Age, ncol = 3) +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  theme(text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)")
ggsave("B07.Arthritis.state.age.map.png", width = 6, height = 6)


#Spatial correlation

#subset for 18-24 SF
B07_state.age1824 <- subset(B07_Arthritis.state.age.join, Age == "Age 18 to 24")


B07_arth_1824SF <- st_as_sf(B07_state.age1824,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_1824SF$geometry)
#assess neighbours
neighbours18.1824 <- poly2nb(B07_arth_1824SF)
neighbours18.1824
B07_arth_1824SF$Proportion <- as.numeric(B07_arth_1824SF$Proportion)
neighbours18.1824ii <- poly2nb(B07_arth_1824SF, queen = FALSE)
neighbours18.1824ii
plot(B07_arth_1824SF, border = "lightgrey")
listw18.1824 <- nb2listw(neighbours18.1824ii)
listw18.1824
globalMoran18.1824 <- moran.test(B07_arth_1824SF$Proportion, listw18.1824, na.action = na.exclude)
globalMoran18.1824
globalMoran18.1824[["estimate"]][["Moran I statistic"]]
globalMoran18.1824[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 18 to 24") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 18 to 24") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 18 to 24")$Proportion)


#subset for 25-29 SF
B07_state.age2529 <- subset(B07_Arthritis.state.age.join, Age == "Age 25 to 29")

B07_arth_2529SF <- st_as_sf(B07_state.age2529,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_2529SF$geometry)
#assess neighbours
neighbours18.2529 <- poly2nb(B07_arth_2529SF)
neighbours18.2529
B07_arth_2529SF$Proportion <- as.numeric(B07_arth_2529SF$Proportion)
neighbours18.2529ii <- poly2nb(B07_arth_2529SF, queen = FALSE)
neighbours18.2529ii
plot(B07_arth_2529SF, border = "lightgrey")
listw18.2529 <- nb2listw(neighbours18.2529ii)
listw18.2529
globalMoran18.2529 <- moran.test(B07_arth_2529SF$Proportion, listw18.2529, na.action = na.exclude)
globalMoran18.2529
globalMoran18.2529[["estimate"]][["Moran I statistic"]]
globalMoran18.2529[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 25 to 29") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 25 to 29") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 25 to 29")$Proportion)

#subset for 30-34 SF
B07_state.age3034 <- subset(B07_Arthritis.state.age.join, Age == "Age 30 to 34")

B07_arth_3034SF <- st_as_sf(B07_state.age3034,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_3034SF$geometry)
#assess neighbours
neighbours18.3034 <- poly2nb(B07_arth_3034SF)
neighbours18.3034
B07_arth_3034SF$Proportion <- as.numeric(B07_arth_3034SF$Proportion)
neighbours18.3034ii <- poly2nb(B07_arth_3034SF, queen = FALSE)
neighbours18.3034ii
plot(B07_arth_3034SF, border = "lightgrey")
listw18.3034 <- nb2listw(neighbours18.3034ii)
listw18.3034
globalMoran18.3034 <- moran.test(B07_arth_3034SF$Proportion, listw18.3034, na.action = na.exclude)
globalMoran18.3034
globalMoran18.3034[["estimate"]][["Moran I statistic"]]
globalMoran18.3034[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 30 to 34") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 30 to 34") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 30 to 34")$Proportion)


#subset for 35-39 SF
B07_state.age3539 <- subset(B07_Arthritis.state.age.join, Age == "Age 35 to 39")

B07_arth_3539SF <- st_as_sf(B07_state.age3539,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_3539SF$geometry)
#assess neighbours
neighbours18.3539 <- poly2nb(B07_arth_3539SF)
neighbours18.3539
B07_arth_3539SF$Proportion <- as.numeric(B07_arth_3539SF$Proportion)
neighbours18.3539ii <- poly2nb(B07_arth_3539SF, queen = FALSE)
neighbours18.3539ii
plot(B07_arth_3539SF, border = "lightgrey")
listw18.3539 <- nb2listw(neighbours18.3539ii)
listw18.3539
globalMoran18.3539 <- moran.test(B07_arth_3539SF$Proportion, listw18.3539, na.action = na.exclude)
globalMoran18.3539
globalMoran18.3539[["estimate"]][["Moran I statistic"]]
globalMoran18.3539[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 35 to 39") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 35 to 39") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 35 to 39")$Proportion)


#subset for 40-44 SF
B07_state.age4044 <- subset(B07_Arthritis.state.age.join, Age == "Age 40 to 44")

B07_arth_4044SF <- st_as_sf(B07_state.age4044,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_4044SF$geometry)
#assess neighbours
neighbours18.4044 <- poly2nb(B07_arth_4044SF)
neighbours18.4044
B07_arth_4044SF$Proportion <- as.numeric(B07_arth_4044SF$Proportion)
neighbours18.4044ii <- poly2nb(B07_arth_4044SF, queen = FALSE)
neighbours18.4044ii
plot(B07_arth_4044SF, border = "lightgrey")
listw18.4044 <- nb2listw(neighbours18.4044ii)
listw18.4044
globalMoran18.4044 <- moran.test(B07_arth_4044SF$Proportion, listw18.4044, na.action = na.exclude)
globalMoran18.4044
globalMoran18.4044[["estimate"]][["Moran I statistic"]]
globalMoran18.4044[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 40 to 44") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 40 to 44") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 40 to 44")$Proportion)


#subset for 45-49 SF
B07_state.age4549 <- subset(B07_Arthritis.state.age.join, Age == "Age 45 to 49")

B07_arth_4549SF <- st_as_sf(B07_state.age4549,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_4549SF$geometry)
#assess neighbours
neighbours18.4549 <- poly2nb(B07_arth_4549SF)
neighbours18.4549
B07_arth_4549SF$Proportion <- as.numeric(B07_arth_4549SF$Proportion)
neighbours18.4549ii <- poly2nb(B07_arth_4549SF, queen = FALSE)
neighbours18.4549ii
plot(B07_arth_4549SF, border = "lightgrey")
listw18.4549 <- nb2listw(neighbours18.4549ii)
listw18.4549
globalMoran18.4549 <- moran.test(B07_arth_4549SF$Proportion, listw18.4549, na.action = na.exclude)
globalMoran18.4549
globalMoran18.4549[["estimate"]][["Moran I statistic"]]
globalMoran18.4549[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 45 to 49") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 45 to 49") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 45 to 49")$Proportion)

#subset for 50-54 SF
B07_state.age5054 <- subset(B07_Arthritis.state.age.join, Age == "Age 50 to 54")

B07_arth_5054SF <- st_as_sf(B07_state.age5054,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_5054SF$geometry)
#assess neighbours
neighbours18.5054 <- poly2nb(B07_arth_5054SF)
neighbours18.5054
B07_arth_5054SF$Proportion <- as.numeric(B07_arth_5054SF$Proportion)
neighbours18.5054ii <- poly2nb(B07_arth_5054SF, queen = FALSE)
neighbours18.5054ii
plot(B07_arth_5054SF, border = "lightgrey")
listw18.5054 <- nb2listw(neighbours18.5054ii)
listw18.5054
globalMoran18.5054 <- moran.test(B07_arth_5054SF$Proportion, listw18.5054, na.action = na.exclude)
globalMoran18.5054
globalMoran18.5054[["estimate"]][["Moran I statistic"]]
globalMoran18.5054[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 50 to 54") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 50 to 54") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 50 to 54")$Proportion)

#subset for 55-59 SF
B07_state.age5559 <- subset(B07_Arthritis.state.age.join, Age == "Age 55 to 59")

B07_arth_5559SF <- st_as_sf(B07_state.age5559,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_5559SF$geometry)
#assess neighbours
neighbours18.5559 <- poly2nb(B07_arth_5559SF)
neighbours18.5559
B07_arth_5559SF$Proportion <- as.numeric(B07_arth_5559SF$Proportion)
neighbours18.5559ii <- poly2nb(B07_arth_5559SF, queen = FALSE)
neighbours18.5559ii
plot(B07_arth_5559SF, border = "lightgrey")
listw18.5559 <- nb2listw(neighbours18.5559ii)
listw18.5559
globalMoran18.5559 <- moran.test(B07_arth_5559SF$Proportion, listw18.5559, na.action = na.exclude)
globalMoran18.5559
globalMoran18.5559[["estimate"]][["Moran I statistic"]]
globalMoran18.5559[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 55 to 59") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 55 to 59") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 55 to 59")$Proportion)

#subset for 60-64 SF
B07_state.age6064 <- subset(B07_Arthritis.state.age.join, Age == "Age 60 to 64")

B07_arth_6064SF <- st_as_sf(B07_state.age6064,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_6064SF$geometry)
#assess neighbours
neighbours18.6064 <- poly2nb(B07_arth_6064SF)
neighbours18.6064
B07_arth_6064SF$Proportion <- as.numeric(B07_arth_6064SF$Proportion)
neighbours18.6064ii <- poly2nb(B07_arth_6064SF, queen = FALSE)
neighbours18.6064ii
plot(B07_arth_6064SF, border = "lightgrey")
listw18.6064 <- nb2listw(neighbours18.6064ii)
listw18.6064
globalMoran18.6064 <- moran.test(B07_arth_6064SF$Proportion, listw18.6064, na.action = na.exclude)
globalMoran18.6064
globalMoran18.6064[["estimate"]][["Moran I statistic"]]
globalMoran18.6064[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 60 to 64") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 60 to 64") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 60 to 64")$Proportion)


#subset for 65-69 SF
B07_state.age6569 <- subset(B07_Arthritis.state.age.join, Age == "Age 65 to 69")

B07_arth_6569SF <- st_as_sf(B07_state.age6569,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_6569SF$geometry)
#assess neighbours
neighbours18.6569 <- poly2nb(B07_arth_6569SF)
neighbours18.6569
B07_arth_6569SF$Proportion <- as.numeric(B07_arth_6569SF$Proportion)
neighbours18.6569ii <- poly2nb(B07_arth_6569SF, queen = FALSE)
neighbours18.6569ii
plot(B07_arth_6569SF, border = "lightgrey")
listw18.6569 <- nb2listw(neighbours18.6569ii)
listw18.6569
globalMoran18.6569 <- moran.test(B07_arth_6569SF$Proportion, listw18.6569, na.action = na.exclude)
globalMoran18.6569
globalMoran18.6569[["estimate"]][["Moran I statistic"]]
globalMoran18.6569[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 65 to 69") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 65 to 69") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 65 to 69")$Proportion)

#subset for 70 & above SF
B07_state.age70 <- subset(B07_Arthritis.state.age.join, Age == "Age 70 and above")

B07_arth_70SF <- st_as_sf(B07_state.age70,
                          coords = c("long", "lat"),
                          crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_70SF$geometry)
#assess neighbours
neighbours18.70 <- poly2nb(B07_arth_70SF)
neighbours18.70
B07_arth_70SF$Proportion <- as.numeric(B07_arth_70SF$Proportion)
neighbours18.70ii <- poly2nb(B07_arth_70SF, queen = FALSE)
neighbours18.70ii
plot(B07_arth_70SF, border = "lightgrey")
listw18.70 <- nb2listw(neighbours18.70ii)
listw18.70
globalMoran18.70 <- moran.test(B07_arth_70SF$Proportion, listw18.70, na.action = na.exclude)
globalMoran18.70
globalMoran18.70[["estimate"]][["Moran I statistic"]]
globalMoran18.70[["p.value"]]

subset(B07_Arthritis.state.age, Age == "Age 70 and above") %>% top_n(-6, Proportion)
subset(B07_Arthritis.state.age, Age == "Age 70 and above") %>% top_n(5, Proportion)
summary(subset(B07_Arthritis.state.age, Age == "Age 70 and above")$Proportion)


#....................................


#Arthritis prevalence (by sex) by state
B07_Arth_state_sex <- svyby(formula = ~HAVARTH2,
                            by = ~`_STATE` + SEX,
                            design = BRFSS07_DO,
                            FUN = svymean,
                            na.rm = TRUE)
B07_Arthritis_state_sex <- B07_Arth_state_sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("State", "Sex", "Proportion", "SE")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_arth_state_sex_ci <- confint(B07_Arth_state_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Rows 1 to 108 are ci's for HAVARTH2 = 0, therefore delete
B07_arth_state_sex_ci <- B07_arth_state_sex_ci[-c(1:108), ]
#Join ci and proportion tables
B07_Arthritis.state.sex <- bind_cols(B07_Arthritis_state_sex, B07_arth_state_sex_ci)
#Save the table for convenience
#write.csv(B07_Arthritis.state.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B07_Arthritis.state.sex.csv")

# - Map~State(by sex) - #

#Rename the states
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "1"] <- "Alabama"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "2"] <- "Alaska"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "4"] <- "Arizona"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "5"] <- "Arkansas"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "6"] <- "California"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "8"] <- "Colorado"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "9"] <- "Connecticut"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "10"] <- "Delaware"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "11"] <- "District of Columbia"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "12"] <- "Florida"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "13"] <- "Georgia"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "15"] <- "Hawaii"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "16"] <- "Idaho"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "17"] <- "Illinois"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "18"] <- "Indiana"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "19"] <- "Iowa"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "20"] <- "Kansas"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "21"] <- "Kentucky"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "22"] <- "Louisiana"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "23"] <- "Maine"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "24"] <- "Maryland"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "25"] <- "Massachusetts"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "26"] <- "Michigan"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "27"] <- "Minnesota"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "28"] <- "Mississippi"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "29"] <- "Missouri"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "30"] <- "Montana"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "31"] <- "Nebraska"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "32"] <- "Nevada"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "33"] <- "New Hampshire"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "34"] <- "New Jersey"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "35"] <- "New Mexico"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "36"] <- "New York"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "37"] <- "North Carolina"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "38"] <- "North Dakota"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "39"] <- "Ohio"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "40"] <- "Oklahoma"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "41"] <- "Oregon"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "42"] <- "Pennsylvania"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "44"] <- "Rhode Island"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "45"] <- "South Carolina"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "46"] <- "South Dakota"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "47"] <- "Tennessee"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "48"] <- "Texas"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "49"] <- "Utah"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "50"] <- "Vermont"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "51"] <- "Virginia"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "53"] <- "Washington"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "54"] <- "West Virginia"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "55"] <- "Wisconsin"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "56"] <- "Wyoming"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "66"] <- "Guam"
B07_Arthritis.state.sex$State[B07_Arthritis.state.sex$State == "72"] <- "Puerto Rico"

B07_Arthritis.state.sex$region <- tolower(B07_Arthritis.state.sex$State)
B07_Arthritis.state.sex.join <- left_join(us_states, B07_Arthritis.state.sex)

#Map
B07_Arthritis.state.sex.map <- ggplot(data = B07_Arthritis.state.sex.join,
                                      mapping = aes(x = long, y = lat,
                                                    group = group,
                                                    fill = Proportion)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_viridis_c(option = "viridis", direction = -1, limits = c(10, 50), breaks = c(10, 30, 50)) +
  theme_map() +
  facet_wrap(~Sex) +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  theme(text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 12)) +
  labs(fill = "Prevalence (%)")
ggsave("B07.Arthritis.state.sex.map.png", width = 7, height = 3)


#Spatial correlations

#subset Males
B07_state.sex.MM <- subset(B07_Arthritis.state.sex.join, Sex == "Male")

B07_arth_sfMM <- st_as_sf(B07_state.sex.MM,
                          coords = c("long", "lat"),
                          crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_sfMM$geometry)
#neighbours
neighbours <- poly2nb(B07_arth_sfMM)
neighbours
B07_arth_sfMM$Proportion <- as.numeric(B07_arth_sfMM$Proportion)
neighboursii <- poly2nb(B07_arth_sfMM, queen = FALSE)
neighboursii
plot(B07_arth_sfMM, border = "lightgrey")
list <- nb2listw(neighboursii)
list
globalMoran.MM <- moran.test(B07_arth_sfMM$Proportion, list, na.action = na.exclude)
globalMoran.MM
globalMoran.MM[["estimate"]][["Moran I statistic"]]
globalMoran.MM[["p.value"]]

foo <- subset(B07_Arthritis.state.sex, Sex == "Male") %>% top_n(-7, Proportion)
foo[order(foo$Proportion), ]
foo2 <- subset(B07_Arthritis.state.sex, Sex == "Male") %>% top_n(7, Proportion)
foo2[order(-foo2$Proportion), ]
summary(subset(B07_Arthritis.state.sex, Sex == "Male")$Proportion)


#subset Females
B07_state.sex.FF <- subset(B07_Arthritis.state.sex.join, Sex == "Female")

B07_arth_sfFF <- st_as_sf(B07_state.sex.FF,
                          coords = c("long", "lat"),
                          crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B07_arth_sfFF$geometry)
#neighbours
neighbours <- poly2nb(B07_arth_sfFF)
neighbours
B07_arth_sfFF$Proportion <- as.numeric(B07_arth_sfFF$Proportion)
neighboursii <- poly2nb(B07_arth_sfFF, queen = FALSE)
neighboursii
plot(B07_arth_sfFF, border = "lightgrey")
list <- nb2listw(neighboursii)
list
globalMoran.FF <- moran.test(B07_arth_sfFF$Proportion, list, na.action = na.exclude)
globalMoran.FF
globalMoran.FF[["estimate"]][["Moran I statistic"]]
globalMoran.FF[["p.value"]]

foo <- subset(B07_Arthritis.state.sex, Sex == "Female") %>% top_n(-7, Proportion)
foo[order(foo$Proportion), ]
foo2 <- subset(B07_Arthritis.state.sex, Sex == "Female") %>% top_n(7, Proportion)
foo2[order(-foo2$Proportion), ]
summary(subset(B07_Arthritis.state.sex, Sex == "Female")$Proportion)

#------------------------------------------------------------------------------------------------------------------------


#Demographic analysis

#1. Arthritis (RDD) prevalence by age 
B07_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B07_Arthtitis_age <- B07_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_arth_age_ci <- confint(B07_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B07_arth_age_ci <- B07_arth_age_ci[-c(1:11), ]
#join ci and proportions
B07_Arthritis.Age <- bind_cols(B07_Arthtitis_age, B07_arth_age_ci)
#Save
#write.csv(B07_Arthritis.Age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B07_Arthritis.age.csv")

#Age logistic regression
B07_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_age_glm)
exp(cbind(OR=coef(B07_age_glm), confint(B07_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B07_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B07_Arthritis_sex <- B07_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_arth_sex_ci <- confint(B07_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0
B07_arth_sex_ci <- B07_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B07_Arthritis.Sex <- bind_cols(B07_Arthritis_sex, B07_arth_sex_ci)
#save
#write.csv(B07_Arthritis.Sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B07_Arthritis.sex.csv")

#Sex logistic regression
B07_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_sex_glm)
exp(cbind(OR=coef(B07_sex_glm), confint(B07_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B07_Arth_emp <- svyby(formula = ~HAVARTH2,
                      by = ~EMPLOY,
                      design = BRFSS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B07_Arthritis_emp <- B07_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_arth_emp_ci <- confint(B07_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0
B07_arth_emp_ci <- B07_arth_emp_ci[-c(1:7), ]
#join ci and proportion
B07_Arthritis.Employ <- bind_cols(B07_Arthritis_emp, B07_arth_emp_ci)
#save
#write.csv(B07_Arthritis.Employ, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B07_Arthritis.employment.csv")

#Employment status logistic regression
B07_emp_glm <- svyglm(HAVARTH2~EMPLOY + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_emp_glm)
exp(cbind(OR=coef(B07_emp_glm), confint(B07_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B07_inc_glm <- svyglm(HAVARTH2~relevel(INCOME2, ref = ">= $75 0000") + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_inc_glm)
exp(cbind(OR=coef(B07_inc_glm), confint(B07_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B07_edu_glm <- svyglm(HAVARTH2~EDUCA + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_edu_glm)
exp(cbind(OR=coef(B07_edu_glm), confint(B07_edu_glm)))

#------------------------------------------------------------------------------------------------------------------------

#Lifestyle analysis

#1. Arthritis (RDD) by BMI (logistic regression)
B07.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI4` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07.BMI.glm)
exp(cbind(OR=coef(B07.BMI.glm), confint(B07.BMI.glm)))
