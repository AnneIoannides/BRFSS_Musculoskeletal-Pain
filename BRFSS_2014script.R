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


# == 2014 script == #

#DOWNLOAD

B14_url <- "http://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.ZIP"

BRtemp14 <- tempfile()
BRtemp14b <- tempfile()

download.file(B14_url, BRtemp14, mode = "wb")
unzip(zipfile = BRtemp14, exdir = BRtemp14b)
BRFSS14_raw <- read_xpt(file.path(BRtemp14b, "LLCP2014.XPT"))

saveRDS(object = BRFSS14_raw,
        file = "BRFSS_2014.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#CLEANING

#Observe the dataset
str(BRFSS14_raw) 
tail(BRFSS14_raw) 
glimpse(BRFSS14_raw) 
colnames(BRFSS14_raw)

#Units and sampling factors; states; demographic information; socio-economic status; mental health; arthritis diagnosis (& corresponding variables); exercise; sleep; quality of life variables; healthcare-seeking behaviour; substance habits; disability (walking); confounders
BRFSS14 <- dplyr::select(BRFSS14_raw,
                         "_STATE",
                         "_PSU", "_LLCPWT", "_STSTR",
                         "_AGEG5YR", "SEX",
                         "EDUCA", "EMPLOY1", "INCOME2",
                         "HAVARTH3",
                         "_BMI5")

#Observations
str(BRFSS14)
tail(BRFSS14)
glimpse(BRFSS14)
colnames(BRFSS14)

#Remove the US territories for which no analysis will be done
table(BRFSS14$`_STATE`)
BRFSS14$`_STATE` <- as.factor(BRFSS14$`_STATE`)
class(BRFSS14$`_STATE`)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS14))
#30 694 people no BMI data
#Some employment and income information missing
which(colSums(is.na(BRFSS14)) == nrow(BRFSS14))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Now have a look at the arthritis variable (HAVARTH3)
#Observe how many people answered yes, no, don't know, & refused
table(BRFSS14$HAVARTH3)

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS14$HAVARTH3 <- recode(BRFSS14$HAVARTH3,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS14$HAVARTH3 <- unknownToNA(BRFSS14$HAVARTH3, unknown = c("7", "9")))
table(BRFSS14$HAVARTH3)

BRFSS14$HAVARTH3 <- as.factor(BRFSS14$HAVARTH3)
class(BRFSS14$HAVARTH3)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS14$`_AGEG5YR`<- recode(BRFSS14$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS14$`_AGEG5YR` <- unknownToNA(BRFSS14$`_AGEG5YR`, unknown = "14"))
#Check
table(BRFSS14$`_AGEG5YR`)

BRFSS14$`_AGEG5YR` <- as.factor(BRFSS14$`_AGEG5YR`)
class(BRFSS14$`_AGEG5YR`)

#Recode sex data into categories corresponding to BRFSS codebook
BRFSS14$SEX <- recode(BRFSS14$SEX,
                      "1" = "Male",
                      "2" = "Female")
#Change the refused (7) and unknown (9) values to NA
(BRFSS14$SEX <- unknownToNA(BRFSS14$SEX, unknown = c("7", "9")))
#Check
table(BRFSS14$SEX)

BRFSS14$SEX <- as.factor(BRFSS14$SEX)


BRFSS14$EDUCA <- recode(BRFSS14$EDUCA,
                        "1" = "Never attended school/only kindergarten",
                        "2" = "Elementary school",
                        "3" = "Some high school",
                        "4" = "High school graduate",
                        "5" = "Some college/some technical school",
                        "6" = "College graduate",
                        "9" = "9")
#Change refused (9) values to NA
(BRFSS14$EDUCA <- unknownToNA(BRFSS14$EDUCA, unknown = "9"))
table(BRFSS14$EDUCA)

BRFSS14$EDUCA <- as.factor(BRFSS14$EDUCA)

#Recode income into categories corresponding to BRFSS codebook
BRFSS14$INCOME2 <- recode(BRFSS14$INCOME2,
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
(BRFSS14$INCOME2 <- unknownToNA(BRFSS14$INCOME2, unknown = c("77", "99")))
table(BRFSS14$INCOME2)

BRFSS14$INCOME2 <- as.factor(BRFSS14$INCOME2)

#Recode employment into categories corresponding to BRFSS codebook
# The BRFSS employment data is more comprehensive than the other surveys (ie offers options such as 'student' and 'retired'). Thus, this employment status data is labelled according to the BRFSS codebook precisely
BRFSS14$EMPLOY1 <- recode(BRFSS14$EMPLOY1,
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
(BRFSS14$EMPLOY1 <- unknownToNA(BRFSS14$EMPLOY1, unknown = "9"))
table(BRFSS14$EMPLOY1)

BRFSS14$EMPLOY1 <- as.factor(BRFSS14$EMPLOY1)

#BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS14 <- BRFSS14 %>%
  mutate(`_BMI5` = `_BMI5`/100)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS14_dataset <- subset(BRFSS14,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_LLCPWT`) &
                            !is.na(HAVARTH3))

#Check that the sum of the weights is equal to the US population
sum(BRFSS14_dataset$`_LLCPWT`)
#The sum of the weights is 247 103 316 which is acceptable

#Check the number of people (unique PSU's)
length(unique(BRFSS14_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 22 392

#Check the number of unique strata
length(unique(BRFSS14_dataset[["_STSTR"]]))
#The number of unique strata is 1 279

#Used to generate frequency tables (unweighted)

table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 18 to 24")$SEX)
table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 25 to 29")$SEX)
table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 30 to 34")$SEX)
table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 35 to 39")$SEX)
table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 40 to 44")$SEX)
table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 45 to 49")$SEX)
table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 50 to 54")$SEX)
table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 55 to 59")$SEX)
table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 60 to 64")$SEX)
table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 65 to 69")$SEX)
table(subset(BRFSS14_dataset, `_AGEG5YR` == "Age 70 and above")$SEX)


#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS14_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS14_dataset)
#Observe the design oject
BRFSS14_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(BRFSS14_DO,
        file = "BRFSS14_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#_______________________________________________________________________________________________


#ANALYSIS

#Overall prevalence
B14_overall <- svymean(~factor(HAVARTH3),
                       BRFSS14_DO,
                       na.rm = TRUE)
B14_overall.c <- B14_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_overall_ci <- confint(B14_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B14 <- bind_cols(B14_overall.c, B14_overall_ci)
#remove havarth = 0
B14 <- B14[-c(1), ] #final proportion, se and 95% ci
#backup save
#write.csv(B14, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B14_Arthritis.csv")

#------------------------------------------------------------------------------------------------------------------------

#Spatial analysis

#Arthritis prevalence (overall) by state
B14_Arth_state <- svyby(formula = ~HAVARTH3,
                        by = ~`_STATE`,
                        design = BRFSS14_DO,
                        FUN = svymean,
                        na.rm = TRUE)
B14_Arthtitis_state <- B14_Arth_state %>%
  select(1, 3, 5) %>%
  setNames(c("State", "Proportion", "SE")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_arth_state_ci <- confint(B14_Arth_state) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Rows 1-53 are the confidence intervals for the 0-values in the HAVARTH3 variable (ie those who have not been told they have arthritis), therefore I removed these rows
B14_arth_state_ci <- B14_arth_state_ci[-c(1:53), ]
#Join ci and proportion tables
B14_Arthritis.state <- bind_cols(B14_Arthtitis_state, B14_arth_state_ci)
#Save this table for convenience (will be saved to a separate folder therefore change working directory just for these saves)
#write.csv(B14_Arthritis.state, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B14_Arthritis.state.csv")

#Load information for mapping (lat and long for states)
us_states <- map_data("state")

# - Map~State(overall) - #

#Rename the states
B14_Arthritis.state$State[B14_Arthritis.state$State == "1"] <- "Alabama"
B14_Arthritis.state$State[B14_Arthritis.state$State == "2"] <- "Alaska"
B14_Arthritis.state$State[B14_Arthritis.state$State == "4"] <- "Arizona"
B14_Arthritis.state$State[B14_Arthritis.state$State == "5"] <- "Arkansas"
B14_Arthritis.state$State[B14_Arthritis.state$State == "6"] <- "California"
B14_Arthritis.state$State[B14_Arthritis.state$State == "8"] <- "Colorado"
B14_Arthritis.state$State[B14_Arthritis.state$State == "9"] <- "Connecticut"
B14_Arthritis.state$State[B14_Arthritis.state$State == "10"] <- "Delaware"
B14_Arthritis.state$State[B14_Arthritis.state$State == "11"] <- "District of Columbia"
B14_Arthritis.state$State[B14_Arthritis.state$State == "12"] <- "Florida"
B14_Arthritis.state$State[B14_Arthritis.state$State == "13"] <- "Georgia"
B14_Arthritis.state$State[B14_Arthritis.state$State == "15"] <- "Hawaii"
B14_Arthritis.state$State[B14_Arthritis.state$State == "16"] <- "Idaho"
B14_Arthritis.state$State[B14_Arthritis.state$State == "17"] <- "Illinois"
B14_Arthritis.state$State[B14_Arthritis.state$State == "18"] <- "Indiana"
B14_Arthritis.state$State[B14_Arthritis.state$State == "19"] <- "Iowa"
B14_Arthritis.state$State[B14_Arthritis.state$State == "20"] <- "Kansas"
B14_Arthritis.state$State[B14_Arthritis.state$State == "21"] <- "Kentucky"
B14_Arthritis.state$State[B14_Arthritis.state$State == "22"] <- "Louisiana"
B14_Arthritis.state$State[B14_Arthritis.state$State == "23"] <- "Maine"
B14_Arthritis.state$State[B14_Arthritis.state$State == "24"] <- "Maryland"
B14_Arthritis.state$State[B14_Arthritis.state$State == "25"] <- "Massachusetts"
B14_Arthritis.state$State[B14_Arthritis.state$State == "26"] <- "Michigan"
B14_Arthritis.state$State[B14_Arthritis.state$State == "27"] <- "Minnesota"
B14_Arthritis.state$State[B14_Arthritis.state$State == "28"] <- "Mississippi"
B14_Arthritis.state$State[B14_Arthritis.state$State == "29"] <- "Missouri"
B14_Arthritis.state$State[B14_Arthritis.state$State == "30"] <- "Montana"
B14_Arthritis.state$State[B14_Arthritis.state$State == "31"] <- "Nebraska"
B14_Arthritis.state$State[B14_Arthritis.state$State == "32"] <- "Nevada"
B14_Arthritis.state$State[B14_Arthritis.state$State == "33"] <- "New Hampshire"
B14_Arthritis.state$State[B14_Arthritis.state$State == "34"] <- "New Jersey"
B14_Arthritis.state$State[B14_Arthritis.state$State == "35"] <- "New Mexico"
B14_Arthritis.state$State[B14_Arthritis.state$State == "36"] <- "New York"
B14_Arthritis.state$State[B14_Arthritis.state$State == "37"] <- "North Carolina"
B14_Arthritis.state$State[B14_Arthritis.state$State == "38"] <- "North Dakota"
B14_Arthritis.state$State[B14_Arthritis.state$State == "39"] <- "Ohio"
B14_Arthritis.state$State[B14_Arthritis.state$State == "40"] <- "Oklahoma"
B14_Arthritis.state$State[B14_Arthritis.state$State == "41"] <- "Oregon"
B14_Arthritis.state$State[B14_Arthritis.state$State == "42"] <- "Pennsylvania"
B14_Arthritis.state$State[B14_Arthritis.state$State == "44"] <- "Rhode Island"
B14_Arthritis.state$State[B14_Arthritis.state$State == "45"] <- "South Carolina"
B14_Arthritis.state$State[B14_Arthritis.state$State == "46"] <- "South Dakota"
B14_Arthritis.state$State[B14_Arthritis.state$State == "47"] <- "Tennessee"
B14_Arthritis.state$State[B14_Arthritis.state$State == "48"] <- "Texas"
B14_Arthritis.state$State[B14_Arthritis.state$State == "49"] <- "Utah"
B14_Arthritis.state$State[B14_Arthritis.state$State == "50"] <- "Vermont"
B14_Arthritis.state$State[B14_Arthritis.state$State == "51"] <- "Virginia"
B14_Arthritis.state$State[B14_Arthritis.state$State == "53"] <- "Washington"
B14_Arthritis.state$State[B14_Arthritis.state$State == "54"] <- "West Virginia"
B14_Arthritis.state$State[B14_Arthritis.state$State == "55"] <- "Wisconsin"
B14_Arthritis.state$State[B14_Arthritis.state$State == "56"] <- "Wyoming"
B14_Arthritis.state$State[B14_Arthritis.state$State == "66"] <- "Guam"
B14_Arthritis.state$State[B14_Arthritis.state$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B14_Arthritis.state$region <- tolower(B14_Arthritis.state$State)
B14_Arthritis.state_join <- left_join(us_states, B14_Arthritis.state)

#Map
B14Arthritis.state.map <- ggplot(data = B14_Arthritis.state_join,
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
ggsave("B14.Arthritis.state.map.png", width = 4, height = 3)


#Spatial analysis

B14_arth_SF <- st_as_sf(B14_Arthritis.state_join,
                        coords = c("long", "lat"),
                        crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_SF$geometry)
#assess neighbours
neighbours18 <- poly2nb(B14_arth_SF)
neighbours18
B14_arth_SF$Proportion <- as.numeric(B14_arth_SF$Proportion)
neighbours18.ii <- poly2nb(B14_arth_SF, queen = FALSE)
neighbours18.ii
plot(B14_arth_SF, border = "lightgrey")
listw18 <- nb2listw(neighbours18.ii)
listw18
globalMoran18 <- moran.test(B14_arth_SF$Proportion, listw18, na.action = na.exclude)
globalMoran18
globalMoran18[["estimate"]][["Moran I statistic"]]
globalMoran18[["p.value"]]

B14_Arthritis.state %>% top_n(-6, Proportion)
B14_Arthritis.state %>% top_n(5, Proportion)
summary(B14_Arthritis.state$Proportion)


#............................................


#Arthritis prevalence (by age) by state
B14_Arth_state_age <- svyby(formula = ~HAVARTH3,
                            by = ~`_STATE` + `_AGEG5YR`,
                            design = BRFSS14_DO,
                            FUN = svymean,
                            na.rm = TRUE)
B14_Arthritis_state_age <- B14_Arth_state_age %>%
  select(1, 2, 4, 6) %>%
  setNames(c("State", "Age", "Proportion", "SE")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_arth_state_age_ci <- confint(B14_Arth_state_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) 
#Rows 1 to 636 are confidence intervals for HAVARTH3 = 0 (No), therefore I will remove them
B14_arth_state_age_ci <- B14_arth_state_age_ci[-c(1:583), ]
#Join ci and proportion tables
B14_Arthritis.state.age <- bind_cols(B14_Arthritis_state_age, B14_arth_state_age_ci)
#Save this table for convenience
#write.csv(B14_Arthritis.state.age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B14_Arthritis.state.age.csv")

# - Map~State(by age) - #

#Rename state values to state names
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "1"] <- "Alabama"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "2"] <- "Alaska"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "4"] <- "Arizona"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "5"] <- "Arkansas"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "6"] <- "California"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "8"] <- "Colorado"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "9"] <- "Connecticut"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "10"] <- "Delaware"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "11"] <- "District of Columbia"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "12"] <- "Florida"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "13"] <- "Georgia"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "15"] <- "Hawaii"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "16"] <- "Idaho"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "17"] <- "Illinois"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "18"] <- "Indiana"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "19"] <- "Iowa"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "20"] <- "Kansas"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "21"] <- "Kentucky"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "22"] <- "Louisiana"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "23"] <- "Maine"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "24"] <- "Maryland"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "25"] <- "Massachusetts"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "26"] <- "Michigan"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "27"] <- "Minnesota"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "28"] <- "Mississippi"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "29"] <- "Missouri"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "30"] <- "Montana"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "31"] <- "Nebraska"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "32"] <- "Nevada"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "33"] <- "New Hampshire"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "34"] <- "New Jersey"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "35"] <- "New Mexico"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "36"] <- "New York"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "37"] <- "North Carolina"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "38"] <- "North Dakota"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "39"] <- "Ohio"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "40"] <- "Oklahoma"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "41"] <- "Oregon"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "42"] <- "Pennsylvania"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "44"] <- "Rhode Island"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "45"] <- "South Carolina"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "46"] <- "South Dakota"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "47"] <- "Tennessee"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "48"] <- "Texas"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "49"] <- "Utah"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "50"] <- "Vermont"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "51"] <- "Virginia"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "53"] <- "Washington"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "54"] <- "West Virginia"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "55"] <- "Wisconsin"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "56"] <- "Wyoming"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "66"] <- "Guam"
B14_Arthritis.state.age$State[B14_Arthritis.state.age$State == "72"] <- "Puerto Rico"

B14_Arthritis.state.age$region <- tolower(B14_Arthritis.state.age$State)
B14_Arthritis.state.age.join <- left_join(us_states, B14_Arthritis.state.age)

#Map
B14_Arthritis.state.age.map <- ggplot(data = B14_Arthritis.state.age.join,
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
ggsave("B14.Arthritis.state.age.map.png", width = 6, height = 6)


#Spatial correlation

#subset for 18-24 SF
B14_state.age1824 <- subset(B14_Arthritis.state.age.join, Age == "Age 18 to 24")


B14_arth_1824SF <- st_as_sf(B14_state.age1824,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_1824SF$geometry)
#assess neighbours
neighbours18.1824 <- poly2nb(B14_arth_1824SF)
neighbours18.1824
B14_arth_1824SF$Proportion <- as.numeric(B14_arth_1824SF$Proportion)
neighbours18.1824ii <- poly2nb(B14_arth_1824SF, queen = FALSE)
neighbours18.1824ii
plot(B14_arth_1824SF, border = "lightgrey")
listw18.1824 <- nb2listw(neighbours18.1824ii)
listw18.1824
globalMoran18.1824 <- moran.test(B14_arth_1824SF$Proportion, listw18.1824, na.action = na.exclude)
globalMoran18.1824
globalMoran18.1824[["estimate"]][["Moran I statistic"]]
globalMoran18.1824[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 18 to 24") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 18 to 24") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 18 to 24")$Proportion)


#subset for 25-29 SF
B14_state.age2529 <- subset(B14_Arthritis.state.age.join, Age == "Age 25 to 29")

B14_arth_2529SF <- st_as_sf(B14_state.age2529,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_2529SF$geometry)
#assess neighbours
neighbours18.2529 <- poly2nb(B14_arth_2529SF)
neighbours18.2529
B14_arth_2529SF$Proportion <- as.numeric(B14_arth_2529SF$Proportion)
neighbours18.2529ii <- poly2nb(B14_arth_2529SF, queen = FALSE)
neighbours18.2529ii
plot(B14_arth_2529SF, border = "lightgrey")
listw18.2529 <- nb2listw(neighbours18.2529ii)
listw18.2529
globalMoran18.2529 <- moran.test(B14_arth_2529SF$Proportion, listw18.2529, na.action = na.exclude)
globalMoran18.2529
globalMoran18.2529[["estimate"]][["Moran I statistic"]]
globalMoran18.2529[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 25 to 29") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 25 to 29") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 25 to 29")$Proportion)

#subset for 30-34 SF
B14_state.age3034 <- subset(B14_Arthritis.state.age.join, Age == "Age 30 to 34")

B14_arth_3034SF <- st_as_sf(B14_state.age3034,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_3034SF$geometry)
#assess neighbours
neighbours18.3034 <- poly2nb(B14_arth_3034SF)
neighbours18.3034
B14_arth_3034SF$Proportion <- as.numeric(B14_arth_3034SF$Proportion)
neighbours18.3034ii <- poly2nb(B14_arth_3034SF, queen = FALSE)
neighbours18.3034ii
plot(B14_arth_3034SF, border = "lightgrey")
listw18.3034 <- nb2listw(neighbours18.3034ii)
listw18.3034
globalMoran18.3034 <- moran.test(B14_arth_3034SF$Proportion, listw18.3034, na.action = na.exclude)
globalMoran18.3034
globalMoran18.3034[["estimate"]][["Moran I statistic"]]
globalMoran18.3034[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 30 to 34") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 30 to 34") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 30 to 34")$Proportion)


#subset for 35-39 SF
B14_state.age3539 <- subset(B14_Arthritis.state.age.join, Age == "Age 40 to 44")

B14_arth_3539SF <- st_as_sf(B14_state.age3539,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_3539SF$geometry)
#assess neighbours
neighbours18.3539 <- poly2nb(B14_arth_3539SF)
neighbours18.3539
B14_arth_3539SF$Proportion <- as.numeric(B14_arth_3539SF$Proportion)
neighbours18.3539ii <- poly2nb(B14_arth_3539SF, queen = FALSE)
neighbours18.3539ii
plot(B14_arth_3539SF, border = "lightgrey")
listw18.3539 <- nb2listw(neighbours18.3539ii)
listw18.3539
globalMoran18.3539 <- moran.test(B14_arth_3539SF$Proportion, listw18.3539, na.action = na.exclude)
globalMoran18.3539
globalMoran18.3539[["estimate"]][["Moran I statistic"]]
globalMoran18.3539[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 35 to 39") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 35 to 39") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 35 to 39")$Proportion)


#subset for 40-44 SF
B14_state.age4044 <- subset(B14_Arthritis.state.age.join, Age == "Age 40 to 44")

B14_arth_4044SF <- st_as_sf(B14_state.age4044,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_4044SF$geometry)
#assess neighbours
neighbours18.4044 <- poly2nb(B14_arth_4044SF)
neighbours18.4044
B14_arth_4044SF$Proportion <- as.numeric(B14_arth_4044SF$Proportion)
neighbours18.4044ii <- poly2nb(B14_arth_4044SF, queen = FALSE)
neighbours18.4044ii
plot(B14_arth_4044SF, border = "lightgrey")
listw18.4044 <- nb2listw(neighbours18.4044ii)
listw18.4044
globalMoran18.4044 <- moran.test(B14_arth_4044SF$Proportion, listw18.4044, na.action = na.exclude)
globalMoran18.4044
globalMoran18.4044[["estimate"]][["Moran I statistic"]]
globalMoran18.4044[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 40 to 44") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 40 to 44") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 40 to 44")$Proportion)


#subset for 45-49 SF
B14_state.age4549 <- subset(B14_Arthritis.state.age.join, Age == "Age 45 to 49")

B14_arth_4549SF <- st_as_sf(B14_state.age4549,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_4549SF$geometry)
#assess neighbours
neighbours18.4549 <- poly2nb(B14_arth_4549SF)
neighbours18.4549
B14_arth_4549SF$Proportion <- as.numeric(B14_arth_4549SF$Proportion)
neighbours18.4549ii <- poly2nb(B14_arth_4549SF, queen = FALSE)
neighbours18.4549ii
plot(B14_arth_4549SF, border = "lightgrey")
listw18.4549 <- nb2listw(neighbours18.4549ii)
listw18.4549
globalMoran18.4549 <- moran.test(B14_arth_4549SF$Proportion, listw18.4549, na.action = na.exclude)
globalMoran18.4549
globalMoran18.4549[["estimate"]][["Moran I statistic"]]
globalMoran18.4549[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 45 to 49") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 45 to 49") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 45 to 49")$Proportion)

#subset for 50-54 SF
B14_state.age5054 <- subset(B14_Arthritis.state.age.join, Age == "Age 50 to 54")

B14_arth_5054SF <- st_as_sf(B14_state.age5054,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_5054SF$geometry)
#assess neighbours
neighbours18.5054 <- poly2nb(B14_arth_5054SF)
neighbours18.5054
B14_arth_5054SF$Proportion <- as.numeric(B14_arth_5054SF$Proportion)
neighbours18.5054ii <- poly2nb(B14_arth_5054SF, queen = FALSE)
neighbours18.5054ii
plot(B14_arth_5054SF, border = "lightgrey")
listw18.5054 <- nb2listw(neighbours18.5054ii)
listw18.5054
globalMoran18.5054 <- moran.test(B14_arth_5054SF$Proportion, listw18.5054, na.action = na.exclude)
globalMoran18.5054
globalMoran18.5054[["estimate"]][["Moran I statistic"]]
globalMoran18.5054[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 50 to 54") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 50 to 54") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 50 to 54")$Proportion)

#subset for 55-59 SF
B14_state.age5559 <- subset(B14_Arthritis.state.age.join, Age == "Age 55 to 59")

B14_arth_5559SF <- st_as_sf(B14_state.age5559,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_5559SF$geometry)
#assess neighbours
neighbours18.5559 <- poly2nb(B14_arth_5559SF)
neighbours18.5559
B14_arth_5559SF$Proportion <- as.numeric(B14_arth_5559SF$Proportion)
neighbours18.5559ii <- poly2nb(B14_arth_5559SF, queen = FALSE)
neighbours18.5559ii
plot(B14_arth_5559SF, border = "lightgrey")
listw18.5559 <- nb2listw(neighbours18.5559ii)
listw18.5559
globalMoran18.5559 <- moran.test(B14_arth_5559SF$Proportion, listw18.5559, na.action = na.exclude)
globalMoran18.5559
globalMoran18.5559[["estimate"]][["Moran I statistic"]]
globalMoran18.5559[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 55 to 59") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 55 to 59") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 55 to 59")$Proportion)

#subset for 60-64 SF
B14_state.age6064 <- subset(B14_Arthritis.state.age.join, Age == "Age 60 to 64")

B14_arth_6064SF <- st_as_sf(B14_state.age6064,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_6064SF$geometry)
#assess neighbours
neighbours18.6064 <- poly2nb(B14_arth_6064SF)
neighbours18.6064
B14_arth_6064SF$Proportion <- as.numeric(B14_arth_6064SF$Proportion)
neighbours18.6064ii <- poly2nb(B14_arth_6064SF, queen = FALSE)
neighbours18.6064ii
plot(B14_arth_6064SF, border = "lightgrey")
listw18.6064 <- nb2listw(neighbours18.6064ii)
listw18.6064
globalMoran18.6064 <- moran.test(B14_arth_6064SF$Proportion, listw18.6064, na.action = na.exclude)
globalMoran18.6064
globalMoran18.6064[["estimate"]][["Moran I statistic"]]
globalMoran18.6064[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 60 to 64") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 60 to 64") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 60 to 64")$Proportion)


#subset for 65-69 SF
B14_state.age6569 <- subset(B14_Arthritis.state.age.join, Age == "Age 65 to 69")

B14_arth_6569SF <- st_as_sf(B14_state.age6569,
                            coords = c("long", "lat"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_6569SF$geometry)
#assess neighbours
neighbours18.6569 <- poly2nb(B14_arth_6569SF)
neighbours18.6569
B14_arth_6569SF$Proportion <- as.numeric(B14_arth_6569SF$Proportion)
neighbours18.6569ii <- poly2nb(B14_arth_6569SF, queen = FALSE)
neighbours18.6569ii
plot(B14_arth_6569SF, border = "lightgrey")
listw18.6569 <- nb2listw(neighbours18.6569ii)
listw18.6569
globalMoran18.6569 <- moran.test(B14_arth_6569SF$Proportion, listw18.6569, na.action = na.exclude)
globalMoran18.6569
globalMoran18.6569[["estimate"]][["Moran I statistic"]]
globalMoran18.6569[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 65 to 69") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 65 to 69") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 65 to 69")$Proportion)

#subset for 70 & above SF
B14_state.age70 <- subset(B14_Arthritis.state.age.join, Age == "Age 70 and above")

B14_arth_70SF <- st_as_sf(B14_state.age70,
                          coords = c("long", "lat"),
                          crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_70SF$geometry)
#assess neighbours
neighbours18.70 <- poly2nb(B14_arth_70SF)
neighbours18.70
B14_arth_70SF$Proportion <- as.numeric(B14_arth_70SF$Proportion)
neighbours18.70ii <- poly2nb(B14_arth_70SF, queen = FALSE)
neighbours18.70ii
plot(B14_arth_70SF, border = "lightgrey")
listw18.70 <- nb2listw(neighbours18.70ii)
listw18.70
globalMoran18.70 <- moran.test(B14_arth_70SF$Proportion, listw18.70, na.action = na.exclude)
globalMoran18.70
globalMoran18.70[["estimate"]][["Moran I statistic"]]
globalMoran18.70[["p.value"]]

subset(B14_Arthritis.state.age, Age == "Age 70 and above") %>% top_n(-6, Proportion)
subset(B14_Arthritis.state.age, Age == "Age 70 and above") %>% top_n(5, Proportion)
summary(subset(B14_Arthritis.state.age, Age == "Age 70 and above")$Proportion)


#....................................


#Arthritis prevalence (by sex) by state
B14_Arth_state_sex <- svyby(formula = ~HAVARTH3,
                            by = ~`_STATE` + SEX,
                            design = BRFSS14_DO,
                            FUN = svymean,
                            na.rm = TRUE)
B14_Arthritis_state_sex <- B14_Arth_state_sex %>%
  select(1, 2, 4, 6) %>%
  setNames(c("State", "Sex", "Proportion", "SE")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_arth_state_sex_ci <- confint(B14_Arth_state_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Rows 1 to 106 are ci's for HAVARTH3 = 0, therefore delete
B14_arth_state_sex_ci <- B14_arth_state_sex_ci[-c(1:106), ]
#Join ci and proportion tables
B14_Arthritis.state.sex <- bind_cols(B14_Arthritis_state_sex, B14_arth_state_sex_ci)
#Save the table for convenience
#write.csv(B14_Arthritis.state.sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B14_Arthritis.state.sex.csv")

# - Map~State(by sex) - #

#Rename the states
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "1"] <- "Alabama"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "2"] <- "Alaska"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "4"] <- "Arizona"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "5"] <- "Arkansas"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "6"] <- "California"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "8"] <- "Colorado"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "9"] <- "Connecticut"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "10"] <- "Delaware"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "11"] <- "District of Columbia"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "12"] <- "Florida"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "13"] <- "Georgia"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "15"] <- "Hawaii"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "16"] <- "Idaho"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "17"] <- "Illinois"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "18"] <- "Indiana"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "19"] <- "Iowa"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "20"] <- "Kansas"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "21"] <- "Kentucky"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "22"] <- "Louisiana"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "23"] <- "Maine"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "24"] <- "Maryland"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "25"] <- "Massachusetts"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "26"] <- "Michigan"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "27"] <- "Minnesota"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "28"] <- "Mississippi"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "29"] <- "Missouri"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "30"] <- "Montana"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "31"] <- "Nebraska"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "32"] <- "Nevada"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "33"] <- "New Hampshire"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "34"] <- "New Jersey"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "35"] <- "New Mexico"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "36"] <- "New York"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "37"] <- "North Carolina"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "38"] <- "North Dakota"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "39"] <- "Ohio"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "40"] <- "Oklahoma"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "41"] <- "Oregon"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "42"] <- "Pennsylvania"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "44"] <- "Rhode Island"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "45"] <- "South Carolina"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "46"] <- "South Dakota"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "47"] <- "Tennessee"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "48"] <- "Texas"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "49"] <- "Utah"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "50"] <- "Vermont"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "51"] <- "Virginia"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "53"] <- "Washington"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "54"] <- "West Virginia"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "55"] <- "Wisconsin"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "56"] <- "Wyoming"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "66"] <- "Guam"
B14_Arthritis.state.sex$State[B14_Arthritis.state.sex$State == "72"] <- "Puerto Rico"

B14_Arthritis.state.sex$region <- tolower(B14_Arthritis.state.sex$State)
B14_Arthritis.state.sex.join <- left_join(us_states, B14_Arthritis.state.sex)

#Map
B14_Arthritis.state.sex.map <- ggplot(data = B14_Arthritis.state.sex.join,
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
ggsave("B14.Arthritis.state.sex.map.png", width = 7, height = 3)


#Spatial correlations

#subset Males
B14_state.sex.MM <- subset(B14_Arthritis.state.sex.join, Sex == "Male")

B14_arth_sfMM <- st_as_sf(B14_state.sex.MM,
                          coords = c("long", "lat"),
                          crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_sfMM$geometry)
#neighbours
neighbours <- poly2nb(B14_arth_sfMM)
neighbours
B14_arth_sfMM$Proportion <- as.numeric(B14_arth_sfMM$Proportion)
neighboursii <- poly2nb(B14_arth_sfMM, queen = FALSE)
neighboursii
plot(B14_arth_sfMM, border = "lightgrey")
list <- nb2listw(neighboursii)
list
globalMoran.MM <- moran.test(B14_arth_sfMM$Proportion, list, na.action = na.exclude)
globalMoran.MM
globalMoran.MM[["estimate"]][["Moran I statistic"]]
globalMoran.MM[["p.value"]]

foo <- subset(B14_Arthritis.state.sex, Sex == "Male") %>% top_n(-7, Proportion)
foo[order(foo$Proportion), ]
foo2 <- subset(B14_Arthritis.state.sex, Sex == "Male") %>% top_n(7, Proportion)
foo2[order(-foo2$Proportion), ]
summary(subset(B14_Arthritis.state.sex, Sex == "Male")$Proportion)


#subset Females
B14_state.sex.FF <- subset(B14_Arthritis.state.sex.join, Sex == "Female")

B14_arth_sfFF <- st_as_sf(B14_state.sex.FF,
                          coords = c("long", "lat"),
                          crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(region, Proportion) %>%
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>%
  mutate(state = str_to_title(region))
#(check geometry)
class(B14_arth_sfFF$geometry)
#neighbours
neighbours <- poly2nb(B14_arth_sfFF)
neighbours
B14_arth_sfFF$Proportion <- as.numeric(B14_arth_sfFF$Proportion)
neighboursii <- poly2nb(B14_arth_sfFF, queen = FALSE)
neighboursii
plot(B14_arth_sfFF, border = "lightgrey")
list <- nb2listw(neighboursii)
list
globalMoran.FF <- moran.test(B14_arth_sfFF$Proportion, list, na.action = na.exclude)
globalMoran.FF
globalMoran.FF[["estimate"]][["Moran I statistic"]]
globalMoran.FF[["p.value"]]

foo <- subset(B14_Arthritis.state.sex, Sex == "Female") %>% top_n(-7, Proportion)
foo[order(foo$Proportion), ]
foo2 <- subset(B14_Arthritis.state.sex, Sex == "Female") %>% top_n(7, Proportion)
foo2[order(-foo2$Proportion), ]
summary(subset(B14_Arthritis.state.sex, Sex == "Female")$Proportion)

#------------------------------------------------------------------------------------------------------------------------


#Demographic analysis

#1. Arthritis (RDD) prevalence by age 
B14_Arth_age <- svyby(formula = ~HAVARTH3,
                      by = ~`_AGEG5YR`,
                      design = BRFSS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B14_Arthtitis_age <- B14_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_arth_age_ci <- confint(B14_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B14_arth_age_ci <- B14_arth_age_ci[-c(1:11), ]
#join ci and proportions
B14_Arthritis.Age <- bind_cols(B14_Arthtitis_age, B14_arth_age_ci)
#Save
#write.csv(B14_Arthritis.Age, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B14_Arthritis.age.csv")

#Age logistic regression
B14_age_glm <- svyglm(HAVARTH3~`_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS14_DO)
summary(B14_age_glm)
exp(cbind(OR=coef(B14_age_glm), confint(B14_age_glm)))



#2. Arthritis (RDD) prevalence by sex
B14_Arth_sex <- svyby(formula = ~HAVARTH3,
                      by = ~SEX,
                      design = BRFSS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B14_Arthritis_sex <- B14_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_arth_sex_ci <- confint(B14_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B14_arth_sex_ci <- B14_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B14_Arthritis.Sex <- bind_cols(B14_Arthritis_sex, B14_arth_sex_ci)
#save
#write.csv(B14_Arthritis.Sex, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B14_Arthritis.sex.csv")

#Sex logistic regression
B14_sex_glm <- svyglm(HAVARTH3~relevel(SEX, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS14_DO)
summary(B14_sex_glm)
exp(cbind(OR=coef(B14_sex_glm), confint(B14_sex_glm)))



#3. Arthritis (RDD) prevalence by employment status
B14_Arth_emp <- svyby(formula = ~HAVARTH3,
                      by = ~EMPLOY1,
                      design = BRFSS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B14_Arthritis_emp <- B14_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_arth_emp_ci <- confint(B14_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B14_arth_emp_ci <- B14_arth_emp_ci[-c(1:7), ]
#join ci and proportion
B14_Arthritis.Employ <- bind_cols(B14_Arthritis_emp, B14_arth_emp_ci)
#save
#write.csv(B14_Arthritis.Employ, "C:\\Users\\Anne\\Desktop\\Backup csv's\\BRFSS\\B14_Arthritis.employment.csv")

#Employment status logistic regression
B14_emp_glm <- svyglm(HAVARTH3~EMPLOY1 + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS14_DO)
summary(B14_emp_glm)
exp(cbind(OR=coef(B14_emp_glm), confint(B14_emp_glm)))


#4. Arthritis (RDD) by income level (logistic regression)
B14_inc_glm <- svyglm(HAVARTH3~relevel(INCOME2, ref = ">= $75 0000") + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS14_DO)
summary(B14_inc_glm)
exp(cbind(OR=coef(B14_inc_glm), confint(B14_inc_glm)))


#5. Arthritis (RDD) by education level (logistic regression)
B14_edu_glm <- svyglm(HAVARTH3~EDUCA + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS14_DO)
summary(B14_edu_glm)
exp(cbind(OR=coef(B14_edu_glm), confint(B14_edu_glm)))

#------------------------------------------------------------------------------------------------------------------------

#Lifestyle analysis

#1. Arthritis (RDD) by BMI (logistic regression)
B14.BMI.glm <- svyglm(HAVARTH3 ~ `_BMI5` + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS14_DO)
summary(B14.BMI.glm)
exp(cbind(OR=coef(B14.BMI.glm), confint(B14.BMI.glm)))
