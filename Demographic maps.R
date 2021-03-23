#Anne Elizabeth Ioannides

#For the qualification of PhD (Physiology), University of the Witwatersrand, South Africa

#Behavioural Risk Factor Surveillance System (BRFSS)

#Due to very large file size, the workflow for BRFSS analyses is slightly different compared to the other surveys used in my PhD. Each year (data download, cleaning, and analysis) takes place in one script. 
#Each year (except module years) has one R script dedicated to it. Module years done collectively, and BRFSS figures that are constructed across time can be found in the Module years scripts

#Note - "Arthritis" is often used in this script as a synonym for Rheumatic Disorder Diagnosis (RDD). This is because the BRFSS variable for RDD is called "HAVARTH", and therefore "ARTHritis" was an easy-navigation term.

#This script is specifically to create choropleth maps of demographic distributions (age and sex) in the US by state, per year. It serves as an anchor for my results. 
#For convenience, I have uploaded the clean design objects that I used onto Dropbox so that I could easily create the figures using one script
#Although I have created these demographic maps for all years for which spatial data was analysed (2001, 2003, 2005, 2007, 2009, and 2011 to 2018), only years 2001, 2005, 2009, 2013, and 2017 are included within my thesis chapter; the rest are available on GitHub

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

#Load information for mapping (lat and long for states)
us_states <- map_data("state")
options(survey.lonely.psu = "adjust")

#Pull in design objects
BRFSS01 <- readRDS(url("https://www.dropbox.com/s/htgjvljlmyzhqn7/BRFSS01_DO.rds?dl=1", "rb"))
BRFSS03 <- readRDS(url("https://www.dropbox.com/s/jz8za6ffkhdyumq/BRFSS03_DO.rds?dl=1", "rb"))
BRFSS05 <- readRDS(url("https://www.dropbox.com/s/y0hdwwn55k2ri05/BRFSS05_DO.rds?dl=1", "rb"))
BRFSS07 <- readRDS(url("https://www.dropbox.com/s/2djl3r6vg4xx8ct/BRFSS07_DO.rds?dl=1", "rb"))
BRFSS09 <- readRDS(url("https://www.dropbox.com/s/rnxl30ntn2we2l8/BRFSS09_DO.rds?dl=1", "rb"))
BRFSS11 <- readRDS(url("https://www.dropbox.com/s/osr2i7m2i29aufa/BRFSS11_DO.rds?dl=1", "rb"))
BRFSS12 <- readRDS(url("https://www.dropbox.com/s/mg7plp03fbaevav/BRFSS12_DO.rds?dl=1", "rb"))
BRFSS13 <- readRDS(url("https://www.dropbox.com/s/75vpdirjegoeqoh/BRFSS13_DO.rds?dl=1", "rb"))
BRFSS14 <- readRDS(url("https://www.dropbox.com/s/ksz3xmfqzuynf28/BRFSS14_DO.rds?dl=1", "rb"))
BRFSS15 <- readRDS(url("https://www.dropbox.com/s/b629bvg6760j2e0/BRFSS15_DO.rds?dl=1", "rb"))
BRFSS16 <- readRDS(url("https://www.dropbox.com/s/v3kuf9ix2ycnwdk/BRFSS16_DO.rds?dl=1", "rb"))
BRFSS17 <- readRDS(url("https://www.dropbox.com/s/ohinlpl6uhtvf8l/BRFSS17_DO.rds?dl=1", "rb"))
BRFSS18 <- readRDS(url("https://www.dropbox.com/s/j0xr1zjqtsmybyg/BRFSS18_DO.rds?dl=1", "rb"))


#Calculate the prevalence of each age group, and sex, in each state

#2001
#2001-age
B01_age <- svyby(~AGE, ~`_STATE`, BRFSS01, svymean, na.rm = TRUE)
B01_age_sel <- B01_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B01_age_sel$State[B01_age_sel$State == "1"] <- "Alabama"
B01_age_sel$State[B01_age_sel$State == "2"] <- "Alaska"
B01_age_sel$State[B01_age_sel$State == "4"] <- "Arizona"
B01_age_sel$State[B01_age_sel$State == "5"] <- "Arkansas"
B01_age_sel$State[B01_age_sel$State == "6"] <- "California"
B01_age_sel$State[B01_age_sel$State == "8"] <- "Colorado"
B01_age_sel$State[B01_age_sel$State == "9"] <- "Connecticut"
B01_age_sel$State[B01_age_sel$State == "10"] <- "Delaware"
B01_age_sel$State[B01_age_sel$State == "11"] <- "District of Columbia"
B01_age_sel$State[B01_age_sel$State == "12"] <- "Florida"
B01_age_sel$State[B01_age_sel$State == "13"] <- "Georgia"
B01_age_sel$State[B01_age_sel$State == "15"] <- "Hawaii"
B01_age_sel$State[B01_age_sel$State == "16"] <- "Idaho"
B01_age_sel$State[B01_age_sel$State == "17"] <- "Illinois"
B01_age_sel$State[B01_age_sel$State == "18"] <- "Indiana"
B01_age_sel$State[B01_age_sel$State == "19"] <- "Iowa"
B01_age_sel$State[B01_age_sel$State == "20"] <- "Kansas"
B01_age_sel$State[B01_age_sel$State == "21"] <- "Kentucky"
B01_age_sel$State[B01_age_sel$State == "22"] <- "Louisiana"
B01_age_sel$State[B01_age_sel$State == "23"] <- "Maine"
B01_age_sel$State[B01_age_sel$State == "24"] <- "Maryland"
B01_age_sel$State[B01_age_sel$State == "25"] <- "Massachusetts"
B01_age_sel$State[B01_age_sel$State == "26"] <- "Michigan"
B01_age_sel$State[B01_age_sel$State == "27"] <- "Minnesota"
B01_age_sel$State[B01_age_sel$State == "28"] <- "Mississippi"
B01_age_sel$State[B01_age_sel$State == "29"] <- "Missouri"
B01_age_sel$State[B01_age_sel$State == "30"] <- "Montana"
B01_age_sel$State[B01_age_sel$State == "31"] <- "Nebraska"
B01_age_sel$State[B01_age_sel$State == "32"] <- "Nevada"
B01_age_sel$State[B01_age_sel$State == "33"] <- "New Hampshire"
B01_age_sel$State[B01_age_sel$State == "34"] <- "New Jersey"
B01_age_sel$State[B01_age_sel$State == "35"] <- "New Mexico"
B01_age_sel$State[B01_age_sel$State == "36"] <- "New York"
B01_age_sel$State[B01_age_sel$State == "37"] <- "North Carolina"
B01_age_sel$State[B01_age_sel$State == "38"] <- "North Dakota"
B01_age_sel$State[B01_age_sel$State == "39"] <- "Ohio"
B01_age_sel$State[B01_age_sel$State == "40"] <- "Oklahoma"
B01_age_sel$State[B01_age_sel$State == "41"] <- "Oregon"
B01_age_sel$State[B01_age_sel$State == "42"] <- "Pennsylvania"
B01_age_sel$State[B01_age_sel$State == "44"] <- "Rhode Island"
B01_age_sel$State[B01_age_sel$State == "45"] <- "South Carolina"
B01_age_sel$State[B01_age_sel$State == "46"] <- "South Dakota"
B01_age_sel$State[B01_age_sel$State == "47"] <- "Tennessee"
B01_age_sel$State[B01_age_sel$State == "48"] <- "Texas"
B01_age_sel$State[B01_age_sel$State == "49"] <- "Utah"
B01_age_sel$State[B01_age_sel$State == "50"] <- "Vermont"
B01_age_sel$State[B01_age_sel$State == "51"] <- "Virginia"
B01_age_sel$State[B01_age_sel$State == "53"] <- "Washington"
B01_age_sel$State[B01_age_sel$State == "54"] <- "West Virginia"
B01_age_sel$State[B01_age_sel$State == "55"] <- "Wisconsin"
B01_age_sel$State[B01_age_sel$State == "56"] <- "Wyoming"
B01_age_sel$State[B01_age_sel$State == "66"] <- "Guam"
B01_age_sel$State[B01_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B01_age_sel$region <- tolower(B01_age_sel$State)
B01_age_sel_join <- left_join(us_states, B01_age_sel)

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age1824.state.map.png")

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age2529.state.map.png")

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age3034.state.map.png")

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age3539.state.map.png")

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age4044.state.map.png")

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age4549.state.map.png")

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age5054.state.map.png")

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age5559.state.map.png")

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(3.5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age6064.state.map.png")

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age6569.state.map.png")

#Map
B01age.state.map <- ggplot(data = B01_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.age70ab.state.map.png")


#2001-sex
B01_sex <- svyby(~SEX, ~`_STATE`, BRFSS01, svymean, na.rm = TRUE)
B01_sex_sel <- B01_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B01_sex_sel$State[B01_sex_sel$State == "1"] <- "Alabama"
B01_sex_sel$State[B01_sex_sel$State == "2"] <- "Alaska"
B01_sex_sel$State[B01_sex_sel$State == "4"] <- "Arizona"
B01_sex_sel$State[B01_sex_sel$State == "5"] <- "Arkansas"
B01_sex_sel$State[B01_sex_sel$State == "6"] <- "California"
B01_sex_sel$State[B01_sex_sel$State == "8"] <- "Colorado"
B01_sex_sel$State[B01_sex_sel$State == "9"] <- "Connecticut"
B01_sex_sel$State[B01_sex_sel$State == "10"] <- "Delaware"
B01_sex_sel$State[B01_sex_sel$State == "11"] <- "District of Columbia"
B01_sex_sel$State[B01_sex_sel$State == "12"] <- "Florida"
B01_sex_sel$State[B01_sex_sel$State == "13"] <- "Georgia"
B01_sex_sel$State[B01_sex_sel$State == "15"] <- "Hawaii"
B01_sex_sel$State[B01_sex_sel$State == "16"] <- "Idaho"
B01_sex_sel$State[B01_sex_sel$State == "17"] <- "Illinois"
B01_sex_sel$State[B01_sex_sel$State == "18"] <- "Indiana"
B01_sex_sel$State[B01_sex_sel$State == "19"] <- "Iowa"
B01_sex_sel$State[B01_sex_sel$State == "20"] <- "Kansas"
B01_sex_sel$State[B01_sex_sel$State == "21"] <- "Kentucky"
B01_sex_sel$State[B01_sex_sel$State == "22"] <- "Louisiana"
B01_sex_sel$State[B01_sex_sel$State == "23"] <- "Maine"
B01_sex_sel$State[B01_sex_sel$State == "24"] <- "Maryland"
B01_sex_sel$State[B01_sex_sel$State == "25"] <- "Massachusetts"
B01_sex_sel$State[B01_sex_sel$State == "26"] <- "Michigan"
B01_sex_sel$State[B01_sex_sel$State == "27"] <- "Minnesota"
B01_sex_sel$State[B01_sex_sel$State == "28"] <- "Mississippi"
B01_sex_sel$State[B01_sex_sel$State == "29"] <- "Missouri"
B01_sex_sel$State[B01_sex_sel$State == "30"] <- "Montana"
B01_sex_sel$State[B01_sex_sel$State == "31"] <- "Nebraska"
B01_sex_sel$State[B01_sex_sel$State == "32"] <- "Nevada"
B01_sex_sel$State[B01_sex_sel$State == "33"] <- "New Hampshire"
B01_sex_sel$State[B01_sex_sel$State == "34"] <- "New Jersey"
B01_sex_sel$State[B01_sex_sel$State == "35"] <- "New Mexico"
B01_sex_sel$State[B01_sex_sel$State == "36"] <- "New York"
B01_sex_sel$State[B01_sex_sel$State == "37"] <- "North Carolina"
B01_sex_sel$State[B01_sex_sel$State == "38"] <- "North Dakota"
B01_sex_sel$State[B01_sex_sel$State == "39"] <- "Ohio"
B01_sex_sel$State[B01_sex_sel$State == "40"] <- "Oklahoma"
B01_sex_sel$State[B01_sex_sel$State == "41"] <- "Oregon"
B01_sex_sel$State[B01_sex_sel$State == "42"] <- "Pennsylvania"
B01_sex_sel$State[B01_sex_sel$State == "44"] <- "Rhode Island"
B01_sex_sel$State[B01_sex_sel$State == "45"] <- "South Carolina"
B01_sex_sel$State[B01_sex_sel$State == "46"] <- "South Dakota"
B01_sex_sel$State[B01_sex_sel$State == "47"] <- "Tennessee"
B01_sex_sel$State[B01_sex_sel$State == "48"] <- "Texas"
B01_sex_sel$State[B01_sex_sel$State == "49"] <- "Utah"
B01_sex_sel$State[B01_sex_sel$State == "50"] <- "Vermont"
B01_sex_sel$State[B01_sex_sel$State == "51"] <- "Virginia"
B01_sex_sel$State[B01_sex_sel$State == "53"] <- "Washington"
B01_sex_sel$State[B01_sex_sel$State == "54"] <- "West Virginia"
B01_sex_sel$State[B01_sex_sel$State == "55"] <- "Wisconsin"
B01_sex_sel$State[B01_sex_sel$State == "56"] <- "Wyoming"
B01_sex_sel$State[B01_sex_sel$State == "66"] <- "Guam"
B01_sex_sel$State[B01_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B01_sex_sel$region <- tolower(B01_sex_sel$State)
B01_sex_sel_join <- left_join(us_states, B01_sex_sel)

#Map
B01sex.state.map <- ggplot(data = B01_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2001") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B01.sex.state.map.png")

#2003
#2003-age
B03_age <- svyby(~AGE, ~`_STATE`, BRFSS03, svymean, na.rm = TRUE)
B03_age_sel <- B03_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B03_age_sel$State[B03_age_sel$State == "1"] <- "Alabama"
B03_age_sel$State[B03_age_sel$State == "2"] <- "Alaska"
B03_age_sel$State[B03_age_sel$State == "4"] <- "Arizona"
B03_age_sel$State[B03_age_sel$State == "5"] <- "Arkansas"
B03_age_sel$State[B03_age_sel$State == "6"] <- "California"
B03_age_sel$State[B03_age_sel$State == "8"] <- "Colorado"
B03_age_sel$State[B03_age_sel$State == "9"] <- "Connecticut"
B03_age_sel$State[B03_age_sel$State == "10"] <- "Delaware"
B03_age_sel$State[B03_age_sel$State == "11"] <- "District of Columbia"
B03_age_sel$State[B03_age_sel$State == "12"] <- "Florida"
B03_age_sel$State[B03_age_sel$State == "13"] <- "Georgia"
B03_age_sel$State[B03_age_sel$State == "15"] <- "Hawaii"
B03_age_sel$State[B03_age_sel$State == "16"] <- "Idaho"
B03_age_sel$State[B03_age_sel$State == "17"] <- "Illinois"
B03_age_sel$State[B03_age_sel$State == "18"] <- "Indiana"
B03_age_sel$State[B03_age_sel$State == "19"] <- "Iowa"
B03_age_sel$State[B03_age_sel$State == "20"] <- "Kansas"
B03_age_sel$State[B03_age_sel$State == "21"] <- "Kentucky"
B03_age_sel$State[B03_age_sel$State == "22"] <- "Louisiana"
B03_age_sel$State[B03_age_sel$State == "23"] <- "Maine"
B03_age_sel$State[B03_age_sel$State == "24"] <- "Maryland"
B03_age_sel$State[B03_age_sel$State == "25"] <- "Massachusetts"
B03_age_sel$State[B03_age_sel$State == "26"] <- "Michigan"
B03_age_sel$State[B03_age_sel$State == "27"] <- "Minnesota"
B03_age_sel$State[B03_age_sel$State == "28"] <- "Mississippi"
B03_age_sel$State[B03_age_sel$State == "29"] <- "Missouri"
B03_age_sel$State[B03_age_sel$State == "30"] <- "Montana"
B03_age_sel$State[B03_age_sel$State == "31"] <- "Nebraska"
B03_age_sel$State[B03_age_sel$State == "32"] <- "Nevada"
B03_age_sel$State[B03_age_sel$State == "33"] <- "New Hampshire"
B03_age_sel$State[B03_age_sel$State == "34"] <- "New Jersey"
B03_age_sel$State[B03_age_sel$State == "35"] <- "New Mexico"
B03_age_sel$State[B03_age_sel$State == "36"] <- "New York"
B03_age_sel$State[B03_age_sel$State == "37"] <- "North Carolina"
B03_age_sel$State[B03_age_sel$State == "38"] <- "North Dakota"
B03_age_sel$State[B03_age_sel$State == "39"] <- "Ohio"
B03_age_sel$State[B03_age_sel$State == "40"] <- "Oklahoma"
B03_age_sel$State[B03_age_sel$State == "41"] <- "Oregon"
B03_age_sel$State[B03_age_sel$State == "42"] <- "Pennsylvania"
B03_age_sel$State[B03_age_sel$State == "44"] <- "Rhode Island"
B03_age_sel$State[B03_age_sel$State == "45"] <- "South Carolina"
B03_age_sel$State[B03_age_sel$State == "46"] <- "South Dakota"
B03_age_sel$State[B03_age_sel$State == "47"] <- "Tennessee"
B03_age_sel$State[B03_age_sel$State == "48"] <- "Texas"
B03_age_sel$State[B03_age_sel$State == "49"] <- "Utah"
B03_age_sel$State[B03_age_sel$State == "50"] <- "Vermont"
B03_age_sel$State[B03_age_sel$State == "51"] <- "Virginia"
B03_age_sel$State[B03_age_sel$State == "53"] <- "Washington"
B03_age_sel$State[B03_age_sel$State == "54"] <- "West Virginia"
B03_age_sel$State[B03_age_sel$State == "55"] <- "Wisconsin"
B03_age_sel$State[B03_age_sel$State == "56"] <- "Wyoming"
B03_age_sel$State[B03_age_sel$State == "66"] <- "Guam"
B03_age_sel$State[B03_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B03_age_sel$region <- tolower(B03_age_sel$State)
B03_age_sel_join <- left_join(us_states, B03_age_sel)

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age1824.state.map.png")

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age2529.state.map.png")

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age3034.state.map.png")

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age3539.state.map.png")

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age4044.state.map.png")

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age4549.state.map.png")

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age5054.state.map.png")

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age5559.state.map.png")

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age6064.state.map.png")

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(3.5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age6569.state.map.png")

#Map
B03age.state.map <- ggplot(data = B03_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.age70ab.state.map.png")

#2003-sex
B03_sex <- svyby(~SEX, ~`_STATE`, BRFSS03, svymean, na.rm = TRUE)
B03_sex_sel <- B03_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B03_sex_sel$State[B03_sex_sel$State == "1"] <- "Alabama"
B03_sex_sel$State[B03_sex_sel$State == "2"] <- "Alaska"
B03_sex_sel$State[B03_sex_sel$State == "4"] <- "Arizona"
B03_sex_sel$State[B03_sex_sel$State == "5"] <- "Arkansas"
B03_sex_sel$State[B03_sex_sel$State == "6"] <- "California"
B03_sex_sel$State[B03_sex_sel$State == "8"] <- "Colorado"
B03_sex_sel$State[B03_sex_sel$State == "9"] <- "Connecticut"
B03_sex_sel$State[B03_sex_sel$State == "10"] <- "Delaware"
B03_sex_sel$State[B03_sex_sel$State == "11"] <- "District of Columbia"
B03_sex_sel$State[B03_sex_sel$State == "12"] <- "Florida"
B03_sex_sel$State[B03_sex_sel$State == "13"] <- "Georgia"
B03_sex_sel$State[B03_sex_sel$State == "15"] <- "Hawaii"
B03_sex_sel$State[B03_sex_sel$State == "16"] <- "Idaho"
B03_sex_sel$State[B03_sex_sel$State == "17"] <- "Illinois"
B03_sex_sel$State[B03_sex_sel$State == "18"] <- "Indiana"
B03_sex_sel$State[B03_sex_sel$State == "19"] <- "Iowa"
B03_sex_sel$State[B03_sex_sel$State == "20"] <- "Kansas"
B03_sex_sel$State[B03_sex_sel$State == "21"] <- "Kentucky"
B03_sex_sel$State[B03_sex_sel$State == "22"] <- "Louisiana"
B03_sex_sel$State[B03_sex_sel$State == "23"] <- "Maine"
B03_sex_sel$State[B03_sex_sel$State == "24"] <- "Maryland"
B03_sex_sel$State[B03_sex_sel$State == "25"] <- "Massachusetts"
B03_sex_sel$State[B03_sex_sel$State == "26"] <- "Michigan"
B03_sex_sel$State[B03_sex_sel$State == "27"] <- "Minnesota"
B03_sex_sel$State[B03_sex_sel$State == "28"] <- "Mississippi"
B03_sex_sel$State[B03_sex_sel$State == "29"] <- "Missouri"
B03_sex_sel$State[B03_sex_sel$State == "30"] <- "Montana"
B03_sex_sel$State[B03_sex_sel$State == "31"] <- "Nebraska"
B03_sex_sel$State[B03_sex_sel$State == "32"] <- "Nevada"
B03_sex_sel$State[B03_sex_sel$State == "33"] <- "New Hampshire"
B03_sex_sel$State[B03_sex_sel$State == "34"] <- "New Jersey"
B03_sex_sel$State[B03_sex_sel$State == "35"] <- "New Mexico"
B03_sex_sel$State[B03_sex_sel$State == "36"] <- "New York"
B03_sex_sel$State[B03_sex_sel$State == "37"] <- "North Carolina"
B03_sex_sel$State[B03_sex_sel$State == "38"] <- "North Dakota"
B03_sex_sel$State[B03_sex_sel$State == "39"] <- "Ohio"
B03_sex_sel$State[B03_sex_sel$State == "40"] <- "Oklahoma"
B03_sex_sel$State[B03_sex_sel$State == "41"] <- "Oregon"
B03_sex_sel$State[B03_sex_sel$State == "42"] <- "Pennsylvania"
B03_sex_sel$State[B03_sex_sel$State == "44"] <- "Rhode Island"
B03_sex_sel$State[B03_sex_sel$State == "45"] <- "South Carolina"
B03_sex_sel$State[B03_sex_sel$State == "46"] <- "South Dakota"
B03_sex_sel$State[B03_sex_sel$State == "47"] <- "Tennessee"
B03_sex_sel$State[B03_sex_sel$State == "48"] <- "Texas"
B03_sex_sel$State[B03_sex_sel$State == "49"] <- "Utah"
B03_sex_sel$State[B03_sex_sel$State == "50"] <- "Vermont"
B03_sex_sel$State[B03_sex_sel$State == "51"] <- "Virginia"
B03_sex_sel$State[B03_sex_sel$State == "53"] <- "Washington"
B03_sex_sel$State[B03_sex_sel$State == "54"] <- "West Virginia"
B03_sex_sel$State[B03_sex_sel$State == "55"] <- "Wisconsin"
B03_sex_sel$State[B03_sex_sel$State == "56"] <- "Wyoming"
B03_sex_sel$State[B03_sex_sel$State == "66"] <- "Guam"
B03_sex_sel$State[B03_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B03_sex_sel$region <- tolower(B03_sex_sel$State)
B03_sex_sel_join <- left_join(us_states, B03_sex_sel)

#Map
B03sex.state.map <- ggplot(data = B03_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2003") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B03.sex.state.map.png")


#2005
#2005-age
B05_age <- svyby(~AGE, ~`_STATE`, BRFSS05, svymean, na.rm = TRUE)
B05_age_sel <- B05_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B05_age_sel$State[B05_age_sel$State == "1"] <- "Alabama"
B05_age_sel$State[B05_age_sel$State == "2"] <- "Alaska"
B05_age_sel$State[B05_age_sel$State == "4"] <- "Arizona"
B05_age_sel$State[B05_age_sel$State == "5"] <- "Arkansas"
B05_age_sel$State[B05_age_sel$State == "6"] <- "California"
B05_age_sel$State[B05_age_sel$State == "8"] <- "Colorado"
B05_age_sel$State[B05_age_sel$State == "9"] <- "Connecticut"
B05_age_sel$State[B05_age_sel$State == "10"] <- "Delaware"
B05_age_sel$State[B05_age_sel$State == "11"] <- "District of Columbia"
B05_age_sel$State[B05_age_sel$State == "12"] <- "Florida"
B05_age_sel$State[B05_age_sel$State == "13"] <- "Georgia"
B05_age_sel$State[B05_age_sel$State == "15"] <- "Hawaii"
B05_age_sel$State[B05_age_sel$State == "16"] <- "Idaho"
B05_age_sel$State[B05_age_sel$State == "17"] <- "Illinois"
B05_age_sel$State[B05_age_sel$State == "18"] <- "Indiana"
B05_age_sel$State[B05_age_sel$State == "19"] <- "Iowa"
B05_age_sel$State[B05_age_sel$State == "20"] <- "Kansas"
B05_age_sel$State[B05_age_sel$State == "21"] <- "Kentucky"
B05_age_sel$State[B05_age_sel$State == "22"] <- "Louisiana"
B05_age_sel$State[B05_age_sel$State == "23"] <- "Maine"
B05_age_sel$State[B05_age_sel$State == "24"] <- "Maryland"
B05_age_sel$State[B05_age_sel$State == "25"] <- "Massachusetts"
B05_age_sel$State[B05_age_sel$State == "26"] <- "Michigan"
B05_age_sel$State[B05_age_sel$State == "27"] <- "Minnesota"
B05_age_sel$State[B05_age_sel$State == "28"] <- "Mississippi"
B05_age_sel$State[B05_age_sel$State == "29"] <- "Missouri"
B05_age_sel$State[B05_age_sel$State == "30"] <- "Montana"
B05_age_sel$State[B05_age_sel$State == "31"] <- "Nebraska"
B05_age_sel$State[B05_age_sel$State == "32"] <- "Nevada"
B05_age_sel$State[B05_age_sel$State == "33"] <- "New Hampshire"
B05_age_sel$State[B05_age_sel$State == "34"] <- "New Jersey"
B05_age_sel$State[B05_age_sel$State == "35"] <- "New Mexico"
B05_age_sel$State[B05_age_sel$State == "36"] <- "New York"
B05_age_sel$State[B05_age_sel$State == "37"] <- "North Carolina"
B05_age_sel$State[B05_age_sel$State == "38"] <- "North Dakota"
B05_age_sel$State[B05_age_sel$State == "39"] <- "Ohio"
B05_age_sel$State[B05_age_sel$State == "40"] <- "Oklahoma"
B05_age_sel$State[B05_age_sel$State == "41"] <- "Oregon"
B05_age_sel$State[B05_age_sel$State == "42"] <- "Pennsylvania"
B05_age_sel$State[B05_age_sel$State == "44"] <- "Rhode Island"
B05_age_sel$State[B05_age_sel$State == "45"] <- "South Carolina"
B05_age_sel$State[B05_age_sel$State == "46"] <- "South Dakota"
B05_age_sel$State[B05_age_sel$State == "47"] <- "Tennessee"
B05_age_sel$State[B05_age_sel$State == "48"] <- "Texas"
B05_age_sel$State[B05_age_sel$State == "49"] <- "Utah"
B05_age_sel$State[B05_age_sel$State == "50"] <- "Vermont"
B05_age_sel$State[B05_age_sel$State == "51"] <- "Virginia"
B05_age_sel$State[B05_age_sel$State == "53"] <- "Washington"
B05_age_sel$State[B05_age_sel$State == "54"] <- "West Virginia"
B05_age_sel$State[B05_age_sel$State == "55"] <- "Wisconsin"
B05_age_sel$State[B05_age_sel$State == "56"] <- "Wyoming"
B05_age_sel$State[B05_age_sel$State == "66"] <- "Guam"
B05_age_sel$State[B05_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B05_age_sel$region <- tolower(B05_age_sel$State)
B05_age_sel_join <- left_join(us_states, B05_age_sel)

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age1824.state.map.png")

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age2529.state.map.png")

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age3034.state.map.png")

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age3539.state.map.png")

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age4044.state.map.png")

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age4549.state.map.png")

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age5054.state.map.png")

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age5559.state.map.png")

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age6064.state.map.png")

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(3.5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age6569.state.map.png")

#Map
B05age.state.map <- ggplot(data = B05_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.age70ab.state.map.png")


#2005-sex
B05_sex <- svyby(~SEX, ~`_STATE`, BRFSS05, svymean, na.rm = TRUE)
B05_sex_sel <- B05_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B05_sex_sel$State[B05_sex_sel$State == "1"] <- "Alabama"
B05_sex_sel$State[B05_sex_sel$State == "2"] <- "Alaska"
B05_sex_sel$State[B05_sex_sel$State == "4"] <- "Arizona"
B05_sex_sel$State[B05_sex_sel$State == "5"] <- "Arkansas"
B05_sex_sel$State[B05_sex_sel$State == "6"] <- "California"
B05_sex_sel$State[B05_sex_sel$State == "8"] <- "Colorado"
B05_sex_sel$State[B05_sex_sel$State == "9"] <- "Connecticut"
B05_sex_sel$State[B05_sex_sel$State == "10"] <- "Delaware"
B05_sex_sel$State[B05_sex_sel$State == "11"] <- "District of Columbia"
B05_sex_sel$State[B05_sex_sel$State == "12"] <- "Florida"
B05_sex_sel$State[B05_sex_sel$State == "13"] <- "Georgia"
B05_sex_sel$State[B05_sex_sel$State == "15"] <- "Hawaii"
B05_sex_sel$State[B05_sex_sel$State == "16"] <- "Idaho"
B05_sex_sel$State[B05_sex_sel$State == "17"] <- "Illinois"
B05_sex_sel$State[B05_sex_sel$State == "18"] <- "Indiana"
B05_sex_sel$State[B05_sex_sel$State == "19"] <- "Iowa"
B05_sex_sel$State[B05_sex_sel$State == "20"] <- "Kansas"
B05_sex_sel$State[B05_sex_sel$State == "21"] <- "Kentucky"
B05_sex_sel$State[B05_sex_sel$State == "22"] <- "Louisiana"
B05_sex_sel$State[B05_sex_sel$State == "23"] <- "Maine"
B05_sex_sel$State[B05_sex_sel$State == "24"] <- "Maryland"
B05_sex_sel$State[B05_sex_sel$State == "25"] <- "Massachusetts"
B05_sex_sel$State[B05_sex_sel$State == "26"] <- "Michigan"
B05_sex_sel$State[B05_sex_sel$State == "27"] <- "Minnesota"
B05_sex_sel$State[B05_sex_sel$State == "28"] <- "Mississippi"
B05_sex_sel$State[B05_sex_sel$State == "29"] <- "Missouri"
B05_sex_sel$State[B05_sex_sel$State == "30"] <- "Montana"
B05_sex_sel$State[B05_sex_sel$State == "31"] <- "Nebraska"
B05_sex_sel$State[B05_sex_sel$State == "32"] <- "Nevada"
B05_sex_sel$State[B05_sex_sel$State == "33"] <- "New Hampshire"
B05_sex_sel$State[B05_sex_sel$State == "34"] <- "New Jersey"
B05_sex_sel$State[B05_sex_sel$State == "35"] <- "New Mexico"
B05_sex_sel$State[B05_sex_sel$State == "36"] <- "New York"
B05_sex_sel$State[B05_sex_sel$State == "37"] <- "North Carolina"
B05_sex_sel$State[B05_sex_sel$State == "38"] <- "North Dakota"
B05_sex_sel$State[B05_sex_sel$State == "39"] <- "Ohio"
B05_sex_sel$State[B05_sex_sel$State == "40"] <- "Oklahoma"
B05_sex_sel$State[B05_sex_sel$State == "41"] <- "Oregon"
B05_sex_sel$State[B05_sex_sel$State == "42"] <- "Pennsylvania"
B05_sex_sel$State[B05_sex_sel$State == "44"] <- "Rhode Island"
B05_sex_sel$State[B05_sex_sel$State == "45"] <- "South Carolina"
B05_sex_sel$State[B05_sex_sel$State == "46"] <- "South Dakota"
B05_sex_sel$State[B05_sex_sel$State == "47"] <- "Tennessee"
B05_sex_sel$State[B05_sex_sel$State == "48"] <- "Texas"
B05_sex_sel$State[B05_sex_sel$State == "49"] <- "Utah"
B05_sex_sel$State[B05_sex_sel$State == "50"] <- "Vermont"
B05_sex_sel$State[B05_sex_sel$State == "51"] <- "Virginia"
B05_sex_sel$State[B05_sex_sel$State == "53"] <- "Washington"
B05_sex_sel$State[B05_sex_sel$State == "54"] <- "West Virginia"
B05_sex_sel$State[B05_sex_sel$State == "55"] <- "Wisconsin"
B05_sex_sel$State[B05_sex_sel$State == "56"] <- "Wyoming"
B05_sex_sel$State[B05_sex_sel$State == "66"] <- "Guam"
B05_sex_sel$State[B05_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B05_sex_sel$region <- tolower(B05_sex_sel$State)
B05_sex_sel_join <- left_join(us_states, B05_sex_sel)

#Map
B05sex.state.map <- ggplot(data = B05_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2005") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B05.sex.state.map.png")

#2007
#2007-age
B07_age <- svyby(~AGE, ~`_STATE`, BRFSS07, svymean, na.rm = TRUE)
B07_age_sel <- B07_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B07_age_sel$State[B07_age_sel$State == "1"] <- "Alabama"
B07_age_sel$State[B07_age_sel$State == "2"] <- "Alaska"
B07_age_sel$State[B07_age_sel$State == "4"] <- "Arizona"
B07_age_sel$State[B07_age_sel$State == "5"] <- "Arkansas"
B07_age_sel$State[B07_age_sel$State == "6"] <- "California"
B07_age_sel$State[B07_age_sel$State == "8"] <- "Colorado"
B07_age_sel$State[B07_age_sel$State == "9"] <- "Connecticut"
B07_age_sel$State[B07_age_sel$State == "10"] <- "Delaware"
B07_age_sel$State[B07_age_sel$State == "11"] <- "District of Columbia"
B07_age_sel$State[B07_age_sel$State == "12"] <- "Florida"
B07_age_sel$State[B07_age_sel$State == "13"] <- "Georgia"
B07_age_sel$State[B07_age_sel$State == "15"] <- "Hawaii"
B07_age_sel$State[B07_age_sel$State == "16"] <- "Idaho"
B07_age_sel$State[B07_age_sel$State == "17"] <- "Illinois"
B07_age_sel$State[B07_age_sel$State == "18"] <- "Indiana"
B07_age_sel$State[B07_age_sel$State == "19"] <- "Iowa"
B07_age_sel$State[B07_age_sel$State == "20"] <- "Kansas"
B07_age_sel$State[B07_age_sel$State == "21"] <- "Kentucky"
B07_age_sel$State[B07_age_sel$State == "22"] <- "Louisiana"
B07_age_sel$State[B07_age_sel$State == "23"] <- "Maine"
B07_age_sel$State[B07_age_sel$State == "24"] <- "Maryland"
B07_age_sel$State[B07_age_sel$State == "25"] <- "Massachusetts"
B07_age_sel$State[B07_age_sel$State == "26"] <- "Michigan"
B07_age_sel$State[B07_age_sel$State == "27"] <- "Minnesota"
B07_age_sel$State[B07_age_sel$State == "28"] <- "Mississippi"
B07_age_sel$State[B07_age_sel$State == "29"] <- "Missouri"
B07_age_sel$State[B07_age_sel$State == "30"] <- "Montana"
B07_age_sel$State[B07_age_sel$State == "31"] <- "Nebraska"
B07_age_sel$State[B07_age_sel$State == "32"] <- "Nevada"
B07_age_sel$State[B07_age_sel$State == "33"] <- "New Hampshire"
B07_age_sel$State[B07_age_sel$State == "34"] <- "New Jersey"
B07_age_sel$State[B07_age_sel$State == "35"] <- "New Mexico"
B07_age_sel$State[B07_age_sel$State == "36"] <- "New York"
B07_age_sel$State[B07_age_sel$State == "37"] <- "North Carolina"
B07_age_sel$State[B07_age_sel$State == "38"] <- "North Dakota"
B07_age_sel$State[B07_age_sel$State == "39"] <- "Ohio"
B07_age_sel$State[B07_age_sel$State == "40"] <- "Oklahoma"
B07_age_sel$State[B07_age_sel$State == "41"] <- "Oregon"
B07_age_sel$State[B07_age_sel$State == "42"] <- "Pennsylvania"
B07_age_sel$State[B07_age_sel$State == "44"] <- "Rhode Island"
B07_age_sel$State[B07_age_sel$State == "45"] <- "South Carolina"
B07_age_sel$State[B07_age_sel$State == "46"] <- "South Dakota"
B07_age_sel$State[B07_age_sel$State == "47"] <- "Tennessee"
B07_age_sel$State[B07_age_sel$State == "48"] <- "Texas"
B07_age_sel$State[B07_age_sel$State == "49"] <- "Utah"
B07_age_sel$State[B07_age_sel$State == "50"] <- "Vermont"
B07_age_sel$State[B07_age_sel$State == "51"] <- "Virginia"
B07_age_sel$State[B07_age_sel$State == "53"] <- "Washington"
B07_age_sel$State[B07_age_sel$State == "54"] <- "West Virginia"
B07_age_sel$State[B07_age_sel$State == "55"] <- "Wisconsin"
B07_age_sel$State[B07_age_sel$State == "56"] <- "Wyoming"
B07_age_sel$State[B07_age_sel$State == "66"] <- "Guam"
B07_age_sel$State[B07_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B07_age_sel$region <- tolower(B07_age_sel$State)
B07_age_sel_join <- left_join(us_states, B07_age_sel)

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age1824.state.map.png")

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age2529.state.map.png")

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age3034.state.map.png")

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age3539.state.map.png")

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age4044.state.map.png")

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age4549.state.map.png")

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age5054.state.map.png")

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age5559.state.map.png")

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age6064.state.map.png")

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(3.5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age6569.state.map.png")

#Map
B07age.state.map <- ggplot(data = B07_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.age70ab.state.map.png")


#2007-sex
B07_sex <- svyby(~SEX, ~`_STATE`, BRFSS07, svymean, na.rm = TRUE)
B07_sex_sel <- B07_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B07_sex_sel$State[B07_sex_sel$State == "1"] <- "Alabama"
B07_sex_sel$State[B07_sex_sel$State == "2"] <- "Alaska"
B07_sex_sel$State[B07_sex_sel$State == "4"] <- "Arizona"
B07_sex_sel$State[B07_sex_sel$State == "5"] <- "Arkansas"
B07_sex_sel$State[B07_sex_sel$State == "6"] <- "California"
B07_sex_sel$State[B07_sex_sel$State == "8"] <- "Colorado"
B07_sex_sel$State[B07_sex_sel$State == "9"] <- "Connecticut"
B07_sex_sel$State[B07_sex_sel$State == "10"] <- "Delaware"
B07_sex_sel$State[B07_sex_sel$State == "11"] <- "District of Columbia"
B07_sex_sel$State[B07_sex_sel$State == "12"] <- "Florida"
B07_sex_sel$State[B07_sex_sel$State == "13"] <- "Georgia"
B07_sex_sel$State[B07_sex_sel$State == "15"] <- "Hawaii"
B07_sex_sel$State[B07_sex_sel$State == "16"] <- "Idaho"
B07_sex_sel$State[B07_sex_sel$State == "17"] <- "Illinois"
B07_sex_sel$State[B07_sex_sel$State == "18"] <- "Indiana"
B07_sex_sel$State[B07_sex_sel$State == "19"] <- "Iowa"
B07_sex_sel$State[B07_sex_sel$State == "20"] <- "Kansas"
B07_sex_sel$State[B07_sex_sel$State == "21"] <- "Kentucky"
B07_sex_sel$State[B07_sex_sel$State == "22"] <- "Louisiana"
B07_sex_sel$State[B07_sex_sel$State == "23"] <- "Maine"
B07_sex_sel$State[B07_sex_sel$State == "24"] <- "Maryland"
B07_sex_sel$State[B07_sex_sel$State == "25"] <- "Massachusetts"
B07_sex_sel$State[B07_sex_sel$State == "26"] <- "Michigan"
B07_sex_sel$State[B07_sex_sel$State == "27"] <- "Minnesota"
B07_sex_sel$State[B07_sex_sel$State == "28"] <- "Mississippi"
B07_sex_sel$State[B07_sex_sel$State == "29"] <- "Missouri"
B07_sex_sel$State[B07_sex_sel$State == "30"] <- "Montana"
B07_sex_sel$State[B07_sex_sel$State == "31"] <- "Nebraska"
B07_sex_sel$State[B07_sex_sel$State == "32"] <- "Nevada"
B07_sex_sel$State[B07_sex_sel$State == "33"] <- "New Hampshire"
B07_sex_sel$State[B07_sex_sel$State == "34"] <- "New Jersey"
B07_sex_sel$State[B07_sex_sel$State == "35"] <- "New Mexico"
B07_sex_sel$State[B07_sex_sel$State == "36"] <- "New York"
B07_sex_sel$State[B07_sex_sel$State == "37"] <- "North Carolina"
B07_sex_sel$State[B07_sex_sel$State == "38"] <- "North Dakota"
B07_sex_sel$State[B07_sex_sel$State == "39"] <- "Ohio"
B07_sex_sel$State[B07_sex_sel$State == "40"] <- "Oklahoma"
B07_sex_sel$State[B07_sex_sel$State == "41"] <- "Oregon"
B07_sex_sel$State[B07_sex_sel$State == "42"] <- "Pennsylvania"
B07_sex_sel$State[B07_sex_sel$State == "44"] <- "Rhode Island"
B07_sex_sel$State[B07_sex_sel$State == "45"] <- "South Carolina"
B07_sex_sel$State[B07_sex_sel$State == "46"] <- "South Dakota"
B07_sex_sel$State[B07_sex_sel$State == "47"] <- "Tennessee"
B07_sex_sel$State[B07_sex_sel$State == "48"] <- "Texas"
B07_sex_sel$State[B07_sex_sel$State == "49"] <- "Utah"
B07_sex_sel$State[B07_sex_sel$State == "50"] <- "Vermont"
B07_sex_sel$State[B07_sex_sel$State == "51"] <- "Virginia"
B07_sex_sel$State[B07_sex_sel$State == "53"] <- "Washington"
B07_sex_sel$State[B07_sex_sel$State == "54"] <- "West Virginia"
B07_sex_sel$State[B07_sex_sel$State == "55"] <- "Wisconsin"
B07_sex_sel$State[B07_sex_sel$State == "56"] <- "Wyoming"
B07_sex_sel$State[B07_sex_sel$State == "66"] <- "Guam"
B07_sex_sel$State[B07_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B07_sex_sel$region <- tolower(B07_sex_sel$State)
B07_sex_sel_join <- left_join(us_states, B07_sex_sel)

#Map
B07sex.state.map <- ggplot(data = B07_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2007") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B07.sex.state.map.png")

#2009
#2009-age
B09_age <- svyby(~AGE, ~`_STATE`, BRFSS09, svymean, na.rm = TRUE)
B09_age_sel <- B09_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B09_age_sel$State[B09_age_sel$State == "1"] <- "Alabama"
B09_age_sel$State[B09_age_sel$State == "2"] <- "Alaska"
B09_age_sel$State[B09_age_sel$State == "4"] <- "Arizona"
B09_age_sel$State[B09_age_sel$State == "5"] <- "Arkansas"
B09_age_sel$State[B09_age_sel$State == "6"] <- "California"
B09_age_sel$State[B09_age_sel$State == "8"] <- "Colorado"
B09_age_sel$State[B09_age_sel$State == "9"] <- "Connecticut"
B09_age_sel$State[B09_age_sel$State == "10"] <- "Delaware"
B09_age_sel$State[B09_age_sel$State == "11"] <- "District of Columbia"
B09_age_sel$State[B09_age_sel$State == "12"] <- "Florida"
B09_age_sel$State[B09_age_sel$State == "13"] <- "Georgia"
B09_age_sel$State[B09_age_sel$State == "15"] <- "Hawaii"
B09_age_sel$State[B09_age_sel$State == "16"] <- "Idaho"
B09_age_sel$State[B09_age_sel$State == "17"] <- "Illinois"
B09_age_sel$State[B09_age_sel$State == "18"] <- "Indiana"
B09_age_sel$State[B09_age_sel$State == "19"] <- "Iowa"
B09_age_sel$State[B09_age_sel$State == "20"] <- "Kansas"
B09_age_sel$State[B09_age_sel$State == "21"] <- "Kentucky"
B09_age_sel$State[B09_age_sel$State == "22"] <- "Louisiana"
B09_age_sel$State[B09_age_sel$State == "23"] <- "Maine"
B09_age_sel$State[B09_age_sel$State == "24"] <- "Maryland"
B09_age_sel$State[B09_age_sel$State == "25"] <- "Massachusetts"
B09_age_sel$State[B09_age_sel$State == "26"] <- "Michigan"
B09_age_sel$State[B09_age_sel$State == "27"] <- "Minnesota"
B09_age_sel$State[B09_age_sel$State == "28"] <- "Mississippi"
B09_age_sel$State[B09_age_sel$State == "29"] <- "Missouri"
B09_age_sel$State[B09_age_sel$State == "30"] <- "Montana"
B09_age_sel$State[B09_age_sel$State == "31"] <- "Nebraska"
B09_age_sel$State[B09_age_sel$State == "32"] <- "Nevada"
B09_age_sel$State[B09_age_sel$State == "33"] <- "New Hampshire"
B09_age_sel$State[B09_age_sel$State == "34"] <- "New Jersey"
B09_age_sel$State[B09_age_sel$State == "35"] <- "New Mexico"
B09_age_sel$State[B09_age_sel$State == "36"] <- "New York"
B09_age_sel$State[B09_age_sel$State == "37"] <- "North Carolina"
B09_age_sel$State[B09_age_sel$State == "38"] <- "North Dakota"
B09_age_sel$State[B09_age_sel$State == "39"] <- "Ohio"
B09_age_sel$State[B09_age_sel$State == "40"] <- "Oklahoma"
B09_age_sel$State[B09_age_sel$State == "41"] <- "Oregon"
B09_age_sel$State[B09_age_sel$State == "42"] <- "Pennsylvania"
B09_age_sel$State[B09_age_sel$State == "44"] <- "Rhode Island"
B09_age_sel$State[B09_age_sel$State == "45"] <- "South Carolina"
B09_age_sel$State[B09_age_sel$State == "46"] <- "South Dakota"
B09_age_sel$State[B09_age_sel$State == "47"] <- "Tennessee"
B09_age_sel$State[B09_age_sel$State == "48"] <- "Texas"
B09_age_sel$State[B09_age_sel$State == "49"] <- "Utah"
B09_age_sel$State[B09_age_sel$State == "50"] <- "Vermont"
B09_age_sel$State[B09_age_sel$State == "51"] <- "Virginia"
B09_age_sel$State[B09_age_sel$State == "53"] <- "Washington"
B09_age_sel$State[B09_age_sel$State == "54"] <- "West Virginia"
B09_age_sel$State[B09_age_sel$State == "55"] <- "Wisconsin"
B09_age_sel$State[B09_age_sel$State == "56"] <- "Wyoming"
B09_age_sel$State[B09_age_sel$State == "66"] <- "Guam"
B09_age_sel$State[B09_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B09_age_sel$region <- tolower(B09_age_sel$State)
B09_age_sel_join <- left_join(us_states, B09_age_sel)

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age1824.state.map.png")

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age2529.state.map.png")

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age3034.state.map.png")

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age3539.state.map.png")

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age4044.state.map.png")

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age4549.state.map.png")

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age5054.state.map.png")

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age5559.state.map.png")

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age6064.state.map.png")

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(3.5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age6569.state.map.png")

#Map
B09age.state.map <- ggplot(data = B09_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.age70ab.state.map.png")

#2009-sex
B09_sex <- svyby(~SEX, ~`_STATE`, BRFSS09, svymean, na.rm = TRUE)
B09_sex_sel <- B09_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B09_sex_sel$State[B09_sex_sel$State == "1"] <- "Alabama"
B09_sex_sel$State[B09_sex_sel$State == "2"] <- "Alaska"
B09_sex_sel$State[B09_sex_sel$State == "4"] <- "Arizona"
B09_sex_sel$State[B09_sex_sel$State == "5"] <- "Arkansas"
B09_sex_sel$State[B09_sex_sel$State == "6"] <- "California"
B09_sex_sel$State[B09_sex_sel$State == "8"] <- "Colorado"
B09_sex_sel$State[B09_sex_sel$State == "9"] <- "Connecticut"
B09_sex_sel$State[B09_sex_sel$State == "10"] <- "Delaware"
B09_sex_sel$State[B09_sex_sel$State == "11"] <- "District of Columbia"
B09_sex_sel$State[B09_sex_sel$State == "12"] <- "Florida"
B09_sex_sel$State[B09_sex_sel$State == "13"] <- "Georgia"
B09_sex_sel$State[B09_sex_sel$State == "15"] <- "Hawaii"
B09_sex_sel$State[B09_sex_sel$State == "16"] <- "Idaho"
B09_sex_sel$State[B09_sex_sel$State == "17"] <- "Illinois"
B09_sex_sel$State[B09_sex_sel$State == "18"] <- "Indiana"
B09_sex_sel$State[B09_sex_sel$State == "19"] <- "Iowa"
B09_sex_sel$State[B09_sex_sel$State == "20"] <- "Kansas"
B09_sex_sel$State[B09_sex_sel$State == "21"] <- "Kentucky"
B09_sex_sel$State[B09_sex_sel$State == "22"] <- "Louisiana"
B09_sex_sel$State[B09_sex_sel$State == "23"] <- "Maine"
B09_sex_sel$State[B09_sex_sel$State == "24"] <- "Maryland"
B09_sex_sel$State[B09_sex_sel$State == "25"] <- "Massachusetts"
B09_sex_sel$State[B09_sex_sel$State == "26"] <- "Michigan"
B09_sex_sel$State[B09_sex_sel$State == "27"] <- "Minnesota"
B09_sex_sel$State[B09_sex_sel$State == "28"] <- "Mississippi"
B09_sex_sel$State[B09_sex_sel$State == "29"] <- "Missouri"
B09_sex_sel$State[B09_sex_sel$State == "30"] <- "Montana"
B09_sex_sel$State[B09_sex_sel$State == "31"] <- "Nebraska"
B09_sex_sel$State[B09_sex_sel$State == "32"] <- "Nevada"
B09_sex_sel$State[B09_sex_sel$State == "33"] <- "New Hampshire"
B09_sex_sel$State[B09_sex_sel$State == "34"] <- "New Jersey"
B09_sex_sel$State[B09_sex_sel$State == "35"] <- "New Mexico"
B09_sex_sel$State[B09_sex_sel$State == "36"] <- "New York"
B09_sex_sel$State[B09_sex_sel$State == "37"] <- "North Carolina"
B09_sex_sel$State[B09_sex_sel$State == "38"] <- "North Dakota"
B09_sex_sel$State[B09_sex_sel$State == "39"] <- "Ohio"
B09_sex_sel$State[B09_sex_sel$State == "40"] <- "Oklahoma"
B09_sex_sel$State[B09_sex_sel$State == "41"] <- "Oregon"
B09_sex_sel$State[B09_sex_sel$State == "42"] <- "Pennsylvania"
B09_sex_sel$State[B09_sex_sel$State == "44"] <- "Rhode Island"
B09_sex_sel$State[B09_sex_sel$State == "45"] <- "South Carolina"
B09_sex_sel$State[B09_sex_sel$State == "46"] <- "South Dakota"
B09_sex_sel$State[B09_sex_sel$State == "47"] <- "Tennessee"
B09_sex_sel$State[B09_sex_sel$State == "48"] <- "Texas"
B09_sex_sel$State[B09_sex_sel$State == "49"] <- "Utah"
B09_sex_sel$State[B09_sex_sel$State == "50"] <- "Vermont"
B09_sex_sel$State[B09_sex_sel$State == "51"] <- "Virginia"
B09_sex_sel$State[B09_sex_sel$State == "53"] <- "Washington"
B09_sex_sel$State[B09_sex_sel$State == "54"] <- "West Virginia"
B09_sex_sel$State[B09_sex_sel$State == "55"] <- "Wisconsin"
B09_sex_sel$State[B09_sex_sel$State == "56"] <- "Wyoming"
B09_sex_sel$State[B09_sex_sel$State == "66"] <- "Guam"
B09_sex_sel$State[B09_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B09_sex_sel$region <- tolower(B09_sex_sel$State)
B09_sex_sel_join <- left_join(us_states, B09_sex_sel)

#Map
B09sex.state.map <- ggplot(data = B09_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2009") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B09.sex.state.map.png")



#2011
#2011-age
B11_age <- svyby(~`_AGEG5YR`, ~`_STATE`, BRFSS11, svymean, na.rm = TRUE)
B11_age_sel <- B11_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B11_age_sel$State[B11_age_sel$State == "1"] <- "Alabama"
B11_age_sel$State[B11_age_sel$State == "2"] <- "Alaska"
B11_age_sel$State[B11_age_sel$State == "4"] <- "Arizona"
B11_age_sel$State[B11_age_sel$State == "5"] <- "Arkansas"
B11_age_sel$State[B11_age_sel$State == "6"] <- "California"
B11_age_sel$State[B11_age_sel$State == "8"] <- "Colorado"
B11_age_sel$State[B11_age_sel$State == "9"] <- "Connecticut"
B11_age_sel$State[B11_age_sel$State == "10"] <- "Delaware"
B11_age_sel$State[B11_age_sel$State == "11"] <- "District of Columbia"
B11_age_sel$State[B11_age_sel$State == "12"] <- "Florida"
B11_age_sel$State[B11_age_sel$State == "13"] <- "Georgia"
B11_age_sel$State[B11_age_sel$State == "15"] <- "Hawaii"
B11_age_sel$State[B11_age_sel$State == "16"] <- "Idaho"
B11_age_sel$State[B11_age_sel$State == "17"] <- "Illinois"
B11_age_sel$State[B11_age_sel$State == "18"] <- "Indiana"
B11_age_sel$State[B11_age_sel$State == "19"] <- "Iowa"
B11_age_sel$State[B11_age_sel$State == "20"] <- "Kansas"
B11_age_sel$State[B11_age_sel$State == "21"] <- "Kentucky"
B11_age_sel$State[B11_age_sel$State == "22"] <- "Louisiana"
B11_age_sel$State[B11_age_sel$State == "23"] <- "Maine"
B11_age_sel$State[B11_age_sel$State == "24"] <- "Maryland"
B11_age_sel$State[B11_age_sel$State == "25"] <- "Massachusetts"
B11_age_sel$State[B11_age_sel$State == "26"] <- "Michigan"
B11_age_sel$State[B11_age_sel$State == "27"] <- "Minnesota"
B11_age_sel$State[B11_age_sel$State == "28"] <- "Mississippi"
B11_age_sel$State[B11_age_sel$State == "29"] <- "Missouri"
B11_age_sel$State[B11_age_sel$State == "30"] <- "Montana"
B11_age_sel$State[B11_age_sel$State == "31"] <- "Nebraska"
B11_age_sel$State[B11_age_sel$State == "32"] <- "Nevada"
B11_age_sel$State[B11_age_sel$State == "33"] <- "New Hampshire"
B11_age_sel$State[B11_age_sel$State == "34"] <- "New Jersey"
B11_age_sel$State[B11_age_sel$State == "35"] <- "New Mexico"
B11_age_sel$State[B11_age_sel$State == "36"] <- "New York"
B11_age_sel$State[B11_age_sel$State == "37"] <- "North Carolina"
B11_age_sel$State[B11_age_sel$State == "38"] <- "North Dakota"
B11_age_sel$State[B11_age_sel$State == "39"] <- "Ohio"
B11_age_sel$State[B11_age_sel$State == "40"] <- "Oklahoma"
B11_age_sel$State[B11_age_sel$State == "41"] <- "Oregon"
B11_age_sel$State[B11_age_sel$State == "42"] <- "Pennsylvania"
B11_age_sel$State[B11_age_sel$State == "44"] <- "Rhode Island"
B11_age_sel$State[B11_age_sel$State == "45"] <- "South Carolina"
B11_age_sel$State[B11_age_sel$State == "46"] <- "South Dakota"
B11_age_sel$State[B11_age_sel$State == "47"] <- "Tennessee"
B11_age_sel$State[B11_age_sel$State == "48"] <- "Texas"
B11_age_sel$State[B11_age_sel$State == "49"] <- "Utah"
B11_age_sel$State[B11_age_sel$State == "50"] <- "Vermont"
B11_age_sel$State[B11_age_sel$State == "51"] <- "Virginia"
B11_age_sel$State[B11_age_sel$State == "53"] <- "Washington"
B11_age_sel$State[B11_age_sel$State == "54"] <- "West Virginia"
B11_age_sel$State[B11_age_sel$State == "55"] <- "Wisconsin"
B11_age_sel$State[B11_age_sel$State == "56"] <- "Wyoming"
B11_age_sel$State[B11_age_sel$State == "66"] <- "Guam"
B11_age_sel$State[B11_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B11_age_sel$region <- tolower(B11_age_sel$State)
B11_age_sel_join <- left_join(us_states, B11_age_sel)

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age1824.state.map.png")

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age2529.state.map.png")

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age3034.state.map.png")

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age3539.state.map.png")

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age4044.state.map.png")

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age4549.state.map.png")

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age5054.state.map.png")

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age5559.state.map.png")

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age6064.state.map.png")

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age6569.state.map.png")

#Map
B11age.state.map <- ggplot(data = B11_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.age70ab.state.map.png")


#2011-sex
B11_sex <- svyby(~SEX, ~`_STATE`, BRFSS11, svymean, na.rm = TRUE)
B11_sex_sel <- B11_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B11_sex_sel$State[B11_sex_sel$State == "1"] <- "Alabama"
B11_sex_sel$State[B11_sex_sel$State == "2"] <- "Alaska"
B11_sex_sel$State[B11_sex_sel$State == "4"] <- "Arizona"
B11_sex_sel$State[B11_sex_sel$State == "5"] <- "Arkansas"
B11_sex_sel$State[B11_sex_sel$State == "6"] <- "California"
B11_sex_sel$State[B11_sex_sel$State == "8"] <- "Colorado"
B11_sex_sel$State[B11_sex_sel$State == "9"] <- "Connecticut"
B11_sex_sel$State[B11_sex_sel$State == "10"] <- "Delaware"
B11_sex_sel$State[B11_sex_sel$State == "11"] <- "District of Columbia"
B11_sex_sel$State[B11_sex_sel$State == "12"] <- "Florida"
B11_sex_sel$State[B11_sex_sel$State == "13"] <- "Georgia"
B11_sex_sel$State[B11_sex_sel$State == "15"] <- "Hawaii"
B11_sex_sel$State[B11_sex_sel$State == "16"] <- "Idaho"
B11_sex_sel$State[B11_sex_sel$State == "17"] <- "Illinois"
B11_sex_sel$State[B11_sex_sel$State == "18"] <- "Indiana"
B11_sex_sel$State[B11_sex_sel$State == "19"] <- "Iowa"
B11_sex_sel$State[B11_sex_sel$State == "20"] <- "Kansas"
B11_sex_sel$State[B11_sex_sel$State == "21"] <- "Kentucky"
B11_sex_sel$State[B11_sex_sel$State == "22"] <- "Louisiana"
B11_sex_sel$State[B11_sex_sel$State == "23"] <- "Maine"
B11_sex_sel$State[B11_sex_sel$State == "24"] <- "Maryland"
B11_sex_sel$State[B11_sex_sel$State == "25"] <- "Massachusetts"
B11_sex_sel$State[B11_sex_sel$State == "26"] <- "Michigan"
B11_sex_sel$State[B11_sex_sel$State == "27"] <- "Minnesota"
B11_sex_sel$State[B11_sex_sel$State == "28"] <- "Mississippi"
B11_sex_sel$State[B11_sex_sel$State == "29"] <- "Missouri"
B11_sex_sel$State[B11_sex_sel$State == "30"] <- "Montana"
B11_sex_sel$State[B11_sex_sel$State == "31"] <- "Nebraska"
B11_sex_sel$State[B11_sex_sel$State == "32"] <- "Nevada"
B11_sex_sel$State[B11_sex_sel$State == "33"] <- "New Hampshire"
B11_sex_sel$State[B11_sex_sel$State == "34"] <- "New Jersey"
B11_sex_sel$State[B11_sex_sel$State == "35"] <- "New Mexico"
B11_sex_sel$State[B11_sex_sel$State == "36"] <- "New York"
B11_sex_sel$State[B11_sex_sel$State == "37"] <- "North Carolina"
B11_sex_sel$State[B11_sex_sel$State == "38"] <- "North Dakota"
B11_sex_sel$State[B11_sex_sel$State == "39"] <- "Ohio"
B11_sex_sel$State[B11_sex_sel$State == "40"] <- "Oklahoma"
B11_sex_sel$State[B11_sex_sel$State == "41"] <- "Oregon"
B11_sex_sel$State[B11_sex_sel$State == "42"] <- "Pennsylvania"
B11_sex_sel$State[B11_sex_sel$State == "44"] <- "Rhode Island"
B11_sex_sel$State[B11_sex_sel$State == "45"] <- "South Carolina"
B11_sex_sel$State[B11_sex_sel$State == "46"] <- "South Dakota"
B11_sex_sel$State[B11_sex_sel$State == "47"] <- "Tennessee"
B11_sex_sel$State[B11_sex_sel$State == "48"] <- "Texas"
B11_sex_sel$State[B11_sex_sel$State == "49"] <- "Utah"
B11_sex_sel$State[B11_sex_sel$State == "50"] <- "Vermont"
B11_sex_sel$State[B11_sex_sel$State == "51"] <- "Virginia"
B11_sex_sel$State[B11_sex_sel$State == "53"] <- "Washington"
B11_sex_sel$State[B11_sex_sel$State == "54"] <- "West Virginia"
B11_sex_sel$State[B11_sex_sel$State == "55"] <- "Wisconsin"
B11_sex_sel$State[B11_sex_sel$State == "56"] <- "Wyoming"
B11_sex_sel$State[B11_sex_sel$State == "66"] <- "Guam"
B11_sex_sel$State[B11_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B11_sex_sel$region <- tolower(B11_sex_sel$State)
B11_sex_sel_join <- left_join(us_states, B11_sex_sel)

#Map
B11sex.state.map <- ggplot(data = B11_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2011") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B11.sex.state.map.png")

#2012
#2012-age
B12_age <- svyby(~`_AGEG5YR`, ~`_STATE`, BRFSS12, svymean, na.rm = TRUE)
B12_age_sel <- B12_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B12_age_sel$State[B12_age_sel$State == "1"] <- "Alabama"
B12_age_sel$State[B12_age_sel$State == "2"] <- "Alaska"
B12_age_sel$State[B12_age_sel$State == "4"] <- "Arizona"
B12_age_sel$State[B12_age_sel$State == "5"] <- "Arkansas"
B12_age_sel$State[B12_age_sel$State == "6"] <- "California"
B12_age_sel$State[B12_age_sel$State == "8"] <- "Colorado"
B12_age_sel$State[B12_age_sel$State == "9"] <- "Connecticut"
B12_age_sel$State[B12_age_sel$State == "10"] <- "Delaware"
B12_age_sel$State[B12_age_sel$State == "11"] <- "District of Columbia"
B12_age_sel$State[B12_age_sel$State == "12"] <- "Florida"
B12_age_sel$State[B12_age_sel$State == "13"] <- "Georgia"
B12_age_sel$State[B12_age_sel$State == "15"] <- "Hawaii"
B12_age_sel$State[B12_age_sel$State == "16"] <- "Idaho"
B12_age_sel$State[B12_age_sel$State == "17"] <- "Illinois"
B12_age_sel$State[B12_age_sel$State == "18"] <- "Indiana"
B12_age_sel$State[B12_age_sel$State == "19"] <- "Iowa"
B12_age_sel$State[B12_age_sel$State == "20"] <- "Kansas"
B12_age_sel$State[B12_age_sel$State == "21"] <- "Kentucky"
B12_age_sel$State[B12_age_sel$State == "22"] <- "Louisiana"
B12_age_sel$State[B12_age_sel$State == "23"] <- "Maine"
B12_age_sel$State[B12_age_sel$State == "24"] <- "Maryland"
B12_age_sel$State[B12_age_sel$State == "25"] <- "Massachusetts"
B12_age_sel$State[B12_age_sel$State == "26"] <- "Michigan"
B12_age_sel$State[B12_age_sel$State == "27"] <- "Minnesota"
B12_age_sel$State[B12_age_sel$State == "28"] <- "Mississippi"
B12_age_sel$State[B12_age_sel$State == "29"] <- "Missouri"
B12_age_sel$State[B12_age_sel$State == "30"] <- "Montana"
B12_age_sel$State[B12_age_sel$State == "31"] <- "Nebraska"
B12_age_sel$State[B12_age_sel$State == "32"] <- "Nevada"
B12_age_sel$State[B12_age_sel$State == "33"] <- "New Hampshire"
B12_age_sel$State[B12_age_sel$State == "34"] <- "New Jersey"
B12_age_sel$State[B12_age_sel$State == "35"] <- "New Mexico"
B12_age_sel$State[B12_age_sel$State == "36"] <- "New York"
B12_age_sel$State[B12_age_sel$State == "37"] <- "North Carolina"
B12_age_sel$State[B12_age_sel$State == "38"] <- "North Dakota"
B12_age_sel$State[B12_age_sel$State == "39"] <- "Ohio"
B12_age_sel$State[B12_age_sel$State == "40"] <- "Oklahoma"
B12_age_sel$State[B12_age_sel$State == "41"] <- "Oregon"
B12_age_sel$State[B12_age_sel$State == "42"] <- "Pennsylvania"
B12_age_sel$State[B12_age_sel$State == "44"] <- "Rhode Island"
B12_age_sel$State[B12_age_sel$State == "45"] <- "South Carolina"
B12_age_sel$State[B12_age_sel$State == "46"] <- "South Dakota"
B12_age_sel$State[B12_age_sel$State == "47"] <- "Tennessee"
B12_age_sel$State[B12_age_sel$State == "48"] <- "Texas"
B12_age_sel$State[B12_age_sel$State == "49"] <- "Utah"
B12_age_sel$State[B12_age_sel$State == "50"] <- "Vermont"
B12_age_sel$State[B12_age_sel$State == "51"] <- "Virginia"
B12_age_sel$State[B12_age_sel$State == "53"] <- "Washington"
B12_age_sel$State[B12_age_sel$State == "54"] <- "West Virginia"
B12_age_sel$State[B12_age_sel$State == "55"] <- "Wisconsin"
B12_age_sel$State[B12_age_sel$State == "56"] <- "Wyoming"
B12_age_sel$State[B12_age_sel$State == "66"] <- "Guam"
B12_age_sel$State[B12_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B12_age_sel$region <- tolower(B12_age_sel$State)
B12_age_sel_join <- left_join(us_states, B12_age_sel)

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age1824.state.map.png")

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age2529.state.map.png")

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age3034.state.map.png")

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age3539.state.map.png")

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age4044.state.map.png")

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age4549.state.map.png")

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age5054.state.map.png")

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age5559.state.map.png")

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age6064.state.map.png")

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age6569.state.map.png")

#Map
B12age.state.map <- ggplot(data = B12_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.age70ab.state.map.png")

#2012-Sex
B12_sex <- svyby(~SEX, ~`_STATE`, BRFSS12, svymean, na.rm = TRUE)
B12_sex_sel <- B12_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B12_sex_sel$State[B12_sex_sel$State == "1"] <- "Alabama"
B12_sex_sel$State[B12_sex_sel$State == "2"] <- "Alaska"
B12_sex_sel$State[B12_sex_sel$State == "4"] <- "Arizona"
B12_sex_sel$State[B12_sex_sel$State == "5"] <- "Arkansas"
B12_sex_sel$State[B12_sex_sel$State == "6"] <- "California"
B12_sex_sel$State[B12_sex_sel$State == "8"] <- "Colorado"
B12_sex_sel$State[B12_sex_sel$State == "9"] <- "Connecticut"
B12_sex_sel$State[B12_sex_sel$State == "10"] <- "Delaware"
B12_sex_sel$State[B12_sex_sel$State == "11"] <- "District of Columbia"
B12_sex_sel$State[B12_sex_sel$State == "12"] <- "Florida"
B12_sex_sel$State[B12_sex_sel$State == "13"] <- "Georgia"
B12_sex_sel$State[B12_sex_sel$State == "15"] <- "Hawaii"
B12_sex_sel$State[B12_sex_sel$State == "16"] <- "Idaho"
B12_sex_sel$State[B12_sex_sel$State == "17"] <- "Illinois"
B12_sex_sel$State[B12_sex_sel$State == "18"] <- "Indiana"
B12_sex_sel$State[B12_sex_sel$State == "19"] <- "Iowa"
B12_sex_sel$State[B12_sex_sel$State == "20"] <- "Kansas"
B12_sex_sel$State[B12_sex_sel$State == "21"] <- "Kentucky"
B12_sex_sel$State[B12_sex_sel$State == "22"] <- "Louisiana"
B12_sex_sel$State[B12_sex_sel$State == "23"] <- "Maine"
B12_sex_sel$State[B12_sex_sel$State == "24"] <- "Maryland"
B12_sex_sel$State[B12_sex_sel$State == "25"] <- "Massachusetts"
B12_sex_sel$State[B12_sex_sel$State == "26"] <- "Michigan"
B12_sex_sel$State[B12_sex_sel$State == "27"] <- "Minnesota"
B12_sex_sel$State[B12_sex_sel$State == "28"] <- "Mississippi"
B12_sex_sel$State[B12_sex_sel$State == "29"] <- "Missouri"
B12_sex_sel$State[B12_sex_sel$State == "30"] <- "Montana"
B12_sex_sel$State[B12_sex_sel$State == "31"] <- "Nebraska"
B12_sex_sel$State[B12_sex_sel$State == "32"] <- "Nevada"
B12_sex_sel$State[B12_sex_sel$State == "33"] <- "New Hampshire"
B12_sex_sel$State[B12_sex_sel$State == "34"] <- "New Jersey"
B12_sex_sel$State[B12_sex_sel$State == "35"] <- "New Mexico"
B12_sex_sel$State[B12_sex_sel$State == "36"] <- "New York"
B12_sex_sel$State[B12_sex_sel$State == "37"] <- "North Carolina"
B12_sex_sel$State[B12_sex_sel$State == "38"] <- "North Dakota"
B12_sex_sel$State[B12_sex_sel$State == "39"] <- "Ohio"
B12_sex_sel$State[B12_sex_sel$State == "40"] <- "Oklahoma"
B12_sex_sel$State[B12_sex_sel$State == "41"] <- "Oregon"
B12_sex_sel$State[B12_sex_sel$State == "42"] <- "Pennsylvania"
B12_sex_sel$State[B12_sex_sel$State == "44"] <- "Rhode Island"
B12_sex_sel$State[B12_sex_sel$State == "45"] <- "South Carolina"
B12_sex_sel$State[B12_sex_sel$State == "46"] <- "South Dakota"
B12_sex_sel$State[B12_sex_sel$State == "47"] <- "Tennessee"
B12_sex_sel$State[B12_sex_sel$State == "48"] <- "Texas"
B12_sex_sel$State[B12_sex_sel$State == "49"] <- "Utah"
B12_sex_sel$State[B12_sex_sel$State == "50"] <- "Vermont"
B12_sex_sel$State[B12_sex_sel$State == "51"] <- "Virginia"
B12_sex_sel$State[B12_sex_sel$State == "53"] <- "Washington"
B12_sex_sel$State[B12_sex_sel$State == "54"] <- "West Virginia"
B12_sex_sel$State[B12_sex_sel$State == "55"] <- "Wisconsin"
B12_sex_sel$State[B12_sex_sel$State == "56"] <- "Wyoming"
B12_sex_sel$State[B12_sex_sel$State == "66"] <- "Guam"
B12_sex_sel$State[B12_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B12_sex_sel$region <- tolower(B12_sex_sel$State)
B12_sex_sel_join <- left_join(us_states, B12_sex_sel)

#Map
B12sex.state.map <- ggplot(data = B12_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2012") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B12.sex.state.map.png")

#2013
#2013-age
B13_age <- svyby(~`_AGEG5YR`, ~`_STATE`, BRFSS13, svymean, na.rm = TRUE)
B13_age_sel <- B13_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B13_age_sel$State[B13_age_sel$State == "1"] <- "Alabama"
B13_age_sel$State[B13_age_sel$State == "2"] <- "Alaska"
B13_age_sel$State[B13_age_sel$State == "4"] <- "Arizona"
B13_age_sel$State[B13_age_sel$State == "5"] <- "Arkansas"
B13_age_sel$State[B13_age_sel$State == "6"] <- "California"
B13_age_sel$State[B13_age_sel$State == "8"] <- "Colorado"
B13_age_sel$State[B13_age_sel$State == "9"] <- "Connecticut"
B13_age_sel$State[B13_age_sel$State == "10"] <- "Delaware"
B13_age_sel$State[B13_age_sel$State == "11"] <- "District of Columbia"
B13_age_sel$State[B13_age_sel$State == "12"] <- "Florida"
B13_age_sel$State[B13_age_sel$State == "13"] <- "Georgia"
B13_age_sel$State[B13_age_sel$State == "15"] <- "Hawaii"
B13_age_sel$State[B13_age_sel$State == "16"] <- "Idaho"
B13_age_sel$State[B13_age_sel$State == "17"] <- "Illinois"
B13_age_sel$State[B13_age_sel$State == "18"] <- "Indiana"
B13_age_sel$State[B13_age_sel$State == "19"] <- "Iowa"
B13_age_sel$State[B13_age_sel$State == "20"] <- "Kansas"
B13_age_sel$State[B13_age_sel$State == "21"] <- "Kentucky"
B13_age_sel$State[B13_age_sel$State == "22"] <- "Louisiana"
B13_age_sel$State[B13_age_sel$State == "23"] <- "Maine"
B13_age_sel$State[B13_age_sel$State == "24"] <- "Maryland"
B13_age_sel$State[B13_age_sel$State == "25"] <- "Massachusetts"
B13_age_sel$State[B13_age_sel$State == "26"] <- "Michigan"
B13_age_sel$State[B13_age_sel$State == "27"] <- "Minnesota"
B13_age_sel$State[B13_age_sel$State == "28"] <- "Mississippi"
B13_age_sel$State[B13_age_sel$State == "29"] <- "Missouri"
B13_age_sel$State[B13_age_sel$State == "30"] <- "Montana"
B13_age_sel$State[B13_age_sel$State == "31"] <- "Nebraska"
B13_age_sel$State[B13_age_sel$State == "32"] <- "Nevada"
B13_age_sel$State[B13_age_sel$State == "33"] <- "New Hampshire"
B13_age_sel$State[B13_age_sel$State == "34"] <- "New Jersey"
B13_age_sel$State[B13_age_sel$State == "35"] <- "New Mexico"
B13_age_sel$State[B13_age_sel$State == "36"] <- "New York"
B13_age_sel$State[B13_age_sel$State == "37"] <- "North Carolina"
B13_age_sel$State[B13_age_sel$State == "38"] <- "North Dakota"
B13_age_sel$State[B13_age_sel$State == "39"] <- "Ohio"
B13_age_sel$State[B13_age_sel$State == "40"] <- "Oklahoma"
B13_age_sel$State[B13_age_sel$State == "41"] <- "Oregon"
B13_age_sel$State[B13_age_sel$State == "42"] <- "Pennsylvania"
B13_age_sel$State[B13_age_sel$State == "44"] <- "Rhode Island"
B13_age_sel$State[B13_age_sel$State == "45"] <- "South Carolina"
B13_age_sel$State[B13_age_sel$State == "46"] <- "South Dakota"
B13_age_sel$State[B13_age_sel$State == "47"] <- "Tennessee"
B13_age_sel$State[B13_age_sel$State == "48"] <- "Texas"
B13_age_sel$State[B13_age_sel$State == "49"] <- "Utah"
B13_age_sel$State[B13_age_sel$State == "50"] <- "Vermont"
B13_age_sel$State[B13_age_sel$State == "51"] <- "Virginia"
B13_age_sel$State[B13_age_sel$State == "53"] <- "Washington"
B13_age_sel$State[B13_age_sel$State == "54"] <- "West Virginia"
B13_age_sel$State[B13_age_sel$State == "55"] <- "Wisconsin"
B13_age_sel$State[B13_age_sel$State == "56"] <- "Wyoming"
B13_age_sel$State[B13_age_sel$State == "66"] <- "Guam"
B13_age_sel$State[B13_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B13_age_sel$region <- tolower(B13_age_sel$State)
B13_age_sel_join <- left_join(us_states, B13_age_sel)

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age1824.state.map.png")

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age2529.state.map.png")

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age3034.state.map.png")

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age3539.state.map.png")

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age4044.state.map.png")

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age4549.state.map.png")

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age5054.state.map.png")

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age5559.state.map.png")

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age6064.state.map.png")

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(4, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age6569.state.map.png")

#Map
B13age.state.map <- ggplot(data = B13_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.age70ab.state.map.png")


#2013-sex
B13_sex <- svyby(~SEX, ~`_STATE`, BRFSS14, svymean, na.rm = TRUE)
B13_sex_sel <- B13_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B13_sex_sel$State[B13_sex_sel$State == "1"] <- "Alabama"
B13_sex_sel$State[B13_sex_sel$State == "2"] <- "Alaska"
B13_sex_sel$State[B13_sex_sel$State == "4"] <- "Arizona"
B13_sex_sel$State[B13_sex_sel$State == "5"] <- "Arkansas"
B13_sex_sel$State[B13_sex_sel$State == "6"] <- "California"
B13_sex_sel$State[B13_sex_sel$State == "8"] <- "Colorado"
B13_sex_sel$State[B13_sex_sel$State == "9"] <- "Connecticut"
B13_sex_sel$State[B13_sex_sel$State == "10"] <- "Delaware"
B13_sex_sel$State[B13_sex_sel$State == "11"] <- "District of Columbia"
B13_sex_sel$State[B13_sex_sel$State == "12"] <- "Florida"
B13_sex_sel$State[B13_sex_sel$State == "13"] <- "Georgia"
B13_sex_sel$State[B13_sex_sel$State == "15"] <- "Hawaii"
B13_sex_sel$State[B13_sex_sel$State == "16"] <- "Idaho"
B13_sex_sel$State[B13_sex_sel$State == "17"] <- "Illinois"
B13_sex_sel$State[B13_sex_sel$State == "18"] <- "Indiana"
B13_sex_sel$State[B13_sex_sel$State == "19"] <- "Iowa"
B13_sex_sel$State[B13_sex_sel$State == "20"] <- "Kansas"
B13_sex_sel$State[B13_sex_sel$State == "21"] <- "Kentucky"
B13_sex_sel$State[B13_sex_sel$State == "22"] <- "Louisiana"
B13_sex_sel$State[B13_sex_sel$State == "23"] <- "Maine"
B13_sex_sel$State[B13_sex_sel$State == "24"] <- "Maryland"
B13_sex_sel$State[B13_sex_sel$State == "25"] <- "Massachusetts"
B13_sex_sel$State[B13_sex_sel$State == "26"] <- "Michigan"
B13_sex_sel$State[B13_sex_sel$State == "27"] <- "Minnesota"
B13_sex_sel$State[B13_sex_sel$State == "28"] <- "Mississippi"
B13_sex_sel$State[B13_sex_sel$State == "29"] <- "Missouri"
B13_sex_sel$State[B13_sex_sel$State == "30"] <- "Montana"
B13_sex_sel$State[B13_sex_sel$State == "31"] <- "Nebraska"
B13_sex_sel$State[B13_sex_sel$State == "32"] <- "Nevada"
B13_sex_sel$State[B13_sex_sel$State == "33"] <- "New Hampshire"
B13_sex_sel$State[B13_sex_sel$State == "34"] <- "New Jersey"
B13_sex_sel$State[B13_sex_sel$State == "35"] <- "New Mexico"
B13_sex_sel$State[B13_sex_sel$State == "36"] <- "New York"
B13_sex_sel$State[B13_sex_sel$State == "37"] <- "North Carolina"
B13_sex_sel$State[B13_sex_sel$State == "38"] <- "North Dakota"
B13_sex_sel$State[B13_sex_sel$State == "39"] <- "Ohio"
B13_sex_sel$State[B13_sex_sel$State == "40"] <- "Oklahoma"
B13_sex_sel$State[B13_sex_sel$State == "41"] <- "Oregon"
B13_sex_sel$State[B13_sex_sel$State == "42"] <- "Pennsylvania"
B13_sex_sel$State[B13_sex_sel$State == "44"] <- "Rhode Island"
B13_sex_sel$State[B13_sex_sel$State == "45"] <- "South Carolina"
B13_sex_sel$State[B13_sex_sel$State == "46"] <- "South Dakota"
B13_sex_sel$State[B13_sex_sel$State == "47"] <- "Tennessee"
B13_sex_sel$State[B13_sex_sel$State == "48"] <- "Texas"
B13_sex_sel$State[B13_sex_sel$State == "49"] <- "Utah"
B13_sex_sel$State[B13_sex_sel$State == "50"] <- "Vermont"
B13_sex_sel$State[B13_sex_sel$State == "51"] <- "Virginia"
B13_sex_sel$State[B13_sex_sel$State == "53"] <- "Washington"
B13_sex_sel$State[B13_sex_sel$State == "54"] <- "West Virginia"
B13_sex_sel$State[B13_sex_sel$State == "55"] <- "Wisconsin"
B13_sex_sel$State[B13_sex_sel$State == "56"] <- "Wyoming"
B13_sex_sel$State[B13_sex_sel$State == "66"] <- "Guam"
B13_sex_sel$State[B13_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B13_sex_sel$region <- tolower(B13_sex_sel$State)
B13_sex_sel_join <- left_join(us_states, B13_sex_sel)

#Map
B13sex.state.map <- ggplot(data = B13_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2013") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B13.sex.state.map.png")



#2014
#2014-age
B14_age <- svyby(~`_AGEG5YR`, ~`_STATE`, BRFSS14, svymean, na.rm = TRUE)
B14_age_sel <- B14_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B14_age_sel$State[B14_age_sel$State == "1"] <- "Alabama"
B14_age_sel$State[B14_age_sel$State == "2"] <- "Alaska"
B14_age_sel$State[B14_age_sel$State == "4"] <- "Arizona"
B14_age_sel$State[B14_age_sel$State == "5"] <- "Arkansas"
B14_age_sel$State[B14_age_sel$State == "6"] <- "California"
B14_age_sel$State[B14_age_sel$State == "8"] <- "Colorado"
B14_age_sel$State[B14_age_sel$State == "9"] <- "Connecticut"
B14_age_sel$State[B14_age_sel$State == "10"] <- "Delaware"
B14_age_sel$State[B14_age_sel$State == "11"] <- "District of Columbia"
B14_age_sel$State[B14_age_sel$State == "12"] <- "Florida"
B14_age_sel$State[B14_age_sel$State == "13"] <- "Georgia"
B14_age_sel$State[B14_age_sel$State == "15"] <- "Hawaii"
B14_age_sel$State[B14_age_sel$State == "16"] <- "Idaho"
B14_age_sel$State[B14_age_sel$State == "17"] <- "Illinois"
B14_age_sel$State[B14_age_sel$State == "18"] <- "Indiana"
B14_age_sel$State[B14_age_sel$State == "19"] <- "Iowa"
B14_age_sel$State[B14_age_sel$State == "20"] <- "Kansas"
B14_age_sel$State[B14_age_sel$State == "21"] <- "Kentucky"
B14_age_sel$State[B14_age_sel$State == "22"] <- "Louisiana"
B14_age_sel$State[B14_age_sel$State == "23"] <- "Maine"
B14_age_sel$State[B14_age_sel$State == "24"] <- "Maryland"
B14_age_sel$State[B14_age_sel$State == "25"] <- "Massachusetts"
B14_age_sel$State[B14_age_sel$State == "26"] <- "Michigan"
B14_age_sel$State[B14_age_sel$State == "27"] <- "Minnesota"
B14_age_sel$State[B14_age_sel$State == "28"] <- "Mississippi"
B14_age_sel$State[B14_age_sel$State == "29"] <- "Missouri"
B14_age_sel$State[B14_age_sel$State == "30"] <- "Montana"
B14_age_sel$State[B14_age_sel$State == "31"] <- "Nebraska"
B14_age_sel$State[B14_age_sel$State == "32"] <- "Nevada"
B14_age_sel$State[B14_age_sel$State == "33"] <- "New Hampshire"
B14_age_sel$State[B14_age_sel$State == "34"] <- "New Jersey"
B14_age_sel$State[B14_age_sel$State == "35"] <- "New Mexico"
B14_age_sel$State[B14_age_sel$State == "36"] <- "New York"
B14_age_sel$State[B14_age_sel$State == "37"] <- "North Carolina"
B14_age_sel$State[B14_age_sel$State == "38"] <- "North Dakota"
B14_age_sel$State[B14_age_sel$State == "39"] <- "Ohio"
B14_age_sel$State[B14_age_sel$State == "40"] <- "Oklahoma"
B14_age_sel$State[B14_age_sel$State == "41"] <- "Oregon"
B14_age_sel$State[B14_age_sel$State == "42"] <- "Pennsylvania"
B14_age_sel$State[B14_age_sel$State == "44"] <- "Rhode Island"
B14_age_sel$State[B14_age_sel$State == "45"] <- "South Carolina"
B14_age_sel$State[B14_age_sel$State == "46"] <- "South Dakota"
B14_age_sel$State[B14_age_sel$State == "47"] <- "Tennessee"
B14_age_sel$State[B14_age_sel$State == "48"] <- "Texas"
B14_age_sel$State[B14_age_sel$State == "49"] <- "Utah"
B14_age_sel$State[B14_age_sel$State == "50"] <- "Vermont"
B14_age_sel$State[B14_age_sel$State == "51"] <- "Virginia"
B14_age_sel$State[B14_age_sel$State == "53"] <- "Washington"
B14_age_sel$State[B14_age_sel$State == "54"] <- "West Virginia"
B14_age_sel$State[B14_age_sel$State == "55"] <- "Wisconsin"
B14_age_sel$State[B14_age_sel$State == "56"] <- "Wyoming"
B14_age_sel$State[B14_age_sel$State == "66"] <- "Guam"
B14_age_sel$State[B14_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B14_age_sel$region <- tolower(B14_age_sel$State)
B14_age_sel_join <- left_join(us_states, B14_age_sel)

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age1824.state.map.png")

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age2529.state.map.png")

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age3034.state.map.png")

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age3539.state.map.png")

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age4044.state.map.png")

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age4549.state.map.png")

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age5054.state.map.png")

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age5559.state.map.png")

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age6064.state.map.png")

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age6569.state.map.png")

#Map
B14age.state.map <- ggplot(data = B14_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.age70ab.state.map.png")

#2014-sex
B14_sex <- svyby(~SEX, ~`_STATE`, BRFSS14, svymean, na.rm = TRUE)
B14_sex_sel <- B14_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B14_sex_sel$State[B14_sex_sel$State == "1"] <- "Alabama"
B14_sex_sel$State[B14_sex_sel$State == "2"] <- "Alaska"
B14_sex_sel$State[B14_sex_sel$State == "4"] <- "Arizona"
B14_sex_sel$State[B14_sex_sel$State == "5"] <- "Arkansas"
B14_sex_sel$State[B14_sex_sel$State == "6"] <- "California"
B14_sex_sel$State[B14_sex_sel$State == "8"] <- "Colorado"
B14_sex_sel$State[B14_sex_sel$State == "9"] <- "Connecticut"
B14_sex_sel$State[B14_sex_sel$State == "10"] <- "Delaware"
B14_sex_sel$State[B14_sex_sel$State == "11"] <- "District of Columbia"
B14_sex_sel$State[B14_sex_sel$State == "12"] <- "Florida"
B14_sex_sel$State[B14_sex_sel$State == "13"] <- "Georgia"
B14_sex_sel$State[B14_sex_sel$State == "15"] <- "Hawaii"
B14_sex_sel$State[B14_sex_sel$State == "16"] <- "Idaho"
B14_sex_sel$State[B14_sex_sel$State == "17"] <- "Illinois"
B14_sex_sel$State[B14_sex_sel$State == "18"] <- "Indiana"
B14_sex_sel$State[B14_sex_sel$State == "19"] <- "Iowa"
B14_sex_sel$State[B14_sex_sel$State == "20"] <- "Kansas"
B14_sex_sel$State[B14_sex_sel$State == "21"] <- "Kentucky"
B14_sex_sel$State[B14_sex_sel$State == "22"] <- "Louisiana"
B14_sex_sel$State[B14_sex_sel$State == "23"] <- "Maine"
B14_sex_sel$State[B14_sex_sel$State == "24"] <- "Maryland"
B14_sex_sel$State[B14_sex_sel$State == "25"] <- "Massachusetts"
B14_sex_sel$State[B14_sex_sel$State == "26"] <- "Michigan"
B14_sex_sel$State[B14_sex_sel$State == "27"] <- "Minnesota"
B14_sex_sel$State[B14_sex_sel$State == "28"] <- "Mississippi"
B14_sex_sel$State[B14_sex_sel$State == "29"] <- "Missouri"
B14_sex_sel$State[B14_sex_sel$State == "30"] <- "Montana"
B14_sex_sel$State[B14_sex_sel$State == "31"] <- "Nebraska"
B14_sex_sel$State[B14_sex_sel$State == "32"] <- "Nevada"
B14_sex_sel$State[B14_sex_sel$State == "33"] <- "New Hampshire"
B14_sex_sel$State[B14_sex_sel$State == "34"] <- "New Jersey"
B14_sex_sel$State[B14_sex_sel$State == "35"] <- "New Mexico"
B14_sex_sel$State[B14_sex_sel$State == "36"] <- "New York"
B14_sex_sel$State[B14_sex_sel$State == "37"] <- "North Carolina"
B14_sex_sel$State[B14_sex_sel$State == "38"] <- "North Dakota"
B14_sex_sel$State[B14_sex_sel$State == "39"] <- "Ohio"
B14_sex_sel$State[B14_sex_sel$State == "40"] <- "Oklahoma"
B14_sex_sel$State[B14_sex_sel$State == "41"] <- "Oregon"
B14_sex_sel$State[B14_sex_sel$State == "42"] <- "Pennsylvania"
B14_sex_sel$State[B14_sex_sel$State == "44"] <- "Rhode Island"
B14_sex_sel$State[B14_sex_sel$State == "45"] <- "South Carolina"
B14_sex_sel$State[B14_sex_sel$State == "46"] <- "South Dakota"
B14_sex_sel$State[B14_sex_sel$State == "47"] <- "Tennessee"
B14_sex_sel$State[B14_sex_sel$State == "48"] <- "Texas"
B14_sex_sel$State[B14_sex_sel$State == "49"] <- "Utah"
B14_sex_sel$State[B14_sex_sel$State == "50"] <- "Vermont"
B14_sex_sel$State[B14_sex_sel$State == "51"] <- "Virginia"
B14_sex_sel$State[B14_sex_sel$State == "53"] <- "Washington"
B14_sex_sel$State[B14_sex_sel$State == "54"] <- "West Virginia"
B14_sex_sel$State[B14_sex_sel$State == "55"] <- "Wisconsin"
B14_sex_sel$State[B14_sex_sel$State == "56"] <- "Wyoming"
B14_sex_sel$State[B14_sex_sel$State == "66"] <- "Guam"
B14_sex_sel$State[B14_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B14_sex_sel$region <- tolower(B14_sex_sel$State)
B14_sex_sel_join <- left_join(us_states, B14_sex_sel)

#Map
B14sex.state.map <- ggplot(data = B14_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2014") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B14.sex.state.map.png")


#2015
#2015-age
B15_age <- svyby(~`_AGEG5YR`, ~`_STATE`, BRFSS15, svymean, na.rm = TRUE)
B15_age_sel <- B15_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B15_age_sel$State[B15_age_sel$State == "1"] <- "Alabama"
B15_age_sel$State[B15_age_sel$State == "2"] <- "Alaska"
B15_age_sel$State[B15_age_sel$State == "4"] <- "Arizona"
B15_age_sel$State[B15_age_sel$State == "5"] <- "Arkansas"
B15_age_sel$State[B15_age_sel$State == "6"] <- "California"
B15_age_sel$State[B15_age_sel$State == "8"] <- "Colorado"
B15_age_sel$State[B15_age_sel$State == "9"] <- "Connecticut"
B15_age_sel$State[B15_age_sel$State == "10"] <- "Delaware"
B15_age_sel$State[B15_age_sel$State == "11"] <- "District of Columbia"
B15_age_sel$State[B15_age_sel$State == "12"] <- "Florida"
B15_age_sel$State[B15_age_sel$State == "13"] <- "Georgia"
B15_age_sel$State[B15_age_sel$State == "15"] <- "Hawaii"
B15_age_sel$State[B15_age_sel$State == "16"] <- "Idaho"
B15_age_sel$State[B15_age_sel$State == "17"] <- "Illinois"
B15_age_sel$State[B15_age_sel$State == "18"] <- "Indiana"
B15_age_sel$State[B15_age_sel$State == "19"] <- "Iowa"
B15_age_sel$State[B15_age_sel$State == "20"] <- "Kansas"
B15_age_sel$State[B15_age_sel$State == "21"] <- "Kentucky"
B15_age_sel$State[B15_age_sel$State == "22"] <- "Louisiana"
B15_age_sel$State[B15_age_sel$State == "23"] <- "Maine"
B15_age_sel$State[B15_age_sel$State == "24"] <- "Maryland"
B15_age_sel$State[B15_age_sel$State == "25"] <- "Massachusetts"
B15_age_sel$State[B15_age_sel$State == "26"] <- "Michigan"
B15_age_sel$State[B15_age_sel$State == "27"] <- "Minnesota"
B15_age_sel$State[B15_age_sel$State == "28"] <- "Mississippi"
B15_age_sel$State[B15_age_sel$State == "29"] <- "Missouri"
B15_age_sel$State[B15_age_sel$State == "30"] <- "Montana"
B15_age_sel$State[B15_age_sel$State == "31"] <- "Nebraska"
B15_age_sel$State[B15_age_sel$State == "32"] <- "Nevada"
B15_age_sel$State[B15_age_sel$State == "33"] <- "New Hampshire"
B15_age_sel$State[B15_age_sel$State == "34"] <- "New Jersey"
B15_age_sel$State[B15_age_sel$State == "35"] <- "New Mexico"
B15_age_sel$State[B15_age_sel$State == "36"] <- "New York"
B15_age_sel$State[B15_age_sel$State == "37"] <- "North Carolina"
B15_age_sel$State[B15_age_sel$State == "38"] <- "North Dakota"
B15_age_sel$State[B15_age_sel$State == "39"] <- "Ohio"
B15_age_sel$State[B15_age_sel$State == "40"] <- "Oklahoma"
B15_age_sel$State[B15_age_sel$State == "41"] <- "Oregon"
B15_age_sel$State[B15_age_sel$State == "42"] <- "Pennsylvania"
B15_age_sel$State[B15_age_sel$State == "44"] <- "Rhode Island"
B15_age_sel$State[B15_age_sel$State == "45"] <- "South Carolina"
B15_age_sel$State[B15_age_sel$State == "46"] <- "South Dakota"
B15_age_sel$State[B15_age_sel$State == "47"] <- "Tennessee"
B15_age_sel$State[B15_age_sel$State == "48"] <- "Texas"
B15_age_sel$State[B15_age_sel$State == "49"] <- "Utah"
B15_age_sel$State[B15_age_sel$State == "50"] <- "Vermont"
B15_age_sel$State[B15_age_sel$State == "51"] <- "Virginia"
B15_age_sel$State[B15_age_sel$State == "53"] <- "Washington"
B15_age_sel$State[B15_age_sel$State == "54"] <- "West Virginia"
B15_age_sel$State[B15_age_sel$State == "55"] <- "Wisconsin"
B15_age_sel$State[B15_age_sel$State == "56"] <- "Wyoming"
B15_age_sel$State[B15_age_sel$State == "66"] <- "Guam"
B15_age_sel$State[B15_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B15_age_sel$region <- tolower(B15_age_sel$State)
B15_age_sel_join <- left_join(us_states, B15_age_sel)

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age1824.state.map.png")

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age2529.state.map.png")

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age3034.state.map.png")

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age3539.state.map.png")

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age4044.state.map.png")

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age4549.state.map.png")

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age5054.state.map.png")

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age5559.state.map.png")

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age6064.state.map.png")

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age6569.state.map.png")

#Map
B15age.state.map <- ggplot(data = B15_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.age70ab.state.map.png")


#2015-sex
B15_sex <- svyby(~SEX, ~`_STATE`, BRFSS15, svymean, na.rm = TRUE)
B15_sex_sel <- B15_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B15_sex_sel$State[B15_sex_sel$State == "1"] <- "Alabama"
B15_sex_sel$State[B15_sex_sel$State == "2"] <- "Alaska"
B15_sex_sel$State[B15_sex_sel$State == "4"] <- "Arizona"
B15_sex_sel$State[B15_sex_sel$State == "5"] <- "Arkansas"
B15_sex_sel$State[B15_sex_sel$State == "6"] <- "California"
B15_sex_sel$State[B15_sex_sel$State == "8"] <- "Colorado"
B15_sex_sel$State[B15_sex_sel$State == "9"] <- "Connecticut"
B15_sex_sel$State[B15_sex_sel$State == "10"] <- "Delaware"
B15_sex_sel$State[B15_sex_sel$State == "11"] <- "District of Columbia"
B15_sex_sel$State[B15_sex_sel$State == "12"] <- "Florida"
B15_sex_sel$State[B15_sex_sel$State == "13"] <- "Georgia"
B15_sex_sel$State[B15_sex_sel$State == "15"] <- "Hawaii"
B15_sex_sel$State[B15_sex_sel$State == "16"] <- "Idaho"
B15_sex_sel$State[B15_sex_sel$State == "17"] <- "Illinois"
B15_sex_sel$State[B15_sex_sel$State == "18"] <- "Indiana"
B15_sex_sel$State[B15_sex_sel$State == "19"] <- "Iowa"
B15_sex_sel$State[B15_sex_sel$State == "20"] <- "Kansas"
B15_sex_sel$State[B15_sex_sel$State == "21"] <- "Kentucky"
B15_sex_sel$State[B15_sex_sel$State == "22"] <- "Louisiana"
B15_sex_sel$State[B15_sex_sel$State == "23"] <- "Maine"
B15_sex_sel$State[B15_sex_sel$State == "24"] <- "Maryland"
B15_sex_sel$State[B15_sex_sel$State == "25"] <- "Massachusetts"
B15_sex_sel$State[B15_sex_sel$State == "26"] <- "Michigan"
B15_sex_sel$State[B15_sex_sel$State == "27"] <- "Minnesota"
B15_sex_sel$State[B15_sex_sel$State == "28"] <- "Mississippi"
B15_sex_sel$State[B15_sex_sel$State == "29"] <- "Missouri"
B15_sex_sel$State[B15_sex_sel$State == "30"] <- "Montana"
B15_sex_sel$State[B15_sex_sel$State == "31"] <- "Nebraska"
B15_sex_sel$State[B15_sex_sel$State == "32"] <- "Nevada"
B15_sex_sel$State[B15_sex_sel$State == "33"] <- "New Hampshire"
B15_sex_sel$State[B15_sex_sel$State == "34"] <- "New Jersey"
B15_sex_sel$State[B15_sex_sel$State == "35"] <- "New Mexico"
B15_sex_sel$State[B15_sex_sel$State == "36"] <- "New York"
B15_sex_sel$State[B15_sex_sel$State == "37"] <- "North Carolina"
B15_sex_sel$State[B15_sex_sel$State == "38"] <- "North Dakota"
B15_sex_sel$State[B15_sex_sel$State == "39"] <- "Ohio"
B15_sex_sel$State[B15_sex_sel$State == "40"] <- "Oklahoma"
B15_sex_sel$State[B15_sex_sel$State == "41"] <- "Oregon"
B15_sex_sel$State[B15_sex_sel$State == "42"] <- "Pennsylvania"
B15_sex_sel$State[B15_sex_sel$State == "44"] <- "Rhode Island"
B15_sex_sel$State[B15_sex_sel$State == "45"] <- "South Carolina"
B15_sex_sel$State[B15_sex_sel$State == "46"] <- "South Dakota"
B15_sex_sel$State[B15_sex_sel$State == "47"] <- "Tennessee"
B15_sex_sel$State[B15_sex_sel$State == "48"] <- "Texas"
B15_sex_sel$State[B15_sex_sel$State == "49"] <- "Utah"
B15_sex_sel$State[B15_sex_sel$State == "50"] <- "Vermont"
B15_sex_sel$State[B15_sex_sel$State == "51"] <- "Virginia"
B15_sex_sel$State[B15_sex_sel$State == "53"] <- "Washington"
B15_sex_sel$State[B15_sex_sel$State == "54"] <- "West Virginia"
B15_sex_sel$State[B15_sex_sel$State == "55"] <- "Wisconsin"
B15_sex_sel$State[B15_sex_sel$State == "56"] <- "Wyoming"
B15_sex_sel$State[B15_sex_sel$State == "66"] <- "Guam"
B15_sex_sel$State[B15_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B15_sex_sel$region <- tolower(B15_sex_sel$State)
B15_sex_sel_join <- left_join(us_states, B15_sex_sel)

#Map
B15sex.state.map <- ggplot(data = B15_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2015") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B15.sex.state.map.png")

#2016
#2016-age
B16_age <- svyby(~`_AGEG5YR`, ~`_STATE`, BRFSS16, svymean, na.rm = TRUE)
B16_age_sel <- B16_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B16_age_sel$State[B16_age_sel$State == "1"] <- "Alabama"
B16_age_sel$State[B16_age_sel$State == "2"] <- "Alaska"
B16_age_sel$State[B16_age_sel$State == "4"] <- "Arizona"
B16_age_sel$State[B16_age_sel$State == "5"] <- "Arkansas"
B16_age_sel$State[B16_age_sel$State == "6"] <- "California"
B16_age_sel$State[B16_age_sel$State == "8"] <- "Colorado"
B16_age_sel$State[B16_age_sel$State == "9"] <- "Connecticut"
B16_age_sel$State[B16_age_sel$State == "10"] <- "Delaware"
B16_age_sel$State[B16_age_sel$State == "11"] <- "District of Columbia"
B16_age_sel$State[B16_age_sel$State == "12"] <- "Florida"
B16_age_sel$State[B16_age_sel$State == "13"] <- "Georgia"
B16_age_sel$State[B16_age_sel$State == "15"] <- "Hawaii"
B16_age_sel$State[B16_age_sel$State == "16"] <- "Idaho"
B16_age_sel$State[B16_age_sel$State == "17"] <- "Illinois"
B16_age_sel$State[B16_age_sel$State == "18"] <- "Indiana"
B16_age_sel$State[B16_age_sel$State == "19"] <- "Iowa"
B16_age_sel$State[B16_age_sel$State == "20"] <- "Kansas"
B16_age_sel$State[B16_age_sel$State == "21"] <- "Kentucky"
B16_age_sel$State[B16_age_sel$State == "22"] <- "Louisiana"
B16_age_sel$State[B16_age_sel$State == "23"] <- "Maine"
B16_age_sel$State[B16_age_sel$State == "24"] <- "Maryland"
B16_age_sel$State[B16_age_sel$State == "25"] <- "Massachusetts"
B16_age_sel$State[B16_age_sel$State == "26"] <- "Michigan"
B16_age_sel$State[B16_age_sel$State == "27"] <- "Minnesota"
B16_age_sel$State[B16_age_sel$State == "28"] <- "Mississippi"
B16_age_sel$State[B16_age_sel$State == "29"] <- "Missouri"
B16_age_sel$State[B16_age_sel$State == "30"] <- "Montana"
B16_age_sel$State[B16_age_sel$State == "31"] <- "Nebraska"
B16_age_sel$State[B16_age_sel$State == "32"] <- "Nevada"
B16_age_sel$State[B16_age_sel$State == "33"] <- "New Hampshire"
B16_age_sel$State[B16_age_sel$State == "34"] <- "New Jersey"
B16_age_sel$State[B16_age_sel$State == "35"] <- "New Mexico"
B16_age_sel$State[B16_age_sel$State == "36"] <- "New York"
B16_age_sel$State[B16_age_sel$State == "37"] <- "North Carolina"
B16_age_sel$State[B16_age_sel$State == "38"] <- "North Dakota"
B16_age_sel$State[B16_age_sel$State == "39"] <- "Ohio"
B16_age_sel$State[B16_age_sel$State == "40"] <- "Oklahoma"
B16_age_sel$State[B16_age_sel$State == "41"] <- "Oregon"
B16_age_sel$State[B16_age_sel$State == "42"] <- "Pennsylvania"
B16_age_sel$State[B16_age_sel$State == "44"] <- "Rhode Island"
B16_age_sel$State[B16_age_sel$State == "45"] <- "South Carolina"
B16_age_sel$State[B16_age_sel$State == "46"] <- "South Dakota"
B16_age_sel$State[B16_age_sel$State == "47"] <- "Tennessee"
B16_age_sel$State[B16_age_sel$State == "48"] <- "Texas"
B16_age_sel$State[B16_age_sel$State == "49"] <- "Utah"
B16_age_sel$State[B16_age_sel$State == "50"] <- "Vermont"
B16_age_sel$State[B16_age_sel$State == "51"] <- "Virginia"
B16_age_sel$State[B16_age_sel$State == "53"] <- "Washington"
B16_age_sel$State[B16_age_sel$State == "54"] <- "West Virginia"
B16_age_sel$State[B16_age_sel$State == "55"] <- "Wisconsin"
B16_age_sel$State[B16_age_sel$State == "56"] <- "Wyoming"
B16_age_sel$State[B16_age_sel$State == "66"] <- "Guam"
B16_age_sel$State[B16_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B16_age_sel$region <- tolower(B16_age_sel$State)
B16_age_sel_join <- left_join(us_states, B16_age_sel)

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age1824.state.map.png")

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age2529.state.map.png")

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age3034.state.map.png")

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age3539.state.map.png")

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age4044.state.map.png")

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age4549.state.map.png")

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age5054.state.map.png")

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age5559.state.map.png")

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age6064.state.map.png")

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age6569.state.map.png")

#Map
B16age.state.map <- ggplot(data = B16_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70year-olds and above per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.age70ab.state.map.png")

#2016-sex
B16_sex <- svyby(~SEX, ~`_STATE`, BRFSS16, svymean, na.rm = TRUE)
B16_sex_sel <- B16_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B16_sex_sel$State[B16_sex_sel$State == "1"] <- "Alabama"
B16_sex_sel$State[B16_sex_sel$State == "2"] <- "Alaska"
B16_sex_sel$State[B16_sex_sel$State == "4"] <- "Arizona"
B16_sex_sel$State[B16_sex_sel$State == "5"] <- "Arkansas"
B16_sex_sel$State[B16_sex_sel$State == "6"] <- "California"
B16_sex_sel$State[B16_sex_sel$State == "8"] <- "Colorado"
B16_sex_sel$State[B16_sex_sel$State == "9"] <- "Connecticut"
B16_sex_sel$State[B16_sex_sel$State == "10"] <- "Delaware"
B16_sex_sel$State[B16_sex_sel$State == "11"] <- "District of Columbia"
B16_sex_sel$State[B16_sex_sel$State == "12"] <- "Florida"
B16_sex_sel$State[B16_sex_sel$State == "13"] <- "Georgia"
B16_sex_sel$State[B16_sex_sel$State == "15"] <- "Hawaii"
B16_sex_sel$State[B16_sex_sel$State == "16"] <- "Idaho"
B16_sex_sel$State[B16_sex_sel$State == "17"] <- "Illinois"
B16_sex_sel$State[B16_sex_sel$State == "18"] <- "Indiana"
B16_sex_sel$State[B16_sex_sel$State == "19"] <- "Iowa"
B16_sex_sel$State[B16_sex_sel$State == "20"] <- "Kansas"
B16_sex_sel$State[B16_sex_sel$State == "21"] <- "Kentucky"
B16_sex_sel$State[B16_sex_sel$State == "22"] <- "Louisiana"
B16_sex_sel$State[B16_sex_sel$State == "23"] <- "Maine"
B16_sex_sel$State[B16_sex_sel$State == "24"] <- "Maryland"
B16_sex_sel$State[B16_sex_sel$State == "25"] <- "Massachusetts"
B16_sex_sel$State[B16_sex_sel$State == "26"] <- "Michigan"
B16_sex_sel$State[B16_sex_sel$State == "27"] <- "Minnesota"
B16_sex_sel$State[B16_sex_sel$State == "28"] <- "Mississippi"
B16_sex_sel$State[B16_sex_sel$State == "29"] <- "Missouri"
B16_sex_sel$State[B16_sex_sel$State == "30"] <- "Montana"
B16_sex_sel$State[B16_sex_sel$State == "31"] <- "Nebraska"
B16_sex_sel$State[B16_sex_sel$State == "32"] <- "Nevada"
B16_sex_sel$State[B16_sex_sel$State == "33"] <- "New Hampshire"
B16_sex_sel$State[B16_sex_sel$State == "34"] <- "New Jersey"
B16_sex_sel$State[B16_sex_sel$State == "35"] <- "New Mexico"
B16_sex_sel$State[B16_sex_sel$State == "36"] <- "New York"
B16_sex_sel$State[B16_sex_sel$State == "37"] <- "North Carolina"
B16_sex_sel$State[B16_sex_sel$State == "38"] <- "North Dakota"
B16_sex_sel$State[B16_sex_sel$State == "39"] <- "Ohio"
B16_sex_sel$State[B16_sex_sel$State == "40"] <- "Oklahoma"
B16_sex_sel$State[B16_sex_sel$State == "41"] <- "Oregon"
B16_sex_sel$State[B16_sex_sel$State == "42"] <- "Pennsylvania"
B16_sex_sel$State[B16_sex_sel$State == "44"] <- "Rhode Island"
B16_sex_sel$State[B16_sex_sel$State == "45"] <- "South Carolina"
B16_sex_sel$State[B16_sex_sel$State == "46"] <- "South Dakota"
B16_sex_sel$State[B16_sex_sel$State == "47"] <- "Tennessee"
B16_sex_sel$State[B16_sex_sel$State == "48"] <- "Texas"
B16_sex_sel$State[B16_sex_sel$State == "49"] <- "Utah"
B16_sex_sel$State[B16_sex_sel$State == "50"] <- "Vermont"
B16_sex_sel$State[B16_sex_sel$State == "51"] <- "Virginia"
B16_sex_sel$State[B16_sex_sel$State == "53"] <- "Washington"
B16_sex_sel$State[B16_sex_sel$State == "54"] <- "West Virginia"
B16_sex_sel$State[B16_sex_sel$State == "55"] <- "Wisconsin"
B16_sex_sel$State[B16_sex_sel$State == "56"] <- "Wyoming"
B16_sex_sel$State[B16_sex_sel$State == "66"] <- "Guam"
B16_sex_sel$State[B16_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B16_sex_sel$region <- tolower(B16_sex_sel$State)
B16_sex_sel_join <- left_join(us_states, B16_sex_sel)

#Map
B16sex.state.map <- ggplot(data = B16_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2016") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B16.sex.state.map.png")

#2017
#2017-age
B17_age <- svyby(~`_AGEG5YR`, ~`_STATE`, BRFSS17, svymean, na.rm = TRUE)
B17_age_sel <- B17_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B17_age_sel$State[B17_age_sel$State == "1"] <- "Alabama"
B17_age_sel$State[B17_age_sel$State == "2"] <- "Alaska"
B17_age_sel$State[B17_age_sel$State == "4"] <- "Arizona"
B17_age_sel$State[B17_age_sel$State == "5"] <- "Arkansas"
B17_age_sel$State[B17_age_sel$State == "6"] <- "California"
B17_age_sel$State[B17_age_sel$State == "8"] <- "Colorado"
B17_age_sel$State[B17_age_sel$State == "9"] <- "Connecticut"
B17_age_sel$State[B17_age_sel$State == "10"] <- "Delaware"
B17_age_sel$State[B17_age_sel$State == "11"] <- "District of Columbia"
B17_age_sel$State[B17_age_sel$State == "12"] <- "Florida"
B17_age_sel$State[B17_age_sel$State == "13"] <- "Georgia"
B17_age_sel$State[B17_age_sel$State == "15"] <- "Hawaii"
B17_age_sel$State[B17_age_sel$State == "16"] <- "Idaho"
B17_age_sel$State[B17_age_sel$State == "17"] <- "Illinois"
B17_age_sel$State[B17_age_sel$State == "18"] <- "Indiana"
B17_age_sel$State[B17_age_sel$State == "19"] <- "Iowa"
B17_age_sel$State[B17_age_sel$State == "20"] <- "Kansas"
B17_age_sel$State[B17_age_sel$State == "21"] <- "Kentucky"
B17_age_sel$State[B17_age_sel$State == "22"] <- "Louisiana"
B17_age_sel$State[B17_age_sel$State == "23"] <- "Maine"
B17_age_sel$State[B17_age_sel$State == "24"] <- "Maryland"
B17_age_sel$State[B17_age_sel$State == "25"] <- "Massachusetts"
B17_age_sel$State[B17_age_sel$State == "26"] <- "Michigan"
B17_age_sel$State[B17_age_sel$State == "27"] <- "Minnesota"
B17_age_sel$State[B17_age_sel$State == "28"] <- "Mississippi"
B17_age_sel$State[B17_age_sel$State == "29"] <- "Missouri"
B17_age_sel$State[B17_age_sel$State == "30"] <- "Montana"
B17_age_sel$State[B17_age_sel$State == "31"] <- "Nebraska"
B17_age_sel$State[B17_age_sel$State == "32"] <- "Nevada"
B17_age_sel$State[B17_age_sel$State == "33"] <- "New Hampshire"
B17_age_sel$State[B17_age_sel$State == "34"] <- "New Jersey"
B17_age_sel$State[B17_age_sel$State == "35"] <- "New Mexico"
B17_age_sel$State[B17_age_sel$State == "36"] <- "New York"
B17_age_sel$State[B17_age_sel$State == "37"] <- "North Carolina"
B17_age_sel$State[B17_age_sel$State == "38"] <- "North Dakota"
B17_age_sel$State[B17_age_sel$State == "39"] <- "Ohio"
B17_age_sel$State[B17_age_sel$State == "40"] <- "Oklahoma"
B17_age_sel$State[B17_age_sel$State == "41"] <- "Oregon"
B17_age_sel$State[B17_age_sel$State == "42"] <- "Pennsylvania"
B17_age_sel$State[B17_age_sel$State == "44"] <- "Rhode Island"
B17_age_sel$State[B17_age_sel$State == "45"] <- "South Carolina"
B17_age_sel$State[B17_age_sel$State == "46"] <- "South Dakota"
B17_age_sel$State[B17_age_sel$State == "47"] <- "Tennessee"
B17_age_sel$State[B17_age_sel$State == "48"] <- "Texas"
B17_age_sel$State[B17_age_sel$State == "49"] <- "Utah"
B17_age_sel$State[B17_age_sel$State == "50"] <- "Vermont"
B17_age_sel$State[B17_age_sel$State == "51"] <- "Virginia"
B17_age_sel$State[B17_age_sel$State == "53"] <- "Washington"
B17_age_sel$State[B17_age_sel$State == "54"] <- "West Virginia"
B17_age_sel$State[B17_age_sel$State == "55"] <- "Wisconsin"
B17_age_sel$State[B17_age_sel$State == "56"] <- "Wyoming"
B17_age_sel$State[B17_age_sel$State == "66"] <- "Guam"
B17_age_sel$State[B17_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B17_age_sel$region <- tolower(B17_age_sel$State)
B17_age_sel_join <- left_join(us_states, B17_age_sel)

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.age1824.state.map.png")

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.age2529.state.map.png")

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.age3034.state.map.png")

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.age3539.state.map.png")

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.age4044.state.map.png")

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
  ggsave("B17.age4549.state.map.png")

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.age5054.state.map.png")

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.age5559.state.map.png")

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.age6064.state.map.png")

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.age6569.state.map.png")

#Map
B17age.state.map <- ggplot(data = B17_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds and above per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.age70ab.state.map.png")

#2017-sex
B17_sex <- svyby(~SEX, ~`_STATE`, BRFSS17, svymean, na.rm = TRUE)
B17_sex_sel <- B17_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B17_sex_sel$State[B17_sex_sel$State == "1"] <- "Alabama"
B17_sex_sel$State[B17_sex_sel$State == "2"] <- "Alaska"
B17_sex_sel$State[B17_sex_sel$State == "4"] <- "Arizona"
B17_sex_sel$State[B17_sex_sel$State == "5"] <- "Arkansas"
B17_sex_sel$State[B17_sex_sel$State == "6"] <- "California"
B17_sex_sel$State[B17_sex_sel$State == "8"] <- "Colorado"
B17_sex_sel$State[B17_sex_sel$State == "9"] <- "Connecticut"
B17_sex_sel$State[B17_sex_sel$State == "10"] <- "Delaware"
B17_sex_sel$State[B17_sex_sel$State == "11"] <- "District of Columbia"
B17_sex_sel$State[B17_sex_sel$State == "12"] <- "Florida"
B17_sex_sel$State[B17_sex_sel$State == "13"] <- "Georgia"
B17_sex_sel$State[B17_sex_sel$State == "15"] <- "Hawaii"
B17_sex_sel$State[B17_sex_sel$State == "16"] <- "Idaho"
B17_sex_sel$State[B17_sex_sel$State == "17"] <- "Illinois"
B17_sex_sel$State[B17_sex_sel$State == "18"] <- "Indiana"
B17_sex_sel$State[B17_sex_sel$State == "19"] <- "Iowa"
B17_sex_sel$State[B17_sex_sel$State == "20"] <- "Kansas"
B17_sex_sel$State[B17_sex_sel$State == "21"] <- "Kentucky"
B17_sex_sel$State[B17_sex_sel$State == "22"] <- "Louisiana"
B17_sex_sel$State[B17_sex_sel$State == "23"] <- "Maine"
B17_sex_sel$State[B17_sex_sel$State == "24"] <- "Maryland"
B17_sex_sel$State[B17_sex_sel$State == "25"] <- "Massachusetts"
B17_sex_sel$State[B17_sex_sel$State == "26"] <- "Michigan"
B17_sex_sel$State[B17_sex_sel$State == "27"] <- "Minnesota"
B17_sex_sel$State[B17_sex_sel$State == "28"] <- "Mississippi"
B17_sex_sel$State[B17_sex_sel$State == "29"] <- "Missouri"
B17_sex_sel$State[B17_sex_sel$State == "30"] <- "Montana"
B17_sex_sel$State[B17_sex_sel$State == "31"] <- "Nebraska"
B17_sex_sel$State[B17_sex_sel$State == "32"] <- "Nevada"
B17_sex_sel$State[B17_sex_sel$State == "33"] <- "New Hampshire"
B17_sex_sel$State[B17_sex_sel$State == "34"] <- "New Jersey"
B17_sex_sel$State[B17_sex_sel$State == "35"] <- "New Mexico"
B17_sex_sel$State[B17_sex_sel$State == "36"] <- "New York"
B17_sex_sel$State[B17_sex_sel$State == "37"] <- "North Carolina"
B17_sex_sel$State[B17_sex_sel$State == "38"] <- "North Dakota"
B17_sex_sel$State[B17_sex_sel$State == "39"] <- "Ohio"
B17_sex_sel$State[B17_sex_sel$State == "40"] <- "Oklahoma"
B17_sex_sel$State[B17_sex_sel$State == "41"] <- "Oregon"
B17_sex_sel$State[B17_sex_sel$State == "42"] <- "Pennsylvania"
B17_sex_sel$State[B17_sex_sel$State == "44"] <- "Rhode Island"
B17_sex_sel$State[B17_sex_sel$State == "45"] <- "South Carolina"
B17_sex_sel$State[B17_sex_sel$State == "46"] <- "South Dakota"
B17_sex_sel$State[B17_sex_sel$State == "47"] <- "Tennessee"
B17_sex_sel$State[B17_sex_sel$State == "48"] <- "Texas"
B17_sex_sel$State[B17_sex_sel$State == "49"] <- "Utah"
B17_sex_sel$State[B17_sex_sel$State == "50"] <- "Vermont"
B17_sex_sel$State[B17_sex_sel$State == "51"] <- "Virginia"
B17_sex_sel$State[B17_sex_sel$State == "53"] <- "Washington"
B17_sex_sel$State[B17_sex_sel$State == "54"] <- "West Virginia"
B17_sex_sel$State[B17_sex_sel$State == "55"] <- "Wisconsin"
B17_sex_sel$State[B17_sex_sel$State == "56"] <- "Wyoming"
B17_sex_sel$State[B17_sex_sel$State == "66"] <- "Guam"
B17_sex_sel$State[B17_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B17_sex_sel$region <- tolower(B17_sex_sel$State)
B17_sex_sel_join <- left_join(us_states, B17_sex_sel)

B17sex.state.map <- ggplot(data = B17_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2017") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B17.sex.state.map.png")

#2018
#2018 - age
B18_age <- svyby(~`_AGEG5YR`, ~`_STATE`, BRFSS18, svymean, na.rm = TRUE)
B18_age_sel <- B18_age %>%
  select(1:12) %>%
  setNames(c("State", "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and above")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B18_age_sel$State[B18_age_sel$State == "1"] <- "Alabama"
B18_age_sel$State[B18_age_sel$State == "2"] <- "Alaska"
B18_age_sel$State[B18_age_sel$State == "4"] <- "Arizona"
B18_age_sel$State[B18_age_sel$State == "5"] <- "Arkansas"
B18_age_sel$State[B18_age_sel$State == "6"] <- "California"
B18_age_sel$State[B18_age_sel$State == "8"] <- "Colorado"
B18_age_sel$State[B18_age_sel$State == "9"] <- "Connecticut"
B18_age_sel$State[B18_age_sel$State == "10"] <- "Delaware"
B18_age_sel$State[B18_age_sel$State == "11"] <- "District of Columbia"
B18_age_sel$State[B18_age_sel$State == "12"] <- "Florida"
B18_age_sel$State[B18_age_sel$State == "13"] <- "Georgia"
B18_age_sel$State[B18_age_sel$State == "15"] <- "Hawaii"
B18_age_sel$State[B18_age_sel$State == "16"] <- "Idaho"
B18_age_sel$State[B18_age_sel$State == "17"] <- "Illinois"
B18_age_sel$State[B18_age_sel$State == "18"] <- "Indiana"
B18_age_sel$State[B18_age_sel$State == "19"] <- "Iowa"
B18_age_sel$State[B18_age_sel$State == "20"] <- "Kansas"
B18_age_sel$State[B18_age_sel$State == "21"] <- "Kentucky"
B18_age_sel$State[B18_age_sel$State == "22"] <- "Louisiana"
B18_age_sel$State[B18_age_sel$State == "23"] <- "Maine"
B18_age_sel$State[B18_age_sel$State == "24"] <- "Maryland"
B18_age_sel$State[B18_age_sel$State == "25"] <- "Massachusetts"
B18_age_sel$State[B18_age_sel$State == "26"] <- "Michigan"
B18_age_sel$State[B18_age_sel$State == "27"] <- "Minnesota"
B18_age_sel$State[B18_age_sel$State == "28"] <- "Mississippi"
B18_age_sel$State[B18_age_sel$State == "29"] <- "Missouri"
B18_age_sel$State[B18_age_sel$State == "30"] <- "Montana"
B18_age_sel$State[B18_age_sel$State == "31"] <- "Nebraska"
B18_age_sel$State[B18_age_sel$State == "32"] <- "Nevada"
B18_age_sel$State[B18_age_sel$State == "33"] <- "New Hampshire"
B18_age_sel$State[B18_age_sel$State == "34"] <- "New Jersey"
B18_age_sel$State[B18_age_sel$State == "35"] <- "New Mexico"
B18_age_sel$State[B18_age_sel$State == "36"] <- "New York"
B18_age_sel$State[B18_age_sel$State == "37"] <- "North Carolina"
B18_age_sel$State[B18_age_sel$State == "38"] <- "North Dakota"
B18_age_sel$State[B18_age_sel$State == "39"] <- "Ohio"
B18_age_sel$State[B18_age_sel$State == "40"] <- "Oklahoma"
B18_age_sel$State[B18_age_sel$State == "41"] <- "Oregon"
B18_age_sel$State[B18_age_sel$State == "42"] <- "Pennsylvania"
B18_age_sel$State[B18_age_sel$State == "44"] <- "Rhode Island"
B18_age_sel$State[B18_age_sel$State == "45"] <- "South Carolina"
B18_age_sel$State[B18_age_sel$State == "46"] <- "South Dakota"
B18_age_sel$State[B18_age_sel$State == "47"] <- "Tennessee"
B18_age_sel$State[B18_age_sel$State == "48"] <- "Texas"
B18_age_sel$State[B18_age_sel$State == "49"] <- "Utah"
B18_age_sel$State[B18_age_sel$State == "50"] <- "Vermont"
B18_age_sel$State[B18_age_sel$State == "51"] <- "Virginia"
B18_age_sel$State[B18_age_sel$State == "53"] <- "Washington"
B18_age_sel$State[B18_age_sel$State == "54"] <- "West Virginia"
B18_age_sel$State[B18_age_sel$State == "55"] <- "Wisconsin"
B18_age_sel$State[B18_age_sel$State == "56"] <- "Wyoming"
B18_age_sel$State[B18_age_sel$State == "66"] <- "Guam"
B18_age_sel$State[B18_age_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B18_age_sel$region <- tolower(B18_age_sel$State)
B18_age_sel_join <- left_join(us_states, B18_age_sel)

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `18 to 24`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 18- to 24-year-olds per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age1824.state.map.png")

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `25 to 29`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 25- to 29-year-olds per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age2529.state.map.png")

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `30 to 34`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 30- to 34-year-olds per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age3034.state.map.png")

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `35 to 39`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 35- to 39-year-olds per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age3539.state.map.png")

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `40 to 44`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 40- to 44-year-olds per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age4044.state.map.png")

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `45 to 49`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 45- to 49-year-olds per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age4549.state.map.png")

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `50 to 54`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 50- to 54-year-olds per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age5054.state.map.png")

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `55 to 59`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 55- to 59-year-olds per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age5559.state.map.png")

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `60 to 64`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 60- to 64-year-olds per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age6064.state.map.png")

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `65 to 69`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 65- to 69-year-olds per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age6569.state.map.png")

#Map
B18age.state.map <- ggplot(data = B18_age_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = `70 and above`)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(5, 20), breaks = c(5, 10, 15, 20), low = "#0099FF", high = "black") + 
  ggtitle("Percentage of 70-year-olds or above per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.age70ab.state.map.png")


#2018 - sex
B18_sex <- svyby(~SEX1, ~`_STATE`, BRFSS18, svymean, na.rm = TRUE)
B18_sex_sel <- B18_sex %>%
  select(1:3) %>%
  setNames(c("State", "Female", "Male")) %>%
  mutate(State = as.character(State)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))

#Rename the states
B18_sex_sel$State[B18_sex_sel$State == "1"] <- "Alabama"
B18_sex_sel$State[B18_sex_sel$State == "2"] <- "Alaska"
B18_sex_sel$State[B18_sex_sel$State == "4"] <- "Arizona"
B18_sex_sel$State[B18_sex_sel$State == "5"] <- "Arkansas"
B18_sex_sel$State[B18_sex_sel$State == "6"] <- "California"
B18_sex_sel$State[B18_sex_sel$State == "8"] <- "Colorado"
B18_sex_sel$State[B18_sex_sel$State == "9"] <- "Connecticut"
B18_sex_sel$State[B18_sex_sel$State == "10"] <- "Delaware"
B18_sex_sel$State[B18_sex_sel$State == "11"] <- "District of Columbia"
B18_sex_sel$State[B18_sex_sel$State == "12"] <- "Florida"
B18_sex_sel$State[B18_sex_sel$State == "13"] <- "Georgia"
B18_sex_sel$State[B18_sex_sel$State == "15"] <- "Hawaii"
B18_sex_sel$State[B18_sex_sel$State == "16"] <- "Idaho"
B18_sex_sel$State[B18_sex_sel$State == "17"] <- "Illinois"
B18_sex_sel$State[B18_sex_sel$State == "18"] <- "Indiana"
B18_sex_sel$State[B18_sex_sel$State == "19"] <- "Iowa"
B18_sex_sel$State[B18_sex_sel$State == "20"] <- "Kansas"
B18_sex_sel$State[B18_sex_sel$State == "21"] <- "Kentucky"
B18_sex_sel$State[B18_sex_sel$State == "22"] <- "Louisiana"
B18_sex_sel$State[B18_sex_sel$State == "23"] <- "Maine"
B18_sex_sel$State[B18_sex_sel$State == "24"] <- "Maryland"
B18_sex_sel$State[B18_sex_sel$State == "25"] <- "Massachusetts"
B18_sex_sel$State[B18_sex_sel$State == "26"] <- "Michigan"
B18_sex_sel$State[B18_sex_sel$State == "27"] <- "Minnesota"
B18_sex_sel$State[B18_sex_sel$State == "28"] <- "Mississippi"
B18_sex_sel$State[B18_sex_sel$State == "29"] <- "Missouri"
B18_sex_sel$State[B18_sex_sel$State == "30"] <- "Montana"
B18_sex_sel$State[B18_sex_sel$State == "31"] <- "Nebraska"
B18_sex_sel$State[B18_sex_sel$State == "32"] <- "Nevada"
B18_sex_sel$State[B18_sex_sel$State == "33"] <- "New Hampshire"
B18_sex_sel$State[B18_sex_sel$State == "34"] <- "New Jersey"
B18_sex_sel$State[B18_sex_sel$State == "35"] <- "New Mexico"
B18_sex_sel$State[B18_sex_sel$State == "36"] <- "New York"
B18_sex_sel$State[B18_sex_sel$State == "37"] <- "North Carolina"
B18_sex_sel$State[B18_sex_sel$State == "38"] <- "North Dakota"
B18_sex_sel$State[B18_sex_sel$State == "39"] <- "Ohio"
B18_sex_sel$State[B18_sex_sel$State == "40"] <- "Oklahoma"
B18_sex_sel$State[B18_sex_sel$State == "41"] <- "Oregon"
B18_sex_sel$State[B18_sex_sel$State == "42"] <- "Pennsylvania"
B18_sex_sel$State[B18_sex_sel$State == "44"] <- "Rhode Island"
B18_sex_sel$State[B18_sex_sel$State == "45"] <- "South Carolina"
B18_sex_sel$State[B18_sex_sel$State == "46"] <- "South Dakota"
B18_sex_sel$State[B18_sex_sel$State == "47"] <- "Tennessee"
B18_sex_sel$State[B18_sex_sel$State == "48"] <- "Texas"
B18_sex_sel$State[B18_sex_sel$State == "49"] <- "Utah"
B18_sex_sel$State[B18_sex_sel$State == "50"] <- "Vermont"
B18_sex_sel$State[B18_sex_sel$State == "51"] <- "Virginia"
B18_sex_sel$State[B18_sex_sel$State == "53"] <- "Washington"
B18_sex_sel$State[B18_sex_sel$State == "54"] <- "West Virginia"
B18_sex_sel$State[B18_sex_sel$State == "55"] <- "Wisconsin"
B18_sex_sel$State[B18_sex_sel$State == "56"] <- "Wyoming"
B18_sex_sel$State[B18_sex_sel$State == "66"] <- "Guam"
B18_sex_sel$State[B18_sex_sel$State == "72"] <- "Puerto Rico"

#Merge my data with my data
B18_sex_sel$region <- tolower(B18_sex_sel$State)
B18_sex_sel_join <- left_join(us_states, B18_sex_sel)

#Map
B18sex.state.map <- ggplot(data = B18_sex_sel_join,
                           mapping = aes(x = long, y = lat,
                                         group = group,
                                         fill = Male)) +
  geom_polygon(colour = "white", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  labs(fill = "Prevalence (%)") +
  theme(text = element_text(colour = "black", size = 16)) +
  scale_fill_continuous(limits = c(45, 55), breaks = c(45, 50, 55), low = "#FF0000", high = "#000000") +
  ggtitle("Percentage of males per state in 2018") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggsave("B18.sex.state.map.png")


