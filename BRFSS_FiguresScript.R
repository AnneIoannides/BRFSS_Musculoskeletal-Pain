#Anne Elizabeth Ioannides

#For the qualification of PhD (Physiology), University of the Witwatersrand, South Africa

#Behavioural Risk Factor Surveillance System (BRFSS)

#Due to very large file size, the workflow for BRFSS analyses is slightly different compared to the other surveys used in my PhD. Each year (data download, cleaning, and analysis) takes place in one script. 
#Each year (except module years) has one R script dedicated to it. Module years done collectively, and BRFSS figures that are constructed across time can be found in the Module years scripts

#Note - "Arthritis" is often used in this script as a synonym for Rheumatic Disorder Diagnosis (RDD). This is because the BRFSS variable for RDD is called "HAVARTH", and therefore "ARTHritis" was an easy-navigation term.

#This script is specifically to make BRFSS figures that are comprised of data from more than one year. Due to the size of the RAM and processing power required to run design object analyses, I saved each prevalence, 95% CI, SE in a backup csv format.
  #These csv files are openly available (Dropbox) and can be cross-checked with data generated within the annual scripts


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


#Figures for Chapter 3: Temporal Trends

#make quick function for every nth year shown on x-axis
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


#1. Overall prevalence of rdd from 1996 to 2018 (except '06 and '08)

#Pull the csv's in
B96 <- read.csv("https://www.dropbox.com/s/z3wbdbi0z4up1ie/B96_Arthritis.csv?dl=1")
B97 <- read.csv("https://www.dropbox.com/s/x9ba4zin6a6fd95/B97_Arthritis.csv?dl=1")
B98 <- read.csv("https://www.dropbox.com/s/qlz0arla659xvx2/B98_Arthritis.csv?dl=1")
B99 <- read.csv("https://www.dropbox.com/s/vmb1eq1kjv5magh/B99_Arthritis.csv?dl=1")
B00 <- read.csv("https://www.dropbox.com/s/8laybyzliinjyj3/B00_Arthritis.csv?dl=1")
B01 <- read.csv("https://www.dropbox.com/s/6et6d5kz05w7xxv/B01_Arthritis.csv?dl=1")
B02 <- read.csv("https://www.dropbox.com/s/swgzlo0jo5goe0g/B02_Arthritis.csv?dl=1")
B03 <- read.csv("https://www.dropbox.com/s/epp8dpeeqk0kwas/B03_Arthritis.csv?dl=1")
B04 <- read.csv("https://www.dropbox.com/s/dugr8jmv4bo9wzm/B04_Arthritis.csv?dl=1")
B05 <- read.csv("https://www.dropbox.com/s/8hwsntytuxw9jo0/B05_Arthritis.csv?dl=1")
B07 <- read.csv("https://www.dropbox.com/s/u0jzu7ojex4vtxl/B07_Arthritis.csv?dl=1")
B09 <- read.csv("https://www.dropbox.com/s/w34xysnfygt7ibx/B09_Arthritis.csv?dl=1")
B10 <- read.csv("https://www.dropbox.com/s/czgkwbn51h93vmu/B10_Arthritis.csv?dl=1")
B11 <- read.csv("https://www.dropbox.com/s/wkegrk98kowrlhk/B11_Arthritis.csv?dl=1")
B12 <- read.csv("https://www.dropbox.com/s/nku7ad5psyok9f3/B12_Arthritis.csv?dl=1")
B13 <- read.csv("https://www.dropbox.com/s/zd3k0mlnx37c427/B13_Arthritis.csv?dl=1")
B14 <- read.csv("https://www.dropbox.com/s/69stvrp7srshuxa/B14_Arthritis.csv?dl=1")
B15 <- read.csv("https://www.dropbox.com/s/491e3r6j4kiglxo/B15_Arthritis.csv?dl=1")
B16 <- read.csv("https://www.dropbox.com/s/fxytgnkbl0pnw2s/B16_Arthritis.csv?dl=1")
B17 <- read.csv("https://www.dropbox.com/s/jb6veu8tsqr85wc/B17_Arthritis.csv?dl=1")
B18 <- read.csv("https://www.dropbox.com/s/nn1ppdj4zw989ln/B18_Arthritis.csv?dl=1")

#Add year to each file
B96$Year <- c("1996")
B97$Year <- c("1997")
B98$Year <- c("1998")
B99$Year <- c("1999")
B00$Year <- c("2000")
B01$Year <- c("2001")
B02$Year <- c("2002")
B03$Year <- c("2003")
B04$Year <- c("2004")
B05$Year <- c("2005")
B07$Year <- c("2007")
B09$Year <- c("2009")
B10$Year <- c("2010")
B11$Year <- c("2011")
B12$Year <- c("2012")
B13$Year <- c("2013")
B14$Year <- c("2014")
B15$Year <- c("2015")
B16$Year <- c("2016")
B17$Year <- c("2017")
B18$Year <- c("2018")

#Merge
B.overall <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B96, B97, B98, B99, B00, B01, B02, B03, B04, B05, B07, B09, B10, B11, B12, B13, B14, B15, B16, B17, B18))


#Overall temporal plot
B.overall.plot <- ggplot(data = B.overall,
                         aes(x = Year, y = Proportion, group = 1)) +
  geom_line(colour = "#000099") + 
  geom_point(colour = "#000099") + 
  geom_ribbon(aes(ymin = B.overall$CI_Prop_low, ymax = B.overall$CI_Prop_upp), alpha = 0.2, fill = "#000099") +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("BRFSS survey cycles (years)")
ggsave("B.overall.png", width = 18, height = 11)
  
  
#Age-specific temporal plot of rdd from 1996 to 2018 (except '06 and '08)

#Pull the csv's in
B96.age <- read.csv("https://www.dropbox.com/s/1japzbzl1j2spon/B96_Arthritis.age.csv?dl=1")
B97.age <- read.csv("https://www.dropbox.com/s/rydpkqw0r2uvhqs/B97_Arthritis.age.csv?dl=1")
B98.age <- read.csv("https://www.dropbox.com/s/s0kxsqj2wb1qd86/B98_Arthritis.age.csv?dl=1")
B99.age <- read.csv("https://www.dropbox.com/s/hqxvfqr1k05namy/B99_Arthritis.age.csv?dl=1")
B00.age <- read.csv("https://www.dropbox.com/s/f20t5pfsdqthb41/B00_Arthritis.age.csv?dl=1")
B01.age <- read.csv("https://www.dropbox.com/s/973yf1kmepm00wv/B01_Arthritis.age.csv?dl=1")
B02.age <- read.csv("https://www.dropbox.com/s/juf3e814awit12d/B02_Arthritis.age.csv?dl=1")
B03.age <- read.csv("https://www.dropbox.com/s/bcl0ops0y8ve6kk/B03_Arthritis.age.csv?dl=1")
B04.age <- read.csv("https://www.dropbox.com/s/vc3nb01lubcyrmm/B04_Arthritis.age.csv?dl=1")
B05.age <- read.csv("https://www.dropbox.com/s/h45x1clexxqoyhv/B05_Arthritis.age.csv?dl=1")
B07.age <- read.csv("https://www.dropbox.com/s/9t2sq69779igyys/B07_Arthritis.age.csv?dl=1")
B09.age <- read.csv("https://www.dropbox.com/s/ym2bx90oxw1wlpj/B09_Arthritis.age.csv?dl=1")
B10.age <- read.csv("https://www.dropbox.com/s/gvi66oziwqddsd4/B10_Arthritis.age.csv?dl=1")
B11.age <- read.csv("https://www.dropbox.com/s/jf69q8tl0w3g91b/B11_Arthritis.age.csv?dl=1")
B12.age <- read.csv("https://www.dropbox.com/s/rf410z2zcmd1ul0/B12_Arthritis.age.csv?dl=1")
B13.age <- read.csv("https://www.dropbox.com/s/wqtqhircod32dd3/B13_Arthritis.age.csv?dl=1")
B14.age <- read.csv("https://www.dropbox.com/s/utyxrinl0aix1ln/B14_Arthritis.age.csv?dl=1")
B15.age <- read.csv("https://www.dropbox.com/s/t446s0gy72yy7z4/B15_Arthritis.age.csv?dl=1")
B16.age <- read.csv("https://www.dropbox.com/s/qc64o4l49cd0791/B16_Arthritis.age.csv?dl=1")
B17.age <- read.csv("https://www.dropbox.com/s/553er4lq2je53kv/B17_Arthritis.age.csv?dl=1")
B18.age <- read.csv("https://www.dropbox.com/s/9e8s1n6ijz5757s/B18_Arthritis.age.csv?dl=1")


#Add year to each file
B96.age$Year <- c("1996", "1996", "1996", "1996", "1996", "1996", "1996", "1996", "1996", "1996", "1996")
B97.age$Year <- c("1997", "1997", "1997", "1997", "1997", "1997", "1997", "1997", "1997", "1997", "1997")
B98.age$Year <- c("1998", "1998", "1998", "1998", "1998", "1998", "1998", "1998", "1998", "1998", "1998")
B99.age$Year <- c("1999", "1999", "1999", "1999", "1999", "1999", "1999", "1999", "1999", "1999", "1999")
B00.age$Year <- c("2000", "2000", "2000", "2000", "2000", "2000", "2000", "2000", "2000", "2000", "2000")
B01.age$Year <- c("2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001", "2001")
B02.age$Year <- c("2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002", "2002")
B03.age$Year <- c("2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003", "2003")
B04.age$Year <- c("2004", "2004", "2004", "2004", "2004", "2004", "2004", "2004", "2004", "2004", "2004")
B05.age$Year <- c("2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005", "2005")
B07.age$Year <- c("2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007", "2007")
B09.age$Year <- c("2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009", "2009")
B10.age$Year <- c("2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010", "2010")
B11.age$Year <- c("2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011", "2011")
B12.age$Year <- c("2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012", "2012")
B13.age$Year <- c("2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013", "2013")
B14.age$Year <- c("2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014", "2014")
B15.age$Year <- c("2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015", "2015")
B16.age$Year <- c("2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016", "2016")
B17.age$Year <- c("2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017")
B18.age$Year <- c("2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018")


#Merge
B.age <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B96.age, B97.age, B98.age, B99.age, B00.age, B01.age, B02.age, B03.age, B04.age, B05.age, B07.age, B09.age, B10.age, B11.age, B12.age, B13.age, B14.age, B15.age, B16.age, B17.age, B18.age))

#Age temporal plot
B.age.plot <- ggplot(data = B.age,
                     aes(x = Year, y = Proportion, group = Age)) +
  geom_line(colour = "#000099") + 
  geom_point(colour = "#000099") + 
  geom_ribbon(aes(ymin = B.age$CI_Prop_low, ymax = B.age$CI_Prop_upp), alpha = 0.2, fill = "#000099") +
  facet_wrap(facets = ~Age) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("BRFSS survey cycles (years)") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18))
ggsave("B.age.temp.png", width = 18, height = 11)
  
  
#Sex-specific temporal plot of rdd from 1996 to 2018 (except '06 and '08)

#Pull in csv's
B96.sex <- read.csv("https://www.dropbox.com/s/3obhkiuunnr10h9/B96_Arthritis.sex.csv?dl=1")
B97.sex <- read.csv("https://www.dropbox.com/s/grvw4wheefegfnn/B97_Arthritis.sex.csv?dl=1")
B98.sex <- read.csv("https://www.dropbox.com/s/8xr2bv18e02uh9i/B98_Arthritis.sex.csv?dl=1")
B99.sex <- read.csv("https://www.dropbox.com/s/kj6lvqgtmuxtr2v/B99_Arthritis.sex.csv?dl=1")
B00.sex <- read.csv("https://www.dropbox.com/s/jn1xmsg8um77uri/B00_Arthritis.sex.csv?dl=1")
B01.sex <- read.csv("https://www.dropbox.com/s/cxw2yyz6d4t7ti7/B01_Arthritis.sex.csv?dl=1")
B02.sex <- read.csv("https://www.dropbox.com/s/36pfrlv1hb7wv9r/B02_Arthritis.sex.csv?dl=1")
B03.sex <- read.csv("https://www.dropbox.com/s/37siomlkc3m2wfj/B03_Arthritis.sex.csv?dl=1")
B04.sex <- read.csv("https://www.dropbox.com/s/h9pizcop2n5lncl/B04_Arthritis.sex.csv?dl=1")
B05.sex <- read.csv("https://www.dropbox.com/s/ahldmg46ufstt8y/B05_Arthritis.sex.csv?dl=1")
B07.sex <- read.csv("https://www.dropbox.com/s/n4a5ybpvq5nkluc/B07_Arthritis.sex.csv?dl=1")
B09.sex <- read.csv("https://www.dropbox.com/s/8k4kuldy5b0dcdq/B09_Arthritis.sex.csv?dl=1")
B10.sex <- read.csv("https://www.dropbox.com/s/upr4sn96542gzr7/B10_Arthritis.sex.csv?dl=1")
B11.sex <- read.csv("https://www.dropbox.com/s/yk7zykxyo2fa7e7/B11_Arthritis.sex.csv?dl=1")
B12.sex <- read.csv("https://www.dropbox.com/s/pqx704rp3l40f34/B12_Arthritis.sex.csv?dl=1")
B13.sex <- read.csv("https://www.dropbox.com/s/c411gwm8eesv6y6/B13_Arthritis.sex.csv?dl=1")
B14.sex <- read.csv("https://www.dropbox.com/s/tztauktykn0ms61/B14_Arthritis.sex.csv?dl=1")
B15.sex <- read.csv("https://www.dropbox.com/s/0hcbnahkxabp15a/B15_Arthritis.sex.csv?dl=1")
B16.sex <- read.csv("https://www.dropbox.com/s/899oldn03jfq4bu/B16_Arthritis.sex.csv?dl=1")
B17.sex <- read.csv("https://www.dropbox.com/s/x5bdzgrs2r0nfv2/B17_Arthritis.sex.csv?dl=1")
B18.sex <- read.csv("https://www.dropbox.com/s/jtt0e6bcbbpaf4h/B18_Arthritis.sex.csv?dl=1")

#Add year to each data file
B96.sex$Year <- c("1996", "1996")
B97.sex$Year <- c("1997", "1997")
B98.sex$Year <- c("1998", "1998")
B99.sex$Year <- c("1999", "1999")
B00.sex$Year <- c("2000", "2000")
B01.sex$Year <- c("2001", "2001")
B02.sex$Year <- c("2002", "2002")
B03.sex$Year <- c("2003", "2003")
B04.sex$Year <- c("2004", "2004")
B05.sex$Year <- c("2005", "2005")
B07.sex$Year <- c("2007", "2007")
B09.sex$Year <- c("2009", "2009")
B10.sex$Year <- c("2010", "2010")
B11.sex$Year <- c("2011", "2011")
B12.sex$Year <- c("2012", "2012")
B13.sex$Year <- c("2013", "2013")
B14.sex$Year <- c("2014", "2014")
B15.sex$Year <- c("2015", "2015")
B16.sex$Year <- c("2016", "2016")
B17.sex$Year <- c("2017", "2017")
B18.sex$Year <- c("2018", "2018")

#Merge
B.sex <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B96.sex, B97.sex, B98.sex, B99.sex, B00.sex, B01.sex, B02.sex, B03.sex, B04.sex, B05.sex, B07.sex, B09.sex, B10.sex, B11.sex, B12.sex, B13.sex, B14.sex, B15.sex, B16.sex, B17.sex, B18.sex))

#Sex temporal plot
B.sex.plot <- ggplot(data = B.sex,
                     aes(x = Year, y = Proportion, group = Sex)) +
  geom_line(colour = "#000099") + 
  geom_point(colour = "#000099") + 
  geom_ribbon(aes(ymin = B.sex$CI_Prop_low, ymax = B.sex$CI_Prop_upp), alpha = 0.2, fill = "#000099") +
  facet_wrap(facets = ~Sex) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("BRFSS survey cycles (years)") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18))
ggsave("B.sex.temp.png", width = 18, height = 11)


#Employment-specific temporal plot of rdd from 1996 to 2018 (except '06 and '08)

#Pull in csv's
B96.emp <- read.csv("https://www.dropbox.com/s/43qqdb0ny7m129x/B96_Arthritis.employment.csv?dl=1")
B97.emp <- read.csv("https://www.dropbox.com/s/tx4d253dh3hecm9/B97_Arthritis.employment.csv?dl=1")
B98.emp <- read.csv("https://www.dropbox.com/s/k1ob2plv5b1zlbi/B98_Arthritis.employment.csv?dl=1")
B99.emp <- read.csv("https://www.dropbox.com/s/b9gmhdgx3xtqofp/B99_Arthritis.employment.csv?dl=1")
B00.emp <- read.csv("https://www.dropbox.com/s/num4oiyxzeqkmx6/B00_Arthritis.employment.csv?dl=1")
B01.emp <- read.csv("https://www.dropbox.com/s/uohzhp63t3b572c/B01_Arthritis.employment.csv?dl=1")
B02.emp <- read.csv("https://www.dropbox.com/s/lvwxgp76tbdb45c/B02_Arthritis.employment.csv?dl=1")
B03.emp <- read.csv("https://www.dropbox.com/s/mp6k5bo7qxv2o9e/B03_Arthritis.employment.csv?dl=1")
B04.emp <- read.csv("https://www.dropbox.com/s/jm17e474yeo9wdw/B04_Arthritis.employment.csv?dl=1")
B05.emp <- read.csv("https://www.dropbox.com/s/0437sq5ktyq06r3/B05_Arthritis.employment.csv?dl=1")
B07.emp <- read.csv("https://www.dropbox.com/s/rk0qsf69862tszy/B07_Arthritis.employment.csv?dl=1")
B09.emp <- read.csv("https://www.dropbox.com/s/frjgqb5e7w7hv4n/B09_Arthritis.employment.csv?dl=1")
B10.emp <- read.csv("https://www.dropbox.com/s/k4cjkwawl1h26b4/B10_Arthritis.employment.csv?dl=1")
B11.emp <- read.csv("https://www.dropbox.com/s/h9s8h0f6txuxd5t/B11_Arthritis.employment.csv?dl=1")
B12.emp <- read.csv("https://www.dropbox.com/s/tzqi44837zemy32/B12_Arthritis.employment.csv?dl=1")
B13.emp <- read.csv("https://www.dropbox.com/s/aem68fijl80j85e/B13_Arthritis.employment.csv?dl=1")
B14.emp <- read.csv("https://www.dropbox.com/s/u6903b0oxsdy3ml/B14_Arthritis.employment.csv?dl=1")
B15.emp <- read.csv("https://www.dropbox.com/s/3hc0wrxb4wqiu4t/B15_Arthritis.employment.csv?dl=1")
B16.emp <- read.csv("https://www.dropbox.com/s/5nj0hes38v7ox00/B16_Arthritis.employment.csv?dl=1")
B17.emp <- read.csv("https://www.dropbox.com/s/jz8u4frw7ol4b3b/B17_Arthritis.employment.csv?dl=1")
B18.emp <- read.csv("https://www.dropbox.com/s/pc8wzlyckryhmpp/B18_Arthritis.employment.csv?dl=1")


#Add years
B96.emp$Year <- c("1996", "1996", "1996", "1996", "1996", "1996", "1996")
B97.emp$Year <- c("1997", "1997", "1997", "1997", "1997", "1997", "1997")
B98.emp$Year <- c("1998", "1998", "1998", "1998", "1998", "1998", "1998")
B99.emp$Year <- c("1999", "1999", "1999", "1999", "1999", "1999", "1999")
B00.emp$Year <- c("2000", "2000", "2000", "2000", "2000", "2000", "2000")
B01.emp$Year <- c("2001", "2001", "2001", "2001", "2001", "2001", "2001")
B02.emp$Year <- c("2002", "2002", "2002", "2002", "2002", "2002", "2002")
B03.emp$Year <- c("2003", "2003", "2003", "2003", "2003", "2003", "2003")
B04.emp$Year <- c("2004", "2004", "2004", "2004", "2004", "2004", "2004")
B05.emp$Year <- c("2005", "2005", "2005", "2005", "2005", "2005", "2005")
B07.emp$Year <- c("2007", "2007", "2007", "2007", "2007", "2007", "2007")
B09.emp$Year <- c("2009", "2009", "2009", "2009", "2009", "2009", "2009")
B10.emp$Year <- c("2010", "2010", "2010", "2010", "2010", "2010", "2010")
B11.emp$Year <- c("2011", "2011", "2011", "2011", "2011", "2011", "2011")
B12.emp$Year <- c("2012", "2012", "2012", "2012", "2012", "2012", "2012")
B13.emp$Year <- c("2013", "2013", "2013", "2013", "2013", "2013", "2013")
B14.emp$Year <- c("2014", "2014", "2014", "2014", "2014", "2014", "2014")
B15.emp$Year <- c("2015", "2015", "2015", "2015", "2015", "2015", "2015")
B16.emp$Year <- c("2016", "2016", "2016", "2016", "2016", "2016", "2016")
B17.emp$Year <- c("2017", "2017", "2017", "2017", "2017", "2017", "2017")
B18.emp$Year <- c("2018", "2018", "2018", "2018", "2018", "2018", "2018")


#Merge
B.emp <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B96.emp, B97.emp, B98.emp, B99.emp, B00.emp, B01.emp, B02.emp, B03.emp, B04.emp, B05.emp, B07.emp, B09.emp, B10.emp, B11.emp, B12.emp, B13.emp, B14.emp, B15.emp, B16.emp, B17.emp, B18.emp))

#Sex temporal plot
B.emp.plot <- ggplot(data = B.emp,
                     aes(x = Year, y = Proportion, group = Employment)) +
  geom_line(colour = "#000099") + 
  geom_point(colour = "#000099") + 
  geom_ribbon(aes(ymin = B.emp$CI_Prop_low, ymax = B.emp$CI_Prop_upp), alpha = 0.2, fill = "#000099") +
  facet_wrap(facets = ~Employment) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("BRFSS survey cycles (years)") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 18))
ggsave("B.emp.temp.png", width = 18, height = 11)


#Figures for Chapter 5: Demographic Trends

#Age-specific trends
B.agedem.plot <- ggplot(data = B.age,
                        aes(x = Age, y = Proportion)) +
  geom_bar(stat = "identity", fill = "#000099") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE), colour = "black", size = 0.4, width = 0.3, position = position_dodge(0.9)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age group (years)") +
  facet_wrap(facets = ~Year) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16)) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100))
ggsave("B.age.demo.png", width = 18, height = 12)


#Sex-specific trends
B.sexdem.plot <- ggplot(data = B.sex,
                        aes(x = Sex, y = Proportion)) +
  geom_bar(stat = "identity", fill = "#000099") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE), colour = "black", size = 0.4, width = 0.3, position = position_dodge(0.9)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Sex)") +
  facet_wrap(facets = ~Year) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16)) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100))
ggsave("B.sex.demo.png", width = 18, height = 12)

#Spatial figures are done within each annual script (2001, 2003, 2005, 2007, 2009, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)