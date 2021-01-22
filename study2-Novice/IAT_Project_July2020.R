##IAT stuff

#some libraries 
library(tidyverse)
install.packages("devtools")
install.packages("stringr")
devtools::install_github("iatgen/iatgen")
library(iatgen)



library(readr)
#change the destination to the new data 
Annotators_Love_Mohammad_2 <- read_csv("~/Downloads/Annotators_Love_Mohammad 2.csv")

iatnames = c("Q1.RP1" , "Q2.RP2" , "Q3.RP3" , "Q4.RP4" , "Q5.RP5" ,"Q6.RP6" , "Q7.RP7" , "Q8.RN1" ,
"Q9.RN2"   , "Q10.RN3", "Q11.RN4", "Q12.RN5", "Q13.RN6","Q14.RN7", "Q15.LP1", "Q16.LP2",
"Q17.LP3"  , "Q18.LP4", "Q19.LP5", "Q20.LP6", "Q21.LP7","Q22.LN1", "Q23.LN2", "Q24.LN3",
"Q25.LN4"  , "Q26.LN5", "Q27.LN6", "Q28.LN7")

#Black|Communist|Gay|Jewish|Liberal|Mexican|Muslim|Woman#

oldnames_black = paste0("Q",c(267:294))
oldnames_communist = paste0("Q",c(211:238))
oldnames_gay = paste0("Q",c(295:322))
oldnames_jewish = paste0("Q",c(323:350))
oldnames_Liberal = paste0("Q",c(239:266))
oldnames_mexican = paste0("Q",c(155:182))
oldnames_muslim = paste0("Q",c(183:210))
oldnames_woman = paste0("Q",c(351:378))

newnames = iatnames

#we need to do a separate analysis for each condition, n ~ 150
subset_black = Annotators_Love_Mohammad_2%>%
  dplyr::filter(SGT == "Black")%>%
  dplyr::rename_at(vars(oldnames_black), ~ newnames)%>%
  dplyr::select(V1:supervisingjob,newnames)

subset_communist = Annotators_Love_Mohammad_2%>%
  dplyr::filter(SGT == "Communist")%>%
  dplyr::rename_at(vars(oldnames_communist), ~ newnames)%>%
  dplyr::select(V1:supervisingjob,newnames)

subset_gay = Annotators_Love_Mohammad_2%>%
  dplyr::filter(SGT == "Gay")%>%
  dplyr::rename_at(vars(oldnames_gay), ~ newnames)%>%
  dplyr::select(V1:supervisingjob,newnames)

subset_jewish = Annotators_Love_Mohammad_2%>%
  dplyr::filter(SGT == "Jewish")%>%
  dplyr::rename_at(vars(oldnames_jewish), ~ newnames)%>%
  dplyr::select(V1:supervisingjob,newnames)


subset_liberal = Annotators_Love_Mohammad_2%>%
  dplyr::filter(SGT == "Liberal")%>%
  dplyr::rename_at(vars(oldnames_Liberal), ~ newnames)%>%
  dplyr::select(V1:supervisingjob,newnames)


subset_mexican = Annotators_Love_Mohammad_2%>%
  dplyr::filter(SGT == "Mexican")%>%
  dplyr::rename_at(vars(oldnames_mexican), ~ newnames)%>%
  dplyr::select(V1:supervisingjob,newnames)

subset_muslim = Annotators_Love_Mohammad_2%>%
  dplyr::filter(SGT == "Muslim")%>%
  dplyr::rename_at(vars(oldnames_muslim), ~ newnames)%>%
  dplyr::select(V1:supervisingjob,newnames)


subset_woman = Annotators_Love_Mohammad_2%>%
  dplyr::filter(SGT == "Woman")%>%
  dplyr::rename_at(vars(oldnames_woman), ~ newnames)%>%
  dplyr::select(V1:supervisingjob,newnames)

#make a big data including all rows and variables of interest 
dat = dplyr::bind_rows(subset_black,subset_communist,subset_gay,subset_jewish,
                           subset_liberal,subset_mexican,subset_muslim,subset_woman)%>%
  dplyr::filter(is.na(incorrectDevice_msg) == T)%>%
  dplyr::filter(robot == 1)%>%
  dplyr::filter(liberty == 33)%>%
  dplyr::filter(Test_1 == 1 & Test_2 == 1 & Test_3 == 0)%>%
  dplyr::mutate(SD_immigrant_tot = SD_immigrant_1 + SD_immigrant_2 + SD_immigrant_3 +SD_immigrant_4,
                SD_muslim_tot = SD_muslim_1 + SD_muslim_2 + SD_muslim_3 + SD_muslim_4,
                SD_communist_tot = SD_communist_1 + SD_communist_2 + SD_communist_3 + SD_communist_4,
                SD_liberal_tot = SD_liberal_1 + SD_liberal_2 + SD_liberal_3 + SD_liberal_4,
                SD_black_tot = SD_black_1 + SD_black_2 + SD_black_3 + SD_black_4,
                SD_gay_tot = SD_gay_1 + SD_gay_2 + SD_gay_3 + SD_gay_4,
                SD_jewish_tot = SD_jewish_1 + SD_jewish_2 + SD_jewish_3 + SD_jewish_4,
                SD_woman_tot = SD_woman_1 + SD_woman_2 + SD_woman_3 + SD_woman_4,
                hate_immigrant_tot = Immigrant_1+Immigrant_2+Immigrant_3+Immigrant_4+Immigrant_5+Immigrant_6+Immigrant_7,
                hate_muslim_tot = Muslim_1+Muslim_2+Muslim_3+Muslim_4+Muslim_5+Muslim_6+Muslim_7,
                hate_communist_tot = Communist_1+Communist_2+Communist_3+Communist_4+Communist_5+Communist_6+Communist_7,
                hate_liberal_tot = Liberal_1+Liberal_2+Liberal_3+Liberal_4+Liberal_5+Liberal_6+Liberal_7,
                hate_black_tot = Black_1+Black_2+Black_3+Black_4+Black_5+Black_6+Black_7,
                hate_gay_tot = Gay_1+Gay_2+Gay_3+Gay_4+Gay_5+Gay_6+Gay_7,
                hate_jewish_tot = Jew_1+Jew_2+Jew_3+Jew_4+Jew_5+Jew_6+Jew_7,
                hate_woman_tot = Woman_1+Woman_2+Woman_3+Woman_4+Woman_5+Woman_6+Woman_7,
                conservatism_tot = (conservatism+republican)/2,
                SES_tot = (scale(annualincome)+scale(socialclass)+scale(job_1)+scale(job_2)+scale(job_3)+scale(supervisingjob))/6,
                race_white = ifelse(is.na(race_1)==1,0,1))%>%
  dplyr::mutate(hate_tot = hate_immigrant_tot+hate_muslim_tot+hate_communist_tot+hate_liberal_tot+
                  hate_black_tot+hate_gay_tot+hate_jewish_tot+hate_woman_tot)%>%
  dplyr::select(SGT,ends_with("tot"),age,sex,sexualorientation,religion,religiosity_1,race_white,starts_with("Q"))



### Collapse  IAT critical blocks  down ####
dat$compatible.crit <- combineIATfourblocks(dat$Q4.RP4, dat$Q18.LP4, dat$Q14.RN7, dat$Q28.LN7)
dat$incompatible.crit <- combineIATfourblocks(dat$Q7.RP7, dat$Q21.LP7, dat$Q11.RN4, dat$Q25.LN4)

### Collapse  IAT practice blocks ####
dat$compatible.prac <- combineIATfourblocks(dat$Q3.RP3, dat$Q17.LP3, dat$Q13.RN6, dat$Q27.LN6)
dat$incompatible.prac <- combineIATfourblocks(dat$Q6.RP6, dat$Q20.LP6, dat$Q10.RN3, dat$Q24.LN3)

### Clean the IAT ### 
## D-BUILT.IN: USE THIS IF FORCED ERROR CORRECTION WAS ENABLED PER GREENWALD ET AL 2003
clean <- cleanIAT(dat$compatible.prac, dat$compatible.crit, dat$incompatible.prac, dat$incompatible.crit, error.penalty = FALSE)

### NUMBER OF PARTICIPANTS WHO COMPLETED THE IAT ###
sum(!clean$skipped)

### TIMEOUT DROP RATE (% of TRIALS) ###
clean$timeout.rate

### LOWER TAIL DROP RATE (% of TRIALS) ###
# NOTE: DEFAULT DATA CLEANING PROCEDURE DOES NOT DROP FAST TRIALS (ONLY FAST PARTICIPANTS) SO THIS WILL BE ZERO
# UNLESS THAT FEATURES IS TURNED ON
clean$fasttrial.rate

### FAST PARTICIPANT 'BUTTON MASHER' DROP COUNT AND RATE (% of SAMPLE) ###
clean$fastprt.count
clean$fastprt.rate

### ERROR RATE ###
clean$error.rate

### RELIABILITY ANALYSIS ###
IATreliability(clean)$reliability

### SCORING: POSITIVE SCORE FAVORS WHICHEVER BLOCKS WERE ENTERED FIRST INTO CLEANER - AKA COMPATIBLE ###
dscore <- clean$D
mean(clean$D, na.rm=T)
sd(clean$D, na.rm=T)
t.test(clean$D)
t.test(clean$D)$p.value
t.test(clean$D)$conf.int[1:2]
mean(clean$D, na.rm=T) / sd(clean$D, na.rm=T)  


#add dscore to the "dat"
dat$D_score = dscore

dat = dat%>%dplyr::select(-starts_with("Q"))

####now you have a clean data with IAT scores
####start stats 


  
  

  







  
  
  
  

