#0.These libraries are used throughout the script, good to enable them from the beginning:
#========================================================================================
library(dplyr)
library(ggplot2)
library(devtools)

#install some required packages with devtools:
source_url("https://raw.githubusercontent.com/hdg204/UKBB/main/UKBB_Health_Records_Public.R") 
source_url("https://raw.githubusercontent.com/ID690016874/HPDM042/main/find-read-codes/find_read_codes.R") 
source_url("https://raw.githubusercontent.com/hdg204/Rdna-nexus/main/install.R")

#1A. Get read 2/3 codes and descriptions for CRC symptoms
#=======================================================
#import DISCO read codes for CRC symptoms, and lookup tables for all read codes
lkp2 <- read.csv('read2_lkp.csv')
lkp3 <- read.csv('read3_lkp.csv')
disco <- read.csv('1b_DISCO_crc_symptoms.csv')

#reformat DISCO read codes to 5 characters, same as UKBB read code format:
#-------------------------------------------------------------------------
for (i in 1:nrow(disco)) {disco$readcode[i] <- substr(disco$readcode[i], 1, 5)}
for (i in 1:nrow(disco)) {
  if (nchar(disco$readcode[i]) == 4) {disco$readcode[i] <- paste(disco$readcode[i],'.',sep='')}
}
#check how many unique DISCO read codes there are now they've been cut to 5 characters
length(unique(disco$readcode)) #=168

#find read codes in the lookup table which match or start with the DISCO read codes:
#-----------------------------------------------------------------------------------
find_all_symptom_codes <- find_read_codes(unique(disco$readcode)) #returns 255 read codes

#after looking through above table, the following codes do not appear relevant to CRC, remove:
sym_codes_filtered <- find_all_symptom_codes[!(find_all_symptom_codes$code %in% c('1961.','1964.','1966.','196A.',
                                                                                  '25C1.','25D1.','25E1.','X772q','44TB.',
                                                                                  '25F1.','2I18.','2I180','44TB0','44TB1',
                                                                                  '479..','4791.','4792.','4795.','479Z.',
                                                                                  'D00y.','D00y0','D00z0','D00z1','D00z2','E2780',
                                                                                  'Eu50y','J680.','R0903','R090B','R090G','R0939',
                                                                                  'XaCIU','XabrE','XabrF','XaNxS')),]

#remove any duplicates:
sym_codes_filtered <- sym_codes_filtered[!duplicated(sym_codes_filtered$code),]


#import list of symptom codes for rectal bleeding found by Matt Barclay @ UCL:
#----------------------------------------------------------------------------
ucl_codes_rb <- read.csv('rectal_bleeding_comparison.csv')
ucl_codes_rb <- ucl_codes_rb[ucl_codes_rb$code_source == 'Matt',]

#each read code has 2 entries, one with a 'count' value (how many times recorded)
#for 2010 and the other for 2015. These aren't as relevant for this analysis, so
#sum/combine the different counts and then remove duplicated entries:
ucl_codes_rb <- ucl_codes_rb %>% group_by(read) %>% mutate(total_count = sum(count)) %>%
  select(!(c(code_source,year,count))) %>% distinct()

#add term descriptions to the UCL codes:
#-----------------------------------------
#run find_read_codes function again to simultaneously expand code list and add term descriptions
find_ucl_codes <- find_read_codes(unique(ucl_codes_rb$read))
ucl_codes_rb <- ucl_codes_rb %>% rename(code=read) %>% full_join(find_ucl_codes)
#remove irrelevant codes
ucl_codes_rb_filtered <- ucl_codes_rb[!(ucl_codes_rb$code %in% c('XaPpf','XaJYy',
                                                                 'J680.','.I781',
                                                                 'T4762','8I78.',
                                                                 '4I78.')),]

#join all read codes from the 3 sources into one dataframe
#---------------------------------------------------------
sym_codes_filtered <- full_join(sym_codes_filtered, ucl_codes_rb_filtered[,c(1,3:4)])
#remove duplicates
sym_codes_filtered <- sym_codes_filtered[!duplicated(sym_codes_filtered$code),]

#1B. count the number of read codes per each type of symptom:
#============================================================
#get filtered symptom codes which originally came from DISCO (the symptom 'type' for
#each of these read codes - e.g. abdominal pain, weightloss - is already filled in):
#-----------------------------------------------------------------------------------
disco_type <- disco[!duplicated(disco$readcode), -2]
disco_type <- disco_type[disco_type$readcode %in% sym_codes_filtered$code,]
#add new symptom type (abdominal rigidity):
disco_type <- dplyr::mutate(disco_type, 'rigidity' = rep(NA, nrow(disco_type)))

#get filtered symptom codes which didn't originally come from DISCO:
#-------------------------------------------------------------------
not_disco <- sym_codes_filtered$code[!(sym_codes_filtered$code %in% disco$readcode)]

#make a dataframe to fill in with symptom type for each code:
not_disco_type <- data.frame(disco_type[1:length(not_disco),])
not_disco_type$readcode <- not_disco
not_disco_type[,-1] <- NA

#sort all symptoms which didn't come from DISCO consortium into types:
#---------------------------------------------------------------------
not_disco_desc <- sym_codes_filtered[sym_codes_filtered$code %in% not_disco_type$readcode,]
#abdominal pain
abp <- c('1965.','1967.','1974.','25D5.','25D7.','R090D','Xa7xW','Xa83s','XaBDO',
         'XaBDQ','XaBDR','XaD2x','XaD2y','XaD2z','XaY2H','25E2.','25E3.','25E4.',
         '25E5.','25E6.','25E7.','25E8.','25E9.','25EA.')
#rigidity
rig <- c('25F2.','25FZ.')
#abdominal mass incl. pelvic mass
abm <- c('25J1.','25J2.','25J4.','25J6.','25JZ.','25K4.','25L1.','25LZ.','X304O',
         'XaBj4','XaDsr','R0933','R0934','R0935')
#change in bowel habits
cbh <- c('25JA.','XaI94')
#rectal blood loss
rbl <- c('J68z0','J68z2','X30Be','XaFt2','XaIMX','XaJuu','XaJuv','XaYVt',
         ucl_codes_rb_filtered$code[!(ucl_codes_rb_filtered$code %in% sym_codes_filtered_list) &
                                      !duplicated(ucl_codes_rb_filtered$code)])
#appetite change
app <- 'Ua1iv'
#haemoglobin levels
haem <- 'XaBLm' 
#weight loss
wl <- c('XaIu3','XaIxC','XaJM4','XaKwR','XaXTs')
#faecal occult blood test
fob <- c('XaNxT','XaPke')

#fill in symptom type dataframe
#-------------------------------
not_disco_type$weightloss[not_disco_type$readcode %in% wl] <- 1
not_disco_type$loss_appetite[not_disco_type$readcode %in% app] <- 1
not_disco_type$abdo_mass[not_disco_type$readcode %in% abm] <- 1
not_disco_type$abdo_pain[not_disco_type$readcode %in% abp] <- 1
not_disco_type$rectal_bloodloss[not_disco_type$readcode %in% rbl] <- 1
not_disco_type$fob[not_disco_type$readcode %in% fob] <- 1
not_disco_type$change_bowel_habit[not_disco_type$readcode %in% cbh] <- 1
not_disco_type$haemoglobin[not_disco_type$readcode %in% haem] <- 1
not_disco_type$rigidity[not_disco_type$readcode %in% rig] <- 1

#Add all CRC symptom read codes + types from DISCO and not from DISCO into 1 dataframe:
#--------------------------------------------------------------------------------------
symptom_type <- dplyr::filter(disco_type, weightloss==1)
symptom_type <- rbind(symptom_type, dplyr::filter(not_disco_type, weightloss==1),
                      dplyr::filter(disco_type, loss_appetite==1),
                      dplyr::filter(not_disco_type, loss_appetite==1),
                      dplyr::filter(disco_type, iron_def==1),
                      dplyr::filter(disco_type, abdo_mass==1),
                      dplyr::filter(not_disco_type, abdo_mass==1),
                      dplyr::filter(disco_type, abdo_pain==1),
                      dplyr::filter(not_disco_type, abdo_pain==1),
                      dplyr::filter(disco_type, rectal_bloodloss==1),
                      dplyr::filter(not_disco_type, rectal_bloodloss==1),
                      dplyr::filter(disco_type, fob==1),
                      dplyr::filter(not_disco_type, fob==1),
                      dplyr::filter(disco_type, fob==1),
                      dplyr::filter(not_disco_type, fob==1),
                      dplyr::filter(disco_type, change_bowel_habit==1),
                      dplyr::filter(not_disco_type, change_bowel_habit==1),
                      dplyr::filter(disco_type, haemoglobin==1),
                      dplyr::filter(not_disco_type, haemoglobin==1),
                      dplyr::filter(disco_type, rigidity==1),
                      dplyr::filter(not_disco_type, rigidity==1))
#remove any duplicates:
symptom_type <- symptom_type[!duplicated(symptom_type$readcode),]

#2A. Find UKBB participants with symptoms
#=======================================
p_sym <- read_GP(sym_codes_filtered$code)
#number each symptom record, as there are multiple records per participant
p_sym$no <- 1:nrow(p_sym)

#2B. Any participants with a read code for haemoglobin level were included
#Exclude participants with normal haemoglobin level or no measurement
#======================================================================
#df of participants with haemoglobin level recorded:
exclude <- p_sym[p_sym$read_2 %in% c('44TC.','XaBLm') | p_sym$read_3 %in% c('44TC.','XaBLm'),]

#import a table including participant IDs + all the lifestyle variables/chacteristics assessed in this study
#table was generated on the DNA Nexus UKBB research analysis platform
UKBB_var <- read.csv('00_participant.csv') 

#use the above participant table to add participant sex to haemoglobin df:
exclude <- left_join(exclude, UKBB_var[,c(1,5)]) #(sex was in column 5 of the table)

#find participants with low haemoglobin levels ('low' is sex-dependent)
#also only include participants with haemoglobin > 1 as < 1 may be a percentage:
exclude$value1 <- as.numeric(exclude$value1) #convert haemoglobin measure to numeric
include <- exclude[(exclude$p31 == 'Female' & exclude$value1 < 11 & exclude$value1 > 1),]
include <- rbind(include, exclude[(exclude$p31 == 'Male' & exclude$value1 < 13 & exclude$value1 > 1),])
include <- include[!is.na(include$eid),]
#remove any participants with a % symbol in the 'value3' column as these are likely to be percentage measurements
include <- include[!(include$value3 == '%'),]

#remove participant symptom records from p_sym, if the 'symptom' was haemoglobin level that is either healthy or not recorded:
exclude <- exclude[!(exclude$no %in% include$no),]
p_sym <- p_sym[!(p_sym$no %in% exclude$no),]

#2C. Exclude any symptoms which occurred before participants were 40 or 50
#==================================================================
#add date of birth for all participants:
UKBB_var$dob <- 15 #actual DoB unknown for privacy reasons. assume 15th of each month to minimise error
UKBB_var$dob <- apply(UKBB_var[,c(22,3,4)], 1, paste, collapse = "-") #col 3 in UKBB_var table is birth month, col 4 is birth year, col 22 is the new 'dob' column added in the above line
UKBB_var$dob <- as.Date(UKBB_var$dob, format = "%d-%B-%Y")
p_sym <- left_join(p_sym, UKBB_var[,c(1,22)])

#add age at which symptom occurred
library(lubridate)
p_sym$sym_age <- lubridate::time_length(difftime(p_sym$event_dt,
                                                 p_sym$dob),
                                        "years")
p_sym$sym_age <- as.integer(p_sym$sym_age)  

#version 1: exclude symptoms which occurred before participants were 50:
p_sym_50 <- p_sym[p_sym$sym_age >= 50,]
#version 2: exclude symptoms which occurred before participants were 40:
p_sym_40 <- p_sym[p_sym$sym_age >= 40,]

#2D. Include only symptom with earliest date for each patient
#===========================================================
get_earliest_symptom <- function(p_sym_n) {
  p_sym_filtered <- p_sym_n
  p_sym_filtered[,4:5][p_sym_filtered[,4:5] == ''] <- NA #cols 4 and 5 of the participant symptom records table contain read 2 and 3 codes
  p_sym_filtered <- p_sym_filtered %>%
    group_by(eid) %>%
    #get earliest symptom for each patient
    slice_min(event_dt) %>%
    #remove duplicate records (where same symptoms appears multiple times on same date)
    distinct(eid, read_2, read_3, .keep_all = TRUE) 
  p_sym_filtered <- p_sym_filtered[,-9]
  
  return(p_sym_filtered)
}

p_sym_filtered_00 <- get_earliest_symptom(p_sym) #no age threshold
p_sym_filtered_40 <- get_earliest_symptom(p_sym_40) #age threshold for earliest symptom = 40
p_sym_filtered_50 <- get_earliest_symptom(p_sym_50) #age threshold for earliest symptom = 50

#3A. For symptomatic participants, find earliest occurrence of CRC
#================================================================
#get read 2/3 codes and descriptions for CRC
crc_codes_gp <- find_read_codes(c('B13','B14','B575','B1z','B803','B804','B902','BB5N'))
crc_codes_gp_filtered <- crc_codes_gp[! crc_codes_gp$code %in% c('B1z..','B1z0.','B1zy.','B1zz.','B902.','B9020','B902z'),]

#list ICD10 cancer registry codes for CRC
crc_codes_icd10_filtered <- c('C18','C19','C20','C21')

#find earliest occurrence of CRC for each patient
#below function adapted from Harry Green's function first_occurence
first_occurence_age <- function(ICD10='',GP='',OPCS='',cancer='',
                                age_threshold=0, p=p_sym_filtered_00) {
  ICD10_records=read_ICD10(ICD10)%>%mutate(date=epistart)%>%select(eid,date)%>%mutate(source='HES')
  OPCS_records=read_OPCS(OPCS)%>%mutate(date=opdate)%>%select(eid,date)%>%mutate(source='OPCS')
  GP_records=read_GP(GP)%>%mutate(date=event_dt)%>%select(eid,date)%>%mutate(source='GP')
  cancer_records=read_cancer(cancer)%>%select(eid,date)%>%mutate(source='Cancer_Registry')
  all_records=rbind(ICD10_records,OPCS_records)%>%rbind(GP_records)%>%rbind(cancer_records)%>%mutate(date=as.Date(date))
  all_records=left_join(all_records,p[,c(1,9)])
  all_records$crc_age=lubridate::time_length(difftime(all_records$date, all_records$dob), "years")
  all_records=filter(all_records, crc_age >= age_threshold)
  all_records=all_records%>%group_by(eid)%>%top_n(-1,date)%>%distinct()
  return(all_records)
}

crc_00 <- first_occurence_age(ICD10 = crc_codes_icd10_filtered,
                              GP = crc_codes_gp_filtered$code,
                              cancer = crc_codes_icd10_filtered)
crc_40 <- first_occurence_age(ICD10 = crc_codes_icd10_filtered,
                              GP = crc_codes_gp_filtered$code,
                              cancer = crc_codes_icd10_filtered,
                              age_threshold = 40,
                              p = p_sym_filtered_40)
crc_50 <- first_occurence_age(ICD10 = crc_codes_icd10_filtered,
                              GP = crc_codes_gp_filtered$code,
                              cancer = crc_codes_icd10_filtered,
                              age_threshold = 50,
                              p = p_sym_filtered_50)

#3B. Add crc data to participants table
#======================================
#required files:
death <- read.csv('death_death.csv') #date of death - only columns used for analysis were participant ID and date of death formatted: year-month-day
death_cause <- read.csv('death_death_cause.csv') #cause of death - only columns used were participant ID and 'cause_icd10' which contains: ICD10code<space>description of code

#split death_cause column of causes into ICD10 code and description
library(stringr)
death_cause[c('code', 'cause')] <- str_split_fixed(death_cause$cause_icd10, ' ', 2)
#reformat codes into 3 characters to match other ICD10 codes
for (i in 1:nrow(death_cause)) {death_cause$code[i] <- substr(death_cause$code[i], 1, 3)}
#add date of death
death_cause <- left_join(death_cause,death[,c(2,6)]) #col 2 = ID, 6 = date of death

#filter both tables to only include symptomatic participants:
library(dplyr)
death_00 <- filter(death, eid %in% p_sym_filtered_00$eid)
death_40 <- filter(death, eid %in% p_sym_filtered_40$eid)
death_50 <- filter(death, eid %in% p_sym_filtered_50$eid)
death_cause_00 <- filter(death_cause, eid %in% p_sym_filtered_00$eid)
death_cause_40 <- filter(death_cause, eid %in% p_sym_filtered_40$eid)
death_cause_50 <- filter(death_cause, eid %in% p_sym_filtered_50$eid)

#a function to add all the CRC data about participants to a dataframe 'p':
add_crc <- function(p_sym_filtered_xx, crc_xx, death_cause_xx) { 
  #create a table including id, DoB, symptom date and symptom age:
  p <- p_sym_filtered_xx[,c(1,9,3,10)]
  
  #add CRC date, source of info (e.g. GP, cancer registry) and age at diagnosis, and rename some stuff:
  p <- left_join(p, crc_xx) %>% rename('sym_date' = event_dt,
                                       'crc_date' = date,
                                       'crc_source' = source)
  
  #add whether participants had CRC listed as cause of death:
  p$crc_death[p$eid %in% death_cause_xx$eid[death_cause_xx$code %in% crc_codes_icd10_filtered]] <- 1
  p$crc_death[is.na(p$crc_death)] <- 0
  #add date and age of death for all participants
  p <- left_join(p, death_cause_xx[,c(2,9)]) %>% rename('death_date' = date_of_death)
  p$death_age <- lubridate::time_length(difftime(p$death_date,p$dob),"years")
  #if participants died from CRC but there is no other record of having CRC, fill in CRC date and age with death date and age:
  cdo <- p$eid[p$crc_death == 1 & is.na(p$crc_date)]
  p$crc_date[p$eid %in% cdo] <- p$death_date[p$eid %in% cdo]
  p$crc_source[p$eid %in% cdo] <- 'Death'
  p$crc_age[p$eid %in% cdo] <- p$death_age[p$eid %in% cdo]
  
  #calculate time between first symptom and CRC in days:
  p$time_diff <- lubridate::time_length(difftime(p$crc_date, p$sym_date),"days")
  #calculate time between first symptom and death in days
  p$time_diff_death <- lubridate::time_length(difftime(p$death_date, p$sym_date),"days")
  
  #determine whether participants are cases or controls
  p$case[p$time_diff > 0 & p$time_diff <= 730] <- '1' #case
  p$case[p$time_diff > 730] <- '0' #control
  p$case[is.na(p$time_diff)] <- '0'
  p$case[p$time_diff <= 0] <- 'exclude' #exclude
  p$case[p$time_diff_death <= 730 & p$crc_death == 0] <- 'exclude'
  
  #reformat some stuff
  p$crc_age <- as.integer(p$crc_age)
  p$death_age <- as.integer(p$death_age)
  
  return(p)
}

#make dataframe of all symptomatic patients, symptom date/age, CRC date/age, death date/age,
#whether death was due to CRC, time between symptom and CRC, and whether case/control/exclude
p_00 <- add_crc(p_sym_filtered_00, crc_00, death_cause_00)
p_40 <- add_crc(p_sym_filtered_40, crc_40, death_cause_40)
p_50 <- add_crc(p_sym_filtered_50, crc_50, death_cause_50)

#3C: Decide which age threshold to use based on density plots comparing age at first symptom in cases/controls
#=============================================================================================================
density0 <- p_00 %>% ggplot() +
  aes(x = sym_age, fill = as.factor(case)) +
  geom_density(alpha = 0.4) + scale_fill_discrete(name="",labels=c("control","case")) +
  xlab('age at first CRC symptom') + geom_vline(xintercept = 40) + geom_vline(xintercept = 50)

density40 <- p_40 %>% ggplot() +
  aes(x = sym_age, fill = as.factor(case)) +
  geom_density(alpha = 0.4) + scale_fill_discrete(name="",labels=c("control","case")) +
  xlab('age at first CRC symptom')

density50 <- p_50 %>% ggplot() +
  aes(x = sym_age, fill = as.factor(case)) +
  geom_density(alpha = 0.4) + scale_fill_discrete(name="",labels=c("control","case")) +
  xlab('age at first CRC symptom')

#going with age threshold 40 because it increases case to control ratio without excluding too many participants

#4A. Add UKBB variables
#=====================
#import some tables of UKBB variables for all UKBB participants:
var <- read.csv('participant_variables.csv') #lifestyle variables and participant characteristics
fh <- read.csv('family_history.csv') #after editing the analysis, I decided to include family history, which is the only reason it's in a different table to other variables

#list of UKBB variable names/codes in the table vs. what I renamed them, for reference:
UKBB_var_names <- list('eid' = 'eid',
                       'age' = 'p21022',
                       'dob_m' = "p52",
                       'dob_y' = "p34",
                       'sex' = "p31",
                       'TDI' = "p189",
                       'BMI' = "p21001_i0",
                       'waist_circumference' = "p48_i0",
                       'ever_smoked' = "p20160_i0",
                       'delete_this' = "p20116_i1", #'delete' variables were included by accident or just not useful
                       'smoking_status' = "p20116_i0",
                       'delete' = "p1239_i0",
                       'delete' = "p3456_i0",
                       'alcohol_intake' = "p1558_i0",
                       'diabetes' = "p2443_i0",
                       'processed_meat_intake' = "p1349_i0",
                       'PC1' = "p22009_a1",
                       'PC3' = "p22009_a3",
                       'PC2' = "p22009_a2",
                       'PC4' = "p22009_a4",
                       'PC5' = "p22009_a5")

#renaming UKBB variables in the dataframe:
colnames(var) <- c('eid','recruitment_age','delete','delete','sex','TDI','BMI',
                   'waist_circumference','ever_smoked','delete_this',
                   'smoking_status','delete','delete','alcohol_intake',
                   'diabetes','processed_meat_intake','PC1','PC3','PC2',
                   'PC4','PC5')
#delete unnecessary variables:
var <- var[,-c(3,4,10,12,13)]

#add variables to the dataframe of participants (the one with all the CRC/symptom info)
add_variables <- function(p_xx) {
  p_xx_v <-  left_join(p_xx, var)
  
  #some variables had 'NA' if participants didn't answer whereas some had e.g.
  #'prefer not to answer'. Change to be compatible:
  p_xx_v$ever_smoked[p_xx_v$ever_smoked == ''] <- NA
  p_xx_v$smoking_status[p_xx_v$smoking_status == 'Prefer not to answer'] <- NA
  p_xx_v$alcohol_intake[p_xx_v$alcohol_intake == 'Prefer not to answer'] <- NA
  p_xx_v$diabetes[p_xx_v$diabetes == 'Prefer not to answer'] <- NA
  p_xx_v$diabetes[p_xx_v$diabetes == 'Do not know'] <- NA
  p_xx_v$processed_meat_intake[p_xx_v$processed_meat_intake == 'Prefer not to answer'] <- NA
  p_xx_v$processed_meat_intake[p_xx_v$processed_meat_intake == 'Do not know'] <- NA
  
  return(p_xx_v)
}

p_40_v <- add_variables(p_40)

#4B. Add family history
#=====================
fh_40 <- filter(fh, eid %in% p_40_v$eid)

add_fh <- function(fh_xx, p_xx_v) {
  fh <- fh_xx
  colnames(fh) <- c('eid','fh_mat','fh_pat')
  #UKBB self-reported family history is formatted as a list of conditions which participants said their parent had. Use grep to find entries containing bowel cancer:
  fh$fh_mat[grep('Bowel cancer', fh$fh_mat)] <- 1
  fh$fh_pat[grep('Bowel cancer', fh$fh_pat)] <- 1
  fh[,2:3][fh[,2:3] != 1] <- 0 #if participant did not report mother or father having bowel cancer, set value to 0
  
  p <- dplyr::left_join(p_xx_v, fh)
  
  p$fh_mat<- as.integer(p$fh_mat)
  p$fh_pat<- as.integer(p$fh_pat)

  #new variable which is 0 if no parents had bowel cancer, or 1, or 2:
  p$fh <- p$fh_mat + p$fh_pat
  
  return(p)
}

p_40_v <- add_fh(fh_40, p_40_v)

#4C. Add which symptom type each participant had
#==========================================
#symptom_type is a dataframe with the category that each read code falls into
#it's either 1 or NA for each category. change it to 1 or 0, then convert to an integer, so it can be used for maths later:

#TODO: insert this later it's messing up github formatting :(

#add which symptom(s) each participant had as a variable:
add_symptoms <- function(p_xx_v, p_sym_filtered_xx) {
  #get read 2 and 3 codes for all participants
  p_st2 <- left_join(p_xx_v[,1], p_sym_filtered_xx) %>%
    distinct(eid, read_2)
  p_st3 <- left_join(p_xx_v[,1], p_sym_filtered_xx) %>%
    distinct(eid, read_3)
  
  #add symptom types to each read code
  p_st2 <- p_st2[!is.na(p_st2$read_2),]
  p_st2 <- rename(p_st2, 'readcode'=read_2)
  p_st2 <- left_join(p_st2, symptom_type)
  
  p_st3 <- p_st3[!is.na(p_st3$read_3),]
  p_st3 <- rename(p_st3, 'readcode'=read_3)
  p_st3 <- left_join(p_st3, symptom_type)
  
  #add together total number of each symptom type for each participant
  p_st <- rbind(p_st2, p_st3)
  p_st <- p_st %>% 
    group_by(eid) %>% 
    summarise(across(2:13, sum))
  
  #if any value in the symptom columns is >1, convert it to 1
  #(don't need to know how many times one symptom type, e.g. weight loss, was
  #recorded for one participant, just that it was recorded)
  p <- left_join(p_xx_v, p_st)
  p[,32:43][p[,32:43] > 1] <- 1
  p[,32:43][is.na(p[,32:43])] <- 0
  
  return(p)}

p_40_v <- add_symptoms(p_40_v, p_sym_filtered_40)
                            
#4D. Add haemoglobin levels
#==========================
haem <- read_GP(c('44TC.','XaBLm'))
haem$value1 <- as.numeric(haem$value1)
haem <- haem[haem$value1 < 20 & haem$value1 > 1,]
haem <- filter(haem, !is.na(value1))

add_haem <- function(p_xx_v) {
  p <- p_xx_v %>%
    left_join(haem[,c(1,3,6)]) %>%
    rename('haem_level'=value1, 'haem_date'=event_dt)
  
  #calculate time difference between haemoglobin measurement and first symptom:
  p$haem_diff <- lubridate::time_length(difftime(p$sym_date, p$haem_date),"days")
  #change all time difference measurements to a positive number
  p$haem_diff <- sqrt(p$haem_diff^2)
  
  #if a participant has multiple haemoglobin measures, only use the one closest
  #to symptom date (also remove some duplicated records that somehow snuck in here)
  p$haem_diff[is.na(p$haem_diff)] <- 0
  p <- p %>% group_by(eid) %>% top_n(-1, haem_diff) %>%
    distinct(across(-crc_source), .keep_all = TRUE) %>% select(!(haem_diff))
  
  return(p)
}

p_40_v <- add_haem(p_40_v)

#NOTE: these haemoglobin measurements were taken an average of 1153.952
#days away from first symptom (3 years). therefore doing something sensible like only
#including haemoglobin levels measured 2 years of either side of symptom date reduces
#participants with haemoglobin levels to only 2 participants.
#Even with this there were so few haemoglobin measurements that they couldn't be meaningfully analysed with logistic regression
#so haemoglobin level was not included as one of the 24 variables in the study. It's in the table anyway because

#4E. reorder columns
#==================
p_40_v <- p_40_v[,c(1,2,14,15,13,3:12,16:23,45,44,29:31,32:43,24:28)]

#4F. exclude participants who don't meet criteria for case/control
#================================================================
p_40_v <- p_40_v[p_40_v$case != 'exclude',]
p_40_v$case <- as.integer(p_40_v$case)

#also exclude 'rigidity' symptom variable as so few participants had rigidity as a symptom:
p_40_v <- p_40_v[,-40]
#at this point I used participant IDs to exclude any participants who only had rigidity as a symptom and no other symptoms
#code not shown here for anonymity reasons

#4G. Convert some categorical variables to factor variables
#========================================================
factorise <- function(p_xx_v) {
    p <- p_xx_v
    p$ever_smoked <- factor(p$ever_smoked)
    p$smoking_status <- factor(p$smoking_status, levels=c("Never","Previous","Current"))
    p$alcohol_intake <- factor(p$alcohol_intake,
                               levels = c('Never','Special occasions only',
                                          'One to three times a month',
                                          'Once or twice a week',
                                          'Three or four times a week',
                                          'Daily or almost daily'))
    p$processed_meat_intake <- factor(p$processed_meat_intake,
                                      levels = c('Never','Less than once a week',
                                                 'Once a week','2-4 times a week',
                                                 '5-6 times a week',
                                                 'Once or more daily'))
    return(p)
}

p_40_v <- factorise(p_40_v)

#5. Split cohort depending on ancestry and relatedness before working out GRS quintiles & other descriptive stats
#=====================================================
#required files:
#==============
eur_unr <- read.table('unrelated_european_ukbb_participants.txt', header = TRUE)
#==============
#split cohort dataframe into 2 depending on ancestry & relatedness:
p_40_vm <- filter(p_40_v, !(eid %in% eur_unr$n_eid)) #mixed ancestry (this will include some white European individuals excluded from the other list due to relatedness)
p_40_ve <- filter(p_40_v, eid %in% eur_unr$n_eid) #white European ancestry, not including individuals related to 1st or 2nd degree

#6A. Generate polygenic risk score (PRS) (in this code the nomenclature used was genetic risk score (GRS))
#====================================
#generate GRS for all UKBB participants using source_url("https://raw.githubusercontent.com/hdg204/Rdna-nexus/main/install.R")
#note this requires a tab-separated table of risk-associated variants, with the following columns: chr, bp, other, effect, weight
#(other and effect refer to the alleles which do or don't impact CRC risk, weight is the beta)
grs <- generate_grs('207_snp_list.tsv') 

#add GRS to participant dataframe:
add_grs <- function(p_xx_v) {
  #add GRS to participants dataframe
  p_grs <- filter(grs$grs, eid %in% p_xx_v$eid)
  p <- left_join(p_xx_v, p_grs)
  
  #calculate z score (like GRS but scaled, so 1 unit increase = 1 standard deviation increase):
  gm <- mean(p$grs, na.rm = T)
  gsd <- sd(p$grs, na.rm = T)
  p$zscore <- (p$grs - gm)/gsd
  
  #divide GRS into quintiles:
  scorequin <- quantile(p$grs, probs = seq(0, 1, 1/5), na.rm = T)
  p <- p %>% mutate(grs_quintile=NA)
  p$grs_quintile[p$grs<scorequin[2]] <- 1
  p$grs_quintile[p$grs<scorequin[3] & p$grs>scorequin[2]] <- 2
  p$grs_quintile[p$grs<scorequin[4] & p$grs>scorequin[3]] <- 3
  p$grs_quintile[p$grs<scorequin[5] & p$grs>scorequin[4]] <- 4
  p$grs_quintile[p$grs>scorequin[5]] <- 5
  
  return(p)
}

p_40_ve <- add_grs(p_40_ve)
p_40_vm <- add_grs(p_40_vm)

#plot GRS distribution
#--------------------
grs_dist <- ggplot(p_40_ve, aes(x=grs, fill=as.factor(case))) + geom_density(alpha = 0.4) +
  xlab('GRS') + scale_fill_discrete(name='',labels = c('control','case')) + scale_x_continuous(n.breaks = 15) +
  geom_vline(xintercept = mean(p_40_ve$grs[p_40_ve$case == 0], na.rm=T), linetype = 2) +
  geom_vline(xintercept = mean(p_40_ve$grs[p_40_ve$case == 1], na.rm=T), linetype = 3)

grs_dist <- ggplot(p_40_vm, aes(x=grs, fill=as.factor(case))) + geom_density(alpha = 0.4) +
  xlab('GRS') + scale_fill_discrete(name='',labels = c('control','case')) + scale_x_continuous(n.breaks = 15) +
  geom_vline(xintercept = mean(p_40_vm$grs[p_40_vm$case == 0], na.rm=T), linetype = 2) +
  geom_vline(xintercept = mean(p_40_vm$grs[p_40_vm$case == 1], na.rm=T), linetype = 3)

#6B. Get CRC incidence rate by GRS quintile
#===========================================
#Takes a binary variable and returns a proportionality test to find the confidence interval. Returns it as a string (adapted from HG's code)
binaryci <- function(binarylist){
  binarylist=binarylist[!is.na(binarylist)]
  test1=100*prop.test(sum(binarylist), length(binarylist), conf.level=0.95)$conf.int[c(1,2)]%>%as.numeric()%>%signif(2)
  test2=100*prop.test(sum(binarylist), length(binarylist), conf.level=0.95)$estimate%>%as.numeric()%>%signif(2)
  return(paste(as.character(test2),'% (',as.character(test1[1]),'%-',as.character(test1[2]),'%)',sep=''))
}

#get incidence rate for top and bottom quintile:
top_quin_inc_40e <- binaryci(p_40_ve$case[p_40_ve$grs_quintile==5])
bottom_quin_inc_40e <- binaryci(p_40_ve$case[p_40_ve$grs_quintile==1])

top_quin_inc_40m <- binaryci(p_40_vm$case[p_40_vm$grs_quintile==5])
bottom_quin_inc_40m <- binaryci(p_40_vm$case[p_40_vm$grs_quintile==1])

#6C. Cox proportional hazards modelling and survival curve
#==========================================================
#IMPORTANT: This section needs to be run on an up-to-date version of R if possible, otherwise package dependencies etc. cause lots of errors.

#Required packages (uncomment to install)
#========================================
#install.packages('survminer')
#install.packages('ggpubr')
library(ggplot2)
library(dplyr)
library(ggpubr)
library(survminer)

#Create dataframe for survival analysis
#-----------------------------------------
x <- 2 #number of years to measure CRC incidence over
quin_df <- data.frame(grs_quintile = c(5,4,3,2,1)) #a dataframe containing 1 column with numbers 5-1 in rows

make_survframe <- function(p_xx_v) {
  #copy participant dataframe:
  rainbow_40 <- p_xx_v
  rainbow_40$death_t <- lubridate::time_length(difftime(rainbow_40$death_date, rainbow_40$sym_date),"days") #death_t = time between first symptom and death
  rainbow_40$t <- lubridate::time_length(difftime(rainbow_40$crc_date, rainbow_40$sym_date), "days") #t = time between first symptom and cancer
  
  #make dataframe for cumulative hazards plot
  survframe<- rainbow_40 %>% select('eid','sym_age','crc_age','grs','grs_quintile','zscore','death_t','t')
  survframe <- survframe %>% mutate(status=!is.na(crc_age)) #status is TRUE if they ever had CRC and FALSE if they didn't.
  survframe$t[is.na(survframe$t)] <- 729.5 #if participants did not get cancer, set t = 729.5 (mean length in days of 2 years)
  survframe$t[survframe$t>=729.5] <- 729.5 #if participants got cancer >=2 years after symptom then this is outside the time period I want to plot, set t = 2.
  survframe$t <- pmin(survframe$t,survframe$death_t,na.rm=TRUE) #set t to the minimum of t and the death date. t will then be: 729.5, if they lived 2 years cancer free OR time between symptom and cancer diagnosis date if they got cancer OR or time between symptom and death
  survframe$status[survframe$t>=729.5] <- FALSE #status currently = TRUE for anyone who got CRC. change to false if they got CRC after 2 years
  survframe$status <- as.integer(survframe$status) #change TRUE and FALSE in status to 1 and 0

  return(survframe)
}

survframe_e <- make_survframe(p_40_ve)
survframe_m <- make_survframe(p_40_vm)
  
#Run Cox PH model
#---------------
library(survival)
res.cox_e <- coxph(Surv(t, status) ~ grs_quintile, data =  survframe_e)
res.cox_m <- coxph(Surv(t, status) ~ grs_quintile, data =  survframe_m)

#format Cox proportional hazards model for cumulative plot:
fit_stuff <- function(res.cox_x) {
  fit <- survfit(res.cox_x, newdata = quin_df)
  #turn 'upside down' for the plot (otherwise it plots highest GRS quintile in the lowest band on the graph)
  fit2 <- fit
  fit2$surv <- 1-fit2$surv
  fit2$lower <- 1-fit2$lower
  fit2$upper <- 1-fit2$upper
  return(fit2)
}

fit_m <- fit_stuff(res.cox_m)
fit_e <- fit_stuff(res.cox_e)


#Plot cumulative hazards/surivival curve
#=======================================
#graph theme by Harry Green:
HGtheme=theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

rainbow_plot <- function(survframe_x, fit_x) {
  rainbow_surv <- ggsurvplot(fit_x, xlim = c(0, max(survframe_x$t)), conf.int = TRUE,
                             legend.labs=c("Quintile 5","Quintile 4","Quintile 3","Quintile 2","Quintile 1"),
                             risk.table = TRUE, data=quin_df, legend='none',
                             legend.title='GRS Quintile',
                             palette=c('#FF0018','#FFA52C','#008018','#0000F9','#86007D'),
                             ggtheme=HGtheme, ylab='CRC incidence rate', xlab='time since first CRC symptom (years)')
  rainbow_surv$data.survplot$surv <- 1-rainbow_surv$data.survplot$surv
  rainbow_surv$data.survplot$lower <- 1-rainbow_surv$data.survplot$lower
  rainbow_surv$data.survplot$upper <- 1-rainbow_surv$data.survplot$upper
  rainbow_surv$plot$data$surv <- 1-rainbow_surv$plot$data$surv
  rainbow_surv$plot$data$lower <- 1-rainbow_surv$plot$data$lower
  rainbow_surv$plot$data$upper <- 1-rainbow_surv$plot$data$upper
  rainbow_surv$plot <- rainbow_surv$plot + scale_y_continuous(breaks = c(0,0.005, 0.01, 0.015,
                                                                         0.02), limits=c(0, 0.027),
                                                              labels = scales::percent) +
    scale_x_continuous(breaks = c(0, 364.75, 729.5), labels = c(0,1,2))
  return(rainbow_surv$plot)
}

rainbow_plot_e <- rainbow_plot(survframe_e, fit_e)
rainbow_plot_m <- rainbow_plot(survframe_m, fit_m)




#7A. Do logistic  regression testing
#==================================
#IMPORTANT: Back to R version 4.1.1!

#=================================
#make a list with each variable plus its p value and odds ratio for predicting cases or controls:
#-------------------------
library(dplyr)
or_list <- function(p_xx_v, col_list) {
  lr <- replicate(length(col_list), list(OR = NA, L95 = NA, U95 = NA, p = NA), simplify = FALSE)
  for (i in 1:length(col_list)) {
    x <- colnames(p_xx_v)[col_list][i]
    basemod <- glm(noquote(paste0('case~',x)), data=p_xx_v, family=binomial) %>% summary()
    lr[[i]][[1]] <- round(exp(basemod$coefficients[2,1]),2)
    lr[[i]][[2]] <- round(exp(basemod$coefficients[2,1]-1.96*basemod$coefficients[2,2]),2)
    lr[[i]][[3]] <- round(exp(basemod$coefficients[2,1]+1.96*basemod$coefficients[2,2]),2)
    lr[[i]][[4]] <- as.character(signif(basemod$coefficients[2,4],2))
    names(lr)[i] <- x
  }
  return(lr)
}

#final list of variables being tested, for reference:
var_list <- c('sex','sym_age','TDI','BMI','waist_circumference','ever_smoked','smoking_status','alcohol_intake','diabetes','processed_meat_intake','fh_mat','fh_pat','fh','weightloss','loss_appetite','abdo_mass','abdo_pain','rectal_bloodless','fob','change_bowel_habit','haemoglobin','grs','zscore','grs_quintile')

lr_40_e <- or_list(p_40_ve, c(4,7,16:23,26:30,32:37,43:45)) #indexing because some columns don't contain variables, & some variables don't contain enough data
lr_40_m <- or_list(p_40_vm, c(4,7,16:23,26:30,32:37,43:45))

#make list of only variables significantly associated with cases:
#----------------------------------------------------------------
#significant association meaning p <= 0.00208 (Bonferroni correction for 24 tests) and odds ratio confidence intervals >1

lrp_40_e <- list(sex = lr_40_e$sex, sym_age = lr_40_e$sym_age, waist_circumference =
                   lr_40_e$waist_circumference, smoking_status = lr_40_e$smoking_status,
                 ever_smoked = lr_40_e$ever_smoked, rectal_bloodloss = lr_40_e$rectal_bloodloss,
                 change_bowel_habit = lr_40_e$change_bowel_habit, grs = lr_40_e$grs, fob = lr_40_e$fob)

lrp_40_m <- list(sex = lr_40_m$sex, sym_age = lr_40_m$sym_age, smoking_status = lr_40_m$smoking_status,
                 rectal_bloodloss = lr_40_m$rectal_bloodloss, change_bowel_habit = 
                   lr_40_m$change_bowel_habit, grs = lr_40_m$grs, fob = lr_40_m$fob)

#zscore and grs quintile were also significant for both cohorts, but not included because not required for later stages of analysis

#7B. Get ROCAUC measures for signficantly associated variables
#=========================================================
# Takes a formula and a data frame and returns a roc object (Harry Green's code)
#install.packages("pROC")
library(pROC)
rocauc=function(form,dataframe,pt){
  logit <- glm(form, data = dataframe, family = "binomial")
  prob = predict(logit, newdata = dataframe, type = "response")
  if (pt == TRUE) {
    roc = roc(dataframe$case ~ prob, plot = TRUE, print.auc = TRUE, ci=TRUE)
  } else {
    roc = roc(dataframe$case ~ prob, plot = FALSE, print.auc = TRUE, ci=TRUE)}
  return(roc)
}

#add ROCAUCs to list of significantly associated variables
#---------------------------------------------------------
add_rocauc <- function(p_xx_v, lrp_xx) {
  for (i in 1:length(lrp_xx)) {
    x <- names(lrp_xx)[i]
    basemod <- rocauc(noquote(paste0('case~',x)), p_xx_v, F)
    lrp_xx[[i]][[5]] <- round(basemod$ci[2],2)
    lrp_xx[[i]][[6]] <- round(basemod$ci[1],2)
    lrp_xx[[i]][[7]] <- round(basemod$ci[3],2)
  }
  return(lrp_xx)
}

lrp_40_e <- add_rocauc(p_40_ve, lrp_40_e)
lrp_40_m <- add_rocauc(p_40_vm, lrp_40_m)

#8A. Build the risk model by adding variables which cause biggest jump in ROCAUC
#==============================================================================
rocauc_jump <- function(lrp_xx, p_xx_v) {
  #character vector of named variables:
  factors <- names(lrp_xx)
  #create empty list:
  rocp <- replicate(length(lrp_xx), list(ROC = 0, LB = 0, UB = 0), simplify = FALSE)

  for (i in 1:length(lrp_xx)) { #e.g., 1:7
    for (j in 1:length(factors)) { #'factors' decreases in size with each loop, so this will be 1:7, then 1:6, then 1:5...
      
      #print out which model is currently being tested + the ROCAUC of that model:
      print(paste0(names(rocp)[1:i-1],collapse='+'))
      print(factors[j])
      print(paste0('case~',paste0(names(rocp)[1:i-1],collapse='+'),'+',factors[j]))
      
      baseauc <- rocauc(paste0('case~',paste0(names(rocp)[1:i-1],collapse='+'),'+',factors[j]), p_xx_v, FALSE)
      print(baseauc$ci[2])
      print('-------------------')
      
      if (((baseauc$ci[2] == rocp[[i]]$ROC) == TRUE) && ((names(rocp)[i] != factors[j]) == TRUE)) {
        warning(paste('ROC AUC of',factors[j],'matched',names(rocp)[i]))
        #this warning means two different risk models share the highest ROCAUC (so far), so one of them isn't being included in the calculation
        
      } else if ((baseauc$ci[2] > rocp[[i]]$ROC) == TRUE) {
        rocp[[i]]$ROC <- baseauc$ci[2]
        rocp[[i]]$LB <- baseauc$ci[1]
        rocp[[i]]$UB <- baseauc$ci[3]
        names(rocp)[i] <- factors[j]
        maxf <- factors[j]
      }
    }
    #remove the factor with the best ROC AUC from factors
    factors <- factors[factors != maxf]
  }
  return(rocp)
}

rocauc_irm_40_e <- rocauc_jump(lrp_40_e[c(1:4,6:8)],p_40_ve)
rocauc_irm_40_m <- rocauc_jump(lrp_40_m[c(1:6)],p_40_vm)

