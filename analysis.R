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
devtools::source_url("https://raw.githubusercontent.com/hdg204/Rdna-nexus/main/install.R")

p_sym <- read_GP(sym_codes_filtered$code)
#number each symptom record, as there are multiple records per participant
p_sym$no <- 1:nrow(p_sym)

#2. Any participants with a read code for haemoglobin level were included
#Exclude participants with normal haemoglobin level or no measurement
#======================================================================
#df of participants with haemoglobin level recorded:
exclude <- p_sym[p_sym$read_2 %in% c('44TC.','XaBLm') | p_sym$read_3 %in% c('44TC.','XaBLm'),]

#add sex of participants:
UKBB_var <- read.csv('00_participant.csv')
exclude <- left_join(exclude, UKBB_var[,c(1,5)])
exclude$value1 <- as.numeric(exclude$value1)

