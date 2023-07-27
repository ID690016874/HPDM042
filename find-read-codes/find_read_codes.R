#either add read2 and 3 lookup tables to your working environment manually
#or uncomment below lines and replace file/path to download to your environment:
#system('dx download read2_lkp.csv') 
#system('dx download read3_lkp.csv')

library(dplyr)
library(stringr)

find_temp_codes <- function(codes, file1='read2_lkp.csv', file2='read3_lkp.csv') {
  codes_header <- c('code','term_description_r2','term_description_r3')
  
  #If no codes input by user, return empty dataframe
  #-------------------------------------------------
  if (is.na(codes[1]) == TRUE){
    warning('User did not supply any read2 or read3 codes to search for. Returning empty dataframe.')
    warning('Please input a code/a list of codes, each code up to 5 characters in length.
            Codes can contain letters, numbers or full stops and are case sensitive.')
    return(read.table(text = "",col.names = codes_header))
  }
  
  #If user input codes, crop codes down to 5 figures each - the Biobank format
  #---------------------------------------------------------------------------
  for (i in 1:length(codes)) {codes[i] <- substr(codes[i], 1, 5)}

  #Use grep to look for codes in read2/3 lookup tables
  #----------------------------------------------------
  codesg=paste(codes,collapse='\\|^') #turn, e.g. 'code1, code2' into 'code1\\|^code2' for use in a grep
  
  grepcode2=paste('grep \'','^',codesg,'\' ', file1, '> read2.csv',sep='') #output = "grep '^B13\\|^B14' read2_lkp.csv> read2.csv"
  grepcode3=paste('grep \'','^',codesg,'\' ', file2, '> read3.csv',sep='') #output = "grep '^B13\\|^B14' read3_lkp.csv> read3.csv"
  
  system(grepcode2) #run grep command to generate read2.csv
  system(grepcode3) #run grep command to generate read3.csv
  
  #Check whether read2.csv and/or read3.csv are empty
  #---------------------------------------------------
  if (as.numeric(file.info('read2.csv')[1])==0 && as.numeric(file.info('read3.csv')[1])==0){
    warning(paste('Read 2/3 codes beginning with',paste(codes[1:3], collapse = ", "),'etc. could not be found. Returning empty dataframe.'))
    warning('Please ensure input codes are each <= 5 characters. Note that codes are case sensitive.')
    return(read.table(text = "",col.names = codes_header))
  }
  
  any_read2 <- TRUE
  any_read3 <- TRUE
  
  if (as.numeric(file.info('read2.csv')[1])==0){
    warning(paste('Read 2 codes beginning with',paste(codes[1:3], collapse = ", "),'etc. could not be found. Continuing with read 3 codes.'))
    any_read2 <- FALSE
  }
  
  if (as.numeric(file.info('read3.csv')[1])==0){
    warning(paste('Read 3 codes beginning with',paste(codes[1:3], collapse = ", "),'etc. could not be found. Continuing with read 2 codes.'))
    any_read3 <- FALSE
  }
  
  #Import and tidy data from filtered READ 2 codes tables
  #======================================================
  if (any_read2 == TRUE) {
    #Import read2 codes & term descriptions as dataframe:
    #---------------------------------------------------
    read2=read.csv('read2.csv', header=FALSE, sep=',',
                   colClasses = c('character','NULL','character','character'))
    
    #Tidy up imported read2 codes
    #------------------------------
    #Some term descriptions have been inadvertently separated into 2 columns with commas.
    #Remove unnecessary stuff from 2nd column, but keep second half of any term descriptions:
    for (i in 1:nrow(read2)) {
      if ( ((read2$V4[i] == ' ') == TRUE) |
           ((is.na(read2$V4[i])) == TRUE) |
           (nchar(read2$V4[i]) == 5)  == TRUE) {
        read2$V4[i] <- ''
      } 
    }
    
    #Combine first and second halves of term descriptions back into 1 column:
    read2$term_description <- read2$V3
    for (i in 1:nrow(read2)) {
      if ((read2$V4[i] != '') == TRUE) {
        read2$term_description[i] <- paste(read2$V3[i], read2$V4[i], sep=' -')
      }
    }
    read2 <- read2[,c(1,ncol(read2))] %>% rename(code = 'V1')
    read2 <- read2[!(read2$term_description==''), ]
    
    #Some codes have multiple term descriptions. Combine these descriptions into
    #the same field, separated with a semi-colon:
    read2 <- read2 %>%
      group_by(code) %>% 
      mutate(term_description = paste0(term_description, collapse = "; ")) %>%
      slice(1)
  }
  
  #Import and tidy data from filtered READ 3 codes tables
  #======================================================
  if (any_read3 == TRUE) {
    #Import read3 codes & term descriptions as dataframe:
    #---------------------------------------------------
    read3=read.csv('read3.csv', header=FALSE, sep=',',
                   colClasses = c('character','character','character','NULL','NULL'))
    
    #Tidy up imported read3 codes
    #------------------------------
    #Some term descriptions have been inadvertently separated into 2 columns with commas.
    #Remove unnecessary stuff from 2nd column, but keep second half of any term descriptions:
    for (i in 1:nrow(read3)) {
      if ( ((read3$V3[i] == ' ') == TRUE) |
           ((is.na(read3$V3[i])) == TRUE) |
           (nchar(read3$V3[i]) <= 1)  == TRUE) {
        read3$V3[i] <- ''
      } 
    }
    
    #Combine first and second halves of term descriptions back into 1 column:
    read3$term_description <- read3$V2
    for (i in 1:nrow(read3)) {
      if ((read3$V3[i] != '') == TRUE) {
        read3$term_description[i] <- paste(read3$V2[i], read3$V3[i], sep=' -')
      }
    }
    read3 <- read3[,c(1,ncol(read3))] %>% rename(code = 'V1')
    read3 <- read3[!(read3$term_description==''), ]
    
    #Some codes have multiple term descriptions. Combine these descriptions into
    #the same field, separated with a semi-colon:
    read3 <- read3 %>%
      group_by(code) %>% 
      mutate(term_description = paste0(term_description, collapse = "; ")) %>%
      slice(1)
  }
  
  
  #Join read 2 and read 3 tables and return joined table
  #=====================================================
  if (any_read2 == FALSE) {
    read2 <- read3
    read2$term_description <- 'NA'
  }
  
  if (any_read3 == FALSE) {
    read3 <- read2
    read3$term_description <- 'NA'
  }
  
  temp_codes <- full_join(read2, read3, by='code')
  temp_codes <- rename(temp_codes, term_description_r2 = 'term_description.x', term_description_r3 = 'term_description.y')
  
  return(temp_codes)
}


find_read_codes <- function(codes, file1 = 'read2_lkp.csv', file2 = 'read3_lkp.csv', loop_limit = 3) {
  
  #Look for any read 2/3 codes which begin with codes input by user. Make a dataframe
  #containing these read2/3 codes plus their descriptions.
  #=======================================================
  temp_codes <- find_temp_codes(codes, file1 = file1, file2 = file2)
  
  #Some of the descriptions link to another code e.g. 'See XaZfN.' Look for
  #these linked codes and add them to the dataframe.
  #=================================================
  read_codes <- temp_codes
  counter <- 0 #this counter prevents an infinite loop in the case that Code A says 'See Code B'
  #and Code B says 'See Code A'.
  
  while ((counter <= loop_limit) == TRUE) {
    #Extract a list of linked codes from read2/3 descriptions which start with 'See':
    see_codes2 <- temp_codes$term_description_r2[(substr(temp_codes$term_description_r2, 1, 3) == "See") == TRUE] %>%
      str_sub(- 5, - 1)
    see_codes3 <- temp_codes$term_description_r3[(substr(temp_codes$term_description_r3, 1, 3) == "See") == TRUE] %>%
      str_sub(- 5, - 1)
    #Combine linked codes from read2 and read3 descriptions, avoiding duplicates:
    see_codes <- unique(c(see_codes2, see_codes3))
    
    #If there are no descriptions which start with 'See':
    see_codes <- see_codes[!is.na(see_codes)]
    if (length(see_codes) == 0) {
      break #end the while loop
    }
    
    #Look for any read 2/3 codes matching/beginning with linked codes. Make another
    #dataframe containing these codes plus their descriptions:
    temp_codes <- suppressWarnings(find_temp_codes(see_codes, file1 = file1, file2 = file2))
    
    #Add the dataframe of linked codes to the original dataframe of codes input by
    #the user:
    read_codes <- rbind(read_codes, temp_codes)
    
    counter <- counter + 1 #increment counter because one while loop has been completed
  }
  
  #Return results
  #==============
  #Once while loop has ended, read_codes should include any read2/3 codes which start with
  #or match codes input by the user, plus their descriptions. It should also include any
  #codes mentioned in the descriptions - so if the description for B139 is 'See XaFsw',
  #the read_codes dataframe will also include XaFsw and its description.
  return(read_codes)
}
