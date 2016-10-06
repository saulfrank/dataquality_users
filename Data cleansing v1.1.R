#------- SAUL FRANK - DQ User Cleanse --- 5 Oct 2016

library(RJSONIO)
library(data.table)
library(stringr)
library(phonenumber)
options(gsubfn.engine = "R")
library(gsubfn)
library(sqldf)

#set the working directory to be the same as input file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#set file name
fileName <- 'file_test_run.csv'

# ------------ helper functions ----------------
# make first letter of word upper case -- clean extra spaces before
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# test if first letter of word is upper case -- clean extra spaces before
testCap <- function(x) {
  s <- unlist(strsplit(x, " "))
  all(grepl("^[[:upper:]].+$", s))
}

# trim trailing and leading spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# email test
email_tester <- function (x) {
  pattern <- '\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>'
return(grepl(pattern, x, ignore.case=TRUE))
}
# ----------------- end of helper functions ----------------------------


# ------------ cleaning functions ----------------

# ------------ cleaning functions ----------------

#first function A: clean email
cleanerFunction <- function(email_string, name_string, phone_string){
  
  dataqualitymap = NA
  exception_email = NA
  exception_name = NA
  exception_phone = NA
  testList = list()
  
  #----------------------- NAME TESTING ------------------------------
  
  # ---- B1 double, leading or trailing spaces -----
  temp_name = name_string #trim()
  spaces <- (grepl(" +", temp_name, ignore.case=TRUE))
  ifelse(spaces, dataqualitymap <- c(dataqualitymap, 'B1'),TRUE)
  
  #remove trailing and leading spaces
  temp_name = trim(temp_name)
  #remove double spaces
  ifelse(spaces,temp_name <- gsub(" +", ' ', temp_name), FALSE)
  
  # ------ B3 first letters of name to upper case --------
  testList <- c(testList,list(ifelse(!sapply(temp_name,testCap),'B3',NA)))
  temp_name <- sapply(temp_name,simpleCap)
  
  # ---- B2 name count of at least two -----
  word_count <- str_count(temp_name, "\\S+")
  testList <- c(testList,list(ifelse(word_count < 2,'B2',NA)))
  exception_name <- ifelse(word_count < 2,TRUE, '')
  #split based on capital letters
  ifelse(word_count < 2, temp_name <- trim(gsub('([[:upper:]])', ' \\1', temp_name)),FALSE)

  
  # ----- B4 test for numerals and emails --------
  testList <- c(testList,list(ifelse(email_tester(temp_name)|
                                       grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", 
                                             temp_name, ignore.case=TRUE),'B4',NA)))
  exception_name <- ifelse(email_tester(temp_name)|
                             grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", 
                                   temp_name, ignore.case=TRUE), TRUE, exception_name)
  
  # ----- B5 space, special characters or full stop to quickly fill out form --------
  #remove all non-ASCII characters
  temp_ascii <- stringi::stri_trans_general(temp_name, "latin-ascii")
  #if there are less that 2 ASCII characters then flag as exception:
  testList <- c(testList,list(ifelse(nchar(temp_ascii)< 2,'B5',NA)))
  exception_name <- ifelse(nchar(temp_ascii)< 2, TRUE, '')
  
  
  #----------------------- END OF NAME TESTING ------------------------------
  
  
  #----------------------- EMAIL TESTING ------------------------------
  
  # ---- A1 is this email structure an email - using regular expressions:: -----
  emailtest <- email_tester(email_string)
  # remove any special characters assuming non I18n ASCII
  # remove spaces or whitespace
  # is this a valid email address i.e. containing @ and .
  email_pattern <- '[A-Za-z0-9._%+-@]+'
  pre <- str_extract_all(email_string, email_pattern)
  temp_email <- sapply(pre, paste, collapse='')
  testList <- c(testList,list(ifelse(emailtest,NA,'A1')))
  exception_email <- ifelse(emailtest,'', TRUE)
  
  
  # ------ A2 convert email to Lowercase --------
  temp_email <- tolower(temp_email)
  testList <- c(testList,list(ifelse(email_string != temp_email,'A2',NA)))
  #print(paste(email_string," - ",temp_email, " - ", dataqualitymap))
  
  # ----- A3 minimum 6 characters: RFC standard --------
  min_length <- nchar(temp_email)
  testList <- c(testList,list(ifelse(min_length < 6, 'A3',NA)))
  exception_email <- ifelse(min_length < 6, TRUE, exception_email)
  # no programmatic way of fixing this, will need to call customer or validate at input.
  
  # ----- A4 max 254 characters: RFC standard --------
  max_length <- nchar(temp_email)
  testList <- c(testList,list(ifelse(max_length > 254, 'A4',NA)))
  exception_email <- ifelse(max_length > 254, TRUE, exception_email)
  temp_email <- strtrim(temp_email, 254)
  # Other than limiting characters, no programmatic way of fixing this, will need to call customer or validate at input.
  
  #----------------------- END OF EMAIL TESTING ------------------------------
  
  
  #----------------------- PHONE TESTING ------------------------------
  
  international_uk <- grepl('(^\\+44)|(^0044)',phone_string)
  international_other <- grepl('(^\\+[0-35-9])|(^00[0-35-9] )',phone_string)
  
  international_uk <- grepl('(^\\+44)|(^0044)',phone_string)
  international_other <- grepl('(^\\+[0-35-9])|(^00[0-35-9] )',phone_string)
  
  
  #C1
  #phonenumber converts all unknown characters to -
  temp_phone <- sapply(phone_string,letterToNumber)
  temp_phone <- gsub('[- ]','',temp_phone)
  testList <- c(testList,list(ifelse(gsub(' ','',phone_string) != temp_phone,'C1',NA)))
  
  #C4 International phone numbers
  ext <- str_extract(temp_phone, '(^[1-9]{2})|(^[0-9]{3})')
  ext <- ifelse(grepl('^\\+',phone_string),paste0('+',ext),ext)
  ext[!(international_uk | international_other)] <- NA 
  testList <- c(testList,list(ifelse(!is.na(ext) & international_uk,'C4',NA)))
  
  temp_phone[!is.na(ext)] <- str_sub(temp_phone[!is.na(ext)],str_length(ext[!is.na(ext)]))
  
  #C2 
  temp_phone <- gsub('^([0-9]{5})([0-9]+)$', '\\1 \\2', temp_phone)
  test <- grepl('^[0-9A-Za-z]{5} [0-9A-Za-z]{6}$',phone_string)
  test[international_uk] <- grepl('^\\+44 [0-9]{4} [0-9]{6}$',phone_string[international_uk]) 
  testList <- c(testList,list(ifelse(!test,'C2',NA)))
  
  #C3 $ C5
  exception_phone  <-  str_length(temp_phone) != 12 | international_other
  exception_phone[exception_phone==FALSE] <- ''
  testList <- c(testList,list(ifelse(str_length(temp_phone) != 12,'C3',NA)))
  testList <- c(testList,list(ifelse(international_other,'C5',NA)))
  
  #----------------------- END OF PHONE TESTING ------------------------------
  
  
  tmp <- apply(as.data.frame(testList),1,paste, collapse = ',')
  dataqualitymap <- gsub('NA,?','',tmp)
  dataqualitymap <- gsub(',$','',dataqualitymap) %>% strsplit(',')
  dataqualitymap <- sapply(dataqualitymap, toJSON)
  dataqualitymap <- gsub('\\[  \\]','',dataqualitymap)
  
  #output the result:
  return(list(
    r1 = temp_name, 
    r2 = temp_email, 
    r3 = temp_phone, 
    r4 = dataqualitymap, 
    r5 = exception_name, 
    r6 = exception_email,
    r7 = exception_phone,
    r8 = ext
  ))
}

#import messy data into a data frame - specify the filename
#[[[INPUT]]]
mydata = read.csv(fileName, header=TRUE, strip.white=FALSE) #, strip.white=TRUE (to test double spaces, take out)
# convert to data table
setDT(mydata)

#apply to data:: ctrl shift c
 mydata[,
       c(
         "clean_name",
         "clean_email",
         "clean_phone",
         "data_quality_map",
         "name_exception",
         "email_exception",
         "phone_exception",
         "phone_ext"):=cleanerFunction(mydata$email, mydata$name, mydata$phone)
       ]

#cleanerFunction(mydata$email[1:3], mydata$name[1:3], mydata$phone[1:3])

write.csv(mydata, file = "DQ_output.csv",row.names=FALSE)


#exception table using SQL
SQLme <- "select * from mydata where data_quality_map <> ''"
exception_table <- sqldf(SQLme,stringsAsFactors = FALSE)

write.csv(exception_table, file = "DQ_exception.csv",row.names=FALSE)

