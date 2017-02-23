#Parsing the SA ID number to extract useful information
#Neil Rankin (neil.rankin.za@gmail.com)

#Thanks to http://knowles.co.za/generating-south-african-id-numbers/ initially created a java version

SA.ID.number.parse <- function(x) {
  #need to use lubridate
  library(lubridate)
  dob.string <- as.character(x)
  dob.year <- substring(dob.string, 1, 2)
  dob.year.full <- ifelse(as.numeric(dob.year<20), paste(20, dob.year), paste(19, dob.year, sep = ""))
  dob.month <- substring(dob.string, 3, 4)
  dob.day <- substring(dob.string, 5, 6)
  gender <- substring(dob.string, 7, 7)
  male <- ifelse(as.numeric(gender) < 5, FALSE, TRUE) 
  female <- ifelse(as.numeric(gender) < 5, TRUE, FALSE)
  dob.date <- ymd(paste(dob.year.full, dob.month, dob.day, sep = "-"))
  sa.citizen <- ifelse(substring(dob.string, 11, 11) == 0, TRUE, FALSE)
  return(c(dob.year, dob.month, dob.day, dob.year.full, dob.date, male, female, sa.citizen))
}
  

