pacman::p_load(tidyverse,MTurkR)

EstablishConnection <- function(a,b) {
  Sys.setenv(AWS_ACCESS_KEY_ID=a,AWS_SECRET_ACCESS_KEY=b)
  AccountBalance() #this should output the fund balance on the account
}

CheckNameStatus <- function() {
  if(paste(qualification_type_prefix,qualification_type_name,sep='.') %in% existing_qualification_type_names){
    print("This name has already been used before. Go back to the previous step and come up with a new name.")
  }else{
    print("This name is not among existing names. You are safe to proceed.")
  }
}

RegisterNewQualificationType <- function() {
  customQual <- CreateQualificationType(
    name=paste(qualification_type_prefix,qualification_type_name,sep='.'),
    description=qualification_type_description,
    status='Active',
    auto = FALSE)
  customQual$QualificationTypeId
}
