pacman::p_load(dplyr,magrittr,stringr,MTurkR,readr,readxl,tibble,svDialogs)

ConnectToMturk <- function(a,b) {
  Sys.setenv(AWS_ACCESS_KEY_ID=a,AWS_SECRET_ACCESS_KEY=b)
  AccountBalance()
}

RetrieveExistingQualificationTypes <- function(prefix){
  SearchQualificationTypes(prefix,return.all = T) %>% select(Name,QualificationTypeId,CreationTime) %>% map_df(parse_guess) %>% arrange(desc(CreationTime))
}

DeployAQualificationType <- function(name,isExistant){
  full_name <- paste(qualification_type_prefix,name,sep='.')
  if (isExistant){
    if(full_name %in% existing_qualification_type_table$Name){
      existing_qualification_type_table %>% 
        filter(Name==full_name) %>% 
        .$QualificationTypeId
    }else{
      stop('You tried to invoke an existing qualification type but none of the existant qualificaiton types has the name you specified. Change the name and re-run the command.')
    }
  }else{
    if(full_name %in% existing_qualification_type_table$Name){
      stop('You tried to create a new qualification type but the name you specified has been taken by an existant qualification type. Change the name and re-run the command.')
    }else{
      CreateQualificationType(
        name=full_name,
        description='you received this qualification because you completed one of our surveys',
        status='Active',
        auto = FALSE) %>% .$QualificationTypeId %>% as.character()
    }
    
  }
}

AssignDeployedQualificationType <- function(sheet_path, colName){
  workerIds <- readxl::read_excel(sheet_path) %>% 
    .[[colName]] %>% 
    stringr::str_trim('both')
  AssignQualification(deployed_qualification_type,workerIds,value=99)
}

AwardBonus <- function(sheet_path,turkID,assignmentID,bonusAmt,bonusReason){
  df <- readxl::read_excel(sheet_path)
  GrantBonus(df[[turkID]],df[[assignmentID]],df[[bonusAmt]],df[[bonusReason]])
}



