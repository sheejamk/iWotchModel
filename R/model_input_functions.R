# 1. function to get the data for within trial analysis
# 1.1. Get the packages required

require("IPDFileCheck")

#1.2 Get the trial data ready

##########################################################################################################
## I-WOTCH study helper codes
## Creatd by Sheeja Manchira Krishnan
## Assumes the standard R package

#'
#' ##########################################################################################################
#' #' Function to return the mortality rates for a given gender and age
#' #' @param data to look the mortality rates for specific age , gender,year, the needed time cycle,
#' #' the non response or missing code for the rates if any, and the column names of the mortality data
#' #' assumption that the column name for age will be either min age and max age OR age
#' #' column name for gender and year is assumed to be gender.year and gender is similar to males
#' #' @return probability (for the given cycle)
#' #' @examples anyCauseMortalityAgeGender(mortality.data, 12, "male",2016,"week",NA, c("minage","maxage","Males.2016"))
#' anyCauseMortalityAgeGender=function(data,theage,gender,year,reqdtime, nrcode,reqd_columnnames){
#'   #Need to test the data format once we decide the data now asuem the 2016 data
#'   #Assuming the columnname for gender and year are in the form Males.2016 and age bands are given as minage and maxage
#'   if(test.data.columns(data, reqd_columnnames)<0){
#'     return(-1)
#'   }
#'   caps.gender=toupper(gender)
#'   if(is.na(year)){
#'     cols=part.of.columnname(caps.gender,reqd_columnnames)
#'   }else{
#'     cols1=part.of.columnname(caps.gender,reqd_columnnames)
#'     cols2=part.of.columnname(year,reqd_columnnames)
#'     cols=cols2[which(cols2%in%cols1)]
#'   }
#'   if(length(cols)!=0){
#'     if(caps.gender=="MALES" || caps.gender =="MALE" || caps.gender =="MEN" || caps.gender=="MAN"){
#'       cols.male=cols[substring(caps.gender,1,1)== substring(toupper(reqd_columnnames[cols]),1,1)]
#'       caps.genderyear=reqd_columnnames[cols.male]
#'       caps.genderyear.col=cols.male
#'     }else{
#'       if(caps.gender=="FEMALES" || caps.gender =="FEMALE" || caps.gender =="WOMEN" || caps.gender=="WOMAN"){
#'         cols.female=cols[(substring(caps.gender,1,1)== substring(toupper(reqd_columnnames[cols]),1,1))]
#'         caps.genderyear=reqd_columnnames[cols.female]
#'         caps.genderyear.col=cols.female
#'       }else{
#'         print("assumed pattern for caps.gender is not right -code not fully covered ")
#'       }
#'     }
#'   }else{
#'     print("assumed pattern for caps.gender is not right")
#'     return(-1)
#'   }
#'
#'   if(test.data.numeric.norange(genderyear,data,nrcode)<0){
#'     print("Error -mortality rate not numeric ")
#'     return(-1)
#'   }
#'
#'   caps.age=toupper(c("age","years"))
#'   min_low=toupper(c("min","low","lower","minimum"))
#'   max_high=toupper(c("max","high","upper","maximum"))
#'   cols1.caps.age=sapply(caps.age,part.of.columnname,reqd_columnnames)
#'   testing_min=sapply(min_low,part.of.columnname,reqd_columnnames)
#'   testing_max=sapply(max_high,part.of.columnname,reqd_columnnames)
#'   if(any(testing_min!=-1) &&  any(testing_max!=-1)){
#'     age.lower.column=unique(reqd_columnnames[testing_min[testing_min!=-1]])
#'     age.upper.column=unique(reqd_columnnames[testing_max[testing_max!=-1]])
#'     for (i in 1:length(data[[1]])){
#'       if (theage < data[[age.upper.column]][i] && theage >data[[age.lower.column]][i] ){
#'         the.row=i
#'         if(length(the.row)!=0){
#'           mortality.age=as.numeric(data[the.row,caps.genderyear.col])/1000
#'         }else{
#'           print("Error- mortaility corresponding to the age or age range not found")
#'           return(-1)
#'         }
#'       }
#'     }
#'   }else{
#'     if(any(cols1.caps.age!=-1) && any(toupper(reqd_columnnames[cols1.caps.age!=-1])== caps.age)){
#'       age.column=which(toupper(reqd_columnnames[cols1.caps.age!=-1])== caps.age)
#'       the.row=which(data[[age.column]]==theage)
#'       if(length(the.row)!=0){
#'         mortality.age=as.numeric(data[the.row,genderyear.col])/1000
#'       }else{
#'         print("Error- mortaility corresponding to the age not found")
#'         return(-1)
#'       }
#'     }else{
#'       print("Error -agecolumn(s) not matching ")
#'       return(-1)
#'     }
#'   }
#'
#'   if(is.numeric(theage)==TRUE){
#'     # Estimate the daily mortaility rate
#'     daily.rate =-log(1-mortality.age)/365
#'     # estimate the probabilities for a week, month and year
#'     if(reqdtime=="week"){
#'       cycle.prob = 1-exp(-daily.rate*7)
#'     }else{
#'       if(reqdtime=="month"){
#'         cycle.prob = 1-exp(-daily.rate*30)
#'       }else{
#'         if(reqdtime=="year"){
#'           cycle.prob = 1-exp(-daily.rate*365)
#'         }else{
#'           print("cycle not recognised")
#'         }
#'       }
#'     }
#'     return(cycle.prob)
#'   }else{
#'     print("Error-Non numeric age or gender not correct")
#'     return(-1)
#'   }
#' }
#' ##########################################################################################################
#' #' Function to return the mortality rates  given only age information
#' #' @param data to look the mortality rates for specific age , gender,year, the needed time cyce,
#' #' the non response or missing code for the rates if any, and the column names of the mortality data
#' #' assumption that the column name for age will be either min age and max age OR age
#' #' column name for gender and year is assumed to be gender.year and gender is similar to males
#' #' @return probability (for the given cycle)
#' #' @examples any.cause.mortality.age(mortality.data, 12,2016,"week",NA, c("minage","maxage","Males.2016","Females.2016"))
#' anyCauseMortalityAge=function(data,theage,year,reqdtime, nrcode,reqd_columnnames){
#'   res=(anyCauseMortalityAgeGender(data,theage,"male",year,reqdtime, nrcode,reqd_columnnames)+
#'          anyCauseMortalityAgeGender(data,theage,"female",year,reqdtime, nrcode,reqd_columnnames))/2
#'   return(res)
#' }
#'
#'
