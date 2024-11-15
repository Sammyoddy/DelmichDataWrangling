library(tidyverse)
library(readr)
library(stringr)
library(lubridate)

delmichdata <- read_csv("C:/Users/SAMMY/Downloads/Delmich dataset.csv")
delmichdata2 <- delmichdata

delmichdata <- delmichdata %>%select(-9) %>%slice(-11)


condition <- nchar(delmichdata$Student_ID) >= 8 & is.na(delmichdata$First_Name) & is.na(delmichdata$Last_Name) & is.na(delmichdata$Age) & is.na(delmichdata$Gender) & is.na(delmichdata$Course) & is.na(delmichdata$Enrollment_Date) & is.na(delmichdata$Total_Payments)

#Split Student ID column 
split_student_col <- function(delmichdata, condition) {
  delmichdata %>%
    mutate(Temp1 = condition & is.na(First_Name) & is.na(Last_Name) & is.na(Age) & is.na(Gender) & is.na(Course) & is.na(Enrollment_Date) & is.na(Total_Payments)) %>%
    mutate(Student_ID2 = ifelse(Temp1, str_trim(Student_ID), NA_character_)) %>%
    separate(Student_ID2, into = c("Student_ID2", "First_Name2", "Last_Name2", "Age2", "Gender2", "Course2", "Enrollment_Date2", "Total_Payments2"), sep = "\\|", fill = "right", extra = "merge") %>%
    mutate(Student_ID = ifelse(Temp1, Student_ID2, Student_ID),
           First_Name = ifelse(Temp1, First_Name2, First_Name),
           Last_Name = ifelse(Temp1, Last_Name2, Last_Name),
           Age = ifelse(Temp1, Age2, Age),
           Gender = ifelse(Temp1, Gender2, Gender),
           Course = ifelse(Temp1, Course2, Course),
           Enrollment_Date = ifelse(Temp1, Enrollment_Date2, Enrollment_Date),
           Total_Payments = ifelse(Temp1, Total_Payments2, Total_Payments)) %>%
    select(-Student_ID2, -First_Name2, -Last_Name2, -Age2, -Gender2, -Course2, -Enrollment_Date2, -Total_Payments2, -Temp1) %>%
    mutate(across(c(Student_ID, First_Name, Last_Name, Age, Gender, Course, Enrollment_Date), str_trim))%>% 
    mutate(Student_ID = ifelse(str_detect(Student_ID, "%") & nchar(Student_ID) > 4,
                               str_remove(substr(Student_ID, 2, nchar(Student_ID)), "%"),
                               ifelse(str_detect(Student_ID, "%"),
                                      str_remove(Student_ID, "%"),
                                      Student_ID)))
}
delmichdata <- split_student_col(delmichdata, condition)





condition2 <- nchar(delmichdata$Student_ID) > 2 & nchar(delmichdata$First_Name) > 8 & is.na(delmichdata$Last_Name) & is.na(delmichdata$Age) & is.na(delmichdata$Gender) & is.na(delmichdata$Course) & is.na(delmichdata$Enrollment_Date) & is.na(delmichdata$Total_Payments)

#Split First Name column
split_fname_col <- function(delmichdata, condition2) {
  delmichdata %>%
    mutate(Temp2 = condition2) %>%
    mutate(Student_ID3 = ifelse(Temp2==T, First_Name, NA_character_)) %>%
    separate(Student_ID3, into = c("Student_ID3", "First_Name3", "Last_Name3", "Age3", "Gender3", "Course3", "Enrollment_Date3", "Total_Payments3"), sep = "\\|", fill = "right", extra = "merge") %>%
    mutate(First_Name = ifelse(Temp2, First_Name3, First_Name),
           Last_Name = ifelse(Temp2, Last_Name3, Last_Name),
           Age = ifelse(Temp2, Age3, Age),
           Gender = ifelse(Temp2, Gender3, Gender),
           Course = ifelse(Temp2, Course3, Course),
           Enrollment_Date = ifelse(Temp2, Enrollment_Date3, Enrollment_Date),
           Total_Payments = ifelse(Temp2, Total_Payments3, Total_Payments)) %>%
    select(-Student_ID3, -First_Name3, -Last_Name3, -Age3, -Gender3, -Course3, -Enrollment_Date3, -Total_Payments3, -Temp2) %>% 
    mutate(across(c(Student_ID,First_Name,Last_Name,Age,Gender,Course,Enrollment_Date),str_trim)) 
}

delmichdata <- split_fname_col(delmichdata, condition2)




condition3 <- nchar(delmichdata$First_Name) > 5 & str_detect(delmichdata$First_Name, " ") & is.na(delmichdata$Last_Name)

#Split fullname into First Name and Last Name columns 
sepfullname <- function(delmichdata, condition3) {
  delmichdata %>%
    mutate(Temp2 = condition3,
           First_Name_Split = ifelse(Temp2, First_Name, NA_character_)) %>%
    separate(First_Name_Split, into = c("First_NameTemp", "Last_NameTemp"), sep = " ", extra="merge", fill = "right") %>%
    mutate(First_Name = ifelse(Temp2, First_NameTemp, First_Name),
           Last_Name = ifelse(Temp2, Last_NameTemp, Last_Name)) %>%
    select(-First_NameTemp, -Last_NameTemp, -Temp2)
}

delmichdata <- sepfullname(delmichdata, condition3)



condition4 <- nchar(delmichdata$Gender) > 2 & str_detect(delmichdata$Gender, " ") & is.na(delmichdata$Age)

#Split Gender column
sepGenderAge <- function(delmichdata, condition4) {
  delmichdata %>%
    mutate(Temp2 = condition4,
           Gender_Split = ifelse(Temp2, Gender, NA_character_)) %>%
    separate(Gender_Split, into = c("GenderTemp", "AgeTemp"), sep = " ", extra="merge", fill = "right") %>%
    mutate(Gender = ifelse(Temp2, GenderTemp, Gender),
           Age = ifelse(Temp2, AgeTemp, Age)) %>%
    select(-GenderTemp, -AgeTemp, -Temp2)
}

delmichdata <- sepGenderAge(delmichdata, condition4)


#Clean Course column of mispellings, Age and Total_Payments column of symbols 
delmichdata <- delmichdata %>%
  mutate(
    Course = ifelse(str_detect(Course, "Machine.*"), str_replace(Course, "Machine.*", "Machine Learning"), Course),
    Course = ifelse(str_detect(Course, "Web Dev.*"), str_replace(Course, "Web Dev.*", "Web Development"), Course),
    Course = ifelse(str_detect(Course, "Data Analy.*"), str_replace(Course, "Data Analy.*", "Data Analytics"), Course),
    Course = ifelse(str_detect(Course, "4"), str_replace(Course, "4", NA_character_), Course),
    Age = ifelse(str_detect(Age, "\\*"), str_replace(Age, "\\*", ""), Age),
    Total_Payments = ifelse(str_detect(Total_Payments, "\\$") | str_detect(Total_Payments, "\\?") | str_detect(Total_Payments, "," ) 	| str_detect(Total_Payments, "�" ) | str_detect(Total_Payments, "\xa"), 
                            str_remove_all(Total_Payments, "[$�?,\xa]"), Total_Payments)
  ) %>% mutate(Total_Payments=as.numeric(Total_Payments))



#Standardizing date column
clean_date_col <- function(date_vector) {
  sapply(date_vector, function(x) {
    
    date_regex1<-"^\\d{2}/\\d{2}/\\d{4}$"
    date_regex2<-"^\\d{2}-[A-Za-z]{3}-\\d{2}$"
    date_regex3<-"^\\d{4}-\\d{2}-\\d{2}$"
    
    if(is.na(x) || x == "") {
      return(NA)
    } else if(grepl(date_regex1, x)) {
      return(x)
    } else if(grepl(date_regex2, x)) {
      date <- as.Date(x, format="%d-%b-%y")
      return(format(date, "%d/%m/%Y"))
    } else if(grepl(date_regex3, x)) {
      date <- as.Date(x, format="%Y-%m-%d")
      return(format(date, "%d/%m/%Y"))
    }
    return(NA)
  })
}

delmichdata <- delmichdata %>%
  mutate(Enrollment_Date = clean_date_col(Enrollment_Date))


#Standardize and handle inconsistent Age and Gender columns
age_gender_col <- function(delmichdata) {
  
  regex_alphabet <- "^[a-zA-Z]+$" # regex for alphabets
  regex_num <- "^[0-9]+$"  # regex for numbers
  regex_alphanum <- "^(?=.*[a-zA-Z])(?=.*[0-9]).+$" # regex for alphabets and numbers
  
  delmichdata %>%
    mutate(
      Temp1 = str_detect(Age, regex_alphabet) & str_detect(Gender, regex_num),
      Temp2 = str_detect(Gender, regex_alphanum),
      New_Age = ifelse(Temp1, Gender, Age),
      New_Gender = ifelse(Temp1, Age, Gender),

      New_Gender = ifelse(Temp2, str_extract(Gender, "[a-zA-Z]+"), New_Gender),
      New_Age = ifelse(Temp2, str_extract(Gender, "[0-9]+"), New_Age)
    ) %>%
    mutate(Age = New_Age, Gender = toupper(New_Gender)) %>%
    select(-Temp1, -Temp2, -New_Age, -New_Gender)
}

delmichdata_cleaned<- age_gender_col(delmichdata)


write.csv(delmichdata_cleaned,"delmichdata_cleaned.csv")














