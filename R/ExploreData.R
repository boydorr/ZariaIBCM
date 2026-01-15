# Just Elaine trying to work out what's going on in the data and checking for
# things that don't make sense

rm(list=ls())

library(readxl)
library(dplyr)


## Read in data
#_________________________

# contact tracing forms
animal_ct <- as.data.frame(read_excel("data/Animal_Contact_Tracing_Form_-_all_versions_-_False_-_2025-03-24-13-22-00.xlsx"))
human_ct <- as.data.frame(read_excel("data/Human_Contact_Tracing_Form_-_all_versions_-_False_-_2025-03-24-13-22-31.xlsx"))

# investigation forms
animal_inv <- as.data.frame(read_excel("data/Animal_Investigation_Form_-_all_versions_-_False_-_2025-03-24-13-21-30.xlsx"))
human_inv <- as.data.frame(read_excel("data/Human_Investigation_Form_-_all_versions_-_False_-_2025-03-24-13-22-16.xlsx"))



## Explore contact tracing form data
#_________________________

names(animal_ct)
names(human_ct)

# Animal CT data
table(animal_ct$Suspect,useNA = "ifany") # 15 suspect, 6 unknown
table(animal_ct$Lateral_flow_test,useNA = "ifany") # 8 positive, 21 not done
table(animal_ct$DFA_Test,useNA = "ifany") # 6 positive, 23 not done
table(animal_ct$Suspect,animal_ct$DFA_Test,useNA = "ifany") # positive tests are all also suspect
table(animal_ct$State,animal_ct$Suspect,useNA = "ifany") # all in Kaduna state
table(animal_ct$LGA,animal_ct$Suspect,useNA = "ifany")
animal_ct <- animal_ct %>% dplyr::mutate(LGA = recode(LGA,'Sabon-Gari' = 'Sabon Gari'))
table(animal_ct$District,animal_ct$Suspect,useNA = "ifany")
table(animal_ct$Vaccination,animal_ct$Suspect,useNA = "ifany") 
table(animal_ct$Number_of_people_bitten_by_the_same_dog,useNA = "ifany")
table(human_ct$Biter_ID)
# apparently 2 people bitten by two of the dogs, and 3 people by 1, but only one
# repeat biter ID in human data
animal_ct$Date_bitten 
animal_ct$Owner
# have date bitten even for unknown dogs - is this really the date the dog was
# bitten or is it actually the day the dog bit someone?
# no date of symptoms given

# Human CT data
table(human_ct$PEP1_hospital_Pharmacy,useNA = "ifany") # one from unspecified primary health care centre - don't know whether Durumi or Kwangila
table(human_ct$PEP1_hospital_Pharmacy,human_ct$PEP_status,useNA = "ifany")
human_ct <- human_ct %>% 
  dplyr::mutate(PEP1_hospital_Pharmacy = recode(PEP1_hospital_Pharmacy,
                                                'MIBA' = 'Major Ibrahim Bello Abdullahi (MIBA) Hospital',
                                                'Sickbay VTH ABU' = 'ABU Sickbay'))
table(human_ct$Trace_back,useNA = "ifany")
table(human_ct$Source,useNA = "ifany")
table(human_ct$Attacking_species,useNA = "ifany") # all dog bites (and 1 Fog!)
table(human_ct$Rabid,useNA = "ifany")
table(human_ct$Animal_status,useNA = "ifany")
table(human_ct$Patient_outcome,useNA = "ifany")
table(human_ct$Patient_outcome,human_ct$cause_of_death,useNA = "ifany")
table(human_ct$Rabid,human_ct$cause_of_death,useNA = "ifany")
# one person who died of rabies has whether dog was rabid recorded as unknown... mention to Grace
table(human_ct$L_G_A)
table(match(animal_ct$Biter_ID,human_ct$Biter_ID),useNA = "ifany") # no matches
# have date bitten, and date of symptoms but not of death

animal_ct_suspect <- filter(animal_ct, Suspect=="yes")
animal_ct_suspect$Date_bitten
animal_ct_suspect$start # not clear what start and end dates are - data entry?
animal_ct_suspect$end
animal_ct$`_GPS_Location_in_UTMS_latitude`
animal_ct$`_GPS_Location_in_UTMS_longitude`
human_ct$`_Record_your_current_location_latitude`
human_ct$`_Record_your_current_location_longitude`
# ask grace for symptoms started



## Explore investigation form data
#_________________________

# Animal investigations
nrow(animal_inv)
table(animal_inv$Rabies_Assessment_Decision) # 22 suspicious for rabies (1 unknown) - this is more than have Suspect==yes in the CT data (CT data has more unknowns though - 6)
table(animal_inv$Rabies_Assessment_Decision,animal_inv$District)
table(animal_inv$Rabies_Assessment_Decision,animal_inv$"_Local_Government_Area_LGA",useNA = "ifany")
animal_inv$Animal_ID 
table(animal_inv$Animal_ID,useNA = "ifany") # 9 don't have an ID
match(animal_inv$Animal_ID,animal_ct$Biter_ID) # quite a few matches to CT animal IDs, but not all matched
animal_inv$`_Record_your_current_location_latitude`
animal_inv$`_Record_your_current_location_longitude` # 10 don't have GPS
animal_inv$Date # is this symptoms started? Many missing
animal_inv$Date_001 # seem to be same date
animal_inv$Date_of_Investigation # all have investigation date
animal_inv[,c('Date_of_Investigation','Date')] # 2nd date always same as or later than investigation date - test date?
table(animal_inv$Reported_from,useNA = "ifany")

# Human investigations
table(human_inv$`_5_2_Local_Government_Area_LGA`,useNA = "ifany")
human_inv$`_6_2_Date_bitten` # all available
human_inv$`_2_Visit_date` # all available and most very close/same as date bitten
table(human_inv$`_6_4_Biting_animal`,useNA = "ifany")
table(human_inv$`_6_5_Type_of_the_Animal`,useNA = "ifany")
table(human_inv$`_10_Rabies_Assessment_decision`,useNA = "ifany")
human_inv$`_GPS_latitude`
human_inv$`_GPS_longitude`
table(human_inv$`_1_Name_of_Health_facility`,useNA = "ifany")
match(human_inv$`_4_Human_ID`,animal_inv$Animal_ID) # no matches
match(human_inv$`_4_Human_ID`,animal_ct$Biter_ID) # no matches
# no obvious way to match animal and human investigation data


