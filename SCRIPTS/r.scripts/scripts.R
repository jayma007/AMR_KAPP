#load packages
install.packages("broom.helpers")
install.packages("gtsummary")
install.packages("gt")
install.packages("easystats")
install.packages("naniar")
library(dplyr)
library(gtsummary)
library(broom.helpers)
library(tidyr)
library(cards)
library(dplyr)
library(tidyverse)
library(easystats)
library(gt)
library(naniar)

#read excel
library(readxl)
Data <- read_excel("RAW DATA/NEWAMR_KAP_Data.xlsx", 
                              sheet = "KAP.rawdata")
View(Data)

library(readxl)
codedData <- read_excel("RAW DATA/NEWAMR_KAP_Data.xlsx", 
                              sheet = "coded")
View(codedData)

#DEMOGRAPHIC CHARACTERISTICS
NEWAMR_KAP_Data|>
select(1:11) 
 
library(readxl)
Data <- read_excel("G:/Internship 6 CHIRAL CLASSES/R CLASS CHIRAL/AMR-PROJECT-CHIRAL/data/NEWAMR_KAP_Data.xlsx")
View(NEWAMR_KAP_Data)
library(Data <- read_excel("RAW DATA/NEWAMR_KAP_Data.xlsx", 
                              sheet = "KAP.rawdata")
View(NEWAMR_KAP_Data)
#rename variable

Data<- Data|>
 rename(gender=parents_gender)

Data<- Data |>
  rename(parents_sex=gender)

#check missing data

sum(is.na(Data))
miss_var_summary(Data)
gg_miss_var(Data)

#fixing missing data

dim(Data)
datanew<- drop_na(Data)
dim(datanew)

gg_miss_var(datanew)

#duplicated data check

sum(duplicated(datanew))


#table:01, demographic characteristics of the study participants

Data |>
  select(1:11)|>
  tbl_summary(
    statistic = list(
      all_continuous()~ "{mean} [{sd}]"
    )
  ) |>
  as_gt()|>
  gtsave("TABLES/table1updated2.docx")

#Table:02,Major sources of information about antibiotic parents

datanew|>
  select(41:49)|>
  tbl_summary( )|>
  as_gt()|>
  gtsave("TABLES/table2.docx")

#classification of knowledge,attitude and practice level

colnames(data2coded)
datacoded<-
codedData|>
  mutate(knowledge_status=
           case_when(`Knowledge PCT`<50 ~ "poor",
                     `Knowledge PCT`<=79 ~ "moderate",
                     `Knowledge PCT`<=100~ "good" ))

datacoded2<-
  datacoded |>
  mutate(attitude_status=
           case_when(`Attitude PCT`<50 ~ "negative",
                     `Attitude PCT`<=79 ~ "uncertain",
                     `Attitude PCT`<=100~ "positive" ))
datacoded3<-
  datacoded2 |>
  mutate(practice_status=
           case_when(`Practice PCT`<80 ~ "inappropriate",
                     `Practice PCT`<=100~ "appropriate"))

#Table 3 Level of knowledge, attitudes, and practices towards antibiotic resistance among parents with school-going children
datacoded3|>
  select(46:48)|>
  tbl_summary( )|>
  as_gt()|>
  gtsave("TABLES/table3.docx")


#Table 4 Factors associated with the level of knowledge among parents of school-going children
install.packages("nnet")
library(nnet)

mv_model<- multinom(knowledge_status~
                      parents_age+parents_sex+parents_edu
                    +Employment_status+Family_type+monthly_income
                    +Child_sex+Childs_age+no_of_children,family=multinom(link="logit"),data=datacoded3)
summary(mv_model)
report(mv_model)

mv_model |>
  tbl_regression(exponential=T)|>
  bold_p(t=0.05)|>
  as_gt()|>
  gtsave("TABLES/table04.docx")

#Table 5 Factors associated with the level of attitudes towards antibiotic resistance among parents of school-going children
mv_model<- multinom(attitude_status~
                      parents_age+parents_sex+parents_edu
                    +Employment_status+Family_type+monthly_income
                    +Child_sex+Childs_age+no_of_children,family=multinom(link="logit"),data=datacoded3)
summary(mv_model)

mv_model |>
  tbl_regression(exponential=T)|>
  bold_p(t=0.05)|>
  as_gt()|>
  gtsave("TABLES/table05.docx")

#Table 6 Factors associated with the level of practices regarding antibiotic resistance among parents of school-going children

mv_model<- multinom(practice_status~
                      parents_age+parents_sex+parents_edu
                    +Employment_status+Family_type+monthly_income
                    +Child_sex+Childs_age+no_of_children,family=multinom(link="logit"),data=datacoded3)
summary(mv_model)

mv_model |>
  tbl_regression(exponential=T)|>
  bold_p(t=0.05)|>
  as_gt()|>
  gtsave("TABLES/table06.docx")


