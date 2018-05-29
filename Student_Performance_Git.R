# R version 3.3.2
setwd("xxxxxxxxxxxxxx\\Project_Stu_Perform_Data")

smasterdt <- readxl::read_excel("Copy_of_Master_Data_for_Reading.xlsx", na="NA", col_types=col_filter)

## basic checks & data cleaning
str(smasterdt$batch)
unique(smasterdt$batch)
smasterdt$batch <- (ifelse(smasterdt$batch=="32FS", "32", smasterdt$batch))
smasterdt$batch <- as.numeric(smasterdt$batch)
unique(smasterdt$batch)

str(smasterdt$percent_in_12th_diploma)
unique(smasterdt$percent_in_12th_diploma)
sum(is.na(smasterdt$percent_in_12th_diploma))
# smasterdt$percent_in_12th_diploma <- (ifelse(is.na(smasterdt$percent_in_12th_diploma)==T, "", smasterdt$percent_in_12th_diploma))
sum(is.na(smasterdt$percent_in_12th_diploma))
smasterdt$percent_in_12th_diploma <- (ifelse(smasterdt$percent_in_12th_diploma %in% c("73.5(Diploma)"), "73.5", smasterdt$percent_in_12th_diploma))
sum(is.na(smasterdt$percent_in_12th_diploma))
smasterdt$percent_in_12th_diploma <- (ifelse(smasterdt$percent_in_12th_diploma %in% c("2010"), NA, smasterdt$percent_in_12th_diploma))
sum(is.na(smasterdt$percent_in_12th_diploma))
smasterdt$percent_in_12th_diploma <- as.numeric(smasterdt$percent_in_12th_diploma)
unique(smasterdt$percent_in_12th_diploma)


completed_passed <- subset(smasterdt, training_status=="Complete" & final_assessment_result!="Fail")
Hmisc::describe(completed_passed$deployment_status)

completed_passed$depvar <- ifelse(completed_passed$deployment_status %in% c("Confirmed", "Deployed", "Hired"), 0, 1)

completed_passed$stream_in_grad_ece <- ifelse(completed_passed$stream_in_grad=="ECE", 1, 0)
names(completed_passed)


# train and test set
set.seed(11111)
library(caret)
dt_mod <- createDataPartition(y = completed_passed$depvar, p=.70, list=F)
dsp_train_dt <- completed_passed[dt_mod,]
dsp_test_dt  <- completed_passed[-dt_mod,]

ctrl <- trainControl(method="cv", number=2)
system.time(
  glmFit <- train(
    depvar ~ stream_in_grad_ece,
    data = completed_passed,
    method = "glm",
    tuneLength = 2,
    trControl = ctrl
  )
)
glmFit


### remaining models are in private mode
