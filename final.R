#Source Path
setwd("C:/Users/vineetha.p.TECHNOVERT/Desktop/Hackathon")
HR_DataSet<-read.csv("Hr_Data.csv",header = TRUE,na.strings=c(""))
Hr_Prediction_DataSet<-read.csv("HR_No.csv",header = TRUE,na.strings=c(""))

# Packages Used 
library(caret)
library(sqldf)

#DataPartition 
set.seed(3456)
intrain<-createDataPartition(HR_DataSet$Attrition,p=0.7,list=FALSE)
TrainingDataSet<-HR_DataSet[intrain,]
TestingDataSet<-HR_DataSet[-intrain,]

#Logistic regressionModel
Logistic_Regression_Model<-glm(Attrition ~ BusinessTravel+EnvironmentSatisfaction+Gender+MaritalStatus+OverTime+StockOptionLevel+
          JobSatisfaction+employeeid+DistanceFromHome+JobLevel+JobRole
        +RelationshipSatisfaction+Benefits+ExtracurricularActivities,family='binomial',data=TrainingDataSet)
summary(Logistic_Regression_Model)

#Prediction
PredictedProbability<-predict(Logistic_Regression_Model,TestingDataSet,type='response')

Predicted_Attrition


#Confusion Matrix
ConfusionMatrix<-table(predicted=Predicted_Attrition,Actual=TestingDataSet$Attrition)
ConfusionMatrix


#Accuracy
Accuracy<-(sum(diag(ConfusionMatrix))/sum(ConfusionMatrix))*100
Accuracy


a<-predict(Logistic_Regression_Model,Hr_Prediction_DataSet,type='response')
a
b<- data.frame(empid=Hr_Prediction_DataSet$employeeid,a)

b

EmployeeProbability<- data.frame(empid=TestingDataSet$employeeid,PredictedProbability)

EmployeeProbability

HR_Data_Prediction<-sqldf('
            SELECT 
             employeeid
            ,Age
            ,a AS Probability_to_Left
	          ,Attrition
            ,CASE 
            WHEN a >= 0.5
            THEN 1
            ELSE 0
            END AS Predicted_Status
            ,BusinessTravel
            ,Department
            ,DistanceFromHome
            ,EducationField
            ,EnvironmentSatisfaction
            ,Gender
            ,JobInvolvement
            ,JobLevel
            ,JobRole
            ,JobSatisfaction
            ,MaritalStatus
            ,NumCompaniesWorked
            ,Over18
            ,OverTime
            ,PercentSalaryHike
            ,PerformanceRating
            ,RelationshipSatisfaction
            ,StockOptionLevel
            ,TotalWorkingYears
            ,TrainingTimes
            ,WorkLifeBalance
            ,YearsAtCompany
            ,YearsInCurrentRole
            ,YearsSinceLastPromotion
            ,YearsWithCurrManager
            ,YearOfJoining
            ,TechnologyAdoption
            ,Benefits
            ,Facilities
            ,ChallengingLevels
            ,ExtracurricularActivities
            ,YearOfLeaving
            FROM Hr_Prediction_DataSet
            INNER JOIN b ON Hr_Prediction_DataSet.employeeid = b.empid')
HR_Data_Prediction




