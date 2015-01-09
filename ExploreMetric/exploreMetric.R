category <- c('Function', 'Object_Type', 'Operating_Status', 'Position_Type', 'Pre_K', 'Reporting', 'Sharing', 'Student_Type', 'Use') 
ExplanVar <-c('Facility_or_Department', 'Function_Description', 'Fund_Description', 'Job_Title_Description', 'Location_Description', 'Object_Description', 'Position_Extra', 'Program_Description', 'SubFund_Description', 'Sub_Object_Description', 'Text_1', 'Text_2')
classFunction <- c('Aides Compensation', 'Career & Academic Counseling', 'Communications', 'Curriculum Development', 'Data Processing & Information Services', 'Development & Fundraising', 'Enrichment', 'Extended Time & Tutoring', 'Facilities & Maintenance', 'Facilities Planning', 'Finance, Budget, Purchasing & Distribution', 'Food Services', 'Governance', 'Human Resources', 'Instructional Materials & Supplies', 'Insurance', 'Legal', 'Library & Media', 'NO_LABEL', 'Other Compensation', 'Other Non-Compensation', 'Parent & Community Relations', 'Physical Health & Services', 'Professional Development', 'Recruitment', 'Research & Accountability', 'School Administration', 'School Supervision', 'Security & Safety', 'Social & Emotional', 'Special Population Program Management & Support', 'Student Assignment', 'Student Transportation', 'Substitute Compensation', 'Teacher Compensation', 'Untracked Budget Set-Aside', 'Utilities')
classObject <- c('Base Salary/Compensation', 'Benefits', 'Contracted Services', 'Equipment & Equipment Lease', 'NO_LABEL', 'Other Compensation/Stipend', 'Other Non-Compensation', 'Rent/Utilities', 'Substitute Compensation', 'Supplies/Materials', 'Travel & Conferences')
classOperating <- c('Non-Operating', 'Operating, Not PreK-12', 'PreK-12 Operating')
classPosition <- c('(Exec) Director', 'Area Officers', 'Club Advisor/Coach', 'Coordinator/Manager', 'Custodian', 'Guidance Counselor', 'Instructional Coach', 'Librarian', 'NO_LABEL', 'Non-Position', 'Nurse', 'Nurse Aide', 'Occupational Therapist', 'Other', 'Physical Therapist', 'Principal', 'Psychologist', 'School Monitor/Security', 'Sec/Clerk/Other Admin', 'Social Worker', 'Speech Therapist', 'Substitute', 'TA', 'Teacher', 'Vice Principal')
classPreK <- c('NO_LABEL', 'Non PreK', 'PreK') 
classReporting <- c('NO_LABEL', 'Non-School', 'School') 
classSharing <- c('Leadership & Management', 'NO_LABEL', 'School Reported', 'School on Central Budgets', 'Shared Services')
classStudent <- c('Alternative', 'At Risk', 'ELL', 'Gifted', 'NO_LABEL', 'Poverty', 'PreK', 'Special Education', 'Unspecified')
classUse <- c('Business Services', 'ISPD', 'Instruction', 'Leadership', 'NO_LABEL', 'O&M', 'Pupil Services & Enrichment', 'Untracked Budget Set-Aside')


# Generate a scoring template

template <- read.csv('SubmissionFormat.csv')

train <- read.csv('/home/michael/Data//EdClassification/TrainingData.csv')


template <- as.data.frame(mat.or.vec(400277,105))

template[,1] <- train[,1]

for (i in 1:nrow(template)){
  j <- match(train[i,2], classFunction)
  template[i, j+1] <- 1
  
  j <- match(train[i,8], classObject)
  template[i, j+38] <- 1
  
  j <- match(train[i,10], classOperating)
  template[i, j+49] <- 1
  
  j <- match(train[i,7], classPosition)
  template[i, j+52] <- 1
  
  j <- match(train[i,9], classPreK)
  template[i, j+77] <- 1
  
  j <- match(train[i,5], classReporting)
  template[i, j+80] <- 1
  
  j <- match(train[i,4], classSharing)
  template[i, j+83] <- 1
  
  j <- match(train[i,6], classStudent)
  template[i, j+88] <- 1
  
  j <- match(train[i,3], classUse)
  template[i, j+97] <- 1
  
  if(i == 1000*floor(i/1000)){print()}
}

# Make predictions using earlier models with 
# 
# GeneratePredictionsFromRandomForestModels2.py
# 
# from pandas import read_csv, DataFrame
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.cross_validation import cross_val_score
# from sklearn.externals import joblib
# from numpy import savetxt
# 
# category = ['Function', 'Object_Type', 'Operating_Status', 'Position_Type', 'Pre_K', 'Reporting', 'Sharing', 'Student_Type', 'Use']
# for cat in category:
#   filename = 'DataSets/' + cat + 'Train.csv'
# filename1 = 'DataSets/' + cat + 'RFModel.pkl'
# filename2 = 'DataSets/' + cat + 'PredPartTrain.csv'
# rfc = joblib.load(filename1)
# df = read_csv(filename)
# data = df.iloc[:,2:]
# print(data.shape)
# out = rfc.predict_proba(data)    
# dataout = DataFrame(out)
# dataout.to_csv(filename2)

func <- read.csv('/home//michael/Data/EdClassification/PolishedPresentation//DataSets/FunctionPredPartTrain.csv')

score <- 0
for (i in 2:38){
  score <- score + sum(-log(func1[,i]+0.001)*template[,i])/(400277)
}
score

func1 <- as.data.frame(mat.or.vec(nrow(func),ncol(func)))
func1[,1] <- func[,1]
for (i in 1:nrow(func)){
  j <- which.max(func[i,2:38])+1
  func1[i,j] <- 1
  if ( i == 1000*floor(i/1000)){print(i)}
}

# Adjust submit 2 accordingly

sub <- read.csv('/home/michael/Data//EdClassification/PolishedPresentation/Submission/submit2.csv')

for (i in 1:nrow(sub)){
  j <- which.max(sub[i,2:38])+1
  sub[i,j] <- 1
  if ( i == 1000*floor(i/1000)){print(i)}
}

fixme <- (which(sub[,36]==1))
for (i in fixme){
  sub[i,2:38] <- t(c(rep(1/74,34), (.5+1/74), 1/74, 1/74))}

write.csv(sub, '/home/michael/Data//EdClassification/PolishedPresentation/Submission/submit4.csv', row.names = FALSE)

# Predict the classification using:
#   
# GeneratePredictionsFromRandomForestModels2.py
# 
# from pandas import read_csv, DataFrame
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.cross_validation import cross_val_score
# from sklearn.externals import joblib
# from numpy import savetxt
# 
# category = ['Function', 'Object_Type', 'Operating_Status', 'Position_Type', 'Pre_K', 'Reporting', 'Sharing', 'Student_Type', 'Use']
# for cat in category:
#   filename = 'DataSets/' + cat + 'Train.csv'
# filename1 = 'DataSets/' + cat + 'RFModel.pkl'
# filename2 = 'DataSets/' + cat + 'PredClassTrain.csv'
# rfc = joblib.load(filename1)
# df = read_csv(filename)
# data = df.iloc[:,2:]
# print(data.shape)
# out = rfc.predict(data)    
# dataout = DataFrame(out)
# dataout.to_csv(filename2)

func <- read.csv('/home//michael/Data/EdClassification/PolishedPresentation//DataSets/FunctionPredClassTrain.csv')

write.csv(table(func[,2], func[,3]), 'FunctionCrossTable.csv')

cross <- table(func[,2], func[,3])

correct <- mat.or.vec(35,3)
correct[,1] <- rownames(cross)

rownames(correct) <- rownames(cross)
for (i in rownames(cross)){
  correct[i,2] <- cross[i,i]
  correct[i,3] <- sum(cross[i,]) - cross[i,i]
}

# Explore the metric

dat <- read.csv('/home//michael/Data//EdClassification/RefineFunction/trainFeatures.csv')
ind <- createDataPartition(dat[,2], p = .125, list = FALSE, times = 1)
feat <- dat[ind,]

write.csv(feat, paste0('TrainPartition.csv'), row.names = FALSE )

template <- template[,1:38]
template <- template[ind,]


#Use model from RefineFunction to predict probabilies

results <- read.csv('probabilities1.csv')


score <- 0
for (i in 2:38){
  score <- score + sum(-log(results[,i]+0.00001)*template[,i])/(50052)
}
score

sub <- mat.or.vec(50052,38)
sub[,1]<-results[,1]

for (i in 1:nrow(results)){
  j <- which.max(results[i,2:38])+1
  sub[i,j] <- 1
  if ( i == 1000*floor(i/1000)){print(i)}
}

score <- 0
for (i in 2:38){
  score <- score + sum((sub[,i])*template[,i])
}
score/50052

score <- 0
for (i in 2:38){
  score <- score + sum(-log(sub[,i]+0.001)*template[,i])/(50052)
}
score

# 3 January 2015

template <- read.csv('metricTemplate.csv')
dat <- read.csv('/home//michael/Data//EdClassification/RefinePreK/trainFeatures.csv')
library(caret)
ind <- createDataPartition(dat[,2], p = .15, list = FALSE, times = 1)
dat <- dat[ind,]
write.csv(dat, paste0('TestMetricPlaypen.csv'), row.names = FALSE)

template <- template[ind,]

# Use random forest model to create both probability and class predictions.

# PredictProbsPreK.csv
# PredClassPreK.csv

train <- read.csv('/home/michael/Data//EdClassification/TrainingData.csv')
train <- train[ind,]
train<-train[,c(1,9)]
template <- template[,c(1,78:80)]

pred <- read.csv('PredClassPreK.csv')
prob <- read.csv('PredictProbsPreK.csv')

tabled <- table(train[,2], pred[,2])

score <- function(predprobs, templat, eps){
  summed <- c(rep(0,nrow(predprobs)))
  for (i in 2:ncol(predprobs)){
    predprobs[,i] <- predprobs[,i] + eps*(predprobs[,i]==0)
    summed <- summed + predprobs[,i]*templat[,i]
  }
   value <- sum(-log(summed))/nrow(predprobs)
   return(value)
}


ind1<- createDataPartition(pred[,2], p = ..0015, list = FALSE, times = 1)


template <- read.csv('metricTemplate.csv')
dat <- read.csv('/home//michael/Data//EdClassification/RefineFunction/trainFeatures.csv')
# library(caret)
ind <- createDataPartition(dat[,2], p = .15, list = FALSE, times = 1)
dat <- dat[ind,]
write.csv(dat, paste0('TestMetricPlaypen.csv'), row.names = FALSE)

template <- template[ind,]

# Use random forest model to create both probability and class predictions.

# PredictProbsFunction.csv
# PredClassFunction.csv

train <- read.csv('/home/michael/Data//EdClassification/TrainingData.csv')
train <- train[ind,]
train<-train[,c(1,2)]
template <- template[,c(1,2:38)]

pred <- read.csv('PredClassFunction.csv')
prob <- read.csv('PredictProbsFunction.csv')

tabled <- table(train[,2], pred[,2])
write.csv(tabled, 'FunctionTable.csv')

score(prob, template)

# Develop an svm model to hopefully better performance on the metric

train <- read.csv('/home/michael/Data//EdClassification/TrainingData.csv')
train <- train[ind,]
train <- train[, c(1:2,19,23)]
noms <- c(colnames(train), classFunction)
trains <- cbind(train, prob[,2:38])
colnames(trains)<- noms
trains[,3] <- NULL
write.csv(trains, 'trainsFunction.csv', row.names = FALSE)
for (cat in category){dir.create(cat)}
prob1 <- read.csv('logFunction.csv')