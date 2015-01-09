#################################################################
#
#  Explore the Teacher Compensation class in the Function category.
#
###########################################################################

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

classByCat <- c('Function'= classFunction, 'Object_Type' = classObject, 'Operating_Status' = classOperating, 'Position_Type' = classPosition, 'Pre_K' = classPreK, 'Reporting' = classReporting, 'Sharing' = classSharing, 'Student_Type' = classStudent, 'Use'= classUse) 

NumClass <-  c('Function'= 37, 'Object_Type' = 11, 'Operating_Status' = 3, 'Position_Type' = 25, 'Pre_K' = 3, 'Reporting' = 3, 'Sharing' = 5, 'Student_Type' = 9, 'Use'= 8) 

# Create a large data partition for classifying under the ByFunction model.

cat = category[1]
data <- read.csv(paste0('DataSets/', cat, 'Train.csv'))
ind <- createDataPartition(data[,cat], p = .8, list = FALSE, times = 1)
data <- data[ind,]
write.csv(data, paste0('DataSets/', cat, 'PartTest.csv'), row.names = FALSE)

# Use the python script ClassifyInFunction.py to classify the Function category.
# 
# from pandas import read_csv, DataFrame
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.cross_validation import cross_val_score
# from sklearn.externals import joblib
# from numpy import savetxt
# 
# cat = 'Function'
# filename = 'DataSets/' + cat + 'PartTest.csv'
# filename1 = 'DataSets/' + cat + 'RFModel.pkl'
# filename2 = 'DataSets/' + cat + 'Pred.csv'
# filename3 = 'DataSets/' + cat + 'PredProb.csv'
# rfc = joblib.load(filename1)
# df = read_csv(filename)
# data = df.iloc[:,2:]
# print(data.shape)
# out = rfc.predict(data)    
# dataout = DataFrame(out)
# dataout.to_csv(filename2)
# out = rfc.predict_proba(data)    
# dataout = DataFrame(out)
# dataout.to_csv(filename3)

pred <- read.csv(paste0('DataSets/', cat, 'Pred.csv'))
predProb <- read.csv(paste0('DataSets/', cat, 'PredProb.csv'))
dat <- read.csv(paste0('DataSets/', cat, 'PartTest.csv'))

colnames(predProb) <- c('X', classFunction)
predProb <- cbind(dat[,1:2], predProb[,2:38])
predTC <- predProb[pred[,2]=='Teacher Compensation', ]

# Compute the entropy of each prediction
predTC[,40] <- c(rep(0,nrow(predTC)))
for (i in 1:nrow(predTC)){
  predTC[i,40] <- sum(-predTC[i,3:39]*log(predTC[i,3:39])/0.6931472) 
  if (i == 1000*floor(i/1000)){print(i)}
}
  
# Dead end; the same prediction is being made because they have the same predictors.          

# Make new predictors:
# 1.  Create the data; these are classified as Teacher Compensation in the original model.

keeper <- c()
keepers <- predTC[,1]
for (i in 1:nrow(train)){
  if (i == 10000*floor(i/10000)){print(i)}
if (train[i,1] %in% keepers){keeper <- c(keeper,i)}}

trainTC <- train[keeper,]

# find some words:

dir.create('ExamineTC')
for (cat in category){
  dir.create(paste0('ExamineTC/', cat))
  colCat <- match(cat, colnames(trainTC))
  for (var in ExplanVar){
    colVar <- match(var, colnames(trainTC))
    for (i in 1:NumClass[cat]){
      ind <- paste0(cat,i)
      entries <- trainTC[trainTC[,colCat]==classByCat[ind],colVar]
      text <- tolower(toString(entries))
      filename <- paste0('ExamineTC/', cat,'/',gsub('/','',gsub(' ','',classByCat[ind])), '_',var,'.txt')
      writeLines(text, filename)
    }
  }
  
}

# Produce Document Term matrices

library(tm)

for (cat in category){
  corp <- Corpus(DirSource(paste0('ExamineTC/', cat)))
  corp <- tm_map(corp, stripWhitespace)
  corpMat <- DocumentTermMatrix(corp)
  matr <- inspect(corpMat)
  keepers <- c(1)
  for (i in 2:ncol(matr)){
    if (max(matr[,i]) > 1000){keepers <- c(keepers,i)}
  }
  matr <- matr[,keepers]
  write.csv(matr, paste0('ExamineTC/', cat,'DocTermMatrix.csv'))
}

for (cat in category){
  col2 <- c()
  for (i in 1:NumClass[cat]){
    ind <- paste0(cat,i)
    col2 <- c(col2,rep(classByCat[ind], 12))
  }
  col3 <- rep(ExplanVar, NumClass[cat])
  mat <- read.csv(paste0('ExamineTC/', cat,'DocTermMatrix.csv'))
  mat <- cbind(col2,col3,mat[,2:ncol(mat)] )
  names(mat)[1]<-'Classification'
  names(mat)[2]<-'Variable'
  write.csv(mat, paste0('ExamineTC/', cat,'DocTermMatrix.csv'), row.names = FALSE)
  
}

for (cat in category){
  mat <- read.csv(paste0('ExamineTC', cat,'DocTermMatrix.csv'))
  classification <- c()
  for (i in 1:NumClass[cat]){classification <- c(classification, classByCat[paste0(cat,i)])}
  matr <- classification
  for (var in ExplanVar){
    mat1 <- mat[mat[,2]==var, 3:ncol(mat)]
    noms <- c()
    for (label in colnames(mat1)){
      noms <- c(noms, paste0(var,':', sub('X', '', label)))
    }
    colnames(mat1) <- noms
    matr <- cbind(matr,mat1)  
  }
  names(matr)[1] <- 'Classification'
  write.csv(matr, paste0('ExamineTC/', cat,'termClassMatrix.csv'), row.names = FALSE)
}

library(textir)

for (cat in category){
  matr <- read.csv(paste0('ExamineTC', cat,'termClassMatrix.csv'))
  classification <- c()
  for (i in 1:NumClass[cat]){
    ind <- paste0(cat,i)
    classification <- c(classification,classByCat[ind])
  }
  matri <- cbind(classification, tfidf(matr[,2:ncol(matr)]))
  matri[is.na(matri)] <- 0
  write.csv(matri, paste0('ExamineTC/', cat,'termFreq.csv'), row.names = FALSE)
}

# save only those terms with the highest tf-idf's
for (cat in category){
  matri <- read.csv(paste0('ExamineTC', cat,'termFreq.csv'))
  classification <- c()
  for (i in 1:NumClass[cat]){
    ind <- paste0(cat,i)
    classification <- c(classification,classByCat[ind])
  }
  count <- 0
  keepers <- c()
  while(length(keepers) < 50){
    keepers <- c()
    count <- count + 1
    mx <- max(matri[,2:ncol(matri)])/count
    for (i in 2:ncol(matri)){
      if (max(matri[,i]) > mx){keepers <- c(keepers, i)}
    }  
  }
  print(keepers)
  mat <- cbind(classification, matri[,keepers])
  write.csv(mat, paste0('ExamineTC/', cat,'FreqTermFreq.csv'), row.names = FALSE)
}

#  Create a feature table for each category and train and test sets.  Place in Folder:
# DataSets

dir.create('ExamineTC/DataSets')

for (cat in category){
  cols <- colnames(read.csv(paste0('ExamineTC/', cat,'FreqTermFreq.csv')))
  tr <- trainTC[,c('X', cat)]
  count <- 3
  for (tag in cols[2:length(cols)]){
    label <- unlist(strsplit(tag, split = '\\.'))[1]
    word <- unlist(strsplit(tag, split = '\\.'))[2]
    tested <- grep(word, tolower(trainTC[,label]))
    tr[,count] <- c(rep(0, nrow(trainTC)))
    tr[tested,count] <-1
    names(tr)[count] <- tag
    count <- count+1
    print(count)
  }
  print(cat)
  write.csv(tr, paste0('ExamineTC/DataSets/', cat, 'Train.csv'), row.names = FALSE)
}

for (cat in category){
  cols <- colnames(read.csv(paste0('ExamineTC/', cat,'FreqTermFreq.csv')))
  tr <- as.data.frame(test[,1])
  count <- 2
  for (tag in cols[2:length(cols)]){
    label <- unlist(strsplit(tag, split = '\\.'))[1]
    word <- unlist(strsplit(tag, split = '\\.'))[2]
    tested <- grep(word, tolower(test[,label]))
    tr[,count] <- c(rep(0, nrow(test)))
    tr[tested,count] <-1
    names(tr)[count] <- tag
    count <- count+1
    print(count)
  }
  print(cat)
  write.csv(tr, paste0('ExamineTC/DataSets/', cat, 'Test.csv'), row.names = FALSE)
}

library(caret)

for (cat in category){
  print(cat)
  data <- read.csv(paste0('ExamineTC/DataSets/', cat, 'Train.csv'))
  ind <- createDataPartition(data[,cat], p = .1, list = FALSE, times = 1)
  data <- data[ind,]
  write.csv(data, paste0('ExamineTC/DataSets/', cat, 'Partition.csv'), row.names = FALSE)
}

# Run python script to build a random tree model
# 
# from pandas import read_csv
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.cross_validation import cross_val_score
# from sklearn.externals import joblib
# 
# 
# 
# rfc = RandomForestClassifier(n_estimators = 100)
# cat = 'Function'
# filename = 'DataSets/' + cat + 'Partition.csv'
# filename1 = 'DataSets/' + cat + 'RFModel.pkl'
# df = read_csv(filename)
# target = df.iloc[:,1]
# data = df.iloc[:,2:]
# rfc.fit(data, target)
# scores = cross_val_score(rfc, data, target, cv = 10)
# print('Accuracy: %0.2f (+/- %0.2f)' %(scores.mean(), scores.std()*2))
# joblib.dump(rfc, filename1)

# Use original FunctionTree Model to place each test datum into classification:
# Use classification routine ClassByFunction.py in ExamineTC/ folder

# from pandas import read_csv, DataFrame
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.cross_validation import cross_val_score
# from sklearn.externals import joblib
# from numpy import savetxt
# import os
# 
# os.chdir('/home/michael/Data/EdClassification/PolishedPresentation')
# cat = 'Function'
# filename = 'DataSets/' + cat + 'Test.csv'
# filename1 = 'DataSets/' + cat + 'RFModel.pkl'
# filename2 = 'ExamineTC/' + cat + 'Pred.csv'
# filename3 = 'ExamineTC/' + cat + 'PredProb.csv'
# rfc = joblib.load(filename1)
# df = read_csv(filename)
# data = df.iloc[:,1:]
# print(data.shape)
# out = rfc.predict(data)    
# dataout = DataFrame(out)
# dataout.to_csv(filename2)
# out = rfc.predict_proba(data)    
# dataout = DataFrame(out)
# dataout.to_csv(filename3)

# Select those test cases which are initially classified as 'Teacher Compensation'

data <- read.csv('DataSets/FunctionTest.csv')
pred <- read.csv('ExamineTC//FunctionPred.csv')
pred[,1] <- data[,1]
candidates <- pred[pred[,2]=='Teacher Compensation',]
keepers <- candidates[,1]
selection <- read.csv('ExamineTC/DataSets/FunctionTest.csv')
selection <- selection[selection[,1] %in% keepers,]
write.csv(selection, 'ExamineTC/DataSets/testToFunctionPredict2.csv',row.names = FALSE)

# Use the second generation model to predict the Function class:
#   
# ExamineTC/ClassifyInFunction2.py  
# 
# from pandas import read_csv, DataFrame
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.cross_validation import cross_val_score
# from sklearn.externals import joblib
# from numpy import savetxt
# import os
# 
# os.chdir('/home/michael/Data/EdClassification/PolishedPresentation')
# cat = 'Function'
# filename = 'ExamineTC/DataSets/testToFunctionPredict2.csv'
# filename1 = 'ExamineTC/DataSets/' + cat + 'RFModel.pkl'
# filename2 = 'ExamineTC/' + cat + 'Pred2.csv'
# filename3 = 'ExamineTC/' + cat + 'PredProb2.csv'
# rfc = joblib.load(filename1)
# df = read_csv(filename)
# data = df.iloc[:,1:]
# print(data.shape)
# out = rfc.predict(data)    
# dataout = DataFrame(out)
# dataout.to_csv(filename2)
# out = rfc.predict_proba(data)    
# dataout = DataFrame(out)
# dataout.to_csv(filename3)

#Substitute new predictions into old submission

newdat <- read.csv('ExamineTC/FunctionPredProb2.csv')
newdat[,1] <- selection[,1]

sub1 <- sub
for (i in 1:nrow(newdat)){
  j <- match(newdat[i,1], sub1[,1])
  sub1[j,1:38] <- newdat[i,]
}

write.csv(sub1, 'Submission/submit2.csv', row.names = FALSE)

