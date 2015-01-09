######################################################################
# Goal is described in CompetitionDescription.pdf
# For each category
# Function
# Object_Type 
# Operating_Status 
# Position_Type 
# Pre_K 
# Reporting 
# Sharing 
# Student_Type 
# Use 
# the given expenditure must be placed in the appropriate classification.
# want to create a text file for 
# ByCategory x ByClassification x ExplanatoryVariable
##################################################################

train <- read.csv('TrainingData.csv')
test <- read.csv('TestData.csv')


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

for (cat in category){
  dir.create(cat)
  colCat <- match(cat, colnames(train))
  for (var in ExplanVar){
    colVar <- match(var, colnames(train))
    for (i in 1:NumClass[cat]){
      ind <- paste0(cat,i)
      entries <- train[train[,colCat]==classByCat[ind],colVar]
      text <- tolower(toString(entries))
      filename <- paste0(cat,'/',gsub('/','',gsub(' ','',classByCat[ind])), '_',var,'.txt')
      writeLines(text, filename)
    }
  }
   
}

# produce document term matrices

library(tm)

for (cat in category){
  corp <- Corpus(DirSource(cat))
  corp <- tm_map(corp, stripWhitespace)
  corpMat <- DocumentTermMatrix(corp)
  matr <- inspect(corpMat)
  keepers <- c(1)
  for (i in 2:ncol(matr)){
    if (max(matr[,i]) > 1000){keepers <- c(keepers,i)}
  }
  matr <- matr[,keepers]
  write.csv(matr, paste0(cat,'DocTermMatrix.csv'))
}

for (cat in category){
  col2 <- c()
  for (i in 1:NumClass[cat]){
    ind <- paste0(cat,i)
    col2 <- c(col2,rep(classByCat[ind], 12))
  }
  col3 <- rep(ExplanVar, NumClass[cat])
  mat <- read.csv(paste0(cat,'DocTermMatrix.csv'))
                  mat <- cbind(col2,col3,mat[,2:ncol(mat)] )
                  names(mat)[1]<-'Classification'
                  names(mat)[2]<-'Variable'
                  write.csv(mat, paste0(cat,'DocTermMatrix.csv'), row.names = FALSE)
                            
}

# Combine rows with the same Classification; label the columns by the initial 
# explanatory variable underscore the word.

for (cat in category){
  mat <- read.csv(paste0(cat,'DocTermMatrix.csv'))
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
  write.csv(matr, paste0(cat,'termClassMatrix.csv'), row.names = FALSE)
}

library(textir)

for (cat in category){
  matr <- read.csv(paste0(cat,'termClassMatrix.csv'))
  classification <- c()
  for (i in 1:NumClass[cat]){
    ind <- paste0(cat,i)
    classification <- c(classification,classByCat[ind])
  }
  matri <- cbind(classification, tfidf(matr[,2:ncol(matr)]))
  matri[is.na(matri)] <- 0
  write.csv(matri, paste0(cat,'termFreq.csv'), row.names = FALSE)
}

# save only those terms with the highest tf-idf's
for (cat in category){
  matri <- read.csv(paste0(cat,'termFreq.csv'))
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
  write.csv(mat, paste0(cat,'FreqTermFreq.csv'), row.names = FALSE)
}

#  Create a feature table for each category and train and test sets.  Place in Folder:
# DataSets

dir.create('DataSets')

for (cat in category){
  cols <- colnames(read.csv(paste0(cat,'FreqTermFreq.csv')))
  tr <- train[,c('X', cat)]
  count <- 3
  for (tag in cols[2:length(cols)]){
    label <- unlist(strsplit(tag, split = '\\.'))[1]
    word <- unlist(strsplit(tag, split = '\\.'))[2]
    tested <- grep(word, tolower(train[,label]))
    tr[,count] <- c(rep(0, nrow(train)))
    tr[tested,count] <-1
    names(tr)[count] <- tag
    count <- count+1
    print(count)
  }
  print(cat)
  write.csv(tr, paste0('DataSets/', cat, 'Train.csv'), row.names = FALSE)
}

for (cat in category){
  cols <- colnames(read.csv(paste0(cat,'FreqTermFreq.csv')))
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
  write.csv(tr, paste0('DataSets/', cat, 'Test.csv'), row.names = FALSE)
}

# produce partitions for python random forest training
library(caret)

for (cat in category){
  print(cat)
  data <- read.csv(paste0('DataSets/', cat, 'Train.csv'))
  ind <- createDataPartition(data[,cat], p = .1, list = FALSE, times = 1)
  data <- data[ind,]
  write.csv(data, paste0('DataSets/', cat, 'Partition.csv'), row.names = FALSE)
}

#Generate a test data set for python random forest models

for (cat in category){
  print(cat)
  data <- read.csv(paste0('DataSets/', cat, 'Train.csv'))
  ind <- createDataPartition(data[,cat], p = .1, list = FALSE, times = 1)
  data <- data[ind,]
  write.csv(data, paste0('DataSets/', cat, 'PartTest.csv'), row.names = FALSE)
}

############################################################################

# Generate a random forest model in scikit learn:

# GenerateRandomForestModels.py

# from pandas import read_csv
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.cross_validation import cross_val_score
# from sklearn.externals import joblib
# 
# 
# category = ['Function', 'Object_Type', 'Operating_Status', 'Position_Type', 'Pre_K', 'Reporting', 'Sharing', 'Student_Type', 'Use']
# 
# rfc = RandomForestClassifier(n_estimators = 100)
# for cat in category:
#   filename = 'DataSets/' + cat + 'Partition.csv'
# filename1 = 'DataSets/' + cat + 'RFModel.pkl'
# df = read_csv(filename)
# target = df.iloc[:,1]
# data = df.iloc[:,2:]
# rfc.fit(data, target)
# scores = cross_val_score(rfc, data, target, cv = 10)
# print('Accuracy: %0.2f (+/- %0.2f)' %(scores.mean(), scores.std()*2))
# joblib.dump(rfc, filename1)

###################################################################################

#####################################################################################

# make probabalistic predictions of test set

# GenerateSubmissionRandomForest.py

# from pandas import read_csv, DataFrame
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.cross_validation import cross_val_score
# from sklearn.externals import joblib
# from numpy import savetxt
# 
# category = ['Function', 'Object_Type', 'Operating_Status', 'Position_Type', 'Pre_K', 'Reporting', 'Sharing', 'Student_Type', 'Use']
# for cat in category:
#   filename = 'DataSets/' + cat + 'Test.csv'
# filename1 = 'DataSets/' + cat + 'RFModel.pkl'
# filename2 = 'Submission/' + cat + 'PredSubmit.csv'
# rfc = joblib.load(filename1)
# df = read_csv(filename)
# data = df.iloc[:,1:]
# print(data.shape)
# out = rfc.predict_proba(data)    
# dataout = DataFrame(out)
# dataout.to_csv(filename2)


#########################################################################################


# Construct the submission

labels =c('', 'Function__Aides Compensation', 'Function__Career & Academic Counseling', 'Function__Communications', 'Function__Curriculum Development', 'Function__Data Processing & Information Services', 'Function__Development & Fundraising', 'Function__Enrichment', 'Function__Extended Time & Tutoring', 'Function__Facilities & Maintenance', 'Function__Facilities Planning', '\"Function__Finance, Budget, Purchasing & Distribution\"', 'Function__Food Services', 'Function__Governance', 'Function__Human Resources', 'Function__Instructional Materials & Supplies', 'Function__Insurance', 'Function__Legal', 'Function__Library & Media', 'Function__NO_LABEL', 'Function__Other Compensation', 'Function__Other Non-Compensation', 'Function__Parent & Community Relations', 'Function__Physical Health & Services', 'Function__Professional Development', 'Function__Recruitment', 'Function__Research & Accountability', 'Function__School Administration', 'Function__School Supervision', 'Function__Security & Safety', 'Function__Social & Emotional', 'Function__Special Population Program Management & Support', 'Function__Student Assignment', 'Function__Student Transportation', 'Function__Substitute Compensation', 'Function__Teacher Compensation', 'Function__Untracked Budget Set-Aside', 'Function__Utilities', 'Object_Type__Base Salary/Compensation', 'Object_Type__Benefits', 'Object_Type__Contracted Services', 'Object_Type__Equipment & Equipment Lease', 'Object_Type__NO_LABEL', 'Object_Type__Other Compensation/Stipend', 'Object_Type__Other Non-Compensation', 'Object_Type__Rent/Utilities', 'Object_Type__Substitute Compensation', 'Object_Type__Supplies/Materials', 'Object_Type__Travel & Conferences', 'Operating_Status__Non-Operating', '\"Operating_Status__Operating, Not PreK-12\"', 'Operating_Status__PreK-12 Operating', 'Position_Type__(Exec) Director', 'Position_Type__Area Officers', 'Position_Type__Club Advisor/Coach', 'Position_Type__Coordinator/Manager', 'Position_Type__Custodian', 'Position_Type__Guidance Counselor', 'Position_Type__Instructional Coach', 'Position_Type__Librarian', 'Position_Type__NO_LABEL', 'Position_Type__Non-Position', 'Position_Type__Nurse', 'Position_Type__Nurse Aide', 'Position_Type__Occupational Therapist', 'Position_Type__Other', 'Position_Type__Physical Therapist', 'Position_Type__Principal', 'Position_Type__Psychologist', 'Position_Type__School Monitor/Security', 'Position_Type__Sec/Clerk/Other Admin', 'Position_Type__Social Worker', 'Position_Type__Speech Therapist', 'Position_Type__Substitute', 'Position_Type__TA', 'Position_Type__Teacher', 'Position_Type__Vice Principal', 'Pre_K__NO_LABEL', 'Pre_K__Non PreK', 'Pre_K__PreK', 'Reporting__NO_LABEL', 'Reporting__Non-School', 'Reporting__School', 'Sharing__Leadership & Management', 'Sharing__NO_LABEL', 'Sharing__School Reported', 'Sharing__School on Central Budgets', 'Sharing__Shared Services', 'Student_Type__Alternative', 'Student_Type__At Risk', 'Student_Type__ELL', 'Student_Type__Gifted', 'Student_Type__NO_LABEL', 'Student_Type__Poverty', 'Student_Type__PreK', 'Student_Type__Special Education', 'Student_Type__Unspecified', 'Use__Business Services', 'Use__ISPD', 'Use__Instruction', 'Use__Leadership', 'Use__NO_LABEL', 'Use__O&M', 'Use__Pupil Services & Enrichment', 'Use__Untracked Budget Set-Aside')

sub <- sub[,1]
for (cat in category){
dat = read.csv(paste0('Submission/',cat,'PredSubmit.csv'))
sub <- cbind(sub, dat[,2:ncol(dat)])
}
colnames(sub) <- labels
write.csv(sub, 'Submission/submit1.csv', row.names = FALSE)

# column labels are a terrible problem in R because of required punctuation;  ugly but functional:
# copy and paste column headers in a text editor.
