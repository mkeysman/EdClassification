######################################################################
# Goal is described in CompetitionDescription.pdf
# For the category
# Object_Type
# want to find features to help distinguish between


# NO_LABEL
# Special Education
# Unspecified

# the given expenditure must be placed in the appropriate classification.
# want to create a text file for 
# ByCategory x ByClassification x ExplanatoryVariable
##################################################################

train <- read.csv('/home//michael/Data//EdClassification/TrainingData.csv')

ExplanVar <-c('Facility_or_Department', 'Function_Description', 'Fund_Description', 'Job_Title_Description', 'Location_Description', 'Object_Description', 'Position_Extra', 'Program_Description', 'SubFund_Description', 'Sub_Object_Description', 'Text_1', 'Text_2')
classStudent <- c('NO_LABEL',  'Special Education', 'Unspecified')

cat <- 'Student_Type'
  colCat <- match(cat, colnames(train))
  for (var in ExplanVar){
    dir.create(paste0(var))
    colVar <- match(var, colnames(train))
    for (i in classStudent){
      entries <- train[train[,colCat] == i,colVar]
      text <- tolower(toString(entries))
      text <- gsub("[[:punct:]]", "", as.character(text))
      filename <- paste0(var,'/',gsub('/', '_', i,'.txt'))
      writeLines(text, filename)
    }
  }
   


# produce document term matrices

library(tm)

for (var in ExplanVar){
  corp <- Corpus(DirSource(var))
  corp <- tm_map(corp, stripWhitespace)
  corpMat <- DocumentTermMatrix(corp)
  matr <- inspect(corpMat)
  keepers <- c()
  for (i in 1:ncol(matr)){
    if (max(matr[,i]) > 7500){keepers <- c(keepers,i)}
  }
  matr <- matr[,keepers]
  write.csv(matr, paste0(var,'DocTermMatrix.csv'))
}


# Add these words to the training and test features

features <- read.csv('/home//michael/Data//EdClassification/trainFeatures.csv')
features[,1] <- train[,1]
features <- features[,c(1,6,12:279)]
colnum <- 271
for (var in ExplanVar){
  dat <- read.csv(paste0(var,'DocTermMatrix.csv'))
  words <- colnames(dat)
  colVar <- match(var, colnames(train))
  for (word in words[2:ncol(dat)]){
    tested <- grep(word, tolower(train[,colVar]))
    features[,colnum] <- c(rep(0, nrow(train)))
    features[tested,colnum] <-1
    names(features)[colnum]<-paste0(var,'_',word)
    colnum <- colnum + 1
    print(colnum)
  }
}

write.csv(features, 'trainFeatures.csv', row.names = FALSE)

feature <- read.csv('/home//michael/Data//EdClassification/testFeatures.csv')
test <- read.csv('/home//michael/Data//EdClassification/TestData.csv')
colnum <- 270
for (var in ExplanVar){
  dat <- read.csv(paste0(var,'DocTermMatrix.csv'))
  words <- colnames(dat)
  colVar <- match(var, colnames(test))
  for (word in words[2:ncol(dat)]){
    tested <- grep(word, tolower(test[,colVar]))
    feature[,colnum] <- c(rep(0, nrow(test)))
    feature[tested,colnum] <-1
    names(feature)[colnum]<-paste0(var,'_',word)
    colnum <- colnum + 1
    print(colnum)
  }
}

write.csv(feature, 'testFeatures.csv', row.names = FALSE)

# produce partition for python random forest training
library(caret)

  ind <- createDataPartition(features[,2], p = .15, list = FALSE, times = 1)
  feat <- features[ind,]
  write.csv(feat, paste0('TrainPartition.csv'), row.names = FALSE)



############################################################################

# Generate a random forest model in scikit learn:

# GenerateRandomForestModels.py
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
# filename = 'TrainPartition.csv'
# filename1 = 'RFModel.pkl'
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

# ClassifyInStudent.py

# from pandas import read_csv, DataFrame
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.cross_validation import cross_val_score
# from sklearn.externals import joblib
# from numpy import savetxt
# 
# cat = 'Function'
# filename = 'testFeatures.csv'
# filename1 = 'RFModel.pkl'
# filename3 = 'PredictProbs.csv'
# rfc = joblib.load(filename1)
# df = read_csv(filename)
# data = df.iloc[:,1:]
# print(data.shape)
# out = rfc.predict_proba(data)    
# dataout = DataFrame(out)
# dataout.to_csv(filename3)


#########################################################################################


# Construct the submission


dat = read.csv('/home/michael/Data/EdClassification/PolishedPresentation/Submission/submit7.csv')
pred <- read.csv('PredictProbs.csv')
dat[,89:97] <- pred[,2:10]

write.csv(dat, '/home/michael/Data/EdClassification/PolishedPresentation/Submission/submit7.csv', row.names = FALSE)

# column labels are a terrible problem in R because of required punctuation;  ugly but functional:
# copy and paste column headers in a text editor.
