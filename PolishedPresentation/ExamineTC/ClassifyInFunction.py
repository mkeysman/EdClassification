from pandas import read_csv, DataFrame
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import cross_val_score
from sklearn.externals import joblib
from numpy import savetxt
import os

os.chdir('/home/michael/Data/EdClassification/PolishedPresentation')
cat = 'Function'
filename = 'DataSets/' + cat + 'Test.csv'
filename1 = 'DataSets/' + cat + 'RFModel.pkl'
filename2 = 'ExamineTC/' + cat + 'Pred.csv'
filename3 = 'ExamineTC/' + cat + 'PredProb.csv'
rfc = joblib.load(filename1)
df = read_csv(filename)
data = df.iloc[:,1:]
print(data.shape)
out = rfc.predict(data)    
dataout = DataFrame(out)
dataout.to_csv(filename2)
out = rfc.predict_proba(data)    
dataout = DataFrame(out)
dataout.to_csv(filename3)

