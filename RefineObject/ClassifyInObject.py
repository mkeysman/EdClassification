from pandas import read_csv, DataFrame
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import cross_val_score
from sklearn.externals import joblib
from numpy import savetxt

cat = 'Function'
filename = 'testFeatures.csv'
filename1 = 'RFModel.pkl'
filename3 = 'PredictProbs.csv'
rfc = joblib.load(filename1)
df = read_csv(filename)
data = df.iloc[:,1:]
print(data.shape)
out = rfc.predict_proba(data)    
dataout = DataFrame(out)
dataout.to_csv(filename3)

