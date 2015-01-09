from pandas import read_csv, DataFrame
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import cross_val_score
from sklearn.externals import joblib
from numpy import savetxt

cat = 'Object_Type'
filename = 'testFeatures.csv'
filename1 = 'RFEntropyModel.pkl'
filename3 = 'PredEnropyProbs.csv'
rfc = joblib.load(filename1)
df = read_csv(filename)
data = df.iloc[:,1:]
print(data.shape)
out = rfc.predict_proba(data)    
dataout = DataFrame(out)
dataout.to_csv(filename3)

