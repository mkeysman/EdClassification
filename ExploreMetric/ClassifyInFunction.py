from pandas import read_csv, DataFrame
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import cross_val_score
from sklearn.externals import joblib
from numpy import savetxt

cat = 'Function'
filename = 'TestMetricPlaypen.csv'
filename1 = '/home/michael/Data/EdClassification/RefineFunction/RFModel.pkl'
filename3 = 'PredictProbsFunction.csv'
filename4 = 'PredClassFunction.csv'
rfc = joblib.load(filename1)
df = read_csv(filename)
data = df.iloc[:,2:]
print(data.shape)
out = rfc.predict_proba(data)    
dataout = DataFrame(out)
dataout.to_csv(filename3)
out = rfc.predict(data)    
dataout = DataFrame(out)
dataout.to_csv(filename4)

