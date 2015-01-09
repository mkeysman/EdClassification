from pandas import read_csv
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import cross_val_score
from sklearn.externals import joblib



rfc = RandomForestClassifier(n_estimators = 100)
cat = 'Reporting'
filename = 'TrainPartition.csv'
filename1 = 'RFModel.pkl'
df = read_csv(filename)
target = df.iloc[:,1]
data = df.iloc[:,2:]
rfc.fit(data, target)
scores = cross_val_score(rfc, data, target, cv = 10)
print('Accuracy: %0.2f (+/- %0.2f)' %(scores.mean(), scores.std()*2))
joblib.dump(rfc, filename1)
