from pandas import read_csv, DataFrame
from sklearn import linear_model
from sklearn.cross_validation import cross_val_score
from sklearn.externals import joblib



logMult = linear_model.SGDClassifier(loss = 'log')
cat = 'Function'
filename = 'trainsFunction.csv'
filename1 = 'Function/LogisticFunction.pkl'
filename2 = 'logFunction.csv'
df = read_csv(filename)
target = df.iloc[:,1]
data = df.iloc[:,2:]
logMult.fit(data, target)
print(logMult.score(data,target))
joblib.dump(logMult, filename1)
out = logMult.predict_proba(data)
dataout = DataFrame(out)
dataout.to_csv(filename2)

