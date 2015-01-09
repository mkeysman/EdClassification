from pandas import read_csv, DataFrame
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import cross_val_score
from sklearn.externals import joblib
from numpy import savetxt

category = ['Function', 'Object_Type', 'Operating_Status', 'Position_Type', 'Pre_K', 'Reporting', 'Sharing', 'Student_Type', 'Use']
for cat in category:
    filename = 'DataSets/' + cat + 'Test.csv'
    filename1 = 'DataSets/' + cat + 'RFModel.pkl'
    filename2 = 'Submission/' + cat + 'PredSubmit.csv'
    rfc = joblib.load(filename1)
    df = read_csv(filename)
    data = df.iloc[:,1:]
    print(data.shape)
    out = rfc.predict_proba(data)    
    dataout = DataFrame(out)
    dataout.to_csv(filename2)

