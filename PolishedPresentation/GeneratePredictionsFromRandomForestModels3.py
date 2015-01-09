from pandas import read_csv, DataFrame
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import cross_val_score
from sklearn.externals import joblib
from numpy import savetxt

category = ['Function', 'Object_Type', 'Operating_Status', 'Position_Type', 'Pre_K', 'Reporting', 'Sharing', 'Student_Type', 'Use']
for cat in category:
    filename = 'DataSets/' + cat + 'Train.csv'
    filename1 = 'DataSets/' + cat + 'RFModel.pkl'
    filename2 = 'DataSets/' + cat + 'PredClassTrain.csv'
    rfc = joblib.load(filename1)
    df = read_csv(filename)
    data = df.iloc[:,2:]
    print(data.shape)
    out = rfc.predict(data)    
    dataout = DataFrame(out)
    dataout.to_csv(filename2)

