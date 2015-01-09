file = 'PolishedPresentation/Submission/submit1.csv'
file1 = 'PolishedPresentation/labels.csv'
f = open(file, 'r')
line = f.readline()
print(line)
f.close
f = open(file1, 'w')
f.write(line)
f.close
