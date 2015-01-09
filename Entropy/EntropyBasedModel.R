# Generate a new submission based on Entropy splits in the random forests.

sub <- read.csv('/home//michael/Data//EdClassification/PolishedPresentation//Submission/submit11.csv')

# Build a random forest with entropy based splitting rules for each category by 
# 
# GenerateEntropyForestModel.py 
# 
# and generate predictions using 
# 
# ClassifyByEntropy<Cat>.py
# 
# within the appropriate Refine<Cat> folder

dat <- read.csv('/home//michael/Data//EdClassification/RefineFunction/PredEnropyProbs.csv')

sub[2:38] <- dat[2:38]

dat <- read.csv('/home//michael/Data//EdClassification/RefineObject/PredEnropyProbs.csv')

sub[39:49] <- dat[2:12]

dat <- read.csv('/home//michael/Data//EdClassification/RefineOperating/PredEnropyProbs.csv')

sub[50:52] <- dat[2:4]

dat <- read.csv('/home//michael/Data//EdClassification/RefinePosition/PredEnropyProbs.csv')

sub[53:77] <- dat[2:26]

dat <- read.csv('/home//michael/Data//EdClassification/RefinePreK/PredEnropyProbs.csv')

sub[78:80] <- dat[2:4]

dat <- read.csv('/home//michael/Data//EdClassification/RefineReporting/PredEnropyProbs.csv')

sub[81:83] <- dat[2:4]

dat <- read.csv('/home//michael/Data//EdClassification/RefineSharing/PredEnropyProbs.csv')

sub[84:88] <- dat[2:6]

dat <- read.csv('/home//michael/Data//EdClassification/RefineStudent/PredEnropyProbs.csv')

sub[89:97] <- dat[2:10]

dat <- read.csv('/home//michael/Data//EdClassification/RefineUse/PredEnropyProbs.csv')

sub[98:105] <- dat[2:9]

write.csv(sub, '/home//michael/Data//EdClassification/PolishedPresentation//Submission/submit10.csv')
