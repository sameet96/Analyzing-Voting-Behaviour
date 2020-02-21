
rm(list=ls()) # remove all existing objects in the environment
gc() # garbage collection
## above two lines are very standard (personally standard)

## read in data 
setwd('C:/Users/samee/Downloads')
# only forward or double backward slash in the path is allowed
# PC users: your path is in backward slash by default. it will give you error message if you do not change them

dat = read.csv('Gaming Data Set.csv', head=T, stringsAsFactors=F, na.strings='') # 'read.csv' reads in csv file
# head tells if the first line is variable name
# na.strings tells what will be considered NA or missing

View(dat) # 'View' gives a view of the data
dim(dat) # 'dim' gives the dimensions of the data: # of rows, # of columns

matrix.na = is.na(dat)

##################################################################################################################

# percent of male and female dosent match to 100%. Data consist error value of 5.82%

##################################################################################################################

totalNumberCalculaated = dat$Percent_Male + dat$Percent_Female
matrixVector = totalNumberCalculaated == 1
print(matrixVector)
table(matrixVector)
table(totalNumberCalculaated > 1 | totalNumberCalculaated < 1)
table(totalNumberCalculaated > 1)
incorrectValueInMaleFemale = (75/1287)*100
print(incorrectValueInMaleFemale)
##################################################################################################################

# checking population

##################################################################################################################
populationCalculated = dat$Age_18_24 + dat$Age_24_44 + dat$Age_44_64 + dat$Age_Less_than_18 + dat$Age_Older_than_64
matrixForPopulationVerification = populationCalculated == dat$Population
table(matrixForPopulationVerification)
matrixForPopulationVerification
populationCalculated

#checking difference in the derived population value and population value from data set

differenceInPopulation = abs(populationCalculated - dat$Population)
differenceInPopulation

#checking how much data is incorrect by more than 5%
# we get that 2.18% of data is incorrect and 10% of data is incorrect by more than 1%

percentWrong = differenceInPopulation / dat$Population
table(percentWrong > 0.05)
28/1259

##################################################################################################################

#checking data of different races 

##################################################################################################################
derivedTotalPercentOfRaces = dat$Percent_Black + dat$Percent_White +dat$Percent_Other
derivedTotalPercentOfRaces
matrixForRaces = derivedTotalPercentOfRaces == 1
table(matrixForRaces)
293/1287
matrixForRaces
#checking for the difference greater than 1%

print(derivedTotalPercentOfRaces > 1.01 | derivedTotalPercentOfRaces < 0.99)
table(derivedTotalPercentOfRaces > 1.01 | derivedTotalPercentOfRaces < 0.99)
# We can observe that maximum difference is of 1% hence we can ignore it as it may be caused due to rounding up

##################################################################################################################

#checking data of population density. We can derive it by formula : population/size of county

##################################################################################################################
derivedPopulationDensity = dat$Population / dat$Size_of_County
print(derivedPopulationDensity)
roundedPopulationDensity = round(derivedPopulationDensity, digits = 2)
table(derivedPopulationDensity == dat$Population_Density)
#All the values are different in thederivedDensity and actual population density. Lets explore
#checking difference in both derived and actual
difference = derivedPopulationDensity - dat$Population_Density
differencePercent = (abs(difference) / dat$Population_Density)*100
print(differencePercent)
greaterThanThreePercent = differencePercent > 3
table(greaterThanThreePercent)
print(greaterThanThreePercent)
# replacing population density
# As when we derived the value of population density it was wrong and there was a difference between derived 
# values and actual values so we will replace the original column by our derived column

dat[, "Population_Density"]  = roundedPopulationDensity
View(dat)
#dat1 = read.csv('Gaming Data.csv', head=T, stringsAsFactors=F, na.strings='') # 'read.csv' reads in csv file
#View(dat1)
##################################################################################################################

#checking data of vote_for and vote_against compared to total_votes

##################################################################################################################
derivedTotalVotes = dat$Votes_Against + dat$Votes_For
table(derivedTotalVotes == dat$Total_Votes)

#the derived data is correct



##################################################################################################################

#Finding the percentage of church going members

##################################################################################################################
View(dat)
which(dat$No_of_Church_Members == "#NULL!")
dat1 = dat[-497, ]
which(dat1$No_of_Church_Members == "#NULL!")
View(dat1)
matrix.na = is.integer(dat1$No_of_Church_Members)
table(matrix.na)
dat2 = as.numeric(dat1$No_of_Church_Members)
dat2
dat1$No_of_Church_Members = NULL
dat1 = cbind(dat1, dat2)
View(dat1)
library(plyr)
dat1 = rename(dat1, c('dat2' = 'Number_Of_Church_Going_memebers_Derived'))
View(dat1)
rownames(dat1)=seq(length=nrow(dat1))
View(dat1)
rownames(dat1)
dat1$percent_Of_Church_going_members = (dat1$Number_Of_Church_Going_memebers_Derived / dat1$Population) * 100
table(dat1$percent_Of_Church_going_members > 50)
dat1$percent_Of_Church_going_members = round(dat1$percent_Of_Church_going_members, digits = 2)
View(dat1)

##################################################################################################################
#
#Finding the number of church distribution

##################################################################################################################


#Finding out the number of church distribution in a county
dat1$numberofChurches_per_sqkm = round(dat1$No_of_Churches/dat1$Size_of_County, digits = 2)
View(table(dat1$numberofChurches_per_sqkm > .5))
View(dat1)


##################################################################################################################
#
#Creating dummy variables for number of church going members greater than 50%

##################################################################################################################
View(dat1)
dat1$percent_Of_Church_going_members = as.integer((dat1$percent_Of_Church_going_members))
which(dat1$percent_Of_Church_going_members > 50) = which(dat1)
#if((dat1$percent_Of_Church_going_members > 50)){
matrixOfChurchGoing = dat1$percent_Of_Church_going_members >= 50
table(matrixOfChurchGoing)
str(dat1$percent_Of_Church_going_members)
dat1$percent_Of_Church_going_members = as.numeric(dat1$percent_Of_Church_going_members)
dat1$percent_Of_Church_going_members = replace(dat1$percent_Of_Church_going_members, (which(dat1$percent_Of_Church_going_members >= 50)), 'high')
dat1$percent_Of_Church_going_members = as.factor(replace(dat1$percent_Of_Church_going_members, (which(dat1$percent_Of_Church_going_members != 'high')), 'low'))  
View(dat1)

table(dat1$percent_Of_Church_going_members)
#else{replace(dat1$percent_Of_Church_going_members, (which(dat1$percent_Of_Church_going_members < 50)), 'low')}
str(dat1$per)
############################################### 30%  ##################################################################

table(dat1$percent_Of_Church_going_members > 70)
table(dat1$percent_Of_Church_going_members < 30)
table(30 < dat1$percent_Of_Church_going_members < 70)


dat1$percent_Of_Church_going_members = as.numeric(dat1$percent_Of_Church_going_members)

dat1$percent_Of_Church_going_members = replace(dat1$percent_Of_Church_going_members, (which(dat1$percent_Of_Church_going_members < 30)), 0)

dat1$percent_Of_Church_going_members = as.integer(replace(dat1$percent_Of_Church_going_members, ((which(dat1$percent_Of_Church_going_members > 70))), 2))  

dat1$percent_Of_Church_going_members = as.factor(replace(dat1$percent_Of_Church_going_members, (which(dat1$percent_Of_Church_going_members != 0 & dat1$percent_Of_Church_going_members != 2)), 1))
View(dat1)

table(dat1$percent_Of_Church_going_members)



###################################################################################################################
#Deriving a column for population legally allowed to vote
##################################################################################################################
dat1$legalPopulationToVote = dat1$Age_18_24 +dat1$Age_24_44 +dat1$Age_44_64 +dat1$Age_Older_than_64
table(dat1$legalPopulationToVote < dat1$Total_Votes )
incorrectValue = which(dat1$legalPopulationToVote < dat1$Total_Votes)
incorrectValue
for (variable in incorrectValue) {
  print(variable)
  x = dat1$Votes_For[variable, ] - dat1$legalPopulationToVote[variable, ]
  print(x)
}
differenctInTotalVotes = dat1$Total_Votes[dat1$legalPopulationToVote < dat1$Total_Votes,  ] - dat1$legalPopulationToVote[dat1$legalPopulationToVote < dat1$Total_Votes,  ]
View(dat1)


##################################################################################################################

#removing irelevant data
##################################################################################################################


dat1$MSA = NULL
dat1$Percent_Minority = NULL
dat1$Age_Less_than_18 = NULL
dat1$Ballot_Type = NULL
write.csv(dat1, "Gaming_Data_Preprocessed_Final.csv")




##################################################################################################################

#checking outliers

##################################################################################################################

hist(dat[,'Age_Less_than_18'])
par(1,1)
hist(dat1$Age_Less_than_18)
hist((dat$Age_18_24))
plot((dat$Age_24_44))
plot((dat$Age_44_64))
plot((dat$Age_Older_than_64))

plot((dat$Poverty_Level_Rate))

hist(dat$Poverty_Level_Rate)

hist(as.numeric(dat1$Per_Capita_Income))
View(dat1)
dat1$Per_Capita_Income= as.factor(dat1$Per_Capita_Income)
hist(dat1$Per_Capita_Income)
str(dat1$Per_Capita_Income)

boxplot(dat1$Votes_For)
hist(dat1$Votes_For)
which(dat1$Votes_For > 100000)
View(dat1)
