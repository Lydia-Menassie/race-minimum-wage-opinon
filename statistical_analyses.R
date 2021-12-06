#R code for "Public Opinion & Labor Policy: Does self-identified race affect opinion on minimum wage?".
#Below is code and data testing relationships between an individuals' self-identified race to their opinion on raising the minimum wage.
#Author: Lydia Menassie

library(haven)
setwd ("/Users/lydiamenassie/Desktop/Research & Analysis")
anes <- read_sav("anes_timeseries_2020_spss_20210324.sav")

install.packages("car")
library(car)

#MINIMUM WAGE
#1 = Eliminated, 2 = Lowered, 3 = Kept the same, 4 = Raised, 
anes$min_wage <- recode(anes$V202377, "4=1; 3=2; 2=3; 1=4; else=NA")
table(anes$min_wage, anes$race)

#RACE
#1. White, non-Hispanic, 2. Black, non-Hispanic, 3. Hispanic, 4. Asian or Native Hawaiian/other Pacific Islander, non-Hispanic alone, 5. Native American/Alaska Native or other race, non-Hispanic alone 6. Multiple races, non-Hispanic
anes$race <- recode(anes$V201549x, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; else=NA")

wage_raised <- subset(anes, min_wage==4)
wage_same <- subset(anes, min_wage==3)
wage_lowered <- subset(anes, min_wage==2)
wage_elim <- subset(anes, min_wage==1)

table(wage_raised$race)
table(wage_same$race)
table(wage_lowered$race)
table(wage_elim$race)

#proportions 
wage.race <- prop.table(table(anes$min_wage, anes$race), 2)
wage.race * 100


#zero comparison test:
#0=White, 1=NONWhite
anes$nonwhite <- recode(anes$race, "1=0; 2:6=1; else=NA")
#both nonwhite and white
table(anes$nonwhite)


#chi test 
chisq.test(anes$min_wage, anes$race)


# linear regression:
#DV: opinion, IV: nonwhite
wage.lm <- lm(min_wage~nonwhite, data=anes)
summary(wage.lm)


#controlled comparison:
#1 - gender
#0=Male, Female=1
#table for controlling for gender:
anes$female <- recode(anes$V201600, "1=0; 2=1; else=NA")
table(anes$female)
men <- subset(anes, female==0)
wom <- subset(anes, female==1)

prop.table(table(men$min_wage, men$nonwhite), 2) * 100#Table for men
prop.table(table(wom$min_wage, wom$nonwhite), 2) * 100 #Table for women

#frequencies for gender 
#men
table(subset(men, min_wage==1)$nonwhite)
table(subset(men, min_wage==2)$nonwhite)
table(subset(men, min_wage==3)$nonwhite)
table(subset(men, min_wage==4)$nonwhite)

#women
table(subset(wom, min_wage==1)$nonwhite)
table(subset(wom, min_wage==2)$nonwhite)
table(subset(wom, min_wage==3)$nonwhite)
table(subset(wom, min_wage==4)$nonwhite)

#chi-square test controlling for gender:
chisq.test(men$nonwhite, men$min_wage)
chisq.test(wom$nonwhite, wom$min_wage)

#2 - age
#re-coding:
#18-34 = 1, #34-64=2, #over65=3
anes$age <- recode(anes$V201507x, "18:34=1; 34:64=2; -9=NA; else=3;")

young <- subset(anes, age==1)
middle <- subset(anes, age==2)
old <- subset(anes, age==3)

#table controlling for age:
prop.table(table(young$min_wage, young$nonwhite), 2) * 100 #Table for 18:34 
prop.table(table(middle$min_wage, middle$nonwhite), 2) * 100 #Table for 34:64 
prop.table(table(old$min_wage, old$nonwhite), 2) * 100 #Table for over65


#frequencies for age
#young
table(subset(young, min_wage==1)$nonwhite)
table(subset(young, min_wage==2)$nonwhite)
table(subset(young, min_wage==3)$nonwhite)
table(subset(young, min_wage==4)$nonwhite)

#middle
table(subset(middle, min_wage==1)$nonwhite)
table(subset(middle, min_wage==2)$nonwhite)
table(subset(middle, min_wage==3)$nonwhite)
table(subset(middle, min_wage==4)$nonwhite)

#old
table(subset(old, min_wage==1)$nonwhite)
table(subset(old, min_wage==2)$nonwhite)
table(subset(old, min_wage==3)$nonwhite)
table(subset(old, min_wage==4)$nonwhite)


#chi-square test controlling for age:
chisq.test(young$nonwhite, young$min_wage)
chisq.test(middle$nonwhite, middle$min_wage)
chisq.test(old$nonwhite, old$min_wage)


#3 - education
#1 = Less than high school credential, 2 = High school credential, 3 = Some post-high school, no bachelors degree, 4 = Bachelors degree, 5 = Graduate degree
anes$educ <- recode(anes$V201511x, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
table(anes$educ)
#subsets:
no_degree <- subset(anes, educ<=3)
degree_more<- subset(anes, educ>=4)

#table for controlling for education:
prop.table(table(no_degree$min_wage, no_degree$nonwhite), 2) * 100 #Table for no college degree
prop.table(table(degree_more$min_wage, degree_more$nonwhite), 2) * 100 #Table for college degree or more 

#frequencies 
#no_degree
table(subset(no_degree, min_wage==1)$nonwhite)
table(subset(no_degree, min_wage==2)$nonwhite)
table(subset(no_degree, min_wage==3)$nonwhite)
table(subset(no_degree, min_wage==4)$nonwhite)


#degree_more
table(subset(degree_more, min_wage==1)$nonwhite)
table(subset(degree_more, min_wage==2)$nonwhite)
table(subset(degree_more, min_wage==3)$nonwhite)
table(subset(degree_more, min_wage==4)$nonwhite)

#chi-square test - controlling for education
chisq.test(no_degree$nonwhite, no_degree$min_wage)
chisq.test(degree_more$nonwhite, degree_more$min_wage)


#4 - Ideology
#1 = extremely liberal; 2 = liberal; 3 = slightly liberal; 4 = moderate; 5 = slightly conservative; 6 = conservative; 7 = extremely conservative
anes$ideology <- recode(anes$V201200, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")

liberal <- subset(anes, ideology<=3)
conservative <- subset(anes, ideology>=5)

#table for controlling for Ideology:
prop.table(table(liberal$min_wage, liberal$nonwhite), 2) * 100 #Table for no college degree
prop.table(table(conservative$min_wage, conservative$nonwhite), 2) * 100 #Table for college degree or more 


#frequencies for ideology
#liberal
table(subset(liberal, min_wage==1)$nonwhite)
table(subset(liberal, min_wage==2)$nonwhite)
table(subset(liberal, min_wage==3)$nonwhite)
table(subset(liberal, min_wage==4)$nonwhite)


#conservative
table(subset(conservative, min_wage==1)$nonwhite)
table(subset(conservative, min_wage==2)$nonwhite)
table(subset(conservative, min_wage==3)$nonwhite)
table(subset(conservative, min_wage==4)$nonwhite)

#chi-squared - Ideology:
chisq.test(liberal$nonwhite, liberal$min_wage)
chisq.test(conservative$nonwhite, conservative$min_wage)


#5 - Party ID
#1 = Democratic party, 2 = Republican party, 3 = None or ‘independent’
anes$pid_3 <- recode(anes$V201018, "1=1; 4=2; 2=3; else=NA")

dem <- subset(anes, pid_3==1)
ind <- subset(anes, pid_3==2)
rep <- subset(anes, pid_3==3)


prop.table(table(dem$min_wage, dem$nonwhite), 2) * 100 #Table for democrats
prop.table(table(ind$min_wage, ind$nonwhite), 2) * 100 #Table for independents
prop.table(table(rep$min_wage, rep$nonwhite), 2) * 100 #Table for republicans

#frequencies party identification
#dem
table(subset(dem, min_wage==4)$nonwhite)
table(subset(dem, min_wage==3)$nonwhite)
table(subset(dem, min_wage==2)$nonwhite)
table(subset(dem, min_wage==1)$nonwhite)

#ind
table(subset(ind, min_wage==4)$nonwhite)
table(subset(ind, min_wage==3)$nonwhite)
table(subset(ind, min_wage==2)$nonwhite)
table(subset(ind, min_wage==1)$nonwhite)

#rep
table(subset(rep, min_wage==4)$nonwhite)
table(subset(rep, min_wage==3)$nonwhite)
table(subset(rep, min_wage==2)$nonwhite)
table(subset(rep, min_wage==1)$nonwhite)


#chi-squared - Ideology:
chisq.test(dem$nonwhite, dem$min_wage)
chisq.test(ind$nonwhite, ind$min_wage)
chisq.test(rep$nonwhite, rep$min_wage)


#multivariate regression:
#recode:
#white:
anes$white <- recode(anes$V201549x, "1=1; -9=NA; -8=NA; else=0")
#black
anes$black <- recode(anes$V201549x, "2=1; -9=NA; -8=NA; else=0")
#hispanic
anes$hispanic <- recode(anes$V201549x, "3=1; -9=NA; -8=NA; else=0")
#asian
anes$asian <- recode(anes$V201549x, "4=1; -9=NA; -8=NA; else=0")
#native american
anes$native_american <- recode(anes$V201549x, "5=1; -9=NA; -8=NA; else=0")
#multiple races
anes$multiple_races <- recode(anes$V201549x, "6=1; -9=NA; -8=NA; else=0")

mult_wage.lm <- lm(min_wage~white+black+hispanic+asian+native_american+multiple_races+female+age+educ+ideology+pid_3, data=anes)
summary(mult_wage.lm)


# Sample characteristics
# Dependent Variable
mean(anes$min_wage, trim = 0, na.rm = TRUE)

# Independent Variable
prop.table(table(anes$race)) * 100

# Controls
anes$educ_binary <- recode(anes$educ, "1:3=0; 3:5=1; else=NA")
anes$ideology_binary <- recode(anes$ideology, "1:3=0; 5:7=2; else=NA")
prop.table(table(anes$female)) * 100
prop.table(table(anes$age)) * 100
prop.table(table(anes$educ_binary)) * 100
prop.table(table(anes$ideology_binary)) * 100
prop.table(table(anes$pid_3)) * 100
