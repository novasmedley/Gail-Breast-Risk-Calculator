### STATIC VARIABLES
h2 = c(49.3,53.1,62.5,82.5,130.7,218.1,365.5,585.2,943.9,1502.8,2383.9,3883.2,6682.8,144490.8) # 14 age intervals for the woman
h1_star = c(1.0,7.6,26.6,66.1,126.5,186.6,221.1,272.1,334.8,392.3,417.8,443.9,442.1,410.9) # for the woman
F = c(0.5788413,0.5788413); #age <50, and age >=50
# h2 = c(44.12,52.54,67.46,90.92,125.34,195.70,329.84,546.22,910.35,1418.54,2259.35,3611.46,6136.26,14206.63) # for the AVERAGE woman
# h1_star = c(1.22,7.41,22.97,56.49,116.45,195.25,261.54,302.79,367.57,420.29,473.08,494.25,479.76,401.06) #avg woman
# F = c(1.0,1.0) # AVG WOMAN
beta = data.frame(-0.7494824600,0.0108080720,0.0940103059,0.5292641686,0.2186262218,0.9583027845,-0.2880424830,-0.1908113865) #gail model
colnames(beta) <- c('intercept','ageCat','ageMen','nBiops','ageFLB','numRel','nBiopsAndAgeCat','ageFBLAndNumRel')
h1 <- rep(1,14)
ageIntervals <- seq(20,90,by=5)

h2 <- h2*0.00001
h1_star <- h1_star*0.00001
for (j in 1:7) {h1[j] = h1_star[j]*F[1]}
for (j in 8:14) {h1[j] = h1_star[j]*F[2]}


### FUNCTIONS
# categorize risk factors
getAgeCategory <- function(age) {
  value=0
  if (age>=50) value=1
  return(value)
}
getAgeMenCategory <- function(age) {
  value=0
  if (age<12) value=2
  else if (age<=13) value=1
  return(value)
}
getNBiopsCategory <- function(num) {
  value=num
  if (num>=2) value=2
  return(value)
}
getAgeFBLCaetgory <- function(age) {
  value=0
  if (age>=30) value=3
  else if ((age<=29 && age>=25) || age==0) value=2
  else if (age>=20) value=1
  return(value)
}
getNumRelCategory <- function(num){
  value=num
  if(num>=2) value=2
  return(value)
}
CalculateRelativeRisk <- function(riskGroupInfo) {
  betaTotal = sum(riskGroupInfo*beta[1, ])
  relativeRisk = exp(betaTotal)
}
CalculateAbsoluteRisk <- function(jStart,jEnd,relativeRisk){
  prob = 0;
  # calculate all the needed s1, s2 values, which has 1.0 in position 1, and the rest represent age intervals
  s1T <- rep(1,jEnd+1)
  s2T <- rep(1,jEnd+1)
  for (i in 2:(jEnd+1)){
    s1T[i] <- s1T[i-1]*exp(-(h1[i-1]*relativeRisk*5))
    s2T[i] <- s2T[i-1]*exp(-(h2[i-1]*5))
  }
  for (j in jStart:jEnd) { #s1 and s2 at index j is one age interval behind h1 and h2
    prob = prob + ((h1[j]*relativeRisk)/(h1[j]*relativeRisk+h2[j]))*(s1T[j]/s1T[jStart])*(s2T[j]/s2T[jStart])*(1.0 - exp(-5*(h1[j]*relativeRisk+h2[j])) )
  }
  return(prob*100)
}
GetProbability <- function(age,laterAge,ageMen,nBiops,ageFLB,numRel) { 
  riskGroupInfo <- rep(0,length(beta))
  riskGroupInfo[1] = 1
  riskGroupInfo[2] = getAgeCategory(age)
  riskGroupInfo[3] = getAgeMenCategory(ageMen)
  riskGroupInfo[4] = getNBiopsCategory(nBiops)
  riskGroupInfo[5] = getAgeFBLCaetgory(ageFLB)
  riskGroupInfo[6] = getNumRelCategory(numRel)
  riskGroupInfo[7] = riskGroupInfo[4]*riskGroupInfo[2]
  riskGroupInfo[8] = riskGroupInfo[5]*riskGroupInfo[6]
  
  jStart <- findInterval(age,ageIntervals,rightmost.closed=TRUE)
  jEnd <- findInterval(laterAge,ageIntervals,rightmost.closed=TRUE)
  
  RR = CalculateRelativeRisk(riskGroupInfo)
  P = CalculateAbsoluteRisk(jStart,jEnd,RR)
  
  return(P)
}

### DATA
# test
# age = 35
# laterAge = age+5
# ageMen = 14
# nBiops = 0
# ageFLB = 19
# numRel = 0
# P = GetProbability(age,laterAge,ageMen,nBiops,ageFLB,numRel)

