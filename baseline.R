# h2 = c(49.3,43.1,62.5,82.5,130.7,218.1,365.5,585.2,943.9,1502.8,2383.2,3883.2,6682.8,144490.8) # 14 age intervals for the woman
h2 = c(44.12,52.54,67.46,90.92,125.34,195.70,329.84,546.22,910.35,1418.54,2259.35,3611.46,6136.26,14206.63) # for the AVERAGE woman
h2 <- h2*0.00001
# h1_star = c(1.0,7.6,26.6,66.1,126.5,186.6,221.1,272.1,334.8,392.3,417.8,443.9,442.1,410.9) # for the woman
h1_star = c(1.22,7.41,22.97,56.49,116.45,195.25,261.54,302.79,367.57,420.29,473.08,494.25,479.76,401.06) #avg woman
h1_star <- h1_star*0.00001
# F = c(0.5788413,0.5788413); #age <50, and age >=50
F = c(1.0,1.0) # AVG WOMAN
beta = c(-0.7494824600,0.0108080720,0.0940103059,0.5292641686,0.2186262218,0.9583027845,-0.2880424830,-0.1908113865)

h1 <- rep(1,14)
for (j in 1:7)
{
  h1[j] = h1_star[j]*F[1]
}

for (j in 8:14)
{
  h1[j] = h1_star[j]*F[2]
}

a = 40 #tao1
later_age = 90

calculate_prob <- function(age,later_age){
  prob = 0;
  if (age<50 && later_age<50) {
    #calclulate_risk() initial relative risk
    initial_r = exp(beta[1]+beta[2]*2)
    #get j interval based on age
    j_start = 5
    # get j interval based on later_age
    j_end = 14
    
    
    # calculate all the needed s1, s2 values
    s1_t <- rep(1,j_end+1)
    s2_t <- rep(1,j_end+1)
    for (i in 2:j_end+1){
      s1_t[i] <- s1_t[i-1]*exp(-h1[i]*initial_r*5)
      s2_t[i] <- s2_t[i-1]*exp(-h2[i]*5)
    }

    for (j in j_start:j_end) {
      prob = prob + ( (h1[j]*initial_r)/(h1[j]*initial_r+h2[j]) )*(s1_t[j+1]/s1_t[j_start])*(s2_t[j+1]/s2_t[j_start])*(1 - exp(-5*(h1[j]*initial_r+h2[j])) )
    }
  }
#   ifelse (age<50 && later_age>=50) {
#     #caclculate_risk() later relative risk
#   } 
#   ifelse (age>50){
#     
#   }
  
  return(prob*100)
}

p = calculate_prob(a,later_age)


