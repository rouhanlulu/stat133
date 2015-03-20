# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output
  size=n.doctors*n.days
  has_adopted=matrix(rep(0,size),nrow=n.doctors,ncol=n.days,byrow=FALSE)
  has_adopted[,1]=t(t(initial.doctors))

  update.doctors=initial.doctors

  for (i in 2:n.days){
    a=sample(1:n.doctors,size=2,replace=FALSE)
    A1=a[1]
    A2=a[2]
    if (update.doctors[A1] == 1 & update.doctors[A2] == 0){
      update.doctors[A2] <- sample(c(1,0), 1, prob=c(p,1-p))
      has_adopted[,i] <- t(t(update.doctors))
    } else if (update.doctors[A1] == 0 & update.doctors[A2]  == 1){
      update.doctors[A1] <- sample(c(1,0), 1, prob=c(p,1-p))
      has_adopted[,i] <- t(t(update.doctors))
    } else{
      has_adopted[,i] <- t(t(update.doctors))
    }
  }
  return (has_adopted)
}
# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
initial.doctors <- sample(c(1,0), 100, replace=T, prob= c(0.1, 0.9))
p1 <- sim.doctors(initial.doctors, n.doctors = 100, n.days=2500, p=0.11)
plot(1:ncol(trial1), colSums(trial1),col="red",type="l", xlab="Days", ylab="Number of Doctor's Adoption on the Drug",
     main="Stimulation of Drug Adoption",xlim=c(0,2500), ylim=c(0,100), las=1)

p2 <- sim.doctors(initial.doctors, n.doctors = 100, n.days=2500, p=0.24)
lines(1:ncol(trial2), colSums(trial2),col="green")

p3 <- sim.doctors(initial.doctors, n.doctors = 100, n.days=2500, p=0.36)
lines(1:ncol(trial3), colSums(trial3), col="yellow")

p4 <- sim.doctors(initial.doctors, n.doctors = 100, n.days=2500, p=0.47)
lines(1:ncol(trial4), colSums(trial4), col="pink")

p5 <- sim.doctors(initial.doctors, n.doctors = 100, n.days=2500, p=0.53)
lines(1:ncol(trial5), colSums(trial5), col="blue")
