################################################################################
# Practical 
# Fit an epidemic
# Last update by Kathy Leung
################################################################################
rm(list=ls());

# Q2
# Fit serial interval
library(MASS);
serialIntervalData = read.csv('data_interval_between_symptom_onset.csv',header=TRUE,stringsAsFactors=FALSE);
hist(serialIntervalData$infector_infectee_symptom_onset_interval,breaks=0:max(serialIntervalData$infector_infectee_symptom_onset_interval));
# Binomial distribution is not appropriate. It is often used when the number of "trials" is known.
# Due to rounding, the intervals contains zero; Add 1 day to all the intervals to make the estimation easier.
serialIntervalData$infector_infectee_symptom_onset_interval = serialIntervalData$infector_infectee_symptom_onset_interval+1;
# Mean serial interval would not change.
distrName = c('exponential','lognormal','gamma','weibull')
for (ii in 1:length(distrName))
{
  durInfFitted = fitdistr(serialIntervalData$infector_infectee_symptom_onset_interval,distrName[ii]);
  print(distrName[ii]);
  print(durInfFitted$estimate);
  print(durInfFitted$loglik);
  print(AIC(durInfFitted));
}
# Lognormal distribution is selected based on AIC
fitSerialInterval = fitdistr(serialIntervalData$infector_infectee_symptom_onset_interval,'lognormal');
meanSerialInterval = as.numeric(exp(fitSerialInterval$estimate[1]+0.5*fitSerialInterval$estimate[2]^2)-1);
print(meanSerialInterval);

# Q3
# Deterministic SIR model
deterministic_SIR_model <- function(Ro,durInf,totPopulation,seed,dt,tmax)
{
  numSteps = tmax/dt;
  #State variables
  S=rep(NA,numSteps);
  I=rep(NA,numSteps);
  R=rep(NA,numSteps);
  incidence=rep(NA,numSteps);
  time=rep(NA,numSteps);
  #Rate variables
  beta=Ro/durInf/totPopulation;
  gamma=1/durInf;
  #Initial condition
  I[1]=seed;
  R[1]=0;
  S[1]=totPopulation-I[1]-R[1];
  incidence[1]=0;
  time[1]=0;
  #Start simulation
  for (tt in 1: (numSteps-1))
  {
    numInfection=beta*S[tt]*I[tt]*dt;
    numRecovery=gamma*I[tt]*dt;
    # Update state variables
    S[tt+1]=S[tt]-numInfection;
    I[tt+1]=I[tt]+numInfection-numRecovery;
    R[tt+1]=R[tt]+numRecovery;
    incidence[tt+1]=numInfection;
    time[tt+1]=time[tt]+dt;
  }
  return(data.frame(time = time,
                    S = S,
                    I = I,
                    R = R,
                    incidence = incidence));
}
# Calculate daily incidence
dailyIncidence <- function(simRes,dt,repoRate)
{
  numSteps = length(simRes$incidence);
  numDays = numSteps*dt;
  dInc = rep(NA,numDays);
  for(ii in 1:numDays)
  {
    dInc[ii] = repoRate*sum(simRes$incidence[((ii-1)/dt+1):(ii/dt)]);
  }
  return(data.frame(daily_incidence = dInc));
}
# Define likelihood function
totalLogLikelihood <- function(par)
{
  Ro = par[1];
  durInf = par[2];
  totPopulation = 100090;
  seed = 90;
  dt = 0.1;
  reportingRate = 0.15;
  tmax = 365;
  dailyInc = read.csv('daily_incidence_observed.csv',header=TRUE,stringsAsFactors=FALSE);
  determRes = deterministic_SIR_model(Ro,durInf,totPopulation,seed,dt,tmax);
  simdInc = round(dailyIncidence(determRes,dt,reportingRate),digits=0);
  iiLogL = dpois(x=dailyInc$report_incidence,lambda = round(simdInc$daily_incidence[1:length(dailyInc$report_incidence)],digits=0),log=TRUE);
  return(sum(iiLogL));
}

# Calculate the loglikelihood for different R0
# Fit Ro
library(MASS);
totLogL = rep(NA,200);
totLogLVarRepo = rep(NA,200);
serialIntervalData = read.csv('data_interval_between_symptom_onset.csv',header=TRUE,stringsAsFactors=FALSE);
durInfFitted = fitdistr(serialIntervalData$infector_infectee_symptom_onset_interval,'exponential');
1/as.numeric(durInfFitted$estimate)
# Fit Ro from true incidence
for(ii in 1:200)
{
  par1=1+ii*0.01;
  par2=1/as.numeric(durInfFitted$estimate);
  totLogL[ii]=totalLogLikelihood(c(par1,par2));
}
# R0 estimate
totLogL
estR0 = 1+0.01*which.max(totLogL)
estR0

