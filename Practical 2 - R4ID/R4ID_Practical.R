# You are a public health professional in China who are concerned about Birdy Pox Since the population of China has never been exposed before, the potential impacts of the disease are unknown. You already known something about Birdy Pox from another country:
# beta = 2;
# gamma = 0.15. 
# However, you have some approximated betas for China by province. You would like to visualize some endpoints that you are interested in.

#### Step 1 ####
# You received a file titled "beta_ByPrv.rds". This file contains approximated betas for China by province as a result of different contact rates. Please navigate your work environment if necessary and then load the rds file into an object named beta.
getwd()
setwd(????)
beta <- ????

# exercise 1: Could you take a look at the provinces with beta greater than 2? 
# hint: subset

# exercise 2: Could you take a look at what is the class of this current object? What are the classes of each column?
# hint: class

# exercise 3: Could you create a histogram of all the beta estimates?
# hint: hist

### Step 2 #### 
# Assuming the total population is 1, and infection is introduced by having 1/10^6 of the total population at the beginning (<- this should be the same as the example during lecture), using the beta file provided above and observation period of 70 days, could you run a simple SIR model from the package "EpiDyanmics" for all provinces and save their results as a list?
# hint: loop

res <- list() #in order to assign a value into a list, a list must first exists
for(i in ????){
  parameters <- ????
  initials <- ????
  res[[i]] <- ????
  res[[i]]["prv"] <- ???? # keep track of the relevant province
  res[[i]]["beta"] <- ???? # keep track of the relevant beta estimates
}

# exercise 4: Could you take a look at the list object you just created by typing its name into the console window?

#### Step 3 ####
# Using ggplot, can you plot the trajectory (i.e. line plot) of S, I, and R compartments for all provinces in one plot respectively (i.e. 3 figures in total)?  
# hint1: gather
# hint2: are column shown in a reasonable order?
res <- bind_rows(res) #paste all tables in the list together
res <- ????
????

ggplot(????)+
  geom_????()+
  ????

????

#### Step 4: advance and optional ####
