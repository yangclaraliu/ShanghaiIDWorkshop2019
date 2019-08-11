# You are a public health professional in China who are concerned about Summeritis Since the population of China has never been exposed before, the potential impacts of the disease are unknown. You already known something about this Summeritis from another country:
# beta = 2;
# gamma = 0.15. 
# However, you have some approximated betas for China by province. You would like to visualize some endpoints that you are interested in.

#### Step 1 ####
# You received a file titled "beta_ByPrv.rds". This file contains approximated betas for China by province as a result of different contact rates. Please navigate your work environment if necessary and then load the rds file into an object named beta.
getwd()
#setwd()
beta <- readRDS("beta_ByPrv.rds")

# exercise 1: Could you take a look at the provinces with beta greater than 2? 
# hint: subset
beta[beta$beta>2,]
# exercise 2: Could you take a look at what is the class of this current object?
# hint: class
class(beta)
# exercise 3: Could you create a histogram of all the beta estimates?
# hint: hist
hist(beta$beta, xlab = "Beta", main = "")

### Step 2 #### 
# Assuming the total population is 1, and infection is introduced by having 1/10^6 of the total population at the beginning (<- this should be the same as the example during lecture), using the beta file provided above and observation period of 70 days, could you run a simple SIR model from the package "EpiDyanmics" for all provinces and save their results as a list?
# hint: loop
library(EpiDynamics)

res <- list() #in order to assign a value into a list, a list must first exists
for(i in 1:nrow(beta)){
  parameters <- c(beta = beta$beta[i], gamma = 0.15)
  initials <- c(S = 1 - 1e-06, I = 1e-06, R = 0)
  res[[i]] <- SIR(pars = parameters, init = initials, time = 0:70)$results
  res[[i]]["prv"] <- beta$prv[i] # keep track of the relevant province
  res[[i]]["beta"] <- beta$beta[i] # keep track of the relevant beta estimates
}

# exercise 3: Could you take a look at the list object you just created by typing its name into the console window?
res

#### Step 3 ####
# Using ggplot, can you plot the trajectory of S, I, and R compartments for all provinces in one plot respectively (i.e. 3 figures in total)?  
res <- bind_rows(res)
res <- gather(res, key = state, value = value, -prv, -beta, -time)
res$state <- factor(res$state, levels = c("S","I","R"),
                    labels = c("Susceptible","Infectious","Recovered"))

ggplot(res, aes(x = time, y = value, group = prv, color = state, alpha = beta))+
  geom_line()+
  theme_bw()+
#  theme(text = element_text(family = "Garamond", size = 14))+ #Only use for Mac
  facet_grid(rows = vars(state))+
  theme(legend.position = "bottom")

ggsave("sample.png")
