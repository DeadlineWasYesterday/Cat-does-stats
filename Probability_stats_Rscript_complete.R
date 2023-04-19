library(ggplot2)
library(gganimate)
library(dplyr)


#Birthday problem
#How many people do we need in a room to have at least a 50% probability 
# that two people will share the same birthday?


#for a room that has 23 people:
k <- 23
numerator <- prod((365-k+1):365) #product of everything from (365 - k + 1) to 365
denominator <- 365^k

probability <- 1 - numerator/denominator

print(probability*100)


#Exercise 1
#calculate the probability for two more rooms: occupancy 50 and 100.



#Expectation example
#Imagine a game where you toss a coin and roll a die. 
#If the coin comes up heads, you don’t win anything. 
#If it comes up tails, you win money for what comes up on the die. 
#How much should you pay to play this game?

coin <- c(0,1) #defining coin random variable
die <- c(1:6) #defining die random variable

#combining two random variables into one
coin_die <- c(t(outer(coin, die, FUN = "*"))) 

#weighted average
mean(coin_die)


#plotting coin and die outcomes

#coin and die
ggplot(mapping = aes(x=coin_die, y=rep(0.083,12))) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + xlab("Winnings") + ylab("Probability") +
  scale_x_continuous(breaks = seq(0,6,1)) + 
  ggtitle('Flip a coin- if heads roll a die to win money') + 
  theme(plot.title = element_text(hjust = 0.5))


#Exercise 2
#For a game where you get to roll a die twice and you win the highest
#of the two rolls, how much should you pay to play?

first_roll <- rep(1:6, each = 6)
second_roll <- rep(1:6,6)

df <- data.frame(first_roll,second_roll)
df$winnings <- pmax(first_roll, second_roll)

df$probability <- rep(1/36,36)

df$xp <- df$winnings * df$probability

sum(df$probability)
sum(df$xp)

#graph probabilities

ggplot(mapping = aes(x=df$winnings, y=df$probability)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + xlab("Winnings") + ylab("Probability") +
  scale_x_continuous(breaks = seq(0,6,1)) + 
  ggtitle('Win largest value of two dice rolls') + 
  theme(plot.title = element_text(hjust = 0.5))


#Exercise 3
#For a game where you get to roll a die twice and you win the product
#of the two rolls, how much should you pay to play?

first_roll <- rep(1:6, each = 6)
second_roll <- rep(1:6,6)

df2 <- data.frame(first_roll,second_roll)
df2$winnings <- first_roll * second_roll

df2$probability <- rep(1/36,36)

df2$xp <- df2$winnings * df2$probability

sum(df2$probability)
sum(df2$xp)

ggplot(mapping = aes(x=df2$winnings, y=df2$probability)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + xlab("Winnings") + ylab("Probability") +
  scale_x_continuous(breaks = seq(0,36,1)) + 
  ggtitle('Win product of two dice rolls') + 
  theme(plot.title = element_text(hjust = 0.5))



#Testing linearity

#two separate games
df3 <- as.data.frame(expand.grid(df$winnings, df2$winnings))
names(df3) <- c('game1', 'game2')

df3$total_winnings <- df3$game1 + df3$game2

df3$probability <- rep(1/1296,1296)

df3$xp <- df3$total_winnings * df3$probability

sum(df3$xp)

sum(df3$xp) == sum(df$xp) + sum(df2$xp)

#one game
df4 <- df[c('first_roll', 'second_roll')]
df4$pmax <- pmax(df4$first_roll, df4$second_roll)
df4$prod <- df4$first_roll * df4$second_roll
df4$prob <- rep(1/36,36)

df4$total_winnings <- df4$pmax + df4$prod

df$xp <- df4$total_winnings * df4$prob
sum(df$xp)



#What is probability of winning between 10 and 20 dollars in the last game?

sum(df4[(df4$total_winnings < 20 & df4$total_winnings > 10),]$prob)




##Section 2

#Exercies draw a series of normal distributions

#plotting densities for range of values
plot(seq(-5,5, length = 100), dnorm(seq(-5,5, length = 100)))

#plotting a curve directy from the density function
curve(from = -5, to = 5, dnorm)

#histogram of random samples
hist(rnorm(1000), breaks = 10)

#density of random samples
plot(density(rnorm(1000)))



#Binomial probabilities

dbinom(5,5,0.5) #probability of 5 heads in 5 tosses
dbinom(30,50,0.5) #probability of 30 heads in 50 tosses


#graph probability of outcomes in 5 tosses
ggplot(mapping = aes(x=0:5, y=dbinom(0:5, 5, 0.5))) +
  geom_bar(stat="identity", color="purple", fill="lavender") +
  xlab("# successes") + ylab("Probability") +
  ggtitle('Binomial distribution: fair coin, 5 tosses') + 
  theme(plot.title = element_text(hjust = 0.5))


#graph probability of outcomes in 50 tosses
ggplot(mapping = aes(x=0:50, y=dbinom(0:50, 50, 0.5))) +
  geom_bar(stat="identity", color="purple", fill="lavender") +
  xlab("# successes") + ylab("Probability") +
  ggtitle('Binomial distribution: fair coin, 50 tosses') + 
  theme(plot.title = element_text(hjust = 0.5))



#Observing parameter changes
tests <- seq(0.05,0.95,length=11) #varying p between 0.05 and 0.95

#easy animation
for (p in tests) {
  print(ggplot(mapping = aes(x=1:50, y=dbinom(1:50, 50, p))) +
    geom_bar(stat="identity", color="purple", fill="lavender") +
      ylim(0,0.3) + xlab("# successes") + ylab("Probability") +
    ggtitle(sprintf('Binomial distribution: p = %s, 50 tosses', p)) + 
    theme(plot.title = element_text(hjust = 0.5)))
  Sys.sleep(0.35)
  }


#slightly less easy animation
tests <- seq(0.01,0.99,length=110) #varying p between 0.01 and 0.99
tests <- sapply(tests, round, 5)

df5 <- data.frame()
for (p in tests) { 
  temp <- data.frame(1:50, dbinom(1:50,50,p), rep(p,50))
  names(temp) <- c('x','y','p')
  df5 <- rbind(df5,temp)
  }

ggplot(data = df5, aes(x=x, y=y)) + 
  geom_bar(stat='identity', color="purple", fill="lavender") +
  # gganimate specific bits:
  ylim(0,0.3) + xlab("# successes") + ylab("Probability") +
  transition_states(
    p,
    transition_length = 0.01,
    state_length = 0.5
  ) + labs(title = 'Binomial distribution: p = {closest_state}, 50 tosses') +
  theme(plot.title = element_text(hjust = 0.5))


#Exercise: Calculate the mean and variance of 
#the distribution of 50 coin tosses.
#fit a normal distribution to that distribution.

x <- seq(0, 50, length=50)
y <- dnorm(x, 0.5*50, sqrt(0.5*50*0.5))

ggplot(mapping = aes(x, y)) +
  geom_bar(stat="identity", color="purple", fill="lavender") +
  xlab('# success') + ylab('Probability') + 
  labs(title = 'Normal distribution approximating binomial') +
  theme(plot.title = element_text(hjust = 0.5))

#Exercise: smooth it out

x <- seq(0, 50, length=100)
y <- dnorm(x, 0.5*50, sqrt(0.5*50*0.5))
ggplot(mapping = aes(x, y)) +
  geom_bar(stat="identity", color="purple", fill="lavender") +
  ylab('Probability') + xlab('Number of occurrences') +
  labs(title = 'Normal distribution approximating binomial, Smooth') +
  theme(plot.title = element_text(hjust = 0.5))

x <- seq(0, 50, length=1000)
y <- dnorm(x, 0.5*50, sqrt(0.5*50*0.5))
ggplot(mapping = aes(x, y)) +
  geom_bar(stat="identity", color="purple", fill="lavender")+
  ylab('Probability') + xlab('Number of occurrences') +
  labs(title = 'Normal distribution approximating binomial, Smoother') +
  theme(plot.title = element_text(hjust = 0.5))





#Example: A professor receives emails at a rate of 100 emails an hour. 
#What is the probability of receiving exactly 2 emails in a minute?

dpois(2,100/60) #Using poisson

#Using binomial
#100 emails an hour.
# = 100/3600 emails every second.

dbinom(2,60,100/3600) #close

#let's try 100/3600000 emails every millisecond.

dbinom(2,60000,100/3600000) #much closer


#Let's plot the distributions

#the Poisson
ggplot(mapping = aes(x=0:10, y=dpois(0:10, 100/60))) +
  geom_bar(stat="identity", color="purple", fill="lavender")+
  ylab('Probability') + xlab('# emails') +
  labs(title = 'Poisson distribution: for 100/60 emails every minute') +
  theme(plot.title = element_text(hjust = 0.5))


#Binomial 1
ggplot(mapping = aes(x=0:10, y=dbinom(0:10,60,100/3600))) +
  geom_bar(stat="identity", color="purple", fill="lavender")+
  ylab('Probability') + xlab('# emails') +
  labs(title = 'Binomial fit to a Poisson: n = 60, p = 100/3600') +
  theme(plot.title = element_text(hjust = 0.5))


#Binomial 2
ggplot(mapping = aes(x=0:10, y=dbinom(0:10,60000,100/3600000))) +
  geom_bar(stat="identity", color="purple", fill="lavender")+
  ylab('Probability') + xlab('# emails') +
  labs(title = 'Better binomial fit to a Poisson: n = 60000, p = 100/3600000') +
  theme(plot.title = element_text(hjust = 0.5))




#Observing parameter changes
tests <- seq(1,100,length=199) #varying lambda between 1 and 100

df5 <- data.frame()
for (p in tests) { 
  temp <- data.frame(1:100, dpois(1:100,p), rep(p,50))
  names(temp) <- c('x','y','p')
  df5 <- rbind(df5,temp)
}

ggplot(data = df5, aes(x=x, y=y)) + 
  geom_bar(stat='identity', color="purple", fill="lavender") +
  # gganimate specific bits:
  ylim(0,0.3) + xlab("# occurrences") + ylab("Probability") +
  transition_states(
    p,
    transition_length = 0.01,
    state_length = 0.5
  ) + labs(title = 'Poisson distribution: lambda = {closest_state}') +
  theme(plot.title = element_text(hjust = 0.5))




#Exercise: Solve the email problem with the normal distribution.
dnorm(2,100/60,100/60) #rare instance where the probability density has meaning.

#Would you say this approximation is a good one? Why/why not?



#Continuous random variables

#Test the 68, 95, 99 rule for standard normals
pnorm(1,0,1) - pnorm(-1,0,1)
pnorm(2,0,1) - pnorm(-2,0,1)
pnorm(3,0,1) - pnorm(-3,0,1)


#plotting the area

#define function
dlim <- function(x) {
  y <- dnorm(x)
  y[x < -2  |  x > 2] <- NA #area we don't want to shade
  return(y)
}

ggplot(data.frame(x = c(-3.5, 3.5)), aes(x = x)) +  
  stat_function(fun = dlim, geom = "area", fill = "green", alpha = 0.3) +
  stat_function(fun = dnorm) +
  scale_x_continuous(breaks = seq(-5,5,1)) + xlab('X') + ylab('Density') +
  labs(title = 'An innocent normal distribution') +
  theme(plot.title = element_text(hjust = 0.5))


#check if this is true for any normal
pnorm(9,0,3) - pnorm(-9,0,3)


#Exercise: Standardize a normal distribution with mean 5 and sd 4.
x <- seq(-10,20, length = 1000)

#unstandardized
ggplot(mapping = aes(x, dnorm(x,5,4))) + 
  geom_bar(stat='identity', color="purple", fill="lavender") +
  xlab('X') + ylab('Density') +
  labs(title = 'Normal distribution (Mean 5, SD 4)') +
  theme(plot.title = element_text(hjust = 0.5))


#standardized
ggplot(mapping = aes((x-5)/4, dnorm(x,5,4))) + 
  geom_bar(stat='identity', color="purple", fill="lavender") +
  xlab('X') + ylab('Density') +
  labs(title = 'Standardized Normal Distribution') +
  theme(plot.title = element_text(hjust = 0.5))



#Example: Assuming height is normally distributed, the mean 
#height in the US is 70 inches with an sd of 3 inches.

#Calculate the probability of observing an individual taller than 75 inches.
dlim <- function(x) {
  y <- dnorm(x,70,3)
  y[x < 75 ] <- NA #area we don't want to shade
  return(y)}
ggplot(data.frame(x = c(55, 85)), aes(x = x)) +  
  stat_function(fun = dlim, geom = "area", fill = "green", alpha = 0.3) +
  stat_function(fun = dnorm, args = c(70,3)) + xlab('X') + ylab('Density') +
  labs(title = 'Probabiliy of being taller than 75 inches') +
  theme(plot.title = element_text(hjust = 0.5))

#numerically
1 - pnorm(75,70,3)


#Calculate the probability of someone being between 68 and 72 inches.
dlim <- function(x) {
  y <- dnorm(x,70,3)
  y[x < 68 | x > 72 ] <- NA #area we don't want to shade
  return(y)}
ggplot(data.frame(x = c(55, 85)), aes(x = x)) +  
  stat_function(fun = dlim, geom = "area", fill = "green", alpha = 0.3) +
  stat_function(fun = dnorm, args = c(70,3)) + xlab('X') + ylab('Density') +
  labs(title = 'Probabiliy of being between 68 and 72 inches') +
  theme(plot.title = element_text(hjust = 0.5))

#numerically
pnorm(72,70,3) - pnorm(68,70,3)





#Central limit theorem

#Binomial distribution
#Look at the shape, mean and standard deviation.
v <- c()
for (i in 1:10000){ #number of sample means taken
  v[i] <- (mean(rbinom(100,100,0.02))) #mean of each random sample
}
ggplot(mapping = aes(v)) + 
  geom_histogram(binwidth = 0.02, color = 'purple', fill = 'lightblue')+
  xlab('Mean') + ylab('Density') +
  labs(title = '10000 Means of a Binomial. 100 samples each, p=0.02') +
  theme(plot.title = element_text(hjust = 0.5))

mean(v)
sd(v)

#Poisson
v <- c()
for (i in 1:10000){ #number of sample means drawn
  v[i] <- (mean(rpois(100,50))) #mean of each sample
}
ggplot(mapping = aes(v)) + 
  geom_histogram(binwidth = 0.06, color = 'purple', fill = 'lightblue')+
  xlab('Mean') + ylab('Density') +
  labs(title = '10000 Means of Poisson, 100 samples each, lambda=50') +
  theme(plot.title = element_text(hjust = 0.5))

mean(v)
sd(v)

#When working with samples of means, the variance is different from  
#that of the original distribution


#Does taking more sample means change anything?



#Standardize the distribution

v <- c()
for (i in 1:10000){ #number of sample means drawn
  v[i] <- (50 - (mean(rpois(10000,50))))/sqrt(50) #theoretical mean - mean of each sample
}

ggplot(mapping = aes(v)) + 
  geom_histogram(binwidth = 0.0001, color = 'purple', fill = 'lightblue')+
  xlab('Mean') + ylab('Density') +
  labs(title = 'Distribution of 10000 Means of Poisson, lambda=50') +
  theme(plot.title = element_text(hjust = 0.5)) 


sd(v)

sd(v)*sqrt(10000)

#As we increase the number of samples in each random sample, 
#the sd decreases by a factor of sqrt(n)

v <- c()
for (i in 1:10000){ #number of sample means drawn
  v[i] <- (50 - (mean(rpois(10000,50))))/(sqrt(50)/sqrt(10000)) #theoretical mean - mean of each sample
}

df6 <- data.frame(v)
ggplot(df6, aes(x = v)) + 
  geom_histogram(binwidth = 0.1, color = 'purple', fill = 'cornflowerblue')+
  xlab('Mean') + ylab('Density') +
  labs(title = 'Distribution of 10000 Means of Poisson, lambda=50') +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_function(fun = function(x) dnorm(x) * 1000  ,
                color = "darkred", size = 1)





#A professor receives 2 emails every minute.
#What is the probability that it takes more than 50 minutes to receive 100 emails?

1 - pgamma(50,100,2)


#Plotting the area
dlim <- function(x) {
  y <- dgamma(x,100,2)
  y[x < 50 ] <- NA #area we don't want to shade
  return(y)}
ggplot(data.frame(x = c(20, 80)), aes(x = x)) +  
  stat_function(fun = dlim, geom = "area", fill = "green", alpha = 0.3) +
  stat_function(fun = dgamma, args = c(100,2)) + xlab('Time') + ylab('Density') +
  labs(title = 'Probabiliy of taking > 50 minutes to receive 100 emails') +
  theme(plot.title = element_text(hjust = 0.5))




#Deriving a chi-squared from a standard normal
x <- seq(0,5,length=1000)

ggplot(mapping = aes(x, dchisq(x,1))) + 
  geom_bar(stat='identity', color="purple", fill="lavender") +
  xlim(0,5) +
  xlab('X') + ylab('Density') +
  labs(title = 'Chi-squared with 1 degrees of freedom') +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(mapping = aes(x, dnorm(sqrt(x))/sqrt(x))) + 
  geom_bar(stat='identity', color="purple", fill="lavender") +
  xlim(0,5) +
  xlab('X') + ylab('Density') +
  labs(title = 'Chi-squared with 1 degrees of freedom') +
  theme(plot.title = element_text(hjust = 0.5))



#Exercise: 50% alleles in a population are A. 
#If we observe 13, 15 and 12 individuals of type AA, Aa and aa, 
#check if the population is in Hardy-Weinberg equilibrium.

#fraction of alleles
p = q = 0.5

#total individuals
n = 40

#expected homozygous dominant
p^2 * n
#expected homozygous recessive
q^2 * n
#expected heterozygous
2*p*q * n

df7 <- data.frame(c(13,15,12), c(p^2 * n, q^2 * n, 2*p*q * n))
names(df7) <- c("Observed", "Expected")

df7$Observed_minus_expected <- df7$Observed - df7$Expected

df7$Observed_minus_expected_sq <- df7$Observed_minus_expected^2

df7$O_minus_E_sq_scaled_by_E <- df7$Observed_minus_expected_sq/df7$Expected
  
sum(df7$O_minus_E_sq_scaled_by_E)

#p-value
1 - pchisq(sum(df7$O_minus_E_sq_scaled_by_E),1)


#Graph area
dlim <- function(x) {
  y <- dchisq(x,1)
  y[x < 6.6 ] <- NA #area we don't want to shade
  return(y)}
ggplot(data.frame(x = c(4, 20)), aes(x = x)) +  
  stat_function(fun = dlim, geom = "area", fill = "green", alpha = 0.3) +
  stat_function(fun = dchisq, args = c(1)) + xlab('X') + ylab('Density') +
  labs(title = 'Probabiliy of being in HW Equilibrium') +
  theme(plot.title = element_text(hjust = 0.5))





#t test
#The average delivery time from UPS facilities is 4 hours
#Assume the following is data from the UPS facility in Tucson.
#Are the delivery times significantly higher in Tucson?

df8 <- data.frame(
  Time_in_hours = c(3.621064565,6.806926367,5.910962242,
                 3.340469352,3.151597219,4.886555668,4.376954686,2.49537037,
                 1.663548973,5.518074967,3.55424022,4.579830706,6.250722864,
                 4.22325289,3.931023169,1.916792077,6.710312184,5.33141685,
                 3.956724909,1.525592249,2.90757352,4.97155,4.476742745,
                 7.579910555,5.406530683,4.288793605,0.732213705,4.071485757,
                 4.673378227,7.606218223)
)

t.test(df8,mu = 4, alternative = 'greater')

t_value <- (mean(df8$Time_in_hours - 4))/(sd(df8$Time_in_hours)/sqrt(nrow(df8)))

1 - pt(t_value,29)


#graph area
dlim <- function(x) {
  y <- dt(x,29)
  y[x < t_value ] <- NA #area we don't want to shade
  return(y)}
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +  
  stat_function(fun = dlim, geom = "area", fill = "green", alpha = 0.3) +
  stat_function(fun = dt, args = c(29)) + xlab('X') + ylab('Density') +
  labs(title = 'Probabiliy the mean delivery time is higher in Tucson') +
  theme(plot.title = element_text(hjust = 0.5))



#ANOVA
#Following is data for sleeping patterns of a pet dog, cat and otter.
#Is there a significant difference between how long the pets sleep?


df9 <- data.frame(
  stringsAsFactors = FALSE,
               Pet = c("dog","dog","dog","dog",
                       "dog","dog","dog","dog","dog","dog","dog","dog",
                       "dog","dog","dog","dog","dog","dog","dog","dog",
                       "otter","otter","otter","otter","otter","otter","otter",
                       "otter","otter","otter","otter","otter","otter",
                       "otter","otter","otter","otter","otter","otter",
                       "otter","cat","cat","cat","cat","cat","cat","cat",
                       "cat","cat","cat","cat","cat","cat","cat","cat","cat",
                       "cat","cat","cat","cat"),
       hours_sleep = c(7.278865254,8.85591772,
                       8.643271414,6.105899816,6.950926886,11.89534344,3.655697085,
                       7.884098606,10.64688463,7.820061056,8.310862593,
                       8.147812945,9.895519193,9.006419382,5.922803612,
                       8.443448542,4.184856604,11.0468482,6.141931315,8.815905326,
                       7.500016817,10.39495608,7.55300056,9.064013922,
                       10.72286601,10.51052372,11.25833253,9.621362418,6.669513317,
                       8.132502255,6.529838057,8.951327158,12.03047893,
                       6.281385365,6.553154079,10.15545627,8.336893221,
                       8.869564864,6.809259808,8.948157445,10.47507707,10.48503503,
                       10.90454405,9.156321074,10.85516567,10.03306312,
                       9.958138317,10.00917346,10.93699189,11.16094205,8.658996678,
                       12.06596104,7.938965089,8.649281737,9.57925473,
                       13.3281692,12.76940761,7.805529645,8.701006592,
                       10.71945873)
)


anova(lm(hours_sleep~Pet, data = df9))

ggplot(df9, aes(x = Pet, y = hours_sleep)) +
  geom_boxplot()+
  geom_jitter(aes(color=Pet)) +
  xlab("Pet") + ylab("Sleep in hours") +
  labs(title = 'Boxplot for pet sleeping habits') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept=mean(df9$hours_sleep), linetype="dashed",
              size = 1, color = "purple")


df9$for_total_ss <- (mean(df9$hours_sleep) - df9$hours_sleep)^2
df9$for_mean_ss <- c((df9[df9$Pet == 'dog',]$hours_sleep - mean(df9[df9$Pet == 'dog',]$hours_sleep))^2, 
  (df9[df9$Pet == 'otter',]$hours_sleep - mean(df9[df9$Pet == 'otter',]$hours_sleep))^2,
  (df9[df9$Pet == 'cat',]$hours_sleep - mean(df9[df9$Pet == 'cat',]$hours_sleep))^2)

1 - pf(((sum(df9$for_total_ss) - sum(df9$for_mean_ss))/2)/
     (sum(df9$for_mean_ss)/57), 2, 57)

anova(lm(hours_sleep~Pet, data = df9))


#Section 3
#Monty Hall Problem

#10000 games 
x <- sample(c(1,2,3),10000, replace = 1) #door that has prize
y <- sample(c(1,2,3),10000, replace = 1) #door player selected

df7 <- data.frame(x,y)
names(df7) <- c("Door_with_prize", "Door_selected_by_player")

#Monty Hall opens a door without the prize that hasn't been selected at random
df7 <- df7 %>% rowwise() %>% 
  mutate(m = sample(c(1,2,3)[! c(1,2,3) %in% 
                        c(Door_with_prize, Door_selected_by_player)],1))


#Probability of winning if player doesn't switch
sum(df7$Door_selected_by_player == df7$Door_with_prize)/nrow(df7)

##Probability of winning if player does switch
sum(df7$Door_selected_by_player != df7$Door_with_prize)/nrow(df7)



#Maximum likelihood
# generate data from Poisson distribution
# with the parameter lambda=5
df_Poisson <- data.frame(data=rpois(n=2000, lambda=10))

ggplot(df_Poisson, aes(x=data))+ 
  geom_histogram(bins=nrow(unique(df_Poisson))) +
  ylab("Count")+ xlab("data")+
  labs(title = 'Poisson distribution') +
  theme(plot.title = element_text(hjust = 0.5))


#Calculate log likelihood by summing
llh_poisson <- function(lambda, y){
  llh <- sum(dpois(y, lambda, log=TRUE))
  return(llh)
}

lambdas <- seq(1,50, by=0.5)

# compute log-likelihood for all lambda values
ll <- sapply(lambdas,function(x){llh_poisson(x,pull(df_Poisson[1]))})

# save the lambdas and log-likelihoods in a data frame
df10 <- data.frame(ll=ll, lambda=lambdas)

ggplot(df10, aes(x=lambda,y=ll))+
  geom_point(size=1,color="cornflowerblue")+
  xlab("Lambda") +
  ylab("Log Likelihood")+
  geom_vline(xintercept = lambdas[which.max(ll)], color="red",size=1)
  


#Estimating the value of pi

x <- runif(500000, 0, 1)
y <- runif(500000, 0, 1)

df11 <- data.frame(x,y)
df11['ED'] <- sqrt(df11$x^2 + df11$y^2)

(sum(df11$ED < 1)/nrow(df11))*4

df11$Col <- ifelse(df11$ED < 1, 'red','blue')

ggplot(mapping = aes(df11$x, df11$y)) + geom_point(size = 0.1, color = df11$Col)


## Taken from https://jtr13.github.io/cc20/a-basic-introduction-to-markov-chain-monte-carlo-method-in-r.html

#Sampling a distribution using a random walk Metropolis Hastings algorithm


#Let's imagine this is a high dimensional and 
#very complicated probability distribution
exp_dist = function(x){
  if (x < 0) {
    return (0)
  } else {
    return (exp(-x)/149293)
  }
}


#The MCMC function
MCMC = function(T, startval, sd, target){
  x = rep(0,T) #Fill out an empty vector with T 0s
  x[1] = startval #Set start value
  for(t in 2:T) { #Iterate over T
    pi_star = rnorm(1,x[t-1],sd) #a new proposed value from a related distribution
    alpha = min(target(pi_star)/target(x[t-1]), 1) #value for alpha
    u = runif(1) #a random value between 0 and 1
    if (u < alpha) { #condition for accepting move
      x[t] = pi_star
    } else {
      x[t] = x[t -1]
    }
  }
  return(x)
}


z = MCMC(10000,3, 1, exp_dist)
z = data.frame(z)
ggplot(data=z,aes(x=z)) + 
  geom_density(color="darkblue", fill="lightblue") +
  stat_function(fun = dexp, colour = "red") + 
  ylab("Kernel Density") +  labs(title = 'MCMC sampling demonstration: Exponential') +
  theme(plot.title = element_text(hjust = 0.5))



#same thing, but for a chisq
chisq_dist = function(x){
  return (dchisq(x,5)/1000000)
}

z = MCMC(10000,3, 1, chisq_dist)
z = data.frame(z)
ggplot(data=z,aes(x=z)) + 
  geom_density(color="darkblue", fill="lightblue") +
  stat_function(fun = dchisq, args = c(5), colour = "red") + 
  ylab("Kernel Density") +  labs(title = 'MCMC sampling demonstration: Chi-squared(5)') +
  theme(plot.title = element_text(hjust = 0.5))

