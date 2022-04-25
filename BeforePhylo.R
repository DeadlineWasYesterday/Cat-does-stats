#load libraries and stuff
library(rethinking)
library(reshape2)
library(animation)
library(globe)
library(sf)
library(spData)
library(rethinking)
library(tidyverse)
library(brms)
library(ggridges)
library(shinystan)
library(bayesplot)
library(tidybayes)
library(ggmcmc)


#overused example. Bayes rule.
Pr_Positive_HIV <- 0.99
Pr_Positive_Healthy <- 0.01
Pr_HIV <- 0.0001
Pr_Positive <- Pr_Positive_HIV * Pr_HIV + Pr_Positive_Healthy * ( 1 - Pr_HIV )
( Pr_HIV_if_Positive <- Pr_Positive_HIV*Pr_HIV / Pr_Positive )


##Grid approximation
# define grid
p_grid <- seq(from=0 , to=1 , length.out=11)
# compute likelihood at each value in grid
likelihood <- dbinom(3 , size=5 , prob=p_grid)
#find max likelihood
p_grid[(likelihood == max(likelihood))]


#Not powerful. Prone to resolution error.
#same code, but with more grid values
p_grid <- seq(from=0 , to=1 , length.out=20) 
likelihood <- dbinom(3 , size=5 , prob=p_grid)
p_grid[(likelihood == max(likelihood))]

#Does not scale.
#Do not run if you love your computer.
p_grid <- seq(from=0 , to=1 , length.out=1000000000) 
likelihood <- dbinom(3 , size=5 , prob=p_grid)
p_grid[(likelihood == max(likelihood))]




##installing rethinking
remove.packages("rstan")
#ignore the error
if (file.exists(".RData")) file.remove(".RData")

Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1) # only necessary for Linux without the nodejs library / headers
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
example(stan_model, package = "rstan", run.dontrun = TRUE)

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
#select yes first, and then no.
install.packages('devtools', dependencies = T)
devtools::install_github("rmcelreath/rethinking")
#select 3 and you should be good.

#dummy code for prior
x <- seq(0,1,length=1000)
beta_dist <- data.frame(cbind(x, dbeta(x,2,8)))
beta_dist <- melt(beta_dist,x)
g <- ggplot(beta_dist, aes(x,value, color=variable))
g+geom_line() + labs(title="Prior") + labs(x="Probability", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('frames/prior.jpg')

#dummy code for posterior
beta_dist <- data.frame(cbind(x, dbeta(x,6,4)))
beta_dist <- melt(beta_dist,x)
g <- ggplot(beta_dist, aes(x,value, color=variable))
g+geom_line() + labs(title="Posterior") + labs(x="Probability", y="Density") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('frames/posterior.jpg')




##quadratic approximation
qa <- function(h,c) {
cat.qa <- map(
  alist(
    H ~ dbinom( H+C ,p) ,  # binomial likelihood
    p ~ dbeta(2,8)     #prior
  ) ,
  data=list(H=h,C=c) )

return(c(precis(cat.qa)$mean, precis(cat.qa)$sd))
}

p <- seq(from=0,to=1,length.out = 100)
#plot everything once 
plot(p, dnorm(p,qa(0,0)[1],qa(0,0)[2]), type = 'l', lty = 1, lwd = 2, col = col.alpha(2,0.7)) #prior
plot(p, dnorm(p,qa(1,0)[1],qa(1,0)[2])) #1 human in the house
plot(p, dnorm(p,qa(2,0)[1],qa(2,0)[2])) #2 humans in the house
plot(p, dnorm(p,qa(3,0)[1],qa(3,0)[2])) #3...
plot(p, dnorm(p,qa(3,1)[1],qa(3,1)[2])) #3... and 1 cat
plot(p, dnorm(p,qa(3,2)[1],qa(3,2)[2]),
     ylim = c(0,3.5), type = 'l', lty = 1, 
     lwd = 3, col = col.alpha(2,0.7),
     main='Posterior',
     xlab="Probability that cats are innocent",
     ylab="Density",
     #sub="Bayesian Updating",
     col.main="blue", col.lab="black") #3... and 2 cats
savePlot( concat("frames/frame_",2000,".jpg"), 
          type = "jpg", device = dev.cur())


##ridgeplot
df0 <- data.frame(rep('0_Prior', 100000), p,rnorm(100000,qa(0,0)[1],qa(0,0)[2]))
df1 <- data.frame(rep('1H', 100000), p,rnorm(100000,qa(1,0)[1],qa(1,0)[2])) 
df2 <- data.frame(rep('2H', 100000), p,rnorm(100000,qa(2,0)[1],qa(2,0)[2]))
df3 <- data.frame(rep('3H', 100000), p,rnorm(100000,qa(3,0)[1],qa(3,0)[2]))
df4 <- data.frame(rep('3H1C', 100000), p,rnorm(100000,qa(3,1)[1],qa(3,1)[2]))
df5 <- data.frame(rep('3H2C', 100000), p,rnorm(100000,qa(3,2)[1],qa(3,2)[2]))
names(df0) <- c('Occupancy','i', 'p')
names(df1) <- c('Occupancy','i', 'p')
names(df2) <- c('Occupancy','i', 'p')
names(df3) <- c('Occupancy','i', 'p')
names(df4) <- c('Occupancy','i', 'p')
names(df5) <- c('Occupancy','i', 'p')
df <- rbind(df0,df1,df2,df3,df4,df5)

#Almost as big a disappointment as my life.
ggplot(df, aes(x = p, y = Occupancy)) +
  geom_density_ridges(scale = 2) + 
  scale_y_discrete(expand = c(0.2, 3)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  labs(
    title = 'Bayesian updating',
    subtitle = 'Shows probability that the cats are innocent'
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + 
  theme(axis.title.y = element_blank())
ggsave('disappointment.jpg')


##animate Bayesian updating
df <- data.frame(c('prior', '1H', '2H', '3H', '3H1C', '3H2C'),
                 c(qa(0,0)[1], qa(1,0)[1], qa(2,0)[1], qa(3,0)[1], qa(3,1)[1], qa(3,2)[1]),
                 c(qa(0,0)[2], qa(1,0)[2], qa(2,0)[2], qa(3,0)[2], qa(3,1)[2], qa(3,2)[2]))
names(df) <- c('Occupancy', 'mean', 'sd')
ud <- c('Prior', 'Prior to 1H', '1H to 2H', '2H to 3H', '3H to 3H1C',
        '3H1C to 3H2C')
res <- 10

windows()
ani.record(reset = TRUE)
for (i in 1:5) {
pseqm <- seq(from = df$mean[i], to = df$mean[i+1], length.out = res)
pseqs <- seq(from = df$sd[i], to = df$sd[i+1], length.out = res)
for (j in 1:res) {
  ani.record()
  
  plot(p, dnorm(p,df$mean[i], df$sd[i]), 
       ylim = c(0,5.0), type = 'l', lty = 2, #fix ylim after changing prior 
       lwd = 3, col = col.alpha(2,0.7),
       main=ud[i+1],
       xlab="Probability that cats are innocent",
       ylab="Density",
       #sub="Bayesian Updating",
       col.main="blue", col.lab="black")
  lines(p, dnorm(p,pseqm[j], pseqs[j]), type = 'l', lty = 1, 
        lwd = 3, col = col.alpha(4,0.7))
  legend(0.6, 3,legend = c(ud[i], ud[i+1]),
         col=c("red", "blue"), lty=1:2, cex=1,
         title="Line types", text.font=4, bg='lightblue')
  savePlot( concat("frames/frame_",1000+i*100+j,".jpg"), 
            type = "jpg", device = dev.cur())
  
} }


 
## now we can replay it, with an appropriate pause between
oopts = ani.options(interval = 0.05)
ani.replay()



##Using a normal prior.
qa <- function(h,c) {
  cat.qa <- map(
    alist(
      H ~ dbinom( H+C ,p) ,  # binomial likelihood
      p ~ dnorm(0.2,0.1)     #prior
    ) ,
    data=list(H=h,C=c) )
  
  return(c(precis(cat.qa)$mean, precis(cat.qa)$sd))
}

