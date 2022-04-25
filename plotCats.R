##All code copied from the internet.
#library(devtools)
#install_github("Gibbsdavidl/CatterPlots")

library(CatterPlots)

catplot(dogs, cats, size=0.1, catcolor=c(0.7,0.7,0.7,1),
        cat=3,
        xlab="Households with dogs",
        ylab="Households with cats",
        main="Household Pet Ownership in US States")



x <- -10:10
y <- -x^2 + 10
purr <- catplot(xs=x, ys=y, cat=3, catcolor='#000000FF')
cats(purr, -x, -y, cat=4, catcolor='#FF0000')

# for more fun ...
meow <- multicat(xs=x, ys=y, cat=c(1,2,3), catcolor=list('#33FCFF','#FF0000'), canvas=c(-0.1,1.1, -0.1, 1.1))
morecats(meow, x, 10*sin(x)+40, size=0.05, cat=c(4,5,6), catcolor=list('#0495EE','#EE7504'), type="line")

# random cats
meow <- multicat(xs=x, ys=rnorm(21),
                 cat=c(1,2,3,4,5,6,7,8,9,10),
                 catcolor=list('#33FCFF'),
                 canvas=c(-0.1,1.1, -0.1, 1.1),
                 xlab="some cats", ylab="other cats", main="Random Cats")



##from https://r-charts.com/miscellaneous/ggcats/

# install.packages("remotes")
# remotes::install_github("R-CoderDotCom/ggcats@main")
library(ggcats)
# install.packages("ggplot2")
library(ggplot2)

grid <- expand.grid(1:5, 3:1)

df <- data.frame(x = grid[, 1],
                 y = grid[, 2],
                 image = c("nyancat", "bongo",
                           "colonel", "grumpy",
                           "hipster", "lil_bub",
                           "maru", "mouth",
                           "pop", "pop_close", 
                           "pusheen", "pusheen_pc",
                           "toast", "venus",
                           "shironeko"))

ggplot(df) +
  geom_cat(aes(x, y, cat = image), size = 5) +
  geom_text(aes(x, y - 0.5, label = image), size = 2.5) +
  xlim(c(0.25, 5.5)) + 
  ylim(c(0.25, 3.5)) 

# Scatter plot
ggplot(iris, aes(Petal.Length, Petal.Width)) +
  geom_cat(cat = "nyancat", size = 4)

# Create a new column
iris$cat <- factor(iris$Species,
                   labels = c("pusheen", "toast",
                              "venus"))

# Scatter plot by group
ggplot(iris, aes(Petal.Length, Petal.Width)) +
  geom_cat(aes(cat = cat), size = 4)




# install.packages("Ecdat")
library(Ecdat)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("gganimate")
library(gganimate)

# Data frame
dat <-
  incomeInequality %>%
  select(Year, P99, median) %>%
  rename(income_median = median,
         income_99percent = P99) %>%
  pivot_longer(cols = starts_with("income"),
               names_to = "income",
               names_prefix = "income_")

# Cats for each line
dat$cat <- rep(NA, 132)
dat$cat[which(dat$income == "median")] <- "nyancat"
dat$cat[which(dat$income == "99percent")] <- rep(c("pop_close", "pop"), 33)

# Animation
ggplot(dat, aes(x = Year, y = value, group = income, color = income)) +
  geom_line(size = 2) +
  ggtitle("ggcats, a core package of the memeverse") +
  geom_cat(aes(cat = cat), size = 5) +
  xlab("Cats") +
  ylab("Cats") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  transition_reveal(Year)



##from https://towardsdatascience.com/stranger-things-adding-cats-to-your-plots-on-r-9f46acb798bb


data(incomeInequality)
newData <-
  incomeInequality %>%
  select(Year, P99, median) %>%
  rename(income_median = median,
         income_99percent = P99) %>%
  pivot_longer(cols = starts_with("income"),
               names_to = "income",
               names_prefix = "income_")
newData$cat <- rep(NA, 132)
newData$cat[which(newData$income == "median")] <- rep(c("pop_close", "pop"), 33)
newData$cat[which(newData$income == "99percent")] <- "bongo"
ggplot(newData, aes(x = Year, y = value, group = income, color = income)) +
  geom_line(size = 2.5) +
  labs(title="Testing ggcats with gganimate", 
       subtitle = "Pop cat and Bongo cat") +
  geom_cat(aes(cat = cat), size = 6) +
  transition_reveal(Year)
