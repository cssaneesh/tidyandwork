dice <- c(1,2,3,4,5,6)

luck <- function(x) {
  my_luck <- sample(x = dice, size = 1, replace = T)
  return(my_luck)
}

luck()
names <- c('Saneesh', 'Appu', 'Sanusha')



pick_one <- function(x){
  pick_one <- (sample(x = names, size = 1, replace = F))
  return(pick_one)  
}

pick_one()

my_luck <- 2

if (my_luck== luck()){
  print('Congratulations')
} else{
  print('Try again')
}

luck()


# Column 1
Model = c('Pulsar 220', 'DUKE 200', 'Platina', 'Pulsar 180')
# Column 2
Power = c(220, 200, 100, 180)
# Two columns in a box
box_1 <- data.frame(Model, Power)

str(box_1)

box_2 <-
  data.frame(Model = c('Pulsar 220', 'DUKE 200', 'Platina', 'Pulsar 180'),
             # names inside ' ' or " " and seperate with ,
             Power = c(220, 200, 100, 180))
str(box_1)
str(box_2)

box_2

library(tidyverse)
box_2 %>% filter(Model== c('Platina',  'DUKE 200'))

box_2 %>% filter(Model== 'Platina'|'DUKE 200')

names(box_2)
sort(box_2$Model)

library(ggplot2) 

first_plot <- ggplot(data= box_2,
                     mapping = aes(
                       x = Model,
                       y = Power,
                       fill = Model
                     )) +
  geom_col()

first_plot

second_plot <- first_plot+
  aes(reorder(Model, Power))

second_plot

third_plot <- second_plot+labs(x= 'Made and Model', y= 'Power CC')

third_plot

box_2$Speed <- c(180, 165, 80, 145)


ggplot(box_2, aes(Power, Speed, col= Model))+
  geom_point()

modl <- lm(Speed ~ Power, data = box_2)

summary(modl)

-4.03+0.83*100

library(bayestestR)
library(easystats)
library(rstanarm)
model.bayes <- stan_glm(Speed ~ Power, data = box_2)
model.bayes
summary(model.bayes)

posteriors <- insight::get_parameters(model.bayes)

plot(posteriors)

posteriors %>% mutate(Intercept= `(Intercept)`) %>% 
  ggplot( aes(Power, Intercept))
  
  
  
library(tidyverse)  

score <- data.frame(class=c(2,3,4,5,6,7,8,9,10,11,12),
                    no_students=c(9,28,59,165,244,206,146,60,24,5,1))  
view(score)
str(score)

score <-
  score %>% mutate(percent = round(no_students / sum(no_students) * 100, 2))

ggplot(data = score, mapping = aes(x = class, 
                                   y= no_students), col='red')+
  geom_histogram(aes(),fill='lightpink2', col= 'red', stat= 'identity')

boxplot(score$no_students)
mean(score$no_students)

str(score)
summary(score)

corn <- data.frame(treatment=c(rep('ctrl', 20), rep('exp', 20)), 
                   units=c(380,283,356,350,345, 321,349,410,384,455,
                           366,402,329,316,360,356,462,399,272,431, 
                           361,434,406,427,430, 447,403,318,420,339,401,393,467,477,
                           410,375,426,407,392,326))


ggplot(corn, aes(treatment, units))+
  geom_boxplot()

corn.test <- t.test(corn$units)
corn.test

qqnorm(corn$units)


?t.test
