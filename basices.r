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

my_luck <- 5

if (my_luck== luck()){
  print('Congratulations')
} else{
  print('Try again')
}



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
  ggplot( aes(Power, Intercept))+
  