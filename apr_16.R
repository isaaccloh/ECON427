library(wooldridge)
library(ggplot2)

View(wage1)

# Run regression with Mincer earnings function
reg = lm(log(wage) ~ educ + exper + I(exper^2), data = wage1)
summary(reg)

educ_mean = mean(wage1$educ)

mincer = function(exper){
  0.1279975 + 0.0903658 * educ_mean + 0.0410089 * exper - 0.0007136 * exper^2
}

ggplot(wage1, aes(x = exper, y = lwage)) +
  geom_point(color = 'purple') +
  geom_function(fun = mincer, color = 'black')
