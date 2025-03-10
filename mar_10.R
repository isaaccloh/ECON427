library(wooldridge)

# View dataset
View(beauty)

# Run regressions
reg = lm(log(wage) ~ belavg + abvavg + exper + educ + south,
         data = beauty)
summary(reg) # View results

# Histogram
hist(beauty$looks)
