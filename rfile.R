
library(Hmisc)

movies = read.csv("movies.csv", header = TRUE) %>% 
  mutate(title = as.character(title),
         domgross = as.numeric(as.character(domgross)),
         intgross = as.numeric(as.character(intgross)),
         domgross_2013. = as.numeric(as.character(domgross_2013.)),
         intgross_2013. = as.numeric(as.character(intgross_2013.)),
         decade = floor(2013/10)*10,
         domprofit_2013 = domgross_2013.*.5,
         intprofit_2013 = intgross_2013.*.4,
         returns_2013 = (domprofit_2013+intprofit_2013)-budget_2013.,
         budget_size_2013 = cut(x[,7],c(0,25000000,50000000,75000000,100000000,125000000,150000000,175000000,200000000,225000000,250000000,275000000),labels=F)

#data transformations

hist.data.frame(movies[, -c(2,3)])
pairs(movies[, -c(2:6, 10)])

movies_transformed = movies %>% 
  mutate(year = (year)^2,
         budget = log(budget),
         domgross = log(domgross),
         intgross = log(intgross), 
         budget_2013. = log(budget_2013.),
         domgross_2013. = log(domgross_2013.),
         intgross_2013. = log(intgross_2013.))

hist.data.frame(movies_transformed[, -c(2,3)])
pairs(movies_transformed[, -c(2:6, 10)])
```

