```{r}
library(Hmisc)
```


```{r}
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
         budget_size_2013 = cut(budget_2013.,seq(from=0,to=275000000,by=25000000),labels=F)
```

```{r}
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

