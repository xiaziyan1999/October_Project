```{r}
library(Hmisc)
library(tidyverse)
```


```{r}
movies = read.csv("movies.csv", header = TRUE) %>% 
  mutate(title = as.character(title),
         domgross = as.numeric(as.character(domgross)),
         intgross = as.numeric(as.character(intgross)),
         domgross_2013. = as.numeric(as.character(domgross_2013.)),
         intgross_2013. = as.numeric(as.character(intgross_2013.)),
         decade = floor(year/10)*10,
         domprofit_2013 = domgross_2013.*.5,
         intprofit_2013 = intgross_2013.*.4,
         returns_2013 = (domprofit_2013+intprofit_2013)-budget_2013.,
         budget_size_2013 = as.factor(cut(budget_2013.,c(0,1000000,20000000,50000000,100000000,500000000),labels=F)))
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

```{r}
library(dplyr)
library(RColorBrewer)
summary(movies$returns_2013[which(movies$budget_size_2013==1 & movies$decade==2010)])

decademovies<-movies %>% 
  group_by(budget_size_2013, decade) %>% 
  dplyr::summarize(min_return = min(returns_2013, na.rm=TRUE),
                   q1_return = quantile(returns_2013, .25, na.rm=TRUE),
                   med_return = median(returns_2013, na.rm=TRUE),
                   q3_return = quantile(returns_2013, .75, na.rm=TRUE),
                   max_return = max(returns_2013, na.rm=TRUE), 
                   avg_return = mean(returns_2013, na.rm=TRUE))

movies %>% 
  group_by(budget_size_2013, binary) %>% 
  dplyr::summarize(min_return = min(returns_2013, na.rm=TRUE),
                   q1_return = quantile(returns_2013, .25, na.rm=TRUE),
                   med_return = median(returns_2013, na.rm=TRUE),
                   q3_return = quantile(returns_2013, .75, na.rm=TRUE),
                   max_return = max(returns_2013, na.rm=TRUE), 
                   avg_return = mean(returns_2013, na.rm=TRUE))

decademovies %>% ggplot(aes(x=decade, y=avg_return/100000000, group=budget_size_2013,color=budget_size_2013,ylab="Average Returns")) +
  geom_line() + theme_bw() + scale_color_brewer(palette = "Set2")
```

