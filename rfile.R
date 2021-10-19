```{r}
library(Hmisc)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
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
         profit_2013 = (domprofit_2013+intprofit_2013)-budget_2013.,
         budget_size_2013 = as.factor(cut(budget_2013.,c(0,1000000,20000000,50000000,100000000,150000000, 500000000),labels=F)),
         ROI = profit_2013/budget_2013.)
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
summary(movies$profit_2013[which(movies$budget_size_2013==1 & movies$decade==2010)])

decademovies<-movies %>% 
  group_by(budget_size_2013, decade) %>% 
  dplyr::summarize(min_return = min(profit_2013, na.rm=TRUE),
                   q1_return = quantile(profit_2013, .25, na.rm=TRUE),
                   med_return = median(profit_2013, na.rm=TRUE),
                   q3_return = quantile(profit_2013, .75, na.rm=TRUE),
                   max_return = max(profit_2013, na.rm=TRUE), 
                   avg_return = mean(profit_2013, na.rm=TRUE))

binarymovies<-movies %>% 
  group_by(binary,decade) %>% 
  dplyr::summarize(min_return = min(profit_2013, na.rm=TRUE),
                   q1_return = quantile(profit_2013, .25, na.rm=TRUE),
                   med_return = median(profit_2013, na.rm=TRUE),
                   q3_return = quantile(profit_2013, .75, na.rm=TRUE),
                   max_return = max(profit_2013, na.rm=TRUE), 
                   avg_return = mean(profit_2013, na.rm=TRUE))

ROImovies<-movies %>%
  group_by(binary,decade) %>%
  dplyr::summarize(avg_ROI=mean(ROI,na.rm = TRUE))

budgetmovies<-movies %>%
  group_by(as.character(budget_size_2013)) %>%
  dplyr::summarize(maxprofits=max(profit_2013,na.rm=TRUE))
names(budgetmovies)<-c("BudgetGroup","MaxProfits")
```

```{r plots}
decademovies %>% ggplot(aes(x=decade, y=avg_return/100000000, group=budget_size_2013,color=budget_size_2013,ylab="Average Returns")) +
  geom_line() + theme_bw() + scale_color_brewer(palette = "Set2")

ROImovies %>% ggplot(aes(x=decade,y=avg_ROI,group=binary,color=binary)) +
  geom_line() + theme_bw() + scale_color_brewer(palette = "Set2")

budgetmovies %>% ggplot(aes(x=BudgetGroup,y=MaxProfits/1000000000,fill=BudgetGroup)) + 
  geom_bar(stat="identity") + theme_bw() + scale_color_brewer(palette = "Set2")
```

