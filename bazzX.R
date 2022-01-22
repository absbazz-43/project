library(tidyverse)
library(broom)
d=mtcars %>% 
  group_by(am)
do(d,tidy(lm(mpg~hp,data=.)))

#################using do
fd=mtcars %>% 
  group_by(am) %>% 
do(tidy(lm(mpg~hp,data=.)))
fd

#################using tidyverse
library(tidyverse)
library(broom)
y=mtcars %>% 
  nest(-am) %>% 
  mutate(
         fit = map(data, ~ lm(mpg ~ hp,data = .)),
         results = map(fit, tidy)) %>% 
  unnest(results) 

y
warning(call. = F)


fe=mtcars %>% 
     nest(-am) %>% 
     mutate(
       fit= map(data, ~ lm(mpg ~ hp,data = .x)),
       results = map(fit,tidy)) %>% 
     unnest(results)
fe













