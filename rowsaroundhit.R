# https://twitter.com/jhilden/status/1557368903885635584
# filtering all rows where a condition is fulfilled 
# PLUS the following and preceding rows

column_1 <- c(1:5)
column_2 <- c(1:5)
my_column <- c(1:3,10,5)

d <- data.frame(column_1, column_2, my_column)

# https://stackoverflow.com/a/13155669
d[which(d$my_column == 10) + c(-1:1), ]

# https://stackoverflow.com/a/54349439
library(dplyr)
d %>% 
  filter( (my_column == 10) | 
            lead(my_column == 10) | 
            lag(my_column == 10)
          )
