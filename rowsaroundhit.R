# https://twitter.com/jhilden/status/1557368903885635584
# filtering all rows where a condition is fulfilled 
# PLUS the following and preceding rows

column_1 <- c(1:11)
column_2 <- c(1:11)
my_column <- c(1:2,100,4:8,100,10:11)
d <- data.frame(column_1, column_2, my_column)

# https://stackoverflow.com/a/54349439
library(dplyr)
d %>% 
  filter( (my_column == 100) | 
            lead(my_column == 100, n = 2) |
            lag(my_column == 100, n = 2)
          )

# More than one row, eg. 2?
# https://stackoverflow.com/a/44100311
inds = which(d$my_column == 100)
rows <- lapply(inds, function(x) (x-2):(x+2))
d[unlist(rows),]

