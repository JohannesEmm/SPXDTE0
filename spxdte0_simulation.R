require(tidyverse)



rbinom(n = 1000, size = 1, prob = 1/6)


data.frame(replicate(10,rbinom(n = 1000, size = 1, prob = 1/6)))