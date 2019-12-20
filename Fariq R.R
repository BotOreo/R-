library(tidyverse)
pulsar_star <- read.csv('listings.csv', header=T, na.strings=c("","NA"))
head(pulsar_star)

pulsar_star$last_review <- toString(pulsar_star$last_review)
str(pulsar_star)

pulsar_star$last_review <- data.frame(lapply(pulsar_star$last_review, as.character), stringsAsFactors = FALSE)

#Missing Value#
sum(is.na(pulsar_star$neighbourhood_group))
is.na(pulsar_star$last_review)
colnames(pulsar_star)[colSums(is.na(pulsar_star)) > 0]

#Remove trailing whitespace#
trimws <- function (x) gsub("^\\s+|\\s+$", "", x)
pulsar_star$last_review <- trim(pulsar_star$last_review)


i <-sapply(pulsar_star, is.factor)
pulsar_star[i] <-lapply(pulsar_star[i], as.Date)

library(dplyr)
pulsar_star %>% mutate_if(is.factor, as.Date) -> pulsar_star