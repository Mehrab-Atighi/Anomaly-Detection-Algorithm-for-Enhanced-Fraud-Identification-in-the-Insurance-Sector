library(xtable)
library(tidyverse)
data1 = readxl::read_excel("F:/thesis/Type/thesis article 1/elsarticle/Tables data/Table1-data.xlsx" , sheet = "Sheet2")
data2 = readxl::read_excel("F:/thesis/Type/thesis article 1/elsarticle/Tables data/Table2-data.xlsx" , sheet = "Sheet2")

xtable(ddigits = 3)

table2 = data2 %>ata1, % filter(Metrics == "F1") %>% pivot_wider(
  names_from = c(dataset),
  values_from = c(Sum.of.Metric_Value))

