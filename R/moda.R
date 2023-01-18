#cargamos la tabla
library("readr")
evominining_table <- read_tsv('Evominingtable.tsv')
columnGLU6P<- evominining_table$`Glycolysis_2--Glucose_6_phosphate_isomerase_4`

class(columnGLU6P)

getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda <- getmoda(evominining_table$`PYR_THR_AA_1--Alanine_dehydrogenase_2`)
print(moda)

library(dplyr)

evominining_table %>% filter_all(any_vars(. %in% c(moda)))
