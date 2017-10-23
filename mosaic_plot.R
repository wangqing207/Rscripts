library("vcd")
library(MASS)
data("Titanic")
mosaic(~ Sex + Age+Class + Survived, data = aa,
main = "Survival on the Titanic", shade = T, legend = TRUE)