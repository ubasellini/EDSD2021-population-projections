
## EDSD: RANDOM GROUPS for ASSIGNMENT 

rm(list = ls())
EDSDstudents <- c("Josephine","Ozer","Silvio","Liliana",
                  "Milena","Gonzalo","Silvia",
                  "Ursula","Anna","Ainhoa",
                  "Arno","Rafael","Ozge",
                  "Alon","Lucas","Marylin","Gianluca",
                  "Paola","Alice")
m <- length(EDSDstudents)

## ask a random number from a student
set.seed(42)
my_groups <- sample(EDSDstudents,size = m)

## here are the groups
(group1 <- my_groups[1:4])
(group2 <- my_groups[1:4+4])
(group3 <- my_groups[1:4+8])
(group4 <- my_groups[1:4+12])
(group5 <- my_groups[1:3+16])

