rm(list=ls(all=TRUE))

#setwd("")

#Use install.packages if the packages are not installed
#install.packages("repmis")
library(repmis)
n
source_data("https://github.com/rajsiddarth119/Datasets/blob/master/titanic.rdata?raw=true")


# Profile 5 sample records
idx <- sample(1:nrow(titanic.raw), 5)
titanic.raw[idx, ]
class(titanic.raw)
summary(titanic.raw)

# minimum support: supp=0.1
# minimum confidence: conf=0.8
# maximum length of rules: maxlen=10

#install.packages("arules")
library(arules)
rules.all <- apriori(titanic.raw)
inspect(rules.all)

# Rules with rhs containing "Survived" only
rules <- apriori(titanic.raw,
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No",
                                         "Survived=Yes"),
                                   default="lhs"))


# Keep three decimal places
quality(rules) <- round(quality(rules), digits=3)

# Order rules by lift
rules.sorted <- sort(rules, by="lift")

inspect(rules.sorted)

# Observation
# Redundant Rules
##    Rule #2 provides no extra knowledge in addition to rule #1, since rules #1 tells us that all 2nd-class children survived.
##    When a rule (such as #2) is a super rule of another rule (#1) and the former has the same or a lower lift, the former rule (#2) is considered to be redundant.
##    Other redundant rules in the above result are rules #4, #7 and #8, compared respectively with #3, #6 and #5.

# Remove Redundant Rules

## find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1
## which rules are redundant
which(redundant)

## remove redundant rules
rules.pruned <- rules.sorted[!redundant]


# Remaining Rules
inspect(rules.pruned)

#The rule states only that all children of class 2 survived, but provides no information at all to compare the survival rates of diferent classes.

# Rules about Children
rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(default="none", rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                         "Age=Child", "Age=Adult")))

rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)

#install.packages("arulesViz")
library(arulesViz)
plot(rules.all)

plot(rules.all, method = "grouped")

plot(rules.all, method = "graph")

plot(rules.all, method = "graph", control = list(type = "items"))

plot(rules.all, method = "paracoord", control = list(reorder = TRUE))


# Reference
# Yanchang Zhao
# http://www.RDataMining.com