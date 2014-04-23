################################################################
# R script using only the base package
################################################################
# dynamic programming designed for the knapsack problem
#
# knapsack problem ::
# decision variables: include item x (1) or not (0)
# linear constraints: total set of items implies a cost lower than the capacity xw<=C
# objective function: maximise total value v = xv=V

# with thanks to the coursera course on discrete optimisation
# of professor Pascal Van Hentenryck

# manual specification of a tiny toy dataset
# in agreement with course notes

# maximum weight
maxw <- 11
# the combinations of value - weight for each of 4 items
tdta <- scan(sep="")
8 4
10 5
15 8
4 3

dta <- matrix(tdta,nrow=4,byrow=T)

# if not included manually... import it as follows:
# my data import
# dtaName <- "data/ks_4_0"
# dta <- read.table(dtaName,sep=" ")
# nri <- dta[1,1]
# maxw <- dta[1,2]
# dta <- dta[-1,]
dimnames(dta)[[1]] <- paste0("i",1:nrow(dta))
dimnames(dta)[[2]] <- c("v","w")
dta <- data.frame(dta)

# reorder the items according to their weight to get near the maximum as soon as possible
dta <- dta[rev(order(dta[,2])),]

# remove combinations that are invalid from the start
# only consider items with a weight that is less than the capacity
dta <- dta[dta[,'w']<=maxw,]

################################################################
# all possible solutions, only if limited number of combinations
ptm <- proc.time()
# create set of all possible combinations in a design matrix
poss <- as.matrix(expand.grid(lapply(1:nrow(dta), function(.x) 0:1)))
dimnames(poss)[[2]] <- dimnames(dta)[[1]]
# create list with for each combination the selection, the total value and weight
result <- apply(poss,1,function(.x) c(list(poss=.x),as.list(colSums(dta[.x==1,]))))
# remove invalid solutions (weigth > maximum capacity)
result <- result[unlist(lapply(result,function(.x) .x$w <= maxw))]
# order valid solutions to get best on top
result <- result[order(unlist(lapply(result,function(.x) .x$v)),decreasing=T)]
# select best solution
optResult <- result[[1]]
# stop the time and show the difference to obtain duration
(time <- proc.time() - ptm)

# results
optResult
time
