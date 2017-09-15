rm(list = ls())
#a <- 4
#print(a)

#b <- c(4, 5, 6, 7, 3, 2, 4, 4, 5, 6, 2, 6, 12, 3, 5, 6, 7, 8, 9, 0, 8, 5, 2, 5, 1, 4, 6)

##x <- c(4.7, 3.5)
##print(x)
##x_as_int <- as.integer(x)
##print(x_as_int)

##names <- c("Bot", "Joe", "Kim")
##name <- "Chris"
##print(names)
##print(name)

#R auto converts all data in a vector to whatever it can to make it all of the same type
x <- c(4.7, "joe", 3.5)
print(x)


y <- c(10, 20, 30, 40)
z <- y[c(3, 2)]
print(z)
print(length(z))

y[3] <- 300                           #vectors are mutable
y[c(3, 2)] <- c(300,200)              #selective replacement based on selection vector

#######
y <- c(10, 20, 30, 40)
x <- c(-10, 10, -10, 10)
z <- x*y                              #-100, 200, -300, 400
#shorter vectors get recycled -- starts back at the beginning

x <- c(-10, 10)
z <- x*y                              #-100 200 -300 400

##################
#Logical vectors

x <- c(10, 20, 30, 40)
y <- c(5, 25, 15, 400)

z <- x<y                              # False, True, False, True

x_some    <- x[z]                        # Takes each one that is True in vector z, can flip by preceding with ! (!z gives opposite vals)


ages         <- c(37, 24, 18, 41, 22)
mean_age     <- mean(ages)
selector     <- ages > mean_age          #T F F T F
gt_mean_ages <- ages[selector]

gt_mean_ages <- ages[ages>mean(ages)]



samp         <- rnorm(100, mean = 20, sd = 3)           #gives vector of random numbers from normal distribution with given parameters
print(samp)
samp_sort    <- sort(samp)
print(samp_sort)

length(samp[samp>25])
eightythird_pctl <- as.integer(length(samp_sort)*.83)
print(samp_sort[eightythird_pctl])
#OR quantile(samp, 0.83), can give a vector for percentile values

samp_zero_cent <- samp-mean(samp)
print(samp_zero_cent)
sort(samp_zero_cent)




ages         <- c(10, 20, NA, 7)
names        <- c("Jim", "Kim", "Bob", "Joe")
mean_age     <- mean(ages)                                   # NA

na_ages      <- is.na(ages)                                  # F F T F
non_na_ages  <- ages[!na_ages]                               # 10 20 7
non_na_names <- names[!na_ages]                              # Jim Kim Joe

print(mean(ages, na.rm = T))                                 #removes the NA values before doing function

ages_order   <- order(ages)                                  #gives an indexing vector
print(ages_order)

ages_sorted  <- ages[ages_order]
names_sorted <- names[ages_order]
print(ages_sorted)
print(names_sorted)


#######################

ages                             <- c(10, 20, NA, 7)
names(ages)                      <- c("Joe", "Kim", "Bob", "Jim")
print(ages)

ages_subset                      <- ages[c("Joe", "Bob")]
print(ages_subset)


melanie                          <- list(2, c(24, 21),
                                         c("Rayne", "Dane"),
                                         c("AA", "BA", "MS", "PhD"),
                                         list(("Cuban"), c("life", "awesome"))
                                         )
melanie_part_2                   <- melanie[2]                                      # gives you a list containing element 2: list(c(24,21))
kids_ages                        <- melanie[[2]]                                    # creates a vector from a list: c(24,21)
print(melanie[[2]][2])                                                              # 21

attr(melanie, "CurrentWorkshop") <- "Software Carpentry"
 
print(melanie)


print(attr(ages, "names"))                                                          #metadata we previously associated with ages is also an attribute



names(melanie)                   <- c("numkids", "ageskids", "nameskids", "degrees","spouse")       # could have done in original list with list(numkids=2, etc)


print(melanie)

kids_ages                        <- melanie$ageskids                                # 24 21
kids_ages                        <- melanie[["ageskids"]]                           # 24 21

str(melanie)                                                                        # structure of object

####################

samp1                            <- rnorm(100, mean = 20, sd = 3)
samp2                            <- rnorm(100, mean = 10, sd = 3)

t_res                            <- t.test(samp1,samp2)

print(t_res)
str(t_res)

answer                           <- t_res$p.value




######################
#shouldn't have used "names" as a vector name before, it is a reserved word in R

firstnames                       <- c("Joe", "Kim", "Bob", "Jim")
ages                             <- c(10, 20, NA, 7)
people                           <- list(firstnames = firstnames, ages = ages)

people                           <- data.frame(firstnames, ages, stringsAsFactors = FALSE)
people$dietarypreference         <- c("Vegan", NA, NA, "Authoritarian")



print(people)
print(people$firstnames)
print(people[[1]])

colnames(people)                 <- c("First", "Age", "Diet")
rownames(people)                 <- c("p1", "p2", "p3", "p4")

print(people[ c(T, F, T, F), c(3, 2)])
print(people[ c("p3", "p2"), c(3, 2)])

print(people$Age)

non_na_people                    <- people[ !is.na(people$Age) , ]
print(non_na_people)

people_sorted_by_age             <- people[ order(people$Age), ]
print(people_sorted_by_age)

###################################################
# built in data frames

data()
USArrests
USArrests$State                  <- rownames(USArrests)     

#how many states have a murder rate > .5 rape rate

high_ratio                       <- USArrests[USArrests$Murder > (0.5*USArrests$Rape), ]
print(high_ratio)
print(length(high_ratio$Murder))


#highest combined crime rate?

USArrests$Total                  <- USArrests$Murder + USArrests$Assault + USArrests$Rape
high_total                       <- USArrests[ order(USArrests$Murder), ]
print(high_total[ length(high_total$Total), ])


#split data into high_murder and low_murder data frames based on median murder rate, run t-test on resulting assault vectors

murder_breakdown                 <- list( lowmurder = USArrests[ USArrests$Murder < median(USArrests$Murder), ], highmurder = USArrests[ USArrests$Murder > median(USArrests$Murder), ])

print(murder_breakdown$lowmurder)
print(murder_breakdown$highmurder)


##########################################
rm(list =ls())
print(head(iris))

#function variables are local in R, as in Python

coeff_of_var <- function(x, remove_nas = FALSE){
  sdx <- sd(x, na.rm = remove_nas)
  mx <- mean(x, na.rm = remove_nas)
  covx <- sdx/mx
  return(covx)
}


sdx <- "Hi"
samp <- rnorm(100, mean=20, sd=3)
answ <- coeff_of_var(samp, TRUE)                                                        #3/20
print(answ)
print(sdx)

##########parameter strategy
#coeff_of_var <- function(x, ...){
#  sdx <- sd(x, ...)
#  mx <- mean(x, ...)
#  covx <- sdx/mx
#  return(covx)
#}


#sdx <- "Hi"
#samp <- rnorm(100, mean=20, sd=3)
#answ <- coeff_of_var(samp, na.rm=TRUE)                                                        #3/20
#print(answ)
#print(sdx)


diff_of_cov <- function(x, y, remove_nas = FALSE){
  covx <- coeff_of_var(x, remove_nas)
  covy <- coeff_of_var(y, remove_nas)
  return(covx-covy)
}

samp1 <- rnorm(100, mean=20, sd=3)
samp2 <- rnorm(50, mean=20, sd=1)
samp3 <- rnorm(100, mean=20, sd=5)

samples <- list(samp1=samp1, samp2=samp2, samp3=samp3)

covs = list(samp1 = coeff_of_var(samples$samp1),
            samp2 = coeff_of_var(samples$samp2),
            samp3 = coeff_of_var(samples$samp3))
str(covs)

print(coeff_of_var)

#faster way
covs <- lapply(samples, coeff_of_var)
str(covs)


iris2 <- iris
iris2$Species <- NULL # delete a column
print(iris2)

answ <- df()

#my effort didn't work
#coeff_of_cols <- function(x, remove_na = FALSE){
#  answ <- df[ 1, length(x[ 1, ])]
#  for(colnum in length(x[ 1, ])){
#    answ[1, colnum] <- coeff_of_var((x[ , 1]), na.rm = remove_na)
#    return(answ)
#  }
#  return(answ)
#}

coeff_of_cols <- function(df){
  covs <- lapply(df, coeff_of_var)
  return(covs)
}

answer <- coeff_of_cols(iris2, F)
print(answer)

answ <- df[ 1, length(iris2[ 1, ])]
print(answ)





#########################
#file io

?write.table

