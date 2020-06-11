require(cowplot)
require(tidyverse)
require(ggpubr)
require(BayesFactor)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./BayesGen-BX.R")

# this deals with the conditions from Experiment 1
# it models 1 -> 11 -> 111 -> 1111 & 1 -> 2 -> 3 -> 4
# and now also deals with 21, 211, 31, 22 conditions

# -------------------------- fixed and free parameters ------------------------------------- #
# fixed (for now)
phi <- 1              # phi = 1 -> uniform prior over all hypotheses, phi > 1 -> greater prior belief in larger hypotheses
thetaType <- .2          # theta = 1 -> strong sampling
thetaRep3 <- .05
thetaRep2 <- thetaRep3# - .05
thetaRep <- thetaRep3# - .1

nits <- 1000          # number of iterations

test <- c(.51, .707, .9)  # this is overridden in l. 47
nt <- length(test)                                    # number of test items

bg1 <- matrix(data = c(rep(0, nits*nt)),   # create enough 0s for each iteration x each test item - to be filled with predicted generalisation probability
              nrow = nt)                   # 1 row for each test item (high, medium, low sim)
bg11 <- bg111 <- bg1111 <- bg2 <- bg3 <- bg4 <- bg1    # make all others in the same way - need one matrix per condition
bg2v1 <- bg2v2 <- bg3v2 <- bg4v2 <- bg1
bg31 <- bg21 <- bg211 <- bg2

target1 <- .45    
target2 <- .46
target3 <- .44
target4 <- .47
typesd <- .06      # when randomly generating stimulus values for types, how much standard deviation to use? 

probIgnore <- 0  # probability that an individual ignores repetitions - ignore/not sort of represents sampling with/without replacement?
tokensd <- .009    # when randomly generating stimulus values for tokens, how much standard deviation to use? tokensd < typesd

ignores <- rbinom(nits, size = 1, prob = probIgnore)           # if 1 -> on that iteration, ignore repetition
ignores <- matrix(rep(ignores, nt), nrow = nt, byrow = TRUE)   # just copy the ignores across 3 rows to conform to following ifelse condition
# --------------------------- generate predictions ------------------------------ #
for( i in 1:nits ) {     # average out predictions over nits iterations
  
  train1 <- rnorm(1, mean = target1, sd = typesd) #runif(1)*.2 + .4 # generates number randomly from a uniform distribution, then /.2 makes it smallish, then +.4 to approach .5
  train2 <- rnorm(1, mean = target2, sd = typesd) #runif(1)*.2 + .4 # 2nd training exemplar
  train3 <- rnorm(1, mean = target3, sd = typesd) #runif(1)*.2 + .4 # 3rd training exemplar
  train4 <- rnorm(1, mean = target4, sd = typesd) #runif(1)*.2 + .4 # 4th training exemplar
  
  # generalisation from adding types
  # testH <- (max(c(train1, train2, train3, train4)) + .01)
  # test <- c(testH, testH + ((.9 - testH) / 2), .9)
  
  # for each iteration, fill the next column with gen probabilities for high, medium, low sim categories (in that order)
  bg1[,i] <- BayesGen(train1, test, thetaType, phi)                # generalisation based on 1 training exemplar
  bg11[,i] <- BayesGen(c(train1, train2), test, thetaType, phi)    # generalisation based on 2 training exemplars
  bg111[,i] <- BayesGen(c(train1, train2, train3), test, thetaType, phi)          # based on 3 training exemplars
  bg1111[,i] <- BayesGen(c(train1, train2, train3, train4), test, thetaType, phi)          # 4 training exemplars
  
  # generalisation from adding tokens 
  train1a <- rnorm(1, mean = train1, sd = tokensd)  # the repetition is constrained to be within a range around the first training item
  train1b <- rnorm(1, mean = train1, sd = tokensd)  # the above is true for all tokens
  train1c <- rnorm(1, mean = train1, sd = tokensd)
  
  # is the second token ignored or treated as a discrete exemplar?
  # bg2[,i] <- if_else(ignores[,i] == 1,     # if the set probIgnore dictates ignoring a repetition in this iteration,
  #                    BayesGen(train1, test, thetaRep, phi),              # then ignore repetition and behave as if only 1 training exemplar
  #                    # BayesGen(c(train1, train1), test, theta, phi),              # then ignore repetition and behave as if only 1 training exemplar
  #                    BayesGen(c(train1, train1a), test, thetaRep, phi))  # otherwise, treat as 2 (only slightly) discrete exemplars
  # 
  bg2[,i] <- (BayesGen(train1, test, thetaType, phi))*1/2 +
                (BayesGen(train1a, test, thetaRep, phi))*1/2
  
  # bg3[,i] <- if_else(ignores[,i] == 1,
  #                    BayesGen(train1, test, thetaRep, phi),              # then ignore repetitions and behave as if only 1 training exemplar
  #                    BayesGen(c(train1, train1a, train1b), test, thetaRep, phi))  # otherwise, treat as 3 discrete exemplars
  
  bg3[,i] <- (BayesGen(train1, test, thetaType, phi))*1/3 +
                (BayesGen(c(train1a, train1b), test, thetaRep, phi))*2/3
  
  # bg4[,i] <- if_else(ignores[,i] == 1,
  #                    BayesGen(train1, test, thetaRep, phi),              # then ignore repetitions and behave as if only 1 training exemplar
  #                    BayesGen(c(train1, train1a, train1b, train1c), test, thetaRep, phi)) # otherwise, treat as 4 discrete exemplars
  
  bg4[,i] <- (BayesGen(train1, test, thetaType, phi))*1/4 +
    (BayesGen(c(train1a, train1b, train1c), test, thetaRep, phi))*3/4
  
  # adding novel instances with 1 repetition
  bg21[,i] <- (BayesGen(c(train1, train2), test, thetaType, phi))*2/3 +
                 (BayesGen(train1a, test, thetaRep2, phi))*1/3
  bg211[,i] <- (BayesGen(c(train1, train2, train3), test, thetaType, phi))*3/4 +
                 (BayesGen(train1a, test, thetaRep3, phi))*1/4
  
  # adding novel instances with 2 repetitions
  bg31[,i] <- (BayesGen(c(train1, train2), test, thetaType, phi))*2/4 +
                (BayesGen(c(train1a, train1b), test, thetaRep2, phi))*2/4
}

# ------------------- categorise predictions into 10 bins/response choices -------------------- #
catNames <- c("high", "med", "low")                                # names of similarity categories
# respLabs <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10')   # names of response options (1-10 scale)
# 
# bg2respProbs <- function(bgprobs) {  # function to convert bayes generalisation probabilities (0-1) to response (1-10 categories) probabilities 
#   bgxx <- bgprobs                    # which condition's generalisation probabilities are we dealing with?
#   rownames(bgxx) <- catNames         # specify rownames that represent similarity categories
#   # bgxx <- round(bgxx*10/11) + 1      # *10 to make <1 probabilities->integers, *1/11 shifts from 11-pt scale (0-10)->10-pt scale (0-9), +1 to shift to 1-10, round forces belonging to 1/10 categories
#   max(bgxx)                          # max should be 10
#   min(bgxx)                          # min should be 1
#   
#   bgxx <- as.data.frame(t(bgxx))   # transpose so that similarity categories occupy columns instead of rows, then convert to dataframe
#   
#   highN <- transform(bgxx, group = cut(high, breaks = seq(1, 11, by = 1),     # create a new group column that groups high responses into 10 bins
#                                        labels = respLabs,                     # label each bin
#                                        include.lowest = TRUE, right = FALSE)) # each bin includes responses that go up to the higher interval boundary
#   medN <- transform(bgxx, group = cut(med, breaks = seq(1, 11, by = 1), labels = respLabs, include.lowest = TRUE, right = FALSE)) # repeat for med responses
#   lowN <- transform(bgxx, group = cut(low, breaks = seq(1, 11, by = 1), labels = respLabs, include.lowest = TRUE, right = FALSE)) # repeat for low responses
#   
#   respProbs <- rbind(table(highN$group), table(medN$group), table(lowN$group))  # combine these grouped responses together
#   rownames(respProbs) <- catNames                 # label them according to similarity category
#   respProbs <- respProbs/nits                     # divide the counts/number of observations to get the probabilities
#   respProbs                                       # print these response probabilities                            
# }

# ----------------------- plot predictions ------------------------------ #
# ------------------ effect of adding types  ---------------------------- #
# bg1 <- round(bg1*10) + 1     # the logic of this arithmetic is explained in line 82
# bg11 <- round(bg11*10) + 1
# bg111 <- round(bg111*10) + 1
# bg1111 <- round(bg1111*10) + 1

bg1 <- rowMeans(bg1)          # calculate the mean of the N(nits) generalisation probabilities for each similarity category
bg11 <- rowMeans(bg11)        # for 11 condition
bg111 <- rowMeans(bg111)      # for 111 condition etc.,
bg1111 <- rowMeans(bg1111)

vals <- as.data.frame(cbind(test, bg1, bg11, bg111, bg1111))           # combine the similarity category values with generalisation probs for each condtiion
vals <- gather(vals, key = "genCond", value = "genProb", bg1:bg1111)   # gather so each row is a different generalisation probability

trains <- c(train1, train2, train3, train4)        # collate the training stimulus values
trains <- as.data.frame(trains)

ggTypes <- ggplot(vals, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
  geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
  geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
  theme_bw(base_size = 12) +
  labs(title = "Adding types with 1 token", x = "Stimulus value", y = "Generalisation Probability") +
  scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  scale_color_manual(name = "Evidence Sample", labels = c("1", "11", "111", "1111"), values = c("red", "coral1", "seagreen", "slateblue3")) +
  scale_linetype_manual(name = "Evidence Sample", labels = c("1", "11", "111", "1111"), values = c("solid", "longdash", "dotdash", "dotted")) +
  geom_point(data = trains, aes(x = trains, y = 1)) +    # show training stimulus values
  geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
  theme(legend.position = c(.8, .7))
# ggTypes

# # ----------------- effect of adding tokens ------------------------ #
# bg2 <- round(bg2*10) + 1
# bg3 <- round(bg3*10) + 1
# bg4 <- round(bg4*10) + 1

bg2 <- rowMeans(bg2)
bg3 <- rowMeans(bg3)
bg4 <- rowMeans(bg4)

valsTokes <- as.data.frame(cbind(test, bg1, bg2, bg3, bg4))
valsTokes <- gather(valsTokes, key = "genCond", value = "genProb", -test)

trainsTokes <- c(train1, train1a, train1b, train1c)
trainsTokes <- as.data.frame(trainsTokes)

ggTokens <- ggplot(valsTokes, aes(x = test, y = genProb)) +
  geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
  geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
  theme_bw(base_size = 12) +
  labs(title = "Adding tokens with 1 type", x = "Stimulus value", y = "Generalisation Probability") +
  scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  scale_color_manual(name = "Evidence Sample", labels = c("1", "2", "3", "4"), values = c("red", "coral1", "seagreen", "slateblue3")) +
  scale_linetype_manual(name = "Evidence Sample", labels = c("1", "2", "3", "4"), values = c("solid", "longdash", "dotdash", "dotted")) +
  geom_point(data = trainsTokes, aes(x = trainsTokes, y = 1)) +
  geom_vline(xintercept = test, linetype = "dashed", colour = "blue") +
  theme(legend.position = c(.8, .7))
ggTokens

# plot_grid(ggTypes, ggTokens, labels = "AUTO", ncol = 1)  # plot types and tokens graphs next to each other

# adding novel instances with 1 repetition
bg21 <- rowMeans(bg21)
bg211 <- rowMeans(bg211)

valsTy1 <- as.data.frame(cbind(test, bg2, bg21, bg211))
valsTy1 <- gather(valsTy1, key = "genCond", value = "genProb", bg2:bg211)
trainsTy1 <- c(train1, train2, train3, train1a, train1b)
trainsTy1 <- as.data.frame(trainsTy1)

ggTypes1 <- ggplot(valsTy1, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
  geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
  geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
  theme_bw(base_size = 12) +
  labs(title = "With 2 tokens", x = "Stimulus value", y = "Generalisation Probability") +
  scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  scale_color_manual(name = "Evidence Sample", labels = c("2", "21", "211"), values = c("red", "coral1", "seagreen")) +
  scale_linetype_manual(name = "Evidence Sample", labels = c("2", "21", "211"), values = c("solid", "longdash", "dotdash")) +
  geom_point(data = trainsTy1, aes(x = trainsTy1, y = 1)) +    # show training stimulus values
  geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
  theme(legend.position = c(.8, .7))
# ggTypes1

# adding novel instances with 2 repetitions
bg31 <- rowMeans(bg31)
valsTy2 <- as.data.frame(cbind(test, bg3, bg31))
valsTy2 <- gather(valsTy2, key = "genCond", value = "genProb", bg3:bg31)
trainsTy2 <- c(train1, train2, train1a, train1b)
trainsTy2 <- as.data.frame(trainsTy2)

ggTypes2 <- ggplot(valsTy2, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
  geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
  geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
  theme_bw(base_size = 12) +
  labs(title = "With 3 tokens", x = "Stimulus value", y = "Generalisation Probability") +
  scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  scale_color_manual(name = "Evidence Sample", labels = c("3", "31"), values = c("red", "coral1")) +
  scale_linetype_manual(name = "Evidence Sample", labels = c("3", "31"), values = c("solid", "longdash")) +
  geom_point(data = trainsTy1, aes(x = trainsTy1, y = 1)) +    # show training stimulus values
  geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
  theme(legend.position = c(.8, .7))
# ggTypes2

# adding repetitions with 2 novel instances
valsTok2 <- as.data.frame(cbind(test, bg11, bg21, bg31))
valsTok2 <- gather(valsTok2, key = "genCond", value = "genProb", bg11:bg31)
trainsTok2 <- c(train1, train2, train1a, train1b)
trainsTok2 <- as.data.frame(trainsTok2)

ggTok2 <- ggplot(valsTok2, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
  geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
  geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
  theme_bw(base_size = 12) +
  labs(title = "With 2 types", x = "Stimulus value", y = "Generalisation Probability") +
  scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  scale_color_manual(name = "Evidence Sample", labels = c("11", "21", "31"), values = c("red", "coral1", "seagreen")) +
  scale_linetype_manual(name = "Evidence Sample", labels = c("11", "21", "31"), values = c("solid", "longdash", "dotdash")) +
  geom_point(data = trainsTok2, aes(x = trainsTok2, y = 1)) +    # show training stimulus values
  geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
  theme(legend.position = c(.8, .7))
# ggTok2

# adding repetitions with 3 novel instances
valsTok3 <- as.data.frame(cbind(test, bg111, bg211))
valsTok3 <- gather(valsTok3, key = "genCond", value = "genProb", bg111:bg211)
trainsTok3 <- c(train1, train2, train3, train1a)
trainsTok3 <- as.data.frame(trainsTok3)

ggTok3 <- ggplot(valsTok3, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
  geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
  geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
  theme_bw(base_size = 12) +
  labs(title = "With 3 types", x = "Stimulus value", y = "Generalisation Probability") +
  scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  scale_color_manual(name = "Evidence Sample", labels = c("111", "211"), values = c("red", "coral1")) +
  scale_linetype_manual(name = "Evidence Sample", labels = c("111", "211"), values = c("solid", "longdash")) +
  geom_point(data = trainsTok3, aes(x = trainsTok3, y = 1)) +    # show training stimulus values
  geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
  theme(legend.position = c(.8, .7))
# ggTok3

plot_grid(ggTypes, ggTypes1, ggTypes2, ggTokens, ggTok2, ggTok3, labels = "AUTO", ncol = 3)  # plot types and tokens graphs next to each other

# --------------- calculate sum of squares errors ---------------- #
partMeans <- read.csv("./csv/means_types-tokens.csv")
predMeans <- rbind(vals, valsTokes)

totMeans <- cbind(partMeans, predMeans)
totMeans$genProb <- totMeans$genProb*10

totMeans$sse <- (totMeans$meanResp - totMeans$genProb)^2
totMeans$genProb <- ifelse(totMeans$genProb < 1, 1, totMeans$genProb)  # because minimum rating = 1

# sum(totMeans$sse)   # for model comparison, comparing theta model vs. pIgnore model

ggscatter(totMeans, x = "genProb", y = "meanResp",
          add = "reg.line", conf.int = TRUE, cor.method = "pearson", cor.coef = FALSE,
          xlab = "Model Predictions", ylab = "Mean Participant Ratings", main = "A. Experiment 1") +
  coord_cartesian(xlim = c(1,9)) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(limits = c(1,9), breaks = 1:9)

corr <- correlationBF(x = totMeans$genProb, y = totMeans$meanResp)
corr
