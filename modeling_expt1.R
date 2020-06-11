
# ------------------------------------------------------------------------- #
# --------- Adding types, but not tokens, affects property induction ------ #
# ----------------- Modeling Experiment 1 conditions ---------------------- #
# -------------------------------------------------------------------------- #

require(cowplot)
require(tidyverse)
require(ggpubr)
require(BayesFactor)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./BayesGen-BX.R")

# ------------ PARAMETERS ----------------------------- #
phi <- 1                 # phi = 1: uniform prior over hypotheses, phi > 1: greater prior belief in larger hypotheses
nits <- 1000             # number of iterations

# ------ initial values for parameters ---------------- #
# THETA to represent sampling assumptions/informational value
thetaType <- .2   # theta = 1: strong sampling, theta = 0: weak sampling
thetaToken <- .05

# TRAINING items
target1 <- .45    # first type
target2 <- .46    # second type
target3 <- .44    # third type
target4 <- .47    # 4th type

typesd <- .06     # when randomly generating stimulus values for types, how much standard deviation to use? 
tokensd <- .009   # when randomly generating stimulus values for tokens, how much standard deviation to use? tokensd < typesd

# TEST items
test <- c(.51, .707, .9) # similarity values for high-, med-, low-sim test/generalization items
nt <- length(test)       # number of test items

# inconsequential details
bg1 <- matrix(data = c(rep(0, nits*nt)),   # create enough 0s for each iteration to fill with predicted generalisation probability
              nrow = nt)                   # 1 row for each test item (high, medium, low sim)
bg11 <- bg111 <- bg1111 <- bg2 <- bg3 <- bg4 <- bg1    # need one matrix per condition
bg2v1 <- bg2v2 <- bg3v2 <- bg4v2 <- bg1
bg31 <- bg21 <- bg211 <- bg2

# --------------------------- generate predictions ------------------------------ #
parits <- 10

testPattern1 <- matrix(data = c(rep(0, parits*3)), nrow = parits)
testPattern2 <- matrix(data = c(rep(0, parits*3)), nrow = parits)

typesIncreaseH <- matrix(data = c(rep(0, parits)), nrow = parits)
typesDecreaseM <- matrix(data = c(rep(0, parits)), nrow = parits)
typesDecreaseL <- matrix(data = c(rep(0, parits)), nrow = parits)

# use a loop
for (a in 1:parits) {
  
  thetaType <- runif(1, min = .2, max = .4)
  thetaToken <- runif(1, min = thetaType - .2, max = thetaType - .1)
  
  targetsM <- runif(1, .2, .6)
  targetsW <- runif(1, 0, .2)
  
  targets <- runif(4, targetsM - targetsW, targetsM + targetsW)
  targets <- sort(targets)
  target1 <- targets[2]
  target2 <- targets[3]
  target3 <- targets[1]
  target4 <- targets[4]
  
  typesd <- runif(1, .01, .1)
  tokensd <- typesd/6

    for( i in 1:nits ) {     # average out predictions over nits iterations
      
      bg1 <- matrix(data = c(rep(0, nits*nt)),   # create enough 0s for each iteration to fill with predicted generalisation probability
                    nrow = nt)                   # 1 row for each test item (high, medium, low sim)
      bg11 <- bg111 <- bg1111 <- bg2 <- bg3 <- bg4 <- bg1    # need one matrix per condition
      bg2v1 <- bg2v2 <- bg3v2 <- bg4v2 <- bg1
      bg31 <- bg21 <- bg211 <- bg2
      
      train1 <- rnorm(1, mean = target1, sd = typesd) # generates value for 1st type from a normal distribution
      train2 <- rnorm(1, mean = target2, sd = typesd)
      train3 <- rnorm(1, mean = target3, sd = typesd)
      train4 <- rnorm(1, mean = target4, sd = typesd)
      
      # generalisation from adding types
      testH <- (max(c(train1, train2, train3, train4)) + .05)
      test <- c(testH, testH + ((.9 - testH) / 2), .9)
      
      # for each iteration, fill the next column with gen probabilities for high, medium, low sim categories (in that order)
      bg1[,i] <- BayesGen(train1, test, thetaType, phi)                # generalisation based on 1 type
      bg11[,i] <- BayesGen(c(train1, train2), test, thetaType, phi)    # generalisation based on 2 types
      bg111[,i] <- BayesGen(c(train1, train2, train3), test, thetaType, phi)          # based on 3 types
      bg1111[,i] <- BayesGen(c(train1, train2, train3, train4), test, thetaType, phi)          # 4 types
      
      # generalisation from adding tokens 
      train1a <- rnorm(1, mean = train1, sd = tokensd)  # the token is constrained to be within a range around the first type
      train1b <- rnorm(1, mean = train1, sd = tokensd)  # the above is true for all tokens
      train1c <- rnorm(1, mean = train1, sd = tokensd)
      
      bg2[,i] <- (BayesGen(train1, test, thetaType, phi))*1/2 +
                    (BayesGen(train1a, test, thetaToken, phi))*1/2
      
      bg3[,i] <- (BayesGen(train1, test, thetaType, phi))*1/3 +
                    (BayesGen(c(train1a, train1b), test, thetaToken, phi))*2/3
      
      bg4[,i] <- (BayesGen(train1, test, thetaType, phi))*1/4 +
        (BayesGen(c(train1a, train1b, train1c), test, thetaToken, phi))*3/4
      
      # adding types with 2 tokens
      bg21[,i] <- (BayesGen(c(train1, train2), test, thetaType, phi))*2/3 +
                     (BayesGen(train1a, test, thetaToken, phi))*1/3
      bg211[,i] <- (BayesGen(c(train1, train2, train3), test, thetaType, phi))*3/4 +
                     (BayesGen(train1a, test, thetaToken, phi))*1/4
      
      # adding types with 3 tokens
      bg31[,i] <- (BayesGen(c(train1, train2), test, thetaType, phi))*2/4 +
                    (BayesGen(c(train1a, train1b), test, thetaToken, phi))*2/4
    }
    
    # ----------------------- plot predictions ------------------------------ #
    # ------------------ effect of adding types  ---------------------------- #
    bg1 <- rowMeans(bg1)          # calculate the mean of the N(nits) generalisation probabilities for each similarity category
    bg11 <- rowMeans(bg11)        # for 11 condition
    bg111 <- rowMeans(bg111)      # for 111 condition etc.,
    bg1111 <- rowMeans(bg1111)
    
    vals <- as.data.frame(cbind(test, bg1, bg11, bg111, bg1111))           # combine the similarity category values with generalisation probs for each condtiion
    vals <- gather(vals, key = "genCond", value = "genProb", bg1:bg1111)   # gather so each row is a different generalisation probability
    
    # typeIncrease1 <- vals[1,3] < vals[4,3]  # bg1[1] < bg11[1]
    # typeIncrease2 <- vals[4,3] < vals[7,3]
    # typeIncrease3 <- vals[7,3] < vals[10,3]
    # 
    # typeDecreaseM1 <- bg1[2] > bg11[2]
    # typeDecreaseM2 <- bg11[2] > bg111[2]
    # typeDecreaseM3 <- bg111[2] > bg1111[2]
    # 
    # pattern1 <- c(typeIncrease1, typeIncrease2, typeIncrease3)
    # pattern2 <- c(typeDecreaseM1, typeDecreaseM2, typeDecreaseM3)
    # 
    # testPattern1[a,] <- pattern1
    # testPattern2[a,] <- pattern2
    typesIncreaseH[a,] <- bg1[1] < bg1111[1]
    typesDecreaseM[a,] <- bg1[2] > bg1111[2]
    typesDecreaseL[a,] <- bg1[3] > bg1111[3]
}

typesIncreaseH
typesDecreaseM
typesDecreaseL

# 
# 
# trains <- c(train1, train2, train3, train4)        # collate the training stimulus values
# trains <- as.data.frame(trains)
# 
# ggTypes <- ggplot(vals, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
#   geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
#   geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
#   theme_bw(base_size = 12) +
#   labs(title = "Adding types with 1 token", x = "Stimulus value", y = "Generalisation Probability") +
#   scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
#   scale_color_manual(name = "Evidence Sample", labels = c("1", "11", "111", "1111"), values = c("red", "coral1", "seagreen", "slateblue3")) +
#   scale_linetype_manual(name = "Evidence Sample", labels = c("1", "11", "111", "1111"), values = c("solid", "longdash", "dotdash", "dotted")) +
#   geom_point(data = trains, aes(x = trains, y = 1)) +    # show training stimulus values
#   geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
#   theme(legend.position = c(.8, .7))
# 
# # # ----------------- effect of adding tokens ------------------------ #
# bg2 <- rowMeans(bg2)
# bg3 <- rowMeans(bg3)
# bg4 <- rowMeans(bg4)
# 
# valsTokes <- as.data.frame(cbind(test, bg1, bg2, bg3, bg4))
# valsTokes <- gather(valsTokes, key = "genCond", value = "genProb", -test)

# trainsTokes <- c(train1, train1a, train1b, train1c)
# trainsTokes <- as.data.frame(trainsTokes)
# 
# ggTokens <- ggplot(valsTokes, aes(x = test, y = genProb)) +
#   geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
#   geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
#   theme_bw(base_size = 12) +
#   labs(title = "Adding tokens with 1 type", x = "Stimulus value", y = "Generalisation Probability") +
#   scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
#   scale_color_manual(name = "Evidence Sample", labels = c("1", "2", "3", "4"), values = c("red", "coral1", "seagreen", "slateblue3")) +
#   scale_linetype_manual(name = "Evidence Sample", labels = c("1", "2", "3", "4"), values = c("solid", "longdash", "dotdash", "dotted")) +
#   geom_point(data = trainsTokes, aes(x = trainsTokes, y = 1)) +
#   geom_vline(xintercept = test, linetype = "dashed", colour = "blue") +
#   theme(legend.position = c(.8, .7))
# 
# # adding types with 2 tokens
# bg21 <- rowMeans(bg21)
# bg211 <- rowMeans(bg211)
# 
# valsTy1 <- as.data.frame(cbind(test, bg2, bg21, bg211))
# valsTy1 <- gather(valsTy1, key = "genCond", value = "genProb", bg2:bg211)
# trainsTy1 <- c(train1, train2, train3, train1a, train1b)
# trainsTy1 <- as.data.frame(trainsTy1)
# 
# ggTypes1 <- ggplot(valsTy1, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
#   geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
#   geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
#   theme_bw(base_size = 12) +
#   labs(title = "With 2 tokens", x = "Stimulus value", y = "Generalisation Probability") +
#   scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
#   scale_color_manual(name = "Evidence Sample", labels = c("2", "21", "211"), values = c("red", "coral1", "seagreen")) +
#   scale_linetype_manual(name = "Evidence Sample", labels = c("2", "21", "211"), values = c("solid", "longdash", "dotdash")) +
#   geom_point(data = trainsTy1, aes(x = trainsTy1, y = 1)) +    # show training stimulus values
#   geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
#   theme(legend.position = c(.8, .7))
# # ggTypes1
# 
# # adding types with 3 tokens
# bg31 <- rowMeans(bg31)
# valsTy2 <- as.data.frame(cbind(test, bg3, bg31))
# valsTy2 <- gather(valsTy2, key = "genCond", value = "genProb", bg3:bg31)
# trainsTy2 <- c(train1, train2, train1a, train1b)
# trainsTy2 <- as.data.frame(trainsTy2)
# 
# ggTypes2 <- ggplot(valsTy2, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
#   geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
#   geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
#   theme_bw(base_size = 12) +
#   labs(title = "With 3 tokens", x = "Stimulus value", y = "Generalisation Probability") +
#   scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
#   scale_color_manual(name = "Evidence Sample", labels = c("3", "31"), values = c("red", "coral1")) +
#   scale_linetype_manual(name = "Evidence Sample", labels = c("3", "31"), values = c("solid", "longdash")) +
#   geom_point(data = trainsTy1, aes(x = trainsTy1, y = 1)) +    # show training stimulus values
#   geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
#   theme(legend.position = c(.8, .7))
# # ggTypes2
# 
# # adding tokens with 2 types
# valsTok2 <- as.data.frame(cbind(test, bg11, bg21, bg31))
# valsTok2 <- gather(valsTok2, key = "genCond", value = "genProb", bg11:bg31)
# trainsTok2 <- c(train1, train2, train1a, train1b)
# trainsTok2 <- as.data.frame(trainsTok2)
# 
# ggTok2 <- ggplot(valsTok2, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
#   geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
#   geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
#   theme_bw(base_size = 12) +
#   labs(title = "With 2 types", x = "Stimulus value", y = "Generalisation Probability") +
#   scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
#   scale_color_manual(name = "Evidence Sample", labels = c("11", "21", "31"), values = c("red", "coral1", "seagreen")) +
#   scale_linetype_manual(name = "Evidence Sample", labels = c("11", "21", "31"), values = c("solid", "longdash", "dotdash")) +
#   geom_point(data = trainsTok2, aes(x = trainsTok2, y = 1)) +    # show training stimulus values
#   geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
#   theme(legend.position = c(.8, .7))
# # ggTok2
# 
# # adding tokens with 3 types
# valsTok3 <- as.data.frame(cbind(test, bg111, bg211))
# valsTok3 <- gather(valsTok3, key = "genCond", value = "genProb", bg111:bg211)
# trainsTok3 <- c(train1, train2, train3, train1a)
# trainsTok3 <- as.data.frame(trainsTok3)
# 
# ggTok3 <- ggplot(valsTok3, aes(x = test, y = genProb)) +     # plot predicted generalisation ratings
#   geom_point(aes(x = test, y = genProb, group = genCond, colour = genCond), size = 2) +
#   geom_line(aes(colour = genCond, linetype = genCond), size = 1.4) +
#   theme_bw(base_size = 12) +
#   labs(title = "With 3 types", x = "Stimulus value", y = "Generalisation Probability") +
#   scale_x_continuous(limits = c(min(trains), 1), breaks = seq(.4, 1, by = .1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
#   scale_color_manual(name = "Evidence Sample", labels = c("111", "211"), values = c("red", "coral1")) +
#   scale_linetype_manual(name = "Evidence Sample", labels = c("111", "211"), values = c("solid", "longdash")) +
#   geom_point(data = trainsTok3, aes(x = trainsTok3, y = 1)) +    # show training stimulus values
#   geom_vline(xintercept = test, linetype = "dashed", colour = "blue")  +
#   theme(legend.position = c(.8, .7))
# # ggTok3


