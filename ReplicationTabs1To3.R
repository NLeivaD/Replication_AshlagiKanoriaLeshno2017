################################################################################
#### PAPER'S TABLES REPLICATIONS: ASHLAGI, I., KANORIA, Y. & LESHNO, J. D.  ####
#### UNBALANCED RANDOM MATCHING MARKETS: THE STARK EFFECT OF COMPETITION    ####
#### NICOLAS LEIVA DIAZ - NLEIVAD@FEN.UCHILE.CL                             ####
#### OCTOBER 2023                                                           ####
################################################################################

# CLEAN ALL WORK SPACE
rm(list = ls())

# DISCLAIMER: THIS RESULTS REPLICATIONS RELIES ON MATCHINGR PACKAGE MANTAINED BY
# JAN TILLY AND STARGAZER FOR TABLES.
library(matchingR)
library(stargazer)

# I. TABLES 1 AND 2
# I.1. DEFINE THE PARAMETERS
NumMen <- c(100, 200, 500, 1000, 2000, 5000)
DiffSets <- c(-10, -5, -1, 0, 1, 2, 3, 5, 10)
Simulations <- 1000

# I.2. CREATE OBJECTS TO STORE THE RESULTS:
#       A. A MATRIX TO STORE RESULTS FOR TABLE 1, MEN'S AVERAGE RANK OF WIVES
#          UNDER THE MOSM, WOSM AND EST. A MATRIX WITH 18 ROWS (3 ROWS THAT ARE
#          AVERAGE RANK UNDER MOSM, WOSM AND EST FOR EACH MEN SIZE) AND 9
#          COLUMNS (FOR EACH WOMEN SIZE)
Table1 <- matrix(data = NA, nrow = 18, ncol = 9)
#       B. A MATRIX TO STORE RESULTS FOR TABLE 2, PERCENTAGE OF MEN WITH 
#          MULTIPLE STABLE PARTNERS, A MATRIX WITH 6 ROWS (FOR EACH MEN SIZE) 
#          AND WITH 9 COLUMNS (FOR EACH WOMEN SIZE)
Table2 <- matrix(data = NA, nrow = 6, ncol = 9)
#       C. AN AUXILIARY COUNTER FOR ROW NUMBER DEPENDING ON MEN SIZE
row <- 0

# I.3. DRAW SIMULATIONS: FOR EACH NUMBER OF MEN
for (m in NumMen) {
  # DEFINE ROW NUMBER AS 1 MORE THAN ACTUAL
  row <- row + 1
  # DEFINE AN AUXILIARY COUNTER FOR COLUMN NUMBER DEPENDING ON WOMEN SIZE, START
  # IN ZERO FOR EACH MEN SIZE
  col <- 0
  # FOR EACH NUMBER OF WOMEN DEFINED BY THE DIFFERENCE
  for (diff in DiffSets) {
    # DEFINE COLUMN NUMBER AS 1 MORE THAN ACTUAL
    col <- col + 1
    # DEFINE THE WOMEN SIZE AS THE MEN SIZE PLUS THE DIFFERENCE
    NumWomen <- m + diff
    # DEFINE A MATRIX TO STORE THE RESULTS FROM THE 1000 SIMULATIONS FOR EACH
    # INDICATOR:
    #     A. PERCENTAGE OF MEN WITH MULTIPLE STABLE PARTNERS
    fractions <- double(length = Simulations)
    #     B. MEN'S AVERAGE RANK OF WIVES UNDER MOSM
    AvgRankMOSM <- double(length = Simulations)
    #     C. MEN'S AVERAGE RANK OF WIVES UNDER WOSM
    AvgRankWOSM <- double(length = Simulations)
    # DEFINE THE MEN'S AVERAGE RANK OF WIVES UNDER EST, IS A DETERMINISTIC TERM
    # AND DON'T NEED SIMULATION, IT GOES TO THE CORRESPONDING COLUMN AND FOR THE
    # ROW 3 TIMES THE ACTUAL ROW NUMBER.
    Table1[3*row, col] <- ifelse(diff > 0, # IF DIFF > 0, WOMEN > MEN IN SIZE
                                 # IN THAT CASE ADD THE CORRESPONDING FORMULA
                                 (NumWomen/m)*log(NumWomen/(NumWomen - M)),
                                 # IN OTHER CASE
                                 # IF DIFF < 0, WOMEN < MEN IN SIZE
                                 ifelse(diff < 0, # APPLY THE CORRESPONDING FORMULA
                                        NumWomen/(1+((m/NumWomen)*log(m/(m - NumWomen)))),
                                        # IN OTHER CASE DIFF IS 0 AND TABLE HAS NA
                                        NA))
    # DRAW 1000 SIMULATIONS
    for (s in 1:Simulations) {
      # ON EACH SIMULATION START DEFINING THE PREFERENCE MATRIX AS AN EMPTY 
      # MATRIX WITH THE NUMBER OF COLUMNS EQUAL TO THE NUMBER OF MEN AND THE 
      # NUMBER OF ROWS EQUAL TO THE NUMBER OF WOMEN
      M <- matrix(data = NA, ncol = m, nrow = NumWomen)
      # NOW DRAW FOR EACH AGENT THE COMPLETE PREFERENCE AS DRAWING UNIFORMLY
      # RANDOM COMPLETE PREFERENCES
      for (column in 1:m) {
        M[,column] <- sample(1:NumWomen, size = NumWomen, replace = F)
      }
      # NOW DEFINE THE PREFERENCE FOR THE WOMEN, SIMILAR TO WHAT HAS BEEN DONE 
      # TO THE MEN, DEFINE AN EMPTY MATRIX WITH AS MANY ROWS AS MEN AND AS MANY 
      # COLS AS WOMEN
      W <- matrix(data = NA, ncol = NumWomen, nrow = m)
      # NOW DRAW FOR EACH AGENT THE COMPLETE PREFERENCE
      for (column in 1:NumWomen) {
        W[,column] <- sample(1:m, size = m, replace = F)
      }
      # NOW COMPUTE THE MOSM AND WOSM WITH THE galeShapley.marriageMarket 
      # FUNCTION
      MOSM <- galeShapley.marriageMarket(proposerPref = M, reviewerPref = W)
      WOSM <- galeShapley.marriageMarket(proposerPref = W, reviewerPref = M)
      # NOW CREATE THE FRACTION OF MEN WITH STABLE PARTNERS, START IN ZERO
      StablePartners <- 0
      # FOR EACH MAN CHECK THREE CONDITIONS:
      #     A. HAS A PARTNER IN MOSM
      #     B. HAS A PARTNER IN WOSM
      #     C. PARTNER UNDER MOSM AND WOSM ARE DIFFERENT
      # IF THE THREE CONDITIONS ARE TRUE, THEN ADD ONE TO THE STABLE PARTNERS
      for (man in 1:m) {
        # CHECK IF THE MAN HAS STABLE PARTNER UNDER MOSM BY CHECKING IF HIS
        # ASSIGNMENT IS NA (MISSING VALUE), IF FALSE THERE IS A PARTNER
        cond1 <- is.na(MOSM$proposals[man]) == FALSE
        # CHECK IF THE MAN HAS STABLE PARTNER UNDER WOSM BY CHECKING IF HIS
        # ASSIGNMENT IS NA (MISSING VALUE), IF FALSE THERE IS A PARTNER
        cond2 <- is.na(WOSM$engagements[man]) == FALSE
        #  CHECK IF THEIR ASSIGNMENTS ARE DIFFERENT
        cond3 <- MOSM$proposals[man] != WOSM$engagements[man]
        # IF ALL CONDITIONS ARE MET, THUS HAVE MULTIPLE STABLE PARTNERS, ADD A 1,
        # IF NOT, ADD A 0
        StablePartners <- StablePartners + ifelse(cond1 & cond2 & cond3, 1, 0)
      }
      # DEFINE AS A FRACTION
      StablePartners <- StablePartners / m
      # ADD TO THE TABLE 2 IN THE CORRESPONDING ROW AND COLUMN
      fractions[s] <- StablePartners*100
      # CREATE AN EMPTY VECTOR TO STORE MOSM RANKS
      Ranks_MOSM <- double()
      # NOW IDENTIFY THE MEN WITH PARTNER
      Matched_MOSM <- which(!is.na(MOSM$proposals))
      # FOR EACH MAN WITH PARTNER, IDENTIFY THE RANKING OF THEIR PARTNER
      for (man in Matched_MOSM) {
        # IDENTIFY THE PARTNER
        Partner <- MOSM$proposals[man]
        # IDENTIFY THE RANK
        Rank <- which(M[, man] == Partner)
        # ADD THE RANK TO RANKS
        Ranks_MOSM <- append(Ranks_MOSM, Rank)
      }
      # GET THE MEAN OF THE RANKS AND STORE IT
      AvgRankMOSM[s] <- mean(Ranks_MOSM)
      # REPEAT FOR WOSM
      Ranks_WOSM <- double()
      # NOW IDENTIFY THE MEN WITH PARTNER
      Matched_WOSM <- which(!is.na(WOSM$engagements))
      # FOR EACH MAN WITH PARTNER, IDENTIFY THE RANKING OF THEIR PARTNER
      for (man in Matched_WOSM) {
        # IDENTIFY THE PARTNER
        Partner <- WOSM$engagements[man]
        # IDENTIFY THE RANK
        Rank <- which(M[, man] == Partner)
        # ADD THE RANK TO RANKS
        Ranks_WOSM <- append(Ranks_WOSM, Rank)
      }
      # GET THE MEAN OF THE RANKS AND STORE IT
      AvgRankWOSM[s] <- mean(Ranks_WOSM)
    }
    # NOW WITH THE 1000 SIMULATIONS GET THE MEANS AND ADD IT TO THE TABLES
    Table2[row, col] <- mean(fractions)
    Table1[(3*row-2), col] <- mean(AvgRankMOSM)
    Table1[(3*row-1), col] <- mean(AvgRankWOSM)
  }
}

stargazer(Table1, summary = F, digits = 1)
stargazer(Table2, summary = F, digits = 1)
