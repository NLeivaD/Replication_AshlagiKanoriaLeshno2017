################################################################################
#### PAPER'S FIGURES REPLICATIONS: ASHLAGI, I., KANORIA, Y. & LESHNO, J. D. ####
#### UNBALANCED RANDOM MATCHING MARKETS: THE STARK EFFECT OF COMPETITION    ####
#### NICOLAS LEIVA DIAZ - NLEIVAD@FEN.UCHILE.CL                             ####
#### OCTOBER 2023                                                           ####
################################################################################

# CLEAN ALL WORK SPACE
rm(list = ls())

# DISCLAIMER: THIS RESULTS REPLICATIONS RELIES ON MATCHINGR PACKAGE MANTAINED BY
# JAN TILLY AND GGPLOT2 FOR GRAPHS.
library(matchingR)
library(ggplot2)

# ALSO WAS CREATED A FUNCTION TO APPLY RANDOM SERIAL DICTATORSHIP AND GET A
# RESULT SIMILAR TO THE FUNCTION galeShapley.marriageMarket FROM MATCHINGR
source("RandomSerialDictatorship.R")

# I. FIGURES 1 TO 3
# I.1. DEFINE THE PARAMETERS
NumWomen <- 40
NumMen <- 20:60
Simulations <- 10000

# I.2. CREATE OBJECTS TO STORE THE RESULTS:
#       A. AN OBJECT TO STORE THE FRACTION OF MEN WITH STABLE PARTNERS
fractions <- matrix(data = NA, nrow = Simulations, ncol = length(NumMen))
#       B. AN OBJECT TO STORE THE MEN'S AVERAGE RANK OF WIVES UNDER MOSM
AvgRankMOSM <- matrix(data = NA, nrow = Simulations, ncol = length(NumMen))
#       C. AN OBJECT TO STORE THE MEN'S AVERAGE RANK OF WIVES UNDER WOSM
AvgRankWOSM <- matrix(data = NA, nrow = Simulations, ncol = length(NumMen))
#       D. AN OBJECT TO STORE THE MEN'S AVERAGE RANK OF WIVES UNDER RSD
AvgRankRSD <- matrix(data = NA, nrow = Simulations, ncol = length(NumMen))

# I.3. DRAW SIMULATIONS: FOR EACH NUMBER OF MANS
for (m in NumMen) {
  # DRAW 10000 SIMULATIONS
  for (s in 1:Simulations) {
    # ON EACH SIMULATION START DEFINING THE PREFERENCE MATRIX AS AN EMPTY MATRIX
    # WITH THE NUMBER OF COLUMNS EQUAL TO THE NUMBER OF MEN AND THE NUMBER OF
    # ROWS EQUAL TO THE NUMBER OF WOMEN
    M <- matrix(data = NA, ncol = m, nrow = NumWomen)
    # NOW DRAW FOR EACH AGENT THE COMPLETE PREFERENCE AS DRAWING UNIFORMLY
    # RANDOM COMPLETE PREFERENCES
    for (col in 1:m) {
      M[,col] <- sample(1:NumWomen, size = NumWomen, replace = F)
    }
    # NOW DEFINE THE PREFERENCE FOR THE WOMEN, SIMILAR TO WHAT HAS BEEN DONE TO
    # THE MEN, DEFINE AN EMPTY MATRIX WITH AS MANY ROWS AS MEN AND AS MANY COLS
    # AS WOMEN
    W <- matrix(data = NA, ncol = NumWomen, nrow = m)
    # NOW DRAW FOR EACH AGENT THE COMPLETE PREFERENCE
    for (col in 1:NumWomen) {
      W[,col] <- sample(1:m, size = m, replace = F)
    }
    # NOW COMPUTE THE MOSM AND WOSM WITH THE galeShapley.marriageMarket FUNCTION
    MOSM <- galeShapley.marriageMarket(proposerPref = M, reviewerPref = W)
    WOSM <- galeShapley.marriageMarket(proposerPref = W, reviewerPref = M)
    RSD <- RandomSerialDictatorship(prefs = M)
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
    # APPEND TO THE SIMULATIONS, ROW S AND COL M - 19
    fractions[s, (m - 19)] <- StablePartners*100
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
    AvgRankMOSM[s, (m - 19)] <- mean(Ranks_MOSM)
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
    AvgRankWOSM[s, (m - 19)] <- mean(Ranks_WOSM)
    # FINALLY REPEAT FOR RSD
    Ranks_RSD <- double()
    # NOW IDENTIFY THE MEN WITH PARTNER
    Matched_RSD <- which(!is.na(RSD$proposals))
    # FOR EACH MAN WITH PARTNER, IDENTIFY THE RANKING OF THEIR PARTNER
    for (man in Matched_RSD) {
      # IDENTIFY THE PARTNER
      Partner <- RSD$proposals[man]
      # IDENTIFY THE RANK
      Rank <- which(M[, man] == Partner)
      # ADD THE RANK TO RANKS
      Ranks_RSD <- append(Ranks_RSD, Rank)
    }
    # GET THE MEAN OF THE RANKS AND STORE IT
    AvgRankRSD[s, (m - 19)] <- mean(Ranks_RSD)
  }
}
# I.4. MAKE PLOTS
# FIGURE 1
## GET THE DATA FRAME TO MAKE GRAPHS IN GGPLOT
AvgFraction <- data.frame(NumMen = NumMen, Means = colMeans(fractions))
AvgFraction$Low <- NA
AvgFraction$High <- NA
for (i in 1:nrow(AvgFraction)) {
  AvgFraction$Low[i] <- quantile(fractions[,i], 0.025)
  AvgFraction$High[i] <- quantile(fractions[,i], 0.975)
}
## MAKE THE PLOT
ggplot(data = AvgFraction) +
  geom_line(aes(x = NumMen, y = Means), color = "blue") +
  geom_point(aes(x = NumMen, y = Means), color = "blue", shape = 16) +
  geom_line(aes(x = NumMen, y = Low), linetype = "longdash", color = "blue") +
  geom_line(aes(x = NumMen, y = High), linetype = "longdash", color = "blue") +
  theme_minimal() +
  annotate("rect", xmin = 39.5, xmax = 40.5, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  labs(title = "Porcentaje de Hombres con Múltiples Parejas Estables",
       x = "Número de Hombres", y = "Porcentaje",
       subtitle = paste0("10 mil simulaciones de mercados aleatorios",
                         " con 40 mujeres."),
       caption = paste0("Línea con Puntos es la media de las 10 mil ",
                        "simulaciones. \nÁrea entre las líneas discontinuas",
                        " representa el 95% de los datos. \n",
                        "Área sombreada es un mercado balanceado.")) +
  scale_x_continuous(expand = c(0.01,0), breaks = seq(20, 60, 2)) +
  scale_y_continuous(expand = c(0.01,0), breaks = seq(0, 90, 10))
## SAVE THE PLOT
ggsave("Fig1.png", width = 633, height = 392, units = "px")
# FIGURE 2
## GET THE DATA FRAME TO MAKE GRAPHS IN GGPLOT
### MOSM
AvgRanksMOSM <- data.frame(NumMen = NumMen, Means = colMeans(AvgRankMOSM))
### WOSM
AvgRanksWOSM <- data.frame(NumMen = NumMen, Means = colMeans(AvgRankWOSM))
## MAKE PLOT
ggplot() +
  geom_line(data = AvgRanksMOSM, aes(x = NumMen, y = Means, color = "MOSM")) +
  geom_point(data = AvgRanksMOSM, aes(x = NumMen, y = Means, color = "MOSM")) +
  geom_line(data = AvgRanksWOSM, aes(x = NumMen, y = Means, color = "WOSM")) +
  geom_point(data = AvgRanksWOSM, aes(x = NumMen, y = Means, color = "WOSM")) +
  scale_color_manual(name = "Matching",
                     breaks = c("MOSM", "WOSM"),
                     values = c("MOSM" = "blue", "WOSM" = "red")) +
  theme_minimal() +
  annotate("rect", xmin = 39.5, xmax = 40.5, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  labs(title = "Ranking Promedio de Mujeres Emparejadas",
       x = "Número de Hombres", y = "Ranking Promedio",
       subtitle = paste0("10 mil simulaciones de mercados aleatorios",
                         " con 40 mujeres."),
       caption = paste0("Línea con Puntos es la media de las 10 mil ",
                        "simulaciones. \n",
                        "Área sombreada es un mercado balanceado.")) +
  scale_x_continuous(expand = c(0.01,0), breaks = seq(20, 60, 2)) +
  scale_y_continuous(expand = c(0.08,0), breaks = seq(0, 20, 2))
## SAVE THE PLOT
ggsave("Fig2.png", width = 633, height = 392, units = "px")
# FIGURE 3
## GET THE DATA FRAME TO MAKE GRAPHS IN GGPLOT
### MOSM
AvgRanksMOSM$Low <- NA
AvgRanksMOSM$High <- NA
for (i in 1:nrow(AvgRanksMOSM)) {
  AvgRanksMOSM$Low[i] <- quantile(AvgRankMOSM[,i], 0.025)
  AvgRanksMOSM$High[i] <- quantile(AvgRankMOSM[,i], 0.975)
}
#### WOSM
AvgRanksWOSM$Low <- NA
AvgRanksWOSM$High <- NA
for (i in 1:nrow(AvgRanksWOSM)) {
  AvgRanksWOSM$Low[i] <- quantile(AvgRankWOSM[,i], 0.025)
  AvgRanksWOSM$High[i] <- quantile(AvgRankWOSM[,i], 0.975)
}
#### RSD
AvgRanksRSD <- data.frame(NumMen = NumMen, Means = colMeans(AvgRankRSD))
AvgRanksRSD$Low <- NA
AvgRanksRSD$High <- NA
for (i in 1:nrow(AvgRanksRSD)) {
  AvgRanksRSD$Low[i] <- quantile(AvgRankRSD[,i], 0.025)
  AvgRanksRSD$High[i] <- quantile(AvgRankRSD[,i], 0.975)
}
## MAKE PLOT
ggplot() +
  ### MOSM
  #### AVERAGE LINE
  geom_line(data = AvgRanksMOSM, aes(x = NumMen, y = Means, color = "MOSM")) +
  geom_point(data = AvgRanksMOSM, aes(x = NumMen, y = Means, color = "MOSM")) +
  #### 95% DISTRIBUTION
  geom_line(data = AvgRanksMOSM, aes(x = NumMen, y = Low), 
            linetype = "longdash", color = "blue") +
  geom_line(data = AvgRanksMOSM, aes(x = NumMen, y = High), 
            linetype = "longdash", color = "blue") +
  ### WOSM
  #### AVERAGE LINE
  geom_line(data = AvgRanksWOSM, aes(x = NumMen, y = Means, color = "WOSM")) +
  geom_point(data = AvgRanksWOSM, aes(x = NumMen, y = Means, color = "WOSM")) +
  #### 95% DISTRIBUTION
  geom_line(data = AvgRanksWOSM, aes(x = NumMen, y = Low), 
            linetype = "longdash", color = "red") +
  geom_line(data = AvgRanksWOSM, aes(x = NumMen, y = High), 
            linetype = "longdash", color = "red") +
  ### RSD
  #### AVERAGE LINE
  geom_line(data = AvgRanksRSD, aes(x = NumMen, y = Means, color = "RSD")) +
  geom_point(data = AvgRanksRSD, aes(x = NumMen, y = Means, color = "RSD")) +
  #### 95% DISTRIBUTION
  geom_line(data = AvgRanksRSD, aes(x = NumMen, y = Low), 
            linetype = "longdash", color = "green") +
  geom_line(data = AvgRanksRSD, aes(x = NumMen, y = High), 
            linetype = "longdash", color = "green") +
  ### PLOT ELEMENTS
  #### LEGEND
  scale_color_manual(name = "Matching",
                     breaks = c("MOSM", "WOSM", "RSD"),
                     values = c("MOSM" = "blue", "WOSM" = "red", 
                                "RSD" = "green")) +
  #### OVERALL THEME
  theme_minimal() +
  #### SHADE AREA BALANCED MARKET
  annotate("rect", xmin = 39.5, xmax = 40.5, ymin = -Inf, 
           ymax = Inf, alpha = 0.2) +
  #### LABELS
  labs(title = "Ranking Promedio de Mujeres Emparejadas",
       x = "Número de Hombres", y = "Ranking Promedio",
       subtitle = paste0("10 mil simulaciones de mercados aleatorios",
                         " con 40 mujeres."),
       caption = paste0("Línea con Puntos es la media de las 10 mil ",
                        "simulaciones. \nÁrea entre las líneas discontinuas",
                        " representa el 95% de los datos. \n",
                        "Área sombreada es un mercado balanceado.")) +
  #### AXIS LIMITS + TICKS
  ##### X AXIS
  scale_x_continuous(expand = c(0.01,0), breaks = seq(20, 60, 2)) +
  ##### Y AXIS
  scale_y_continuous(expand = c(0.08,0), breaks = seq(0, 20, 2))
## SAVE THE PLOT
ggsave("Fig2.png", width = 633, height = 392, units = "px")
