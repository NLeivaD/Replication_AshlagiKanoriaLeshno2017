################################################################################
#### PAPER'S FIGURES REPLICATIONS: ASHLAGI, I., KANORIA, Y. & LESHNO, J. D. ####
#### UNBALANCED RANDOM MATCHING MARKETS: THE STARK EFFECT OF COMPETITION    ####
#### NICOLAS LEIVA DIAZ - NLEIVAD@FEN.UCHILE.CL                             ####
#### OCTOBER 2023                                                           ####
################################################################################

# CLEAN ALL WORK SPACE
rm(list = ls())

# DISCLAIMER: THIS RESULTS REPLICATIONS RELIES ON MATCHINGR PACKAGE MANTAINED BY
# JAN TILLY AND PLOT3D WITH GGPLOT2 FOR GRAPHS.
library(matchingR)
library(plot3D)
library(ggplot2)
library(latex2exp)

# I. FIGURE 4
# I.1. DEFINE THE PARAMETERS
NumWomen <- 40
NumMen <- c(40, 41, 45, 60)
betas <- 0:20
gammas <- -20:20
Simulations <- 2000

# I.2. CREATE AN OBJECT TO STORE THE FRACTION OF MEN WITH STABLE PARTNERS UNDER 
# THE DIFFERENT COMBINATIONS. THIS WILL BE A LIST WITH 4 ELEMENTS, WHERE EACH
# ELEMENT WILL BE ASSIGNED FOR EACH NUMBER OF MEN. THIS ELEMENTS WILL BE DATA
# FRAMES THAT WILL CONTAIN THE COMBINATIONS OF BETA, GAMMA AND THE FRACTION.
fractions <- list("NumMen_40" = matrix(data = NA, nrow = length(betas),
                                       ncol = length(gammas)),
                  "NumMen_41" = matrix(data = NA, nrow = length(betas),
                                       ncol = length(gammas)),
                  "NumMen_45" = matrix(data = NA, nrow = length(betas),
                                       ncol = length(gammas)),
                  "NumMen_60" = matrix(data = NA, nrow = length(betas),
                                       ncol = length(gammas)))
# ASSIGN THE COLUMN NAMES
colnames(fractions[["NumMen_40"]]) <- gammas
rownames(fractions[["NumMen_40"]]) <- betas
colnames(fractions[["NumMen_41"]]) <- gammas
rownames(fractions[["NumMen_41"]]) <- betas
colnames(fractions[["NumMen_45"]]) <- gammas
rownames(fractions[["NumMen_45"]]) <- betas
colnames(fractions[["NumMen_60"]]) <- gammas
rownames(fractions[["NumMen_60"]]) <- betas

# I.3. DRAW SIMULATIONS: FOR EACH NUMBER OF MEN
# I.3.A NUMBER OF MEN 40
Nm <- 40
# DEFINE THE ROW NUMBER AS 0
rownum <- 0
# FOR EACH BETA
for (beta in betas) {
  # INCREASE THE ROW NUMBER
  rownum <- rownum + 1
  # DEFINE THE COL NUMBER AS 0
  colnum <- 0
  # FOR EACH GAMMA
  for (gamma in gammas) {
    # INCREASE THE COL NUMBER
    colnum <- colnum + 1
    # NOW THE COMBINATION IS CREATED, LET AUX BE THE VECTOR THAT WILL CONTAIN
    # THE FRACTION OF THE 2000 SIMULATIONS
    aux <- vector()
    # DRAW 2000 SIMULATIONS
    for (s in 1:Simulations) {
      # SIMULATE THE MEN CHARACTERISTICS
      MenChars <- data.frame(Men = 1:Nm, xA = runif(Nm), xD = runif(Nm))
      # SIMULATE THE WOMEN CHARACTERISTICS
      WomenChars <- data.frame(Women = 1:NumWomen, xA = runif(NumWomen), 
                                xD = runif(NumWomen))
      # CREATE THE UTILITIES MATRIX FOR MEN AND WOMEN
      M <- matrix(data = NA, nrow = NumWomen, ncol = Nm)
      W <- matrix(data = NA, nrow = Nm, ncol = NumWomen)
      # ASSIGN THE UTILITIES MATRIX, FOR EACH MAN
      for (m in 1:Nm) {
        M[,m] <- beta * WomenChars$xA -
          gamma * (MenChars$xD[m] - WomenChars$xD)^2 + rlogis(NumWomen)
      }
      rm(m)
      # FOR EACH WOMAN
      for (w in 1:NumWomen) {
        W[,w] <- beta * MenChars$xA -
          gamma * (WomenChars$xD[w] - MenChars$xD)^2 + rlogis(Nm)
      }
      # NOW CHARACTERISTICS AND EPSILON ARE NO LONGER NEEDED SO THEY ARE
      # ERASED IN ORDER TO OPTIMIZE STORAGE MEMORY
      rm(MenChars, WomenChars, w)
      # WITH THE UTILITIES MATRIX DEFINED, COMPUTE MOSM AND WOSM
      MOSM <- galeShapley.marriageMarket(proposerUtils = M,
                                          reviewerUtils = W)
      WOSM <- galeShapley.marriageMarket(proposerUtils = W,
                                          reviewerUtils = M)
      # NOW M AND W MATRICES ARE NO LONGER USEFUL
      rm(M, W)
      # COUNT HOW MANY MEN HAVE DIFFERENT PARTNERS
      StablePartners <- length(which(MOSM$proposals != WOSM$engagements))/Nm*100
      # ASSIGN IT TO THE AUX VECTOR
      aux <- append(aux, StablePartners)
      # NOW STABLEPARTNERS ARE NO LONGER USEFUL
      rm(StablePartners)
    }
    # ASSIGN THE MEAN OF THIS FRACTION, 
    fractions$NumMen_40[rownum, colnum] <- mean(aux)
  }
}

# I.3.B NUMBER OF MEN 41
Nm <- 41
# DEFINE THE ROW NUMBER AS 0
rownum <- 0
# FOR EACH BETA
for (beta in betas) {
  # INCREASE THE ROW NUMBER
  rownum <- rownum + 1
  # DEFINE THE COL NUMBER AS 0
  colnum <- 0
  # FOR EACH GAMMA
  for (gamma in gammas) {
    # INCREASE THE COL NUMBER
    colnum <- colnum + 1
    # NOW THE COMBINATION IS CREATED, LET AUX BE THE VECTOR THAT WILL CONTAIN
    # THE FRACTION OF THE 2000 SIMULATIONS
    aux <- vector()
    # DRAW 2000 SIMULATIONS
    for (s in 1:Simulations) {
      # SIMULATE THE MEN CHARACTERISTICS
      MenChars <- data.frame(Men = 1:Nm, xA = runif(Nm), xD = runif(Nm))
      # SIMULATE THE WOMEN CHARACTERISTICS
      WomenChars <- data.frame(Women = 1:NumWomen, xA = runif(NumWomen), 
                               xD = runif(NumWomen))
      # CREATE THE UTILITIES MATRIX FOR MEN AND WOMEN
      M <- matrix(data = NA, nrow = NumWomen, ncol = Nm)
      W <- matrix(data = NA, nrow = Nm, ncol = NumWomen)
      # ASSIGN THE UTILITIES MATRIX, FOR EACH MAN
      for (m in 1:Nm) {
        M[,m] <- beta * WomenChars$xA -
          gamma * (MenChars$xD[m] - WomenChars$xD)^2 + rlogis(NumWomen)
      }
      rm(m)
      # FOR EACH WOMAN
      for (w in 1:NumWomen) {
        W[,w] <- beta * MenChars$xA -
          gamma * (WomenChars$xD[w] - MenChars$xD)^2 + rlogis(Nm)
      }
      # NOW CHARACTERISTICS AND EPSILON ARE NO LONGER NEEDED SO THEY ARE
      # ERASED IN ORDER TO OPTIMIZE STORAGE MEMORY
      rm(MenChars, WomenChars, w)
      # WITH THE UTILITIES MATRIX DEFINED, COMPUTE MOSM AND WOSM
      MOSM <- galeShapley.marriageMarket(proposerUtils = M,
                                         reviewerUtils = W)
      WOSM <- galeShapley.marriageMarket(proposerUtils = W,
                                         reviewerUtils = M)
      # NOW M AND W MATRICES ARE NO LONGER USEFUL
      rm(M, W)
      # COUNT HOW MANY MEN HAVE DIFFERENT PARTNERS
      StablePartners <- length(which(MOSM$proposals != WOSM$engagements))/Nm*100
      # ASSIGN IT TO THE AUX VECTOR
      aux <- append(aux, StablePartners)
      # NOW STABLEPARTNERS ARE NO LONGER USEFUL
      rm(StablePartners)
    }
    # ASSIGN THE MEAN OF THIS FRACTION, 
    fractions$NumMen_41[rownum, colnum] <- mean(aux)
  }
}

# I.3.C NUMBER OF MEN 45
Nm <- 45
# DEFINE THE ROW NUMBER AS 0
rownum <- 0
# FOR EACH BETA
for (beta in betas) {
  # INCREASE THE ROW NUMBER
  rownum <- rownum + 1
  # DEFINE THE COL NUMBER AS 0
  colnum <- 0
  # FOR EACH GAMMA
  for (gamma in gammas) {
    # INCREASE THE COL NUMBER
    colnum <- colnum + 1
    # NOW THE COMBINATION IS CREATED, LET AUX BE THE VECTOR THAT WILL CONTAIN
    # THE FRACTION OF THE 2000 SIMULATIONS
    aux <- vector()
    # DRAW 2000 SIMULATIONS
    for (s in 1:Simulations) {
      # SIMULATE THE MEN CHARACTERISTICS
      MenChars <- data.frame(Men = 1:Nm, xA = runif(Nm), xD = runif(Nm))
      # SIMULATE THE WOMEN CHARACTERISTICS
      WomenChars <- data.frame(Women = 1:NumWomen, xA = runif(NumWomen), 
                               xD = runif(NumWomen))
      # CREATE THE UTILITIES MATRIX FOR MEN AND WOMEN
      M <- matrix(data = NA, nrow = NumWomen, ncol = Nm)
      W <- matrix(data = NA, nrow = Nm, ncol = NumWomen)
      # ASSIGN THE UTILITIES MATRIX, FOR EACH MAN
      for (m in 1:Nm) {
        M[,m] <- beta * WomenChars$xA -
          gamma * (MenChars$xD[m] - WomenChars$xD)^2 + rlogis(NumWomen)
      }
      rm(m)
      # FOR EACH WOMAN
      for (w in 1:NumWomen) {
        W[,w] <- beta * MenChars$xA -
          gamma * (WomenChars$xD[w] - MenChars$xD)^2 + rlogis(Nm)
      }
      # NOW CHARACTERISTICS AND EPSILON ARE NO LONGER NEEDED SO THEY ARE
      # ERASED IN ORDER TO OPTIMIZE STORAGE MEMORY
      rm(MenChars, WomenChars, w)
      # WITH THE UTILITIES MATRIX DEFINED, COMPUTE MOSM AND WOSM
      MOSM <- galeShapley.marriageMarket(proposerUtils = M,
                                         reviewerUtils = W)
      WOSM <- galeShapley.marriageMarket(proposerUtils = W,
                                         reviewerUtils = M)
      # NOW M AND W MATRICES ARE NO LONGER USEFUL
      rm(M, W)
      # COUNT HOW MANY MEN HAVE DIFFERENT PARTNERS
      StablePartners <- length(which(MOSM$proposals != WOSM$engagements))/Nm*100
      # ASSIGN IT TO THE AUX VECTOR
      aux <- append(aux, StablePartners)
      # NOW STABLEPARTNERS ARE NO LONGER USEFUL
      rm(StablePartners)
    }
    # ASSIGN THE MEAN OF THIS FRACTION, 
    fractions$NumMen_45[rownum, colnum] <- mean(aux)
  }
}

# I.3.D NUMBER OF MEN 60
Nm <- 60
# DEFINE THE ROW NUMBER AS 0
rownum <- 0
# FOR EACH BETA
for (beta in betas) {
  # INCREASE THE ROW NUMBER
  rownum <- rownum + 1
  # DEFINE THE COL NUMBER AS 0
  colnum <- 0
  # FOR EACH GAMMA
  for (gamma in gammas) {
    # INCREASE THE COL NUMBER
    colnum <- colnum + 1
    # NOW THE COMBINATION IS CREATED, LET AUX BE THE VECTOR THAT WILL CONTAIN
    # THE FRACTION OF THE 2000 SIMULATIONS
    aux <- vector()
    # DRAW 2000 SIMULATIONS
    for (s in 1:Simulations) {
      # SIMULATE THE MEN CHARACTERISTICS
      MenChars <- data.frame(Men = 1:Nm, xA = runif(Nm), xD = runif(Nm))
      # SIMULATE THE WOMEN CHARACTERISTICS
      WomenChars <- data.frame(Women = 1:NumWomen, xA = runif(NumWomen), 
                               xD = runif(NumWomen))
      # CREATE THE UTILITIES MATRIX FOR MEN AND WOMEN
      M <- matrix(data = NA, nrow = NumWomen, ncol = Nm)
      W <- matrix(data = NA, nrow = Nm, ncol = NumWomen)
      # ASSIGN THE UTILITIES MATRIX, FOR EACH MAN
      for (m in 1:Nm) {
        M[,m] <- beta * WomenChars$xA -
          gamma * (MenChars$xD[m] - WomenChars$xD)^2 + rlogis(NumWomen)
      }
      rm(m)
      # FOR EACH WOMAN
      for (w in 1:NumWomen) {
        W[,w] <- beta * MenChars$xA -
          gamma * (WomenChars$xD[w] - MenChars$xD)^2 + rlogis(Nm)
      }
      # NOW CHARACTERISTICS AND EPSILON ARE NO LONGER NEEDED SO THEY ARE
      # ERASED IN ORDER TO OPTIMIZE STORAGE MEMORY
      rm(MenChars, WomenChars, w)
      # WITH THE UTILITIES MATRIX DEFINED, COMPUTE MOSM AND WOSM
      MOSM <- galeShapley.marriageMarket(proposerUtils = M,
                                         reviewerUtils = W)
      WOSM <- galeShapley.marriageMarket(proposerUtils = W,
                                         reviewerUtils = M)
      # NOW M AND W MATRICES ARE NO LONGER USEFUL
      rm(M, W)
      # COUNT HOW MANY MEN HAVE DIFFERENT PARTNERS
      StablePartners <- length(which(MOSM$proposals != WOSM$engagements))/Nm*100
      # ASSIGN IT TO THE AUX VECTOR
      aux <- append(aux, StablePartners)
      # NOW STABLEPARTNERS ARE NO LONGER USEFUL
      rm(StablePartners)
    }
    # ASSIGN THE MEAN OF THIS FRACTION, 
    fractions$NumMen_60[rownum, colnum] <- mean(aux)
  }
}

# I.4 FIGURE'S PANELS
# I.4.A PANEL A - 40 MEN
plot_ly(type = "surface",
        x = gammas,
        y = betas,
        z = ~fractions$NumMen_40, showscale = F) %>%
  layout(scene = list(
    xaxis = list(nticks = 5, title = ""), 
    yaxis = list(nticks = 3, title = ""), 
    zaxis = list(nticks = 4, title = ""),
    aspectratio = list(x = 1, y = 1, z = 1)
  ))

# I.4.B PANEL B - 41 MEN
plot_ly(type = "surface",
        x = gammas,
        y = betas,
        z = ~fractions$NumMen_41, showscale = F) %>%
  layout(scene = list(
    xaxis = list(nticks = 5, title = ""), 
    yaxis = list(nticks = 3, title = ""), 
    zaxis = list(nticks = 4, title = "", range = list(0,60)),
    aspectratio = list(x = 1, y = 1, z = 1)
  ))

# I.4.C PANEL C - 45 MEN
plot_ly(type = "surface",
        x = gammas,
        y = betas,
        z = ~fractions$NumMen_45, showscale = F) %>%
  layout(scene = list(
    xaxis = list(nticks = 5, title = ""), 
    yaxis = list(nticks = 3, title = ""), 
    zaxis = list(nticks = 4, title = "", range = list(0,60)),
    aspectratio = list(x = 1, y = 1, z = 1)
  ))

# I.4.D PANEL D - 60 MEN
plot_ly(type = "surface",
        x = gammas,
        y = betas,
        z = ~fractions$NumMen_60, showscale = F) %>%
  layout(scene = list(
    xaxis = list(nticks = 5, title = ""), 
    yaxis = list(nticks = 3, title = ""), 
    zaxis = list(nticks = 4, title = "", range = list(0,60)),
    aspectratio = list(x = 1, y = 1, z = 1)
  ))

# CLEAN MEMORY
rm(list = ls())

# II. FIGURE 5
# II.1. DEFINE THE PARAMETERS
NumWomen <- 40
NumMen <- 20:60
betas <- c(0, 1, 2, 5, 10, 100)
gammas <- c(0, 1, 2, 5, 10, 100, 1000)
Simulations <- 2000

# II.2. CREATE AN OBJECT TO STORE THE MEN'S WIVES AVERAGE RANK UNDER MOSM
PanelAData <- data.frame(expand.grid(betas, NumMen))
colnames(PanelAData) <- c("Beta", "NumMen")
PanelAData$Rank <- NA
PanelBData <- data.frame(expand.grid(gammas, NumMen))
colnames(PanelBData) <- c("Gamma", "NumMen")
PanelBData$Rank <- NA
PanelCData <- data.frame(expand.grid(c(0, 1, 2, 5, 10, 100), NumMen))
colnames(PanelCData) <- c("BetaGamma", "NumMen")
PanelCData$Rank <- NA

# II.3. DRAW SIMULATIONS
# II.3.A PANEL A
gamma = 0
# FOR EACH ROW IN THE DATA FRAME
for (row in 1:nrow(PanelAData)) {
  # IDENTIFY THE BETA
  beta <- PanelAData$Beta[row]
  # IDENTIFY THE NNUMBER OF MEN
  Nm <- PanelAData$NumMen[row]
  # CREATE A VECTOR TO STORE THE AVERAGE RANK
  aux <- vector()
  # DRAW 2000 SIMULATIONS
  for(s in 1:Simulations){
    # SIMULATE THE MEN CHARACTERISTICS
    MenChars <- data.frame(Men = 1:Nm, xA = runif(Nm), xD = runif(Nm))
    # SIMULATE THE WOMEN CHARACTERISTICS
    WomenChars <- data.frame(Women = 1:NumWomen, xA = runif(NumWomen), 
                             xD = runif(NumWomen))
    # CREATE THE UTILITIES MATRIX FOR MEN AND WOMEN
    M <- matrix(data = NA, nrow = NumWomen, ncol = Nm)
    W <- matrix(data = NA, nrow = Nm, ncol = NumWomen)
    # ASSIGN THE UTILITIES MATRIX, FOR EACH MAN
    for (m in 1:Nm) {
      Utils <- data.frame(Agent = 1:NumWomen,
                          Util = beta * WomenChars$xA -
                            gamma * (MenChars$xD[m] - WomenChars$xD)^2 + 
                            rlogis(NumWomen))
      M[,m] <- order(Utils$Util, decreasing = T)
    }
    rm(m)
    # FOR EACH WOMAN
    for (w in 1:NumWomen) {
      Utils <- data.frame(Agent = 1:Nm,
                          Util = beta * MenChars$xA -
                            gamma * (WomenChars$xD[w] - MenChars$xD)^2 + 
                            rlogis(Nm))
      W[,w] <- order(Utils$Util, decreasing = T)
    }
    # NOW CHARACTERISTICS AND EPSILON ARE NO LONGER NEEDED SO THEY ARE
    # ERASED IN ORDER TO OPTIMIZE STORAGE MEMORY
    rm(MenChars, WomenChars, w, Utils)
    # WITH THE UTILITIES MATRIX DEFINED AND COMPUTE MOSM
    MOSM <- galeShapley.marriageMarket(proposerPref = M,
                                       reviewerPref = W)
    # NOW W MATRIX ARE NO LONGER USEFUL
    rm(W)
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
    aux <- append(aux, mean(Ranks_MOSM))
    rm(M, Ranks_MOSM, Matched_MOSM, MOSM, man, Partner, Rank)
  }
  PanelAData$Rank[row] = mean(aux)
}

# II.3.B PANEL B
beta = 0
# FOR EACH ROW IN THE DATA FRAME
for (row in 1:nrow(PanelBData)) {
  # IDENTIFY THE GAMMA
  gamma <- PanelBData$Gamma[row]
  # IDENTIFY THE NNUMBER OF MEN
  Nm <- PanelBData$NumMen[row]
  # CREATE A VECTOR TO STORE THE AVERAGE RANK
  aux <- vector()
  # DRAW 2000 SIMULATIONS
  for(s in 1:Simulations){
    # SIMULATE THE MEN CHARACTERISTICS
    MenChars <- data.frame(Men = 1:Nm, xA = runif(Nm), xD = runif(Nm))
    # SIMULATE THE WOMEN CHARACTERISTICS
    WomenChars <- data.frame(Women = 1:NumWomen, xA = runif(NumWomen), 
                             xD = runif(NumWomen))
    # CREATE THE UTILITIES MATRIX FOR MEN AND WOMEN
    M <- matrix(data = NA, nrow = NumWomen, ncol = Nm)
    W <- matrix(data = NA, nrow = Nm, ncol = NumWomen)
    # ASSIGN THE UTILITIES MATRIX, FOR EACH MAN
    for (m in 1:Nm) {
      Utils <- data.frame(Agent = 1:NumWomen,
                          Util = beta * WomenChars$xA -
                            gamma * (MenChars$xD[m] - WomenChars$xD)^2 + 
                            rlogis(NumWomen))
      M[,m] <- order(Utils$Util, decreasing = T)
    }
    rm(m)
    # FOR EACH WOMAN
    for (w in 1:NumWomen) {
      Utils <- data.frame(Agent = 1:Nm,
                          Util = beta * MenChars$xA -
                            gamma * (WomenChars$xD[w] - MenChars$xD)^2 + 
                            rlogis(Nm))
      W[,w] <- order(Utils$Util, decreasing = T)
    }
    # NOW CHARACTERISTICS AND EPSILON ARE NO LONGER NEEDED SO THEY ARE
    # ERASED IN ORDER TO OPTIMIZE STORAGE MEMORY
    rm(MenChars, WomenChars, w, Utils)
    # WITH THE UTILITIES MATRIX DEFINED AND COMPUTE MOSM
    MOSM <- galeShapley.marriageMarket(proposerPref = M,
                                       reviewerPref = W)
    # NOW W MATRIX ARE NO LONGER USEFUL
    rm(W)
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
    aux <- append(aux, mean(Ranks_MOSM))
    rm(M, Ranks_MOSM, Matched_MOSM, MOSM, man, Partner, Rank)
  }
  PanelBData$Rank[row] = mean(aux)
}

# II.3.C PANEL C
# FOR EACH ROW IN THE DATA FRAME
for (row in 1:nrow(PanelCData)) {
  # IDENTIFY THE BETA
  beta <- PanelCData$BetaGamma[row]
  # IDENTIFY THE GAMMA
  gamma <- PanelCData$BetaGamma[row]
  # IDENTIFY THE NNUMBER OF MEN
  Nm <- PanelCData$NumMen[row]
  # CREATE A VECTOR TO STORE THE AVERAGE RANK
  aux <- vector()
  # DRAW 2000 SIMULATIONS
  for(s in 1:Simulations){
    # SIMULATE THE MEN CHARACTERISTICS
    MenChars <- data.frame(Men = 1:Nm, xA = runif(Nm), xD = runif(Nm))
    # SIMULATE THE WOMEN CHARACTERISTICS
    WomenChars <- data.frame(Women = 1:NumWomen, xA = runif(NumWomen), 
                             xD = runif(NumWomen))
    # CREATE THE UTILITIES MATRIX FOR MEN AND WOMEN
    M <- matrix(data = NA, nrow = NumWomen, ncol = Nm)
    W <- matrix(data = NA, nrow = Nm, ncol = NumWomen)
    # ASSIGN THE UTILITIES MATRIX, FOR EACH MAN
    for (m in 1:Nm) {
      Utils <- data.frame(Agent = 1:NumWomen,
                          Util = beta * WomenChars$xA -
                            gamma * (MenChars$xD[m] - WomenChars$xD)^2 + 
                            rlogis(NumWomen))
      M[,m] <- order(Utils$Util, decreasing = T)
    }
    rm(m)
    # FOR EACH WOMAN
    for (w in 1:NumWomen) {
      Utils <- data.frame(Agent = 1:Nm,
                          Util = beta * MenChars$xA -
                            gamma * (WomenChars$xD[w] - MenChars$xD)^2 + 
                            rlogis(Nm))
      W[,w] <- order(Utils$Util, decreasing = T)
    }
    # NOW CHARACTERISTICS AND EPSILON ARE NO LONGER NEEDED SO THEY ARE
    # ERASED IN ORDER TO OPTIMIZE STORAGE MEMORY
    rm(MenChars, WomenChars, w, Utils)
    # WITH THE UTILITIES MATRIX DEFINED AND COMPUTE MOSM
    MOSM <- galeShapley.marriageMarket(proposerPref = M,
                                       reviewerPref = W)
    # NOW W MATRIX ARE NO LONGER USEFUL
    rm(W)
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
    aux <- append(aux, mean(Ranks_MOSM))
    rm(M, Ranks_MOSM, Matched_MOSM, MOSM, man, Partner, Rank)
  }
  PanelCData$Rank[row] = mean(aux)
}

# II.4 FIGURE'S PANEL
# I.4.A PANEL A - GAMMA = 0
ggplot(data = PanelAData, aes(x = NumMen, y = Rank, color = as.factor(Beta))) +
  geom_line(aes(alpha = as.factor(Beta))) +
  geom_point(aes(shape = as.factor(Beta), alpha = as.factor(Beta))) +
  scale_alpha_manual(name = "\u03b2",
                     values = c(0.2, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_shape_manual(name = "\u03b2",
                     values = c(0, 2, 1, 19, 4, 15)) +
  scale_color_manual(name = "\u03b2",
                     values = c("blue", "blue", "blue", "blue", "blue", 
                                "blue")) + theme_classic() +
  theme(legend.position = c(0.85, 0.3)) +
  guides(color = guide_legend(ncol = 2), shape = guide_legend(ncol = 2),
         alpha = guide_legend(ncol = 2)) +
  labs(title = "Ranking Promedio de Mujeres Emparejadas",
       x = "Número de Hombres", y = "Ranking Promedio de Mujeres",
       subtitle = paste0("2 mil simulaciones de mercados aleatorios",
                         " con 40 mujeres y \u03b3 = 0")#,
       # caption = expression("Preferencias correlacionadas dadas por la utilidad de un agente i al ser emparejado con un agente j: u"[i]*
       #                        "(j) = \u03b2 x"[j]^A*"- \u03b3(x"[i]^D*"-x"[j]^D*
       #                        ")"^2*"+ \u03b5"[ij])
       ) +
  scale_x_continuous(limits = c(20, 60),
                     breaks = seq(20, 60, 2), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 2), expand = c(0.01,0.01))

# I.4.A PANEL B - BETA = 0
ggplot(data = PanelBData, aes(x = NumMen, y = Rank, color = as.factor(Gamma))) +
  geom_line(aes(alpha = as.factor(Gamma))) +
  geom_point(aes(shape = as.factor(Gamma), alpha = as.factor(Gamma))) +
  scale_alpha_manual(name = "\u03b3",
                     values = c(0.2, 0.2, 0.4, 0.6, 0.8, 1, 1)) +
  scale_shape_manual(name = "\u03b3",
                     values = c(0, 2, 1, 19, 4, 15, 24)) +
  scale_color_manual(name = "\u03b3",
                     values = c("blue", "blue", "blue", "blue", "blue", 
                                "blue", "blue")) + theme_classic() +
  theme(legend.position = c(0.15, 0.7)) +
  guides(color = guide_legend(ncol = 2), shape = guide_legend(ncol = 2),
         alpha = guide_legend(ncol = 2)) +
  labs(title = "Ranking Promedio de Mujeres Emparejadas",
       x = "Número de Hombres", y = "Ranking Promedio de Mujeres",
       subtitle = paste0("2 mil simulaciones de mercados aleatorios",
                         " con 40 mujeres y \u03b2 = 0")#,
       # caption = expression("Preferencias correlacionadas dadas por la utilidad de un agente i al ser emparejado con un agente j: u"[i]*
       #                        "(j) = \u03b2 x"[j]^A*"- \u03b3(x"[i]^D*"-x"[j]^D*
       #                        ")"^2*"+ \u03b5"[ij])
  ) +
  scale_x_continuous(limits = c(20, 60),
                     breaks = seq(20, 60, 2), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 2), expand = c(0.01,0.01))

# I.4.A PANEL A - GAMMA = BETA
ggplot(data = PanelCData, aes(x = NumMen, y = Rank, color = as.factor(BetaGamma))) +
  geom_line(aes(alpha = as.factor(BetaGamma))) +
  geom_point(aes(shape = as.factor(BetaGamma), alpha = as.factor(BetaGamma))) +
  scale_alpha_manual(name = "\u03b2 = \u03b3",
                     values = c(0.2, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_shape_manual(name = "\u03b2 = \u03b3",
                     values = c(0, 2, 1, 19, 4, 15)) +
  scale_color_manual(name = "\u03b2 = \u03b3",
                     values = c("blue", "blue", "blue", "blue", "blue", 
                                "blue")) + theme_classic() +
  theme(legend.position = c(0.85, 0.3)) +
  guides(color = guide_legend(ncol = 2), shape = guide_legend(ncol = 2),
         alpha = guide_legend(ncol = 2)) +
  labs(title = "Ranking Promedio de Mujeres Emparejadas",
       x = "Número de Hombres", y = "Ranking Promedio de Mujeres",
       subtitle = paste0("2 mil simulaciones de mercados aleatorios",
                         " con 40 mujeres")#,
       # caption = expression("Preferencias correlacionadas dadas por la utilidad de un agente i al ser emparejado con un agente j: u"[i]*
       #                        "(j) = \u03b2 x"[j]^A*"- \u03b3(x"[i]^D*"-x"[j]^D*
       #                        ")"^2*"+ \u03b5"[ij])
  ) +
  scale_x_continuous(limits = c(20, 60),
                     breaks = seq(20, 60, 2), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, 2), expand = c(0.01,0.01))
