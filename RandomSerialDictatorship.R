RandomSerialDictatorship <- function(nIndividuals = ncol(prefs),
                nObjects = nrow(prefs),
                prefs) {
  # DEFINE THE OBJECT TO STORE THE RESULTS, SIMILAR TO galeShapley.marriageMarket
  # OUTPUT.
  Output <- list(proposals = double(length = nIndividuals),
                 engagements = double(length = nObjects),
                 single.proposers = double(),
                 single.reviewers = double())
  # DEFINE THE RANDOM ORDER
  Order <- data.frame(Agent = 1:nIndividuals, 
                      Priority = sample(1:nIndividuals, size = nIndividuals, 
                                        replace = F))
  # FOR EACH AGENT IN THE RANDOM ORDER
  for (i in 1:nIndividuals) {
    # IDENTIFY THE AGENT
    Agent <- Order$Agent[Order$Priority == i]
    # EXTRACT THE PREFERENCES
    AgentPrefs <- prefs[, Agent]
    # KEEP ONLY THE NON ASSIGNED
    AgentPrefs <- setdiff(AgentPrefs, Output$proposals)
    # IF THERE IS REMAINING, ASSIGN THE FIRST, IF NOT, ASSIGN NA
    Output$proposals[Agent] <- ifelse(length(AgentPrefs) >= 1, 
                                      AgentPrefs[1], NA)
    # ASSIGN THE AGENT TO THE WOMEN ALSO BY THE SAME LOGIC
    Output$engagements[AgentPrefs[1]] <- ifelse(length(AgentPrefs) >= 1, 
                                      Agent, NA)
  }
  # NOW FOR THOSE NON ASSIGNED REPLACE BY NA
  Output$proposals[Output$proposals == 0] <- NA
  Output$engagements[Output$engagements == 0] <- NA
  # CHECK IF THERE WERE NON ASSIGNED AND ASSIGN TO THE SINGLE'S SET
  for(proposer in 1:nIndividuals){
    if(is.na(Output$proposals[proposer])){
      Output$single.proposers <- append(Output$single.proposers, proposer)
    }
  }
  # FOR THE OTHER SIDE OF THE MARKET
  for(reviewer in 1:nObjects){
    if(is.na(Output$engagements[reviewer])){
      Output$single.reviewers <- append(Output$single.reviewers, reviewer)
    }
  }
  return(Output)
}