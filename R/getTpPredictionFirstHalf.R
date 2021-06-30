getTpPredictionFirstHalf <- function(AWAY_EFG, AWAY_TOVr, AWAY_OREBr, AWAY_DREBr, AWAY_FTF, AWAY_oEFG, AWAY_oTOVr, AWAY_oFTF, HOME_EFG, HOME_TOVr, HOME_OREBr, HOME_DREBr, HOME_FTF, HOME_oEFG, HOME_oTOVr, HOME_oFTF) {

  # librarie
  library(nnet)

  # turn inputs into new data data frame
  df_inputs <- as.data.frame(list("AWAY_EFG" = AWAY_EFG, "AWAY_TOVr" = AWAY_TOVr, "AWAY_OREBr" = AWAY_OREBr,
                                  "AWAY_DREBr" = AWAY_DREBr, "AWAY_FTF" = AWAY_FTF, "AWAY_oEFG" = AWAY_oEFG,
                                  "AWAY_oTOVr" = AWAY_oTOVr, "AWAY_oFTF" = AWAY_oFTF, "HOME_EFG" = HOME_EFG,
                                  "HOME_TOVr" = HOME_TOVr, "HOME_OREBr" = HOME_OREBr, "HOME_DREBr" = HOME_DREBr,
                                  "HOME_FTF" = HOME_FTF, "HOME_oEFG" = HOME_oEFG, "HOME_oTOVr" = HOME_oTOVr,
                                  "HOME_oFTF" = HOME_oFTF))

  # declare Markov-chain states
  states_temp <- c("Ai0", "Ai1", "Ai2", "Ai3", "As0", "Af0", "Af1", "Af2", "Af3", "Ad0", "Ad1", "Ad2", "Ao0", "Ao1", "Ao2", "Bi0", "Bi1", "Bi2", "Bi3", "Bs0", "Bf0", "Bf1", "Bf2", "Bf3", "Bd0", "Bd1", "Bd2", "Bo0", "Bo1", "Bo2")

  # create skeleton transition matrix
  m <- matrix(0.0000, nrow = 30, ncol = 30)
  colnames(m) <- states_temp
  rownames(m) <- states_temp

  # use multinomial logit models to predict games in training set
  for (i in rownames(m)) {
    if (class(logit_models[[i]]) == "list") {
      next
    }
    temp_predict <- as.list(predict(returnData="TRUE", logit_models[[i]], type="probs", newdata=df_inputs))
    for (j in names(temp_predict)) {
      if (j == "1") {
        break
      }
      m[i, j] <- temp_predict[[j]]
    }
  }

  # convert matrix to a named nested list
  m <- as.list(as.data.frame(t(m)))
  for (i in names(m)) {
    m[[i]] <- as.list(m[[i]])
    names(m[[i]]) <- states_temp
  }

  # return predicted transition matrix as nested list
  return(m)

}
