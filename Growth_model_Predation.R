# Population growth model with predation
# 08/07/2024 AIMILIOS PASCHALIS KOUNIS
### Dependencies----
source("https://raw.githubusercontent.com/Gahenstein/Models/main/Growth_Model_K.R")
### Argument explanation----
# a : vector of 6 numbers with initial cohort pop for prey
# b : vector of 6 numbers with initial cohort pop for pred
# t : number of ticks
# gr1 : birth rate for prey
# gr2 : birth rate for pred
# d1 : vector of 6 numbers with mortality of each cohort for prey
# d2 : vector of 6 numbers with mortality of each cohort for pred
# res_prey : for prey initial amount of resources, can be negative
# rgr : resourses regenerated per tick
# ef : predation efficiency of each cohort (how much prey the pred pop would take if unobstructed)
# av : predation avoidance efficiency (how easily predator can be evaded
#      0 : all avoided, 1 : none avoided)
# dmod1 : for prey how much is mortality allowed to scale to resources 
#         (0 : no modifiers, 1 : double, negative : inverse scaling)
# dmod2 : for pred how much is mortality allowed to scale to resources 
#         (0 : no modifiers, 1 : double, negative : inverse scaling)
# gmod1 : for prey how much is birth rate allowed to scale to resources 
#         (0 : no modifiers, 1 : double, negative : inverse scaling)
# gmod2 : for pred how much is birth rate allowed to scale to resources 
#         (0 : no modifiers, 1 : double, negative : inverse scaling)
### Predation growth function----
pred_growth_function <- function (a = rep(20,6), b = rep(5,6), t = 1000,
                                  gr1 = 2, gr2 = 1.3,
                                  d1 = c(60,40,20,20,40,60) / 100, d2 = c(50,30,10,10,30,50) / 100,
                                  res_prey = 20*6, rgr = 200,
                                  ef = c(10,20,80,130,140,10) / 100, av = c(90,40,20,10,20,40) / 100,
                                  dmod1 = 1, dmod2 = 1,
                                  gmod1 = 1, gmod2 = 1) {
  # browser()
  ### Show error if any value is wrong
  {
    if (is.vector(a) == F || length(a) < 6 || is.numeric(a) == F || all(sign(a)>=0) == F){
      stop("'a' must be a numeric vector with 6 positive values")
    }
    if (is.vector(b) == F || length(b) < 6 || is.numeric(b) == F || all(sign(b)>=0) == F){
      stop("'b' must be a numeric vector with 6 positive values")
    }
    if (is.numeric(t) == F || t<=0 ){
      stop("'t' must be a non-zero positive number")
    }
    if (is.numeric(gr1) == F || gr1<0 ){
      stop("'gr1' must be a positive number")
    }
    if (is.numeric(gr2) == F || gr2<0 ){
      stop("'gr2' must be a positive number")
    }
    if (is.vector(d1) == F || length(d1) < 6 || is.numeric(d1) == F || all(sign(d1)>=0) == F ){
      stop("'d1' must be a numeric vector with 6 positive values")
    }
    if (is.vector(d2) == F || length(d2) < 6 || is.numeric(d2) == F || all(sign(d2)>=0) == F ){
      stop("'d2' must be a numeric vector with 6 positive values")
    }
    if (is.numeric(res_prey) == F){
      stop("'res_prey' must be a number")
    }
    if (is.numeric(rgr) == F || rgr<0){
      stop("'rgr' must be a positive number")
    }
    if (is.vector(ef) == F || length(ef) < 6 || is.numeric(ef) == F || all(sign(ef)>=0) == F ){
      stop("'ef' must be a numeric vector with 6 positive values")
    }
    if (is.vector(av) == F || length(av) < 6 || is.numeric(av) == F || all(sign(av)>=0) == F ){
      stop("'av' must be a numeric vector with 6 positive values")
    }
    if (is.numeric(dmod1) == F){
      stop("'dmod1' must be a number")
    }
    if (is.numeric(dmod2) == F){
      stop("'dmod2' must be a number")
    }
    if (is.numeric(gmod1) == F){
      stop("'gmod1' must be a number")
    }
    if (is.numeric(gmod2) == F){
      stop("'gmod2' must be a number")
    }
  }
  ### Lists of parameters
  {
  parameters_prey <- list("t" = t, 
                          "gr" = gr1,
                          "d1" = d1[1],
                          "d2" = d1[2],
                          "d3" = d1[3],
                          "d4" = d1[4],
                          "d5" = d1[5],
                          "d6" = d1[6],
                          "av1" = av[1],
                          "av2" = av[2],
                          "av3" = av[3],
                          "av4" = av[4],
                          "av5" = av[5],
                          "av6" = av[6],
                          "res" = res_prey,
                          "rgr" = rgr
  )
  parameters_pred <- list("t" = t, 
                          "gr" = gr2,
                          "d1" = d2[1],
                          "d2" = d2[2],
                          "d3" = d2[3],
                          "d4" = d2[4],
                          "d5" = d2[5],
                          "d6" = d2[6],
                          "ef1" = ef[1],
                          "ef2" = ef[2],
                          "ef3" = ef[3],
                          "ef4" = ef[4],
                          "ef5" = ef[5],
                          "ef6" = ef[6],
                          "res" = NA
  )
  }
  ### Dataframes with initial populations at tick 0 for 6 cohorts
  {
    # Predator
    results_pred <- data.frame("tick" = 0, 
                               "C1" = b[1], # age 0
                               "C2" = b[2], 
                               "C3" = b[3],
                               "C4" = b[4],
                               "C5" = b[5],
                               "C6" = b[6]
    )
    results_pred$total <- sum(results_pred[,2:7])
        # Average predation efficiency of entire population (so a population with more young less efficient at hunting)
    results_pred$mean_ef <- ((results_pred$C1[1] * parameters_pred$ef1) + (results_pred$C2[1] * parameters_pred$ef2) +
                               (results_pred$C3[1] * parameters_pred$ef3) + (results_pred$C4[1] * parameters_pred$ef4) +
                               (results_pred$C5[1] * parameters_pred$ef5) + (results_pred$C6[1] * parameters_pred$ef6)) / results_pred$total[1]
    # Initial resources equal to pop
    results_pred$res <- results_pred$total
    # Prey
    results_prey <- data.frame("tick" = 0, 
                          "C1" = a[1], # age 0
                          "C2" = a[2], 
                          "C3" = a[3],
                          "C4" = a[4],
                          "C5" = a[5],
                          "C6" = a[6],
                          # pr : number of individuals in cohort that die to predation
                          # calculated as the populaition in a cohort multiplied by
                          # the ratio of available in cohort (preycohort pop * avoidance) divided by
                          # base predation need (pred pop * mean pred_efficiency)
                          "pr1" = NA, 
                          "pr2" = NA, 
                          "pr3" = NA,
                          "pr4" = NA,
                          "pr5" = NA,
                          "pr6" = NA,
                          "res" = res_prey
    )
    results_prey$total <- sum(results_prey[1,paste("C", 1:6, sep = "")])
  }
  ### Loop running growth function for each cohort and saving into results
  for (i in 1:parameters_prey$t) {
    # Prey loop
    results_prey[i+1,"res"] <- results_prey$res[i] - results_prey$total[i] + rgr # Update resources based on pop and res regen
    res_prop_prey <- results_prey[i+1,"res"] / results_prey$total[i] # Available resource ratio
    if (is.finite(res_prop_prey) == F) {res_prop_prey <- 10000}
    res_dmod_prey <- dmod1 - plogis(res_prop_prey, scale = 0.5) * (2*dmod1) # Calculate mortality modifier based on available res
    res_gmod_prey <- plogis(res_prop_prey, scale = 0.5) * (2*gmod1) - gmod1 # Calculate birth modifier based on available res
    results_prey[i+1,"tick"] <- i # Advance time
    results_prey[i+1,"C1"] <- do.call(C1_growth, # Call function for age 0 (births)
                                 list(results_prey[i,"total"] - results_prey[i, "C1"], # Breeding pop is all but age 0
                                      parameters_prey$gr + parameters_prey$gr * res_gmod_prey, # Birth rate modified by available resources
                                      parameters_prey$d1 + parameters_prey$d1 * res_dmod_prey # Mortality of age 0 modified by available res
                                 ))
    results_prey[i+1, "pr1"] <- results_pred$total[i] * results_pred$mean_ef[i] * parameters_prey$av1 # Calculate number of individuals dying to predation
    if (results_prey[i+1, "pr1"] > results_prey[i+1, "C1"]) {results_prey[i+1, "pr1"] <-  results_prey[i+1, "C1"]}
    results_prey[i+1, "C1"] <- results_prey[i+1, "C1"] - results_prey$pr1[i+1]
    results_prey[i+1,"C2"] <- do.call(Cn_growth, # Call function for age 1 growth (survivors)
                                 list(results_prey[i,"C1"], # Survivng age 0
                                      parameters_prey$d2 + parameters_prey$d2 * res_dmod_prey # Mortality of age 1 modified by available res
                                 ))
    results_prey[i+1, "pr2"] <- results_pred$total[i] * results_pred$mean_ef[i] * parameters_prey$av2
    if (results_prey[i+1, "pr2"] > results_prey[i+1, "C2"]) {results_prey[i+1, "pr2"] <-  results_prey[i+1, "C2"]}
    results_prey[i+1, "C2"] <- results_prey[i+1, "C2"] - results_prey$pr2[i+1]
    results_prey[i+1,"C3"] <- do.call(Cn_growth, list(results_prey[i,"C2"], parameters_prey$d3 + parameters_prey$d3 * res_dmod_prey))
    results_prey[i+1, "pr3"] <- results_pred$total[i] * results_pred$mean_ef[i] * parameters_prey$av3
    if (results_prey[i+1, "pr3"] > results_prey[i+1, "C3"]) {results_prey[i+1, "pr3"] <-  results_prey[i+1, "C3"]}
    results_prey[i+1, "C3"] <- results_prey[i+1, "C3"] - results_prey$pr3[i+1]
    results_prey[i+1,"C4"] <- do.call(Cn_growth, list(results_prey[i,"C3"], parameters_prey$d4 + parameters_prey$d4 * res_dmod_prey))
    results_prey[i+1, "pr4"] <- results_pred$total[i] * results_pred$mean_ef[i] * parameters_prey$av4
    if (results_prey[i+1, "pr4"] > results_prey[i+1, "C4"]) {results_prey[i+1, "pr4"] <-  results_prey[i+1, "C4"]}
    results_prey[i+1, "C4"] <- results_prey[i+1, "C4"] - results_prey$pr4[i+1]
    results_prey[i+1,"C5"] <- do.call(Cn_growth, list(results_prey[i,"C4"], parameters_prey$d5 + parameters_prey$d5 * res_dmod_prey))
    results_prey[i+1, "pr5"] <- results_pred$total[i] * results_pred$mean_ef[i] * parameters_prey$av5
    if (results_prey[i+1, "pr5"] > results_prey[i+1, "C5"]) {results_prey[i+1, "pr5"] <-  results_prey[i+1, "C5"]}
    results_prey[i+1, "C5"] <- results_prey[i+1, "C5"] - results_prey$pr5[i+1]
    results_prey[i+1,"C6"] <- do.call(Cn_growth, list(results_prey[i,"C5"], parameters_prey$d6 + parameters_prey$d6 * res_dmod_prey))
    results_prey[i+1, "pr6"] <- results_pred$total[i] * results_pred$mean_ef[i] * parameters_prey$av6
    if (results_prey[i+1, "pr6"] > results_prey[i+1, "C6"]) {results_prey[i+1, "pr6"] <-  results_prey[i+1, "C6"]}
    results_prey[i+1, "C6"] <- results_prey[i+1, "C6"] - results_prey$pr6[i+1]
    results_prey[i+1,"total"] <- sum(results_prey[i+1,2:7]) # Sum of entire population for tick
    results_prey[i+1,"res_dmod"] <- res_dmod_prey # Save mortality modifier
    results_prey[i+1,"res_gmod"] <- res_gmod_prey # Save birth modifier
    results_pred[i+1, "res"] <-  sum(results_prey[i + 1, paste("pr",1:6, sep = "")]) # Save total predation deaths as resources for predators
    # Predator loop
    results_pred[i+1,"res"] <- results_pred[i+1,"res"] - results_pred$total[i] # Update resources based on pop
    res_prop <- results_pred[i+1,"res"] / results_pred$total[i] # Available resource ratio
    if (is.finite(res_prop) == F) {res_prop <- 10000}
    res_dmod <- dmod2 - plogis(res_prop, scale = 0.05) * (2*dmod2) # Calculate mortality modifier based on available res
    res_gmod <- plogis(res_prop, scale = 0.05) * (2*gmod2) - gmod2 # Calculate birth modifier based on available res
    results_pred[i+1,"tick"] <- i # Advance time
    results_pred[i+1,"C1"] <- do.call(C1_growth, # Call function for age 0 (births)
                                 list(results_pred[i,"total"] - sum(results_pred[i, paste("C", 1:2, sep = "")]), # Breeding pop is all but age 0 and 1
                                      parameters_pred$gr + parameters_pred$gr * res_gmod, # Birth rate modified by available resources
                                      parameters_pred$d1 + parameters_pred$d1 * res_dmod # Mortality of age 0 modified by available res
                                 )
    )
    results_pred[i+1,"C2"] <- do.call(Cn_growth, # Call function for age 1 growth (survivors)
                                 list(results_pred[i,"C1"], # Survivng age 0
                                      parameters_pred$d2 + parameters_pred$d2 * res_dmod # Mortality of age 1 modified by available res
                                 )
    )
    results_pred[i+1,"C3"] <- do.call(Cn_growth, list(results_pred[i,"C2"], parameters_pred$d3 + parameters_pred$d3 * res_dmod)) 
    results_pred[i+1,"C4"] <- do.call(Cn_growth, list(results_pred[i,"C3"], parameters_pred$d4 + parameters_pred$d4 * res_dmod))
    results_pred[i+1,"C5"] <- do.call(Cn_growth, list(results_pred[i,"C4"], parameters_pred$d5 + parameters_pred$d5 * res_dmod))
    results_pred[i+1,"C6"] <- do.call(Cn_growth, list(results_pred[i,"C5"], parameters_pred$d6 + parameters_pred$d6 * res_dmod)) 
    results_pred[i+1,"total"] <- sum(results_pred[i+1,paste("C", 1:6, sep = "")]) # Sum of entire popualtion for tick
    results_pred[i+1, "mean_ef"] <- ((results_pred$C1[i+1] * parameters_pred$ef1) + (results_pred$C2[i+1] * parameters_pred$ef2) +
                               (results_pred$C3[i+1] * parameters_pred$ef3) + (results_pred$C4[i+1] * parameters_pred$ef4) +
                               (results_pred$C5[i+1] * parameters_pred$ef5) + (results_pred$C6[i+1] * parameters_pred$ef6)) / results_pred$total[i+1]
    if (is.finite(results_pred[i+1, "mean_ef"]) == F) {results_pred[i+1, "mean_ef"] <- 0}
    results_pred[i+1,"res_dmod"] <- res_dmod # Save mortality modifier
    results_pred[i+1,"res_gmod"] <- res_gmod # Save birth modifier
  }
  ### Plot total population for entire timeframe and cohorts for prey
  {
    layout(matrix(c(1,1,2,3), 2, 2, byrow = T))
    # plot(log10(results$total) ~ results$tick,
    #      type = "l",
    #      xlab = "Time",
    #      ylab = "Log10(Total individuals)",
    #      col = 1,
    #      main = "Prey"
    #      )
    par(mar = c(5,4,4,8), xpd = T)
    plot(results_prey$total ~ results_prey$tick,
         type = "l",
         xlab = "Time",
         ylab = "Total individuals",
         col = 1,
         ylim = c(0,max(results_prey$total) + max(results_prey$total)*0.1),
         yaxs = "i",
         main = "Prey"
    )
    lines(results_prey$C1 ~ results_prey$tick,
          type = "h",
          col = 2)
    lines(results_prey$C2 ~ results_prey$tick,
          type = "h",
          col = 3)
    lines(results_prey$C3 ~ results_prey$tick,
          type = "h",
          col = 4)
    lines(results_prey$C4 ~ results_prey$tick,
          type = "h",
          col = 5)
    lines(results_prey$C5 ~ results_prey$tick,
          type = "h",
          col = 6)
    lines(results_prey$C6 ~ results_prey$tick,
          type = "h",
          col = 7)
    legend("topright",
           legend = c("Total", "Age 0", "Age 1", "Age 2", "Age 3", "Age 4", "Age 5"),
           lty = rep(1,7),
           lwd = 2,
           col = 1:7,
           cex = 0.7,
           ncol = 1,
           inset = c(-0.15,0)
    )
  }
  ### Plot resources and modifiers over time for prey
  {
    par(mar = c(5,4,4,2))
    plot(results_prey$res ~ results_prey$tick,
         type = "l",
         col = 1,
         xlab = "Time",
         ylab = "Resources"
    )
    par(mar = c(5,4,4,6), xpd = T)
    plot(results_prey$res_dmod * 100 ~ results_prey$tick,
         type = "l",
         col = "blue",
         xlab = "Time",
         ylab = "Modifier value (%)",
         ylim = c(min(c(-gmod1, -dmod1)) * 100,max(c(gmod1, dmod1)) * 100)
    )
    lines(results_prey$res_gmod * 100 ~ results_prey$tick,
          type = "l",
          col = "red"
    )
    # barplot(cbind(results_prey$res_dmod, results_prey$res_gmod) ~ results_prey$tick, 
    #         col = c("blue", "red"),
    #         beside = F,
    #         xlab = "Time",
    #         ylab = "Modifier value"
    #         )
    legend("topright",
           c("Mortality", "Birth"),
           col = c("blue", "red"),
           lty = 1,
           cex = 0.7,
           inset = c(-0.45,0)
    )
  }
  ### Plot total population for entire timeframe and cohorts for predator
  {
    layout(matrix(c(1,1,2,3), 2, 2, byrow = T))
    # plot(log10(results_pred$total) ~ results_pred$tick, 
    #      type = "l",
    #      xlab = "Time",
    #      ylab = "Log10(Total individuals)",
    #      col = 1,
    #      main = "Predator"
    #      )
    par(mar = c(5,4,4,8), xpd = T)
    plot(results_pred$total ~ results_pred$tick, 
         type = "l",
         xlab = "Time",
         ylab = "Total individuals",
         col = 1,
         ylim = c(0,max(results_pred$total) + max(results_pred$total)*0.1),
         yaxs = "i",
         main = "Predator"
    )
    lines(results_pred$C1 ~ results_pred$tick,
          type = "h",
          col = 2)
    lines(results_pred$C2 ~ results_pred$tick,
          type = "h",
          col = 3)
    lines(results_pred$C3 ~ results_pred$tick,
          type = "h",
          col = 4)
    lines(results_pred$C4 ~ results_pred$tick,
          type = "h",
          col = 5)
    lines(results_pred$C5 ~ results_pred$tick,
          type = "h",
          col = 6)
    lines(results_pred$C6 ~ results_pred$tick,
          type = "h",
          col = 7)
    legend("topright",
           legend = c("Total", "Age 0", "Age 1", "Age 2", "Age 3", "Age 4", "Age 5"),
           lty = rep(1,7),
           lwd = 2,
           col = 1:7,
           cex = 0.7,
           ncol = 1,
           inset = c(-0.15,0)
    )
  }
  ### Plot resources and modifiers over time for predator
  {
    par(mar = c(5,4,4,2))
    plot(results_pred$res ~ results_pred$tick,
         type = "l",
         col = 1,
         xlab = "Time",
         ylab = "Resources (dead prey)"
    )
    par(mar = c(5,4,4,6), xpd = T)
    plot(results_pred$res_dmod * 100 ~ results_pred$tick,
         type = "l",
         col = "blue",
         xlab = "Time",
         ylab = "Modifier value (%)",
         ylim = c(min(c(-gmod2, -dmod2)) * 100,max(c(gmod2, dmod2)) * 100)
    )
    lines(results_pred$res_gmod * 100 ~ results_pred$tick,
          type = "l",
          col = "red"
    )
    # barplot(cbind(results_pred$res_dmod, results_pred$res_gmod) ~ results_pred$tick, 
    #         col = c("blue", "red"),
    #         beside = F,
    #         xlab = "Time",
    #         ylab = "Modifier value"
    #         )
    legend("topright",
           c("Mortality", "Birth"),
           col = c("blue", "red"),
           lty = 1,
           cex = 0.7,
           inset = c(-0.45,0)
    )
  }
  ### Plot both populations
  {
  par(mfrow = c(1,1))
  plot(results_prey$total ~ results_prey$tick,
       type = "l",
       xlab = "Time",
       ylab = "Total individuals",
       col = 1,
       ylim = c(0,max(results_prey$total) + max(results_prey$total)*0.1),
       yaxs = "i"
  )
    lines(results_pred$total ~ results_prey$tick,
          type = "l",
          col = 2)
    legend("topright",
           legend = c("Prey", "Predator"),
           lty = 1,
           col = 1:2)
  }
  par(mfrow = c(1,1))
  }
### Test----
# pred_growth_function()
