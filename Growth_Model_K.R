# Population growth model with cohorts and carrying capacity
# 01/07/2024 AIMILIOS PASCHALIS KOUNIS

# Function for age 0 growth
# br : number of breeding individuals
# gr : growth rate
# d : mortality
C1_growth <- function(br = NA, gr = NA, d = NA) {
  C1 <- br * gr
  C1 <- C1 - (C1 * d)
  if (C1 < 0) {
    C1 <- 0
  }
  round(C1, 0)
}

# Function for age 1+ growth
# c : number of individuals in previous cohort
# d : mortality
Cn_growth <- function(c = NA, d = NA){
  Cn <- c - (c * d)
  if (Cn < 0){
    Cn <- 0
  }
  round(Cn, 0)
}


# Function for running model
# a : vector of 6 numbers with initial cohort pop
# t : number of ticks
# gr : birth rate
# d : vector of 6 numbers with mortality of each cohort
# res : initial amount of resources, can be negative
# rgr : resourses regenerated per tick
# dmod : how much is mortality allowed to scale to resources (0 : no modifiers, 1 : double, negative : inverse scaling)
# gmod : how much is birth rate allowed to scale to resources (0 : no modifiers, 1 : double, negative : inverse scaling)
# growth_function <- function (a = rep(20,6), t = 100,
#                              gr = 2, d = c(6,4,2,2,4,6) / 10,
#                              res = 20*6, rgr = 200,
#                              dmod = 0.5, gmod = 0.5) {
#   # Show error if any value is wrong
# {
#   if (is.vector(a) == F || length(a) < 6 || is.numeric(a) == F || all(sign(a)>=0) == F){
#     stop("'a' must be a numeric vector with 6 positive values")
#   }
#   if (is.numeric(t) == F || t<=0 ){
#     stop("'t' must be a non-zero positive number")
#   }
#   if (is.numeric(gr) == F || gr<0 ){
#     stop("'gr' must be a positive number")
#   }
#   if (is.vector(d) == F || length(d) < 6 || is.numeric(d) == F || all(sign(d)>=0) == F ){
#     stop("'d' must be a numeric vector with 6 positive values")
#   }
#   if (is.numeric(res) == F){
#     stop("'res' must be a number")
#   }
#   if (is.numeric(rgr) == F || rgr<0){
#     stop("'rgr' must be a positive number")
#   }
#   if (is.numeric(dmod) == F){
#     stop("'dmod' must be a number")
#   }
#   if (is.numeric(gmod) == F){
#     stop("'gmod' must be a number")
#   }
# }
#   # Dataframe with initial population at tick 0 for 6 cohorts
# {  
#   results <- data.frame("tick" = 0, 
#                         "C1" = a[1], # age 0
#                         "C2" = a[2], 
#                         "C3" = a[3],
#                         "C4" = a[4],
#                         "C5" = a[5],
#                         "C6" = a[6],
#                         "res" = res
#   )
#   results$total <- sum(results[,2:7])
#   
#   # List of parameters
#   parameters <- list("t" = t, 
#                      "gr" = gr,
#                      "d1" = d[1],
#                      "d2" = d[2],
#                      "d3" = d[3],
#                      "d4" = d[4],
#                      "d5" = d[5],
#                      "d6" = d[6],
#                      "res" = res,
#                      "rgr" = rgr
#   )
# }
#   # Loop running growth function for each cohort and saving into results
#   for (i in 1:parameters$t) {
#     res <- res - results$total[i] # Update resources based on pop
#     res_prop <- res / results$total[i] # Available resource ratio
#     res_dmod <- dmod - plogis(res_prop, scale = 0.5) * (2*dmod) # Calculate mortality modifier based on available res
#     res_gmod <- plogis(res_prop, scale = 0.5) * (2*gmod) - gmod # Calculate birth modifier based on available res
#     results[i+1,"tick"] <- i # Advance time
#     results[i+1,"C1"] <- do.call(C1_growth, # Call function for age 0 (births)
#                                  list(results[i,"total"] - results[i, "C1"], # Breeding pop is all but age 0
#                                       parameters$gr + parameters$gr * res_gmod, # Birth rate modified by available resources
#                                       parameters$d1 + parameters$d1 * res_dmod # Mortality of age 0 modified by available res
#                                  )
#     )
#     results[i+1,"C2"] <- do.call(Cn_growth, # Call function for age 1 growth (survivors)
#                                  list(results[i,"C1"], # Survivng age 0
#                                       parameters$d2 + parameters$d2 * res_dmod # Mortality of age 1 modified by available res
#                                  )
#     ) 
#     results[i+1,"C3"] <- do.call(Cn_growth, list(results[i,"C2"], parameters$d3 + parameters$d3 * res_dmod)) 
#     results[i+1,"C4"] <- do.call(Cn_growth, list(results[i,"C3"], parameters$d4 + parameters$d4 * res_dmod))
#     results[i+1,"C5"] <- do.call(Cn_growth, list(results[i,"C4"], parameters$d5 + parameters$d5 * res_dmod))
#     results[i+1,"C6"] <- do.call(Cn_growth, list(results[i,"C5"], parameters$d6 + parameters$d6 * res_dmod)) 
#     results[i+1,"total"] <- sum(results[i+1,2:7]) # Sum of entire popualtion for tick
#     results[i+1,"res"] <- res # Save resources
#     results[i+1,"res_dmod"] <- res_dmod # Save mortality modifier
#     results[i+1,"res_gmod"] <- res_gmod # Save birth modifier
#     res <- res + rgr # Regenerate resources
#   }
#   # Plot total population for entire timeframe and cohorts
#   {
#   layout(matrix(c(1,1,2,3), 2, 2, byrow = T))
#   # plot(log10(results$total) ~ results$tick, 
#   #      type = "l",
#   #      xlab = "Time",
#   #      ylab = "Log10(Total individuals)",
#   #      col = 1,
#   #      )
#   par(mar = c(5,4,4,8), xpd = T)
#   plot(results$total ~ results$tick, 
#        type = "l",
#        xlab = "Time",
#        ylab = "Total individuals",
#        col = 1,
#        ylim = c(0,max(results$total) + max(results$total)*0.1),
#        yaxs = "i"
#        )
#   lines(results$C1 ~ results$tick,
#         type = "h",
#         col = 2)
#   lines(results$C2 ~ results$tick,
#         type = "h",
#         col = 3)
#   lines(results$C3 ~ results$tick,
#         type = "h",
#         col = 4)
#   lines(results$C4 ~ results$tick,
#         type = "h",
#         col = 5)
#   lines(results$C5 ~ results$tick,
#         type = "h",
#         col = 6)
#   lines(results$C6 ~ results$tick,
#         type = "h",
#         col = 7)
#   legend("topright",
#          legend = c("Total", "Age 0", "Age 1", "Age 2", "Age 3", "Age 4", "Age 5"),
#          lty = rep(1,7),
#          lwd = 2,
#          col = 1:7,
#          cex = 0.7,
#          ncol = 1,
#          inset = c(-0.15,0)
#   )
#   }
#   # Plot resources and modifiers over time
#   {
#   par(mar = c(5,4,4,2))
#     plot(results$res ~ results$tick,
#        type = "l",
#        col = 1,
#        xlab = "Time",
#        ylab = "Resources"
#        )
#   par(mar = c(5,4,4,6), xpd = T)
#   plot(results$res_dmod ~ results$tick,
#        type = "l",
#        col = "blue",
#        xlab = "Time",
#        ylab = "Modifier value",
#        ylim = c(min(c(-gmod, -dmod)),max(c(gmod, dmod)))
#        )
#   lines(results$res_gmod ~ results$tick,
#        type = "l",
#        col = "red"
#        )
#   # barplot(cbind(results$res_dmod, results$res_gmod) ~ results$tick, 
#   #         col = c("blue", "red"),
#   #         beside = F,
#   #         xlab = "Time",
#   #         ylab = "Modifier value"
#   #         )
#   legend("topright",
#          c("Mortality", "Birth"),
#          col = c("blue", "red"),
#          lty = 1,
#          cex = 0.7,
#          inset = c(-0.45,0)
#          )
#   }
#   par(mfrow = c(1,1))
#   # Spit out initial parameters
#   print(parameters)
#   print(results[1,c(2:7,9)])
# }
# Test function
# growth_function()
# growth_function(a = c(100, sample(1:10, 5)),
#                 t = 100,
#                 gr = sample(10:300, 1)/100,
#                 d = c(sample(50:90, 1)/100, 
#                       sample(30:50, 1)/100, 
#                       sample(10:30, 1)/100, 
#                       sample(10:25, 1)/100, 
#                       sample(30:50, 1)/100,
#                       sample(50:90, 1)/100),
#                 res = sample(0:1000, 1),
#                 rgr = sample(10:1000, 1),
#                 gmod = 1, dmod = 2
# )
