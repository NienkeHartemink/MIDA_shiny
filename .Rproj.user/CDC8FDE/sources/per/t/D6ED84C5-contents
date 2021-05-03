# 
# 
# SIR.stochastic <- function(populations, beta, alpha){
#   ## First extract the current population sizes from the data frame
#   latest <- tail(populations, 1)
#   
#   ## Calculate total size of population
#   pop.size <- latest$Susceptibles + latest$Infecteds + latest$Recovereds
#   rate.infection <- beta * latest$Susceptibles * latest$Infecteds / pop.size
#   rate.recovery <- alpha * latest$Infecteds
#   
#   
#   # Calculate time to next event and carry out event or right type
#   time.to.event <- rexp(1, rate=rate.infection + rate.recovery)
#   next.time <- latest$Time + time.to.event
#   #if prop of infection greater than 1 or less than 1 what happens
#   what.happened <- runif(1)
#   if (what.happened <= rate.infection/(rate.infection + rate.recovery))
#   { # infection event
#     next.susceptibles <- latest$Susceptibles - 1
#     next.infecteds <- latest$Infecteds + 1
#     next.recovereds <- latest$Recovereds 
#     
#   }
#   else
#   { # recovery event
#     next.susceptibles <- latest$Susceptibles
#     next.infecteds <- latest$Infecteds - 1
#     next.recovereds <- latest$Recovereds + 1
#   }        
#   
#   
#   ## Add new row onto populations data frame and return
#   next.populations <- rbind(populations,
#                             data.frame(Time= next.time,
#                                        Susceptibles=next.susceptibles,
#                                        Infecteds=next.infecteds,
#                                        Recovereds=next.recovereds))
# }
# 
# 
# plot.populations.SIR.stochastic <- function(populations){
#   
#   p <- ggplot(data = all_pops) +
#     ylab("Number of individuals") +  xlab("Time (days)") +
#     geom_line(aes(x = Time, y = Susceptibles, group = Sim), col = "steelblue4", alpha = .8) +
#     geom_line(aes(x = Time, y = Infecteds, group = Sim), col = "ivory4", alpha = .8) +
#     geom_line(aes(x = Time, y = Recovereds, group = Sim), col = "palegreen3", alpha = .8) +
#     theme_bw() +
#     theme(panel.border = element_blank(), legend.position = "right",
#           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           axis.line = element_line(colour = "black"))
#   
#   py <- ggplotly(p) %>%
#     layout(legend = list(
#       orientation = "v",
#       x = 0.5, y = -0.4
#     )
#     )
#   
#   return(py)
# }
# 
# 
# all_pops <- c()
# for(loop in 1:10){
# ## Set up initial populations
# pop.size <- 100
# initial.infecteds <- 10
# initial.susceptibles <- pop.size - initial.infecteds
# initial.recovereds <- 0
# ## Transmission and recovery rates
# beta <- .5
# alpha <- .2
# ## Simulation times
# start.time <-0
# end.time <- 200
# ## Set up the initial population sizes and starting time
# populations <- data.frame(Time= start.time,
#                           Susceptibles = initial.susceptibles,
#                           Infecteds = initial.infecteds,
#                           Recovereds = initial.recovereds
#                           )
# 
# current.time <- tail(populations$Time, 1)
# # Is the experiment over?
# is.finished <- tail(populations$Infecteds, 1)
# 
# ### Run the simulation
# while (is.finished > 0 & current.time < end.time ){
#   populations <- SIR.stochastic(populations, beta, alpha)
#   current.time <- tail(populations$Time, 1)
#   is.finished <- tail(populations$Infecteds, 1)
# }
# populations$Sim <- loop
# populations$Sim <- as.factor(populations$Sim)
# all_pops <- rbind(all_pops, populations)
# plot.populations.SIR.stochastic(all_pops)
# }
# 
# 
# ## average
