# ##########################################################
# Shiny Dashboard: MIDA
# Author: Ann Barber
# Initalisation Date: 18/05/2020
# Finalisation Date: 09/06/2020 ......
# Description: Epi-models for Management of Infections and 
# Diseases in Animal Populations - QVE masters course
# ##########################################################

source("pgm/LoadData.R")
source("pgm/RenderFunctions.R")

###########################################################
###########################################################
#----------------------------------------------------------
#----------------------------------------------------------
# User Interface (UI)
#----------------------------------------------------------
#----------------------------------------------------------
###########################################################
###########################################################

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title =  "MIDA",
    titleWidth = 180
  ), # End dashboardHeader
  dashboardSidebar(
    width = 180,
    sidebarMenu(
      menuItem("Home Page", tabName = "home", icon = icon("home")),
      menuItem("SIR Model", tabName = "SIR", icon = icon("stream")),
      menuItem("SIS Model", tabName = "SIS", icon = icon("stream")),
      menuItem("SEIR Model", tabName = "SEIR", icon = icon("stream")),
      menuItem("Stochastic Models", tabName = "stochastic", icon = icon("stream")),
      menuItem("Vocabulary", tabName = "vocab", icon = icon("stream"))
    ) # End sidebarMenu
  ), # End dashboardSidebar
  dashboardBody(
    tags$style(HTML(
      '.main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-size: 16px;
        }
      .content-wrapper,
.left-side {
  background-color: #ffffff;
      }'
    )),
    tabItems(
      
      ###########################################################
      #----------------------------------------------------------
      # 0.0 Home Page (UI)
      #----------------------------------------------------------
      ###########################################################
      
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            align = "center",
            HTML("<h2><b>Management of Infections and Diseases in Animal Populations</b></h2>
             <h3>QVE-30806</h3>"),
            br(),
            img(src = "ReaderCover.png", height = "33%", width = "33%"),
            br(),
            br(),
            HTML("<h4>Application code is available <a href = 'https://github.com/AnnBarber/MIDA_shiny'>here</a> on GitHub</h4>")
          ) # End column
        ) # End fluidRow
      ), # End tabItem
      
      ###########################################################
      #----------------------------------------------------------
      # 1.0 SIR Model - without vital dynamics (UI)
      #----------------------------------------------------------
      ###########################################################
      
      tabItem(
        tabName = "SIR",
        tabsetPanel(
          tabPanel(
            "Without Vital Dynamics",
            fluidRow(
              br(),
              box(
                title = "SIR Model in a Closed Population",
                solidHeader = T,
                status = "primary",
                width = 12,
                collapsible = F,
                fluidRow(
                  box(
                    width = 12,
                    status = "primary",
                    column(
                      width = 3,
                      sliderInput("Beta_SIR", "Transmission Rate Parameter (\u03B2)", 
                                  min = 0, max = 2, value = .5, step = .01)
                    ), # End column 
                    column(
                      width = 3,
                      sliderInput("alpha_SIR", "Recovery Rate Parameter (\u03B1)", 
                                  min = 0, max = 2, value = .2, step = .01)
                    ), # End column
                    column(
                      width = 3,
                      numericInput("popSize_SIR", "Population Size (N)",
                                   value = 100, width = "100%")
                    ), # End column
                    column(
                      width = 3,
                      numericInput("initialInfecteds_SIR", HTML("Initial Number of Infecteds (I<sub>(0)</sub>)"),
                                   value = 1, width = "100%")
                    ) # End column
                  ) # End box
                ), # End fluidRow
                fluidRow(
                  column(
                    width = 6,
                    br(),
                    br(),
                    box(
                      width = 12,
                      status = "primary",
                      withSpinner(htmlOutput("text_SIR"))
                    ) # End box
                  ), # End column
                  column(
                    width = 6,
                    withSpinner(plotlyOutput("SIR_model"))
                  ) # End column
                ) # End fluidRow
              ) # End box
            ) # End fluidRow
          ),# End tabPanel
          
          ###########################################################
          #----------------------------------------------------------
          # 1.1 SIR Model - with vital dynamics (UI)
          #----------------------------------------------------------
          ###########################################################
          
          tabPanel(
            "With Vital Dynamics",
            fluidRow(
              br(),
              box(
                title = "SIR Model in an Open Population",
                solidHeader = T,
                status = "primary",
                width = 12,
                collapsible = F,
                fluidRow(
                  box(
                    width = 12,
                    status = "primary",
                    fluidRow(
                      column(
                        width = 3,
                        sliderInput("Beta_SIR.vd", "Transmission Rate Parameter (\u03B2)", 
                                    min = 0, max = 2, value = .5, step = .01)
                      ), # End column
                      column(
                        width = 3,
                        sliderInput("alpha_SIR.vd", "Recovery Rate Parameter (\u03B1)", 
                                    min = 0, max = 2, value = .2, step = .01)
                      ), # End column
                      column(
                        width = 2,
                        numericInput("Mu_SIR.vd", "Birth and Death Rate Parameter (\u03BC)", 
                                     value = 0.02, width = "100%")
                      ), # End column
                      column(
                        width = 2,
                        numericInput("popSize_SIR.vd", "Initial Population Size (N)",
                                     value = 100, width = "100%")
                      ), # End column
                      column(
                        width = 2,
                        numericInput("initialInfecteds_SIR.vd", HTML("Initial Number of Infecteds (I<sub>(0)</sub>)"),
                                     value = 1, width = "100%")
                      ) # End column
                    ) # End fluidRow
                  ) # End Box
                ), # End fluidRow
                fluidRow(
                  column(
                    width = 6,
                    br(),
                    br(),
                    box(
                      status = "primary",
                      width = 12,
                      collapsible = F,
                      withSpinner(htmlOutput("text_SIR.vd"))
                    ) # End box
                  ), # End column
                  column(
                    width = 6,
                    withSpinner(plotlyOutput("SIR.vd_model"))
                  ) # End column
                ) # End fluidRow
              ) # End box
            ) # End fluidRow
          ), # End tabPanel
          
          ###########################################################
          #----------------------------------------------------------
          # 1.2 SIR Model - with heterogeneity susceptibility (UI)
          #----------------------------------------------------------
          ###########################################################
          
          tabPanel(
            "With Heterogeneity in Susceptibility",
            fluidRow(
              br(),
              box(
                title =  "SIR Model With Heterogeneity in Susceptibility",
                solidHeader = T,
                status = "primary",
                width = 12,
                collapsible = F,
                fluidRow(
                  column(
                    width = 4,
                    sliderInput("alpha_SIR.s", "Recovery Rate Parameter (\u03B1)", 
                                min = 0, max = 2, value = .2, step = .01)
                  ), # End column
                  column(
                    width = 4,
                    sliderInput("fracLsus.s", HTML("Fraction of Individuals With Low Susceptibility (p<sub>L</sub>)"),
                                min = 0, max = 1, value = .60, step = 0.05)
                  ), # End column
                  column(
                    width = 4,
                    numericInput("popSize_SIR.s", "Population Size (N)",
                                 value = 100, width = "100%")
                  ) # End column
                ), # End fluidRow
                fluidRow(
                  column(
                    width = 4,
                    sliderInput("contactRate_SIR.s", "Contact Rate (c)",
                                min = 0, max = 10, value = 3, step = 1)
                  ), # End column
                  column(
                    width = 4,
                    sliderInput("SusLvalue.s", HTML("Susceptibility Value For Individuals With Low Susceptibility (g<sub>L</sub>)"),
                                min = 0, max = 1, value = .25, step = .05)
                  ), # End column
                  column(
                    width = 4,
                    numericInput("initialInfecteds_SIR.s", HTML("Initial Number of Infecteds (I<sub>(0)</sub>)"),
                                 value = 1, width = "100%")
                  ) # End column
                ), # End fluidRow
                fluidRow(
                  column(
                    width = 6,
                    br(),
                    br(),
                    box(
                      status = "primary",
                      width = 12,
                      withSpinner(htmlOutput("text_SIR.s"))
                    ) # End box
                  ), # End column
                  column(
                    width = 6,
                    withSpinner(plotlyOutput("SIR_model.s"))
                  ) # End column
                ) # End fluidRow
              ) # End box
            ) # End fluidRow
          ), # End tabPanel
          
          ###########################################################################
          #--------------------------------------------------------------------------
          # 1.3 SIR Model - with heterogeneity in susceptibility and infectivity (UI)
          #--------------------------------------------------------------------------
          ###########################################################################
          
          tabPanel(
            "With Heterogeneity in Susceptibility and Infectivity",
            fluidRow(
              br(),
              box(
                title =  "SIR Model With Heterogeneity in Susceptibility and Infectivity",
                solidHeader = T,
                status = "primary",
                width = 12,
                collapsible = F,
                fluidRow(
                  column(
                    width = 4,
                    sliderInput("alpha_SIR.si", "Recovery Rate Parameter (\u03B1)", 
                                min = 0, max = 2, value = .2, step = .01)
                  ), # End column
                  column(
                    width = 4,
                    sliderInput("contactRate_SIR.si", "Contact Rate (c)",
                                min = 0, max = 10, value = 3, step = 1)
                  ), # End column
                  column(
                    width = 4,
                    numericInput("popSize_SIR.si", "Initial Population Size (N)",
                                 value = 100, width = "100%")
                  ) # End column
                ), # End fluidRow
                fluidRow(
                  column(
                    width = 4,
                    sliderInput("fracLsus.si", HTML("Fraction of Individuals With Low Susceptibility (p<sub>L</sub>)"),
                                min = 0, max = 1, value = .6, step = .05)
                  ), # End column
                  column(
                    width = 4,
                    sliderInput("SusLvalue.si", HTML("Susceptibility Value For Individuals With Low Susceptibility (g<sub>L</sub>)"),
                                min = 0, max = 1, value = .25, step = .05)
                  ), # End column
                  column(
                    width = 4,
                    numericInput("initialLinfecteds.si", HTML("Initial Number of Individuals With Low Infectivity (I<sub>L(0)</sub>)"),
                                 value = 1, width = "100%")
                  ) # End column
                ), # End fluidRow
                fluidRow(
                  column(
                    width = 4,
                    sliderInput("fracLinf.si", HTML("Fraction of Individuals With Low Infectivity (q<sub>L</sub>)"),
                                min = 0, max = 1, value = .6, step = .05)
                  ), # End column
                  column(
                    width = 4,
                    sliderInput("InfLvalue.si", HTML("Infectivity Value For Individuals With Low Infectivity (f<sub>L</sub>)"),
                                min = 0, max = 1, value = .25, step = .05)
                  ), # End column
                  column(
                    width = 4,
                    numericInput("initialHinfecteds.si", HTML("Initial Number of Individuals With High Infectivity (I<sub>H(0)</sub>)"),
                                 value = 1, width = "100%")
                  ) # End column
                ), # End fluidRow
                fluidRow(
                  column(
                    width = 6,
                    br(),
                    br(),
                    box(
                      status = "primary",
                      width = 12,
                      withSpinner(htmlOutput("text_SIR.si"))
                    ) # End box
                  ), # End column
                  column(
                    width = 6,
                    withSpinner(plotlyOutput("SIR_model.si"))
                  ) # End column
                ) # End fluidRow
              ) # End Box
            ) # End fluidRow
          ) # End tabPanel
        )  # End tabsetPanel
      ), # End tabItem
      
      ###########################################################
      #----------------------------------------------------------
      # 2.0 SIS Model (UI)
      #----------------------------------------------------------
      ###########################################################
      
      tabItem(
        tabName = "SIS",
        fluidRow(
          br(),
          box(
            title =  "SIS Model in a Closed Population",
            solidHeader = T,
            status = "primary",
            width = 12,
            collapsible = F,
            fluidRow(
              column(
                width = 3,
                sliderInput("Beta_SIS", "Transmission Rate Parameter (\u03B2)", 
                            min = 0, max = 2, value = .5, step = .01)
              ), # End column
              column(
                width = 3,
                sliderInput("alpha_SIS", "Recovery Rate Parameter (\u03B1)", 
                            min = 0, max = 2, value = .2, step = .01)
              ), # End column
              column(
                width = 3,
                numericInput("popSize_SIS", "Population Size (N)",
                             value = 100, width = "100%")
              ), # End column
              column(
                width = 3,
                numericInput("initialInfecteds_SIS", HTML("Initial Number of Infecteds (I<sub>(0)</sub>)"),
                             value = 1, width = "100%")
              ) # End column
            ), # End fluidRow
            fluidRow(
              column(
                width = 6,
                br(),
                br(),
                box(
                  status = "primary",
                  width = 12,
                  withSpinner(htmlOutput("text_SIS"))
                ) # End box
              ), # End column
              column(
                width = 6,
                withSpinner(plotlyOutput("SIS_model"))
              ) # End column
            ) # End fluidRow
          ) # End box
        ) # End fluidRow
      ), # End tabItem
      
      ###########################################################
      #----------------------------------------------------------
      # 3. SEIR Model (UI)
      #----------------------------------------------------------
      ###########################################################
      
      tabItem(
        tabName = "SEIR",
        fluidRow(
          br(),
          box(
            title =  "SEIR Model in a Closed Population",
            solidHeader = T,
            status = "primary",
            width = 12,
            collapsible = F,
            fluidRow(
              column(
                width = 2,
                sliderInput("Beta_SEIR", "Transmission Rate Parameter (\u03B2)", 
                            min = 0, max = 2, value = .5, step = .01)
              ), # End column
              column(
                width = 2,
                sliderInput("alpha_SEIR", "Recovery Rate Parameter (\u03B1)", 
                            min = 0, max = 2, value = .2, step = .01)
              ), # End column
              column(
                width = 2,
                sliderInput("Gamma_SEIR", "Latent Period (days)", 
                            min = 0, max = 100, value = 10, step = 1)
              ), # End column
              column(
                width = 3,
                numericInput("popSize_SEIR", "Population Size (N)",
                             value = 100, width = "100%")
              ), # End column
              column(
                width = 3,
                numericInput("initialInfecteds_SEIR", HTML("Initial Number of Infecteds (I<sub>(0)</sub>)"),
                             value = 1, width = "100%")
              ) # End column
            ), # End fluidRow
            fluidRow(
              column(
                width = 6,
                br(),
                br(),
                box(
                  status = "primary",
                  width = 12,
                  withSpinner(htmlOutput("text_SEIR"))
                ) # End box
              ), # End column
              column(
                width = 6,
                withSpinner(plotlyOutput("SEIR_model"))
              ) # End column
            ) # End fluidRow
          ) # End box
        ) # End fluidRow
      ), # End tabItem
      
      ###########################################################
      #----------------------------------------------------------
      # 4.0 SIR Model - Stochastic (UI)
      #----------------------------------------------------------
      ###########################################################
      
      tabItem(
        tabName = "stochastic",
        fluidRow(
          br(),
          box(
            title =  "Stochastic SIR Model in a Closed Population",
            solidHeader = T,
            status = "primary",
            width = 12,
            collapsible = F,
            fluidRow(
              column(
                width = 4,
                sliderInput("timestep_stochastic", "Timestep", 
                            min = 0, max = 1, value = .1, step = .01)
              ), # End column
              column(
                width = 4,
                sliderInput("Beta_stochastic", "Transmission Rate Parameter (\u03B2)", 
                            min = 0, max = 2, value = .5, step = .01)
              ), # End column
              column(
                width = 4,
                sliderInput("alpha_stochastic", "Recovery Rate Parameter (\u03B1)", 
                            min = 0, max = 2, value = .2, step = .01)
              ) # End column
            ), # End fluidRow
            fluidRow(
              column(
                width = 4,
                numericInput("Nsims_stochastic", "Number of Simulations to Run",
                             value = 10, width = "100%")
              ), # End column
              column(
                width = 4,
                numericInput("popSize_stochastic", "Population Size (N)",
                             value = 100, width = "100%")
              ), # End column
              column(
                width = 4,
                numericInput("initialInfecteds_stochastic", HTML("Initial Number of Infecteds (I<sub>(0)</sub>)"),
                             value = 1, width = "100%")
              ) # End column
            ), # End fluidRow
            fluidRow(
              br(),
              box(
                status = "primary",
                width = 12,
                withSpinner(htmlOutput("text_stochastic"))
              ) # End box
            ), # End fluidRow
            fluidRow(
              column(
                width = 4,
                htmlOutput("stochastic_model_text"),
                withSpinner(plotlyOutput("stochastic_model"))
              ), # End column
              column(
                width = 4,
                htmlOutput("stochastic_model__average_text"),
                withSpinner(plotlyOutput("stochastic_model_average"))
              ), # End column
              column(
                width = 4,
                htmlOutput("deterministic_model_text"),
                withSpinner(plotlyOutput("deterministic_model"))
                # plotlyOutput("stochastic_model")
              ) # End column
            ) # End fluidRow
          ) # End box
        ) # End fluidRow
      ), # End tabItem
      
      ###########################################################
      #----------------------------------------------------------
      # 5.0 Glossary (UI)
      #----------------------------------------------------------
      ###########################################################
      
      tabItem(
        tabName = "vocab",
         fluidRow(
        valueBox("S", "Number of susceptible individuals", color = "navy"), 
        valueBox("I", "Number of infected individuals", color = "blue"),
        valueBox("E", "Number of exposed individuals", color = "navy")
        ), # End fluidRow
        fluidRow(
        valueBox("R", "Number of recovered individuals", color = "blue"),
        valueBox("N", "Total population size", color = "navy"),
        valueBox("s", "Proportion susceptible", color = "blue")
        ), # End fluidRow
        fluidRow(
        valueBox(HTML("R<sub>0</sub>"), "Basic reproduction ratio", color = "navy"),
        valueBox(HTML("R<sub>e</sub>"), "Effective reproduction ratio", color = "blue"),
        valueBox("\u03B2", "Transmission rate parameter", color = "navy")
        ), # End fluidRow
        fluidRow(
        valueBox("\u03B1", "Recovery rate parameter", color = "blue"),
        valueBox("\u03BC", "Birth/death rate parameter for a stable population", color = "navy"),
        valueBox("\u03C3", "Rate at which individuals move from the exposed to infectious state (i.e. 1/latent period)", color = "blue")
        ), # End fluidRow
        fluidRow(
        valueBox("c", "Contact rate", color = "navy"),
        valueBox(HTML("p<sub>i</sub>"), "Fraction of individuals with susceptibility of type i (where i = Low or High susceptibility)", color = "blue"),
        valueBox(HTML("q<sub>i</sub>"), "Fraction of individuals with infectivity of type i (where i = Low or High infectivity)", color = "navy")
        ), # End fluidRow
        fluidRow(
        valueBox(HTML("g<sub>i</sub>"), "Susceptibility value for individuals with susceptibility of type i (where i = Low or High susceptibility)", color = "blue"),
        valueBox(HTML("f<sub>i</sub>"), "Infectivity value for individuals with infectivity of type i (where i = Low or High infectivity)", color = "navy")
        ) # End fluidRow
      ) # End tab item
    ) # End tabItems
  ) # End dashboardBody
) # End dashboardPage

###########################################################
###########################################################
#----------------------------------------------------------
#----------------------------------------------------------
# Server
#----------------------------------------------------------
#----------------------------------------------------------
###########################################################
###########################################################

server <- function(input, output){ 
  
  ###########################################################
  #----------------------------------------------------------
  # 1.0 SIR Model - without vital dynamics (Server)
  #----------------------------------------------------------
  ###########################################################
  
  output$SIR_model <- renderPlotly({
    
    # set up initial populations
    pop.size <- input$popSize_SIR
    initial.infecteds <- input$initialInfecteds_SIR
    initial.susceptibles <- pop.size - initial.infecteds
    initial.recovereds <- 0
    
    # transmission and recovery rates
    beta <- input$Beta_SIR
    alpha <- input$alpha_SIR
    
    # simulation times
    start.time <-0
    end.time <- 100
    timestep <- 0.1
    
    # set up the initial population sizes and starting time
    populations <- data.frame(Time= start.time,
                              Susceptibles = initial.susceptibles,
                              Infecteds = initial.infecteds,
                              Recovereds = initial.recovereds)
    current.time <- tail(populations$Time, 1)
    
    # run the simulation
    while (current.time < end.time){
      populations <- SIR(populations, beta, alpha, timestep)
      current.time <- tail(populations$Time, 1)}
    
    plot.populations.SIR(populations)
    
  })
  
  output$text_SIR <- renderText({
    HTML("<h4>SIR Ordinary Differential Equations (ODEs) for a closed population</h4>
    <h5>dS/dt = -\u03B2SI/N</h5>
         <h5>dI/dt = \u03B2SI/N -\u03B1I</h5>
         <h5>dR/dt = \u03B1I</h5>
         <br>
         <h4>The Basic Reproduction Ratio (R<sub>0</sub>)</h4>
   <h5> R<sub>0</sub> is a function of the transmission rate parameter (\u03B2) and
          the recovery rate parameter (\u03B1) </h5>
          <h5>Check out how R<sub>0</sub> changes as you modify \u03B2 & \u03B1</h5>
         <h5>R<sub>0</sub> =  \u03B2 / \u03B1 = ", 
         input$Beta_SIR / input$alpha_SIR)
  })
  
  ###########################################################
  #----------------------------------------------------------
  # 1.1 SIR Model - with vital dynamics (Server)
  #----------------------------------------------------------
  ###########################################################
  
  output$SIR.vd_model <- renderPlotly({
    
    # set up initial populations
    pop.size <- input$popSize_SIR.vd
    initial.infecteds <- input$initialInfecteds_SIR.vd
    initial.susceptibles <- pop.size - initial.infecteds
    initial.recovereds <- 0
    
    # transmission, recovery and vital rates
    beta <- input$Beta_SIR.vd
    alpha <- input$alpha_SIR.vd
    mu <- input$Mu_SIR.vd
    
    # simulation times
    start.time <-0
    end.time <- 200
    timestep <- 0.1
    
    # set up the initial population sizes and starting time
    populations <- data.frame(Time= start.time,
                              Susceptibles = initial.susceptibles,
                              Infecteds = initial.infecteds,
                              Recovereds = initial.recovereds)
    current.time <- tail(populations$Time, 1)
    
    # run the simulation
    while (current.time < end.time){
      populations <- SIR.vd(populations, beta, alpha, mu, timestep)
      current.time <- tail(populations$Time, 1)}
    plot.populations.SIR.vd(populations)
  })
  
  output$text_SIR.vd <- renderText({
    HTML("<h4>SIR Ordinary Differential Equations (ODEs) for an open population</h4>
    <h5>dS/dt = \u03BCN -\u03B2SI/N -\u03BCS</h5>
         <h5>dI/dt = \u03B2SI/N -\u03B1I -\u03BCI</h5>
         <h5>dR/dt = \u03B1I -\u03BCR</h5>
         <h6>Where \u03BC is the birth and death rate parameter for a stable population </h6>
         <br>
         <h4>The Basic Reproduction Ratio (R<sub>0</sub>) & The Effective Reproduction Ratio (R<sub>e</sub>)</h4>
   <h5>R<sub>0</sub> =  \u03B2 / \u03B1+\u03BC = ", 
         round(input$Beta_SIR.vd / (input$alpha_SIR.vd + input$Mu_SIR.vd), digits = 3), "</h5>",
         "<h5>At the endemic equilibrium the proportion susceptible, (s),
          is equal to 1/R<sub>0</sub> =  ", 
         1/(input$Beta_SIR.vd / (input$alpha_SIR.vd + input$Mu_SIR.vd)), "</h5>",
         "<h5>And R<sub>e</sub> = R<sub>0</sub>*s = ",  round(input$Beta_SIR.vd / (input$alpha_SIR.vd + input$Mu_SIR.vd), digits  = 3 # R0
         ), "*", round((1/(input$Beta_SIR.vd / (input$alpha_SIR.vd + input$Mu_SIR.vd))), digits = 3), "= ",
         (input$Beta_SIR.vd / (input$alpha_SIR.vd + input$Mu_SIR.vd) # R0
         )*(1/(input$Beta_SIR.vd / (input$alpha_SIR.vd + input$Mu_SIR.vd))), "</h5>"
    )
  })
  
  #############################################################
  #------------------------------------------------------------
  # 1.2 SIR Model - with heterogeneity susceptibility (Server)
  #------------------------------------------------------------
  #############################################################
  
  output$SIR_model.s <- renderPlotly({
    
    # set up initial populations
    pop.size <- input$popSize_SIR.s
    initial.infecteds <- input$initialInfecteds_SIR.s
    initial.susceptibles.L <- round((pop.size - initial.infecteds) * input$fracLsus.s)
    initial.susceptibles.H <- round((pop.size - initial.infecteds) * (1-input$fracLsus.s))
    initial.recovereds <- 0
    
    # transmission and recovery rates
    beta.L <- input$contactRate_SIR.s * input$SusLvalue.s # c x g
    beta.H <- input$contactRate_SIR.s                # c x g (g = 1 for high susceptibility)
    alpha <- input$alpha_SIR.s
    
    # simulation times
    start.time <-0
    end.time <- 100
    timestep <- 0.1
    
    # set up the initial population sizes and starting time
    populations <- data.frame(Time= start.time,
                              Susceptibles.L = initial.susceptibles.L,
                              Susceptibles.H = initial.susceptibles.H,
                              Infecteds = initial.infecteds,
                              Recovereds = initial.recovereds)
    current.time <- tail(populations$Time, 1)
    # run the simulation
    while (current.time < end.time){
      populations <- SIR.s(populations, beta.L, beta.H, alpha, timestep)
      current.time <- tail(populations$Time, 1)}
    plot.populations.SIR.s(populations)
  })
  
  output$text_SIR.s <- renderText({
    HTML("<h4>SIR Ordinary Differential Equations (ODEs) with heterogeneity in susceptibility</h4>
    <h5>dS<sub>L</sub>/dt = -\u03B2<sub>L</sub>S<sub>L</sub>I/N</h5>
    <h5>dS<sub>H</sub>/dt = -\u03B2<sub>H</sub>S<sub>H</sub>I/N</h5>
         <h5>dI/dt = \u03B2<sub>L</sub>S<sub>L</sub>I/N + \u03B2<sub>H</sub>S<sub>H</sub>I/N -\u03B1I</h5>
         <h5>dR/dt = \u03B1I</h5>
         <h6>Where L is low susceptibility and H is high susceptibility</h6>
         <br>
         <h4>The Effective Reproduction Ratio (R<sub>e</sub>)</h4>
   <h5>R<sub>e</sub> = p<sub>L</sub>\u03B2<sub>L</sub>+p<sub>H</sub>\u03B2<sub>H</sub> / \u03B1 = ",
         ((input$fracLsus.s * (input$contactRate_SIR.s * input$SusLvalue.s)) +
            (((1 - input$fracLsus.s) * input$contactRate_SIR.s) / input$alpha_SIR.s)), "</h5>
         <h6>Where \u03B2<sub>i</sub> = c x g<sub>i</sub></h6>")
  })
  
  ##############################################################################
  #-----------------------------------------------------------------------------
  # 1.3 SIR Model - with heterogeneity in susceptibility and infectivity (Server)
  #-----------------------------------------------------------------------------
  ##############################################################################
  
  output$SIR_model.si <- renderPlotly({
    
    # set up initial populations
    pop.size <- input$popSize_SIR.si
    initial.infecteds.L <- input$initialLinfecteds.si
    initial.infecteds.H <- input$initialHinfecteds.si
    initial.susceptibles.L <- round((pop.size - initial.infecteds.L - initial.infecteds.H) * input$fracLsus.si)
    initial.susceptibles.H <- pop.size - initial.infecteds.L - initial.infecteds.H - initial.susceptibles.L
    initial.recovereds <- 0
    
    # proportions with low/high infectivitity
    qL <- input$fracLinf.si
    qH <- 1 - qL
    
    # susceptibility/infectivity values
    low.g <- input$SusLvalue.si  # low susceptibility
    high.g <- 1                  # high susceptibility
    low.f <- input$InfLvalue.si  # low infectivity
    high.f <- 1                  # high infectivity
    
    # transmission and recovery rates
    beta.LL <- input$contactRate_SIR.si * low.g * low.f
    beta.LH <- input$contactRate_SIR.si * low.g * high.f
    beta.HH <- input$contactRate_SIR.si * high.g * high.f
    beta.HL <- input$contactRate_SIR.si * high.g * low.f
    alpha <- input$alpha_SIR.si
    
    # simulation times
    start.time <-0
    end.time <- 100
    timestep <- 0.1
    
    # set up the initial population sizes and starting time
    populations <- data.frame(Time= start.time,
                              Susceptibles.L = initial.susceptibles.L,
                              Susceptibles.H = initial.susceptibles.H,
                              Infecteds.L = initial.infecteds.L,
                              Infecteds.H = initial.infecteds.H,
                              Recovereds = initial.recovereds)
    current.time <- tail(populations$Time, 1)
    
    # run the simulation
    while (current.time < end.time){
      populations <- SIR.si(populations, beta.LL, beta.HH,
                            beta.LH, beta.HL, qL, qH, alpha, timestep)
      current.time <- tail(populations$Time, 1)}
    
    plot.populations.SIR.si(populations)
  })
  
  output$text_SIR.si <- renderText({
    HTML("<h4>SIR Ordinary Differential Equations (ODEs) with heterogeneity in susceptibility & infectivity</h4>
    <h5>dS<sub>L</sub>/dt = -(\u03B2<sub>LL</sub>S<sub>L</sub>I<sub>L</sub>/N + \u03B2<sub>LH</sub>S<sub>L</sub>I<sub>H</sub>/N)</h5>
    <h5>dS<sub>H</sub>/dt = -(\u03B2<sub>HH</sub>S<sub>H</sub>I<sub>H</sub>/N + \u03B2<sub>HL</sub>S<sub>H</sub>I<sub>L</sub>/N)</h5>
    <h5>dI<sub>L</sub>/dt = q<sub>L</sub>(\u03B2<sub>LL</sub>S<sub>L</sub>I<sub>L</sub>/N + \u03B2<sub>LH</sub>S<sub>L</sub>I<sub>H</sub>/N) +
         q<sub>L</sub>(\u03B2<sub>HH</sub>S<sub>H</sub>I<sub>H</sub>/N + \u03B2<sub>HL</sub>S<sub>H</sub>I<sub>L</sub>/N) - \u03B1I<sub>L</sub></h5>
    <h5>dI<sub>H</sub>/dt = q<sub>H</sub>(\u03B2<sub>LL</sub>S<sub>L</sub>I<sub>L</sub>/N + \u03B2<sub>LH</sub>S<sub>L</sub>I<sub>H</sub>/N) +
         q<sub>H</sub>(\u03B2<sub>HH</sub>S<sub>H</sub>I<sub>H</sub>/N + \u03B2<sub>HL</sub>S<sub>H</sub>I<sub>L</sub>/N) - \u03B1I<sub>H</sub></h5>
    <h5>dR/dt = \u03B1I<sub>L</sub> + \u03B1I<sub>H</sub></h5>
         <h6>Where L is low susceptibility/infectivity and H is high susceptibility/infectivity</h6>
         <br>
         <h4>The Effective Reproduction Ratio (R<sub>e</sub>)</h4>
         <h5>R<sub>e</sub> = c\u2211<sub>i</sub> p<sub>i</sub>g<sub>i</sub>
           (\u2211<sub>j</sub> q<sub>j</sub>f<sub>j</sub>) / \u03B1 = ",
         ((input$contactRate_SIR.si * input$fracLsus.si * input$SusLvalue.si * input$fracLinf.si * input$InfLvalue.si)+
            (input$contactRate_SIR.si * input$fracLsus.si * input$SusLvalue.si * (1-input$fracLinf.si))+
            (input$contactRate_SIR.si * (1-input$fracLsus.si) * input$fracLinf.si * input$InfLvalue.si)+
            (input$contactRate_SIR.si * (1-input$fracLsus.si) * (1-input$fracLinf.si))) / input$alpha_SIR.si, "</h5>")
  })
  
  
  ###########################################################
  #----------------------------------------------------------
  # 2.0 SIS Model (Server)
  #----------------------------------------------------------
  ###########################################################
  
  output$SIS_model <- renderPlotly({
    
    # set up initial populations
    pop.size <- input$popSize_SIS
    initial.infecteds <- input$initialInfecteds_SIS
    initial.susceptibles <- pop.size - initial.infecteds
    
    # transmission and recovery rates
    beta <- input$Beta_SIS
    alpha <- input$alpha_SIS
    
    # simulation times
    start.time <-0
    end.time <- 100
    timestep <- 0.1
    
    # set up the initial population sizes and starting time
    populations <- data.frame(Time= start.time,
                              Susceptibles = initial.susceptibles,
                              Infecteds = initial.infecteds)
    current.time <- tail(populations$Time, 1)
    
    # run the simulation
    while (current.time < end.time){
      populations <- SIS(populations, beta, alpha, timestep)
      current.time <- tail(populations$Time, 1)}
    
    plot.populations.SIS(populations)
  })
  
  output$text_SIS <- renderText({
    HTML("<h4>SIS Ordinary Differential Equations (ODEs) for a closed population</h4>
    <h5>dS/dt = -\u03B2SI/N + \u03B1I</h5>
         <h5>dI/dt = \u03B2SI/N -\u03B1I</h5>
         <br>
         <h4>The Basic Reproduction Ratio (R<sub>0</sub>)</h4>
   <h5>R<sub>0</sub> is a function of the transmission rate parameter (\u03B2) and
          the recovery rate parameter (\u03B1) </h5>
          <h5>Check out how R<sub>0</sub> changes as you modify \u03B2 & \u03B1</h5>
         <h5>R<sub>0</sub> =  \u03B2 / \u03B1 = ", 
         input$Beta_SIS / input$alpha_SIS)
  })
  
  ###########################################################
  #----------------------------------------------------------
  # 3.0 SEIR Model (Server)
  #----------------------------------------------------------
  ###########################################################
  
  output$SEIR_model <- renderPlotly({
    
    # set up initial populations
    pop.size <- input$popSize_SEIR
    initial.infecteds <- input$initialInfecteds_SEIR
    initial.susceptibles <- pop.size - initial.infecteds
    initial.exposeds <- 0
    initial.recovereds <- 0
    
    # transmission, recovery and exit rates
    beta <- input$Beta_SEIR
    alpha <- input$alpha_SEIR
    gamma <- 1/input$Gamma_SEIR
    
    # simulation times
    start.time <-0
    end.time <- 200
    timestep <- 0.1
    
    # set up the initial population sizes and starting time
    populations <- data.frame(Time= start.time,
                              Susceptibles = initial.susceptibles,
                              Exposeds = initial.exposeds,
                              Infecteds = initial.infecteds,
                              Recovereds = initial.recovereds)
    current.time <- tail(populations$Time, 1)
    
    # run the simulation
    while (current.time < end.time){
      populations <- SEIR(populations, beta, alpha, gamma, timestep)
      current.time <- tail(populations$Time, 1)}
    
    plot.populations.SEIR(populations)
  })
  
  output$text_SEIR <- renderText({
    HTML("<h4>SEIR Ordinary Differential Equations (ODEs) for a closed population</h4>
    <h5>dS/dt = -\u03B2SI/N</h5>
    <h5>dE/dt = \u03B2SI/N -\u03C3E</h5>
         <h5>dI/dt = \u03C3E -\u03B1I</h5>
         <h5>dR/dt = \u03B1I</h5>
         <h6>Where \u03C3 is the rate at which individuals move from the latent phase to the infectious phase (i.e. 1/latent period)</h6>
         <br>
         <h4>The Basic Reproduction Ratio (R<sub>0</sub>)</h4>
   <h5> R<sub>0</sub> is a function of the transmission rate parameter (\u03B2) and
          the recovery rate parameter (\u03B1) </h5>
          <h5>Check out how R<sub>0</sub> changes as you modify \u03B2 & \u03B1</h5>
         <h5>R<sub>0</sub> =  \u03B2 / \u03B1 = ", 
         input$Beta_SEIR / input$alpha_SEIR)
  })
  
  ###########################################################
  #----------------------------------------------------------
  # 4.0 SIR Model - Stochastic (Server)
  #----------------------------------------------------------
  ###########################################################
  
  output$stochastic_model <- renderPlotly({
    
    # number of stochastic simulations to run
    Nsims <- input$Nsims_stochastic
    
    all_pops <- c()
    for(loop in 1:Nsims){
      
      # set up initial populations
      pop.size <- input$popSize_stochastic
      initial.infecteds <- input$initialInfecteds_stochastic
      initial.susceptibles <- pop.size - initial.infecteds
      initial.recovereds <- 0
      
      # transmission and recovery rates
      beta <- input$Beta_stochastic
      alpha <- input$alpha_stochastic
      
      # simulation times
      start.time <- 0
      end.time <- 100
      timestep <- input$timestep_stochastic
      
      # set up the initial population sizes and starting time
      populations <- data.frame(Time= start.time,
                                Susceptibles = initial.susceptibles,
                                Infecteds = initial.infecteds,
                                Recovereds = initial.recovereds)
      
      
      current.time <- tail(populations$Time, 1)
      
      # Is the experiment over? ---- are there any infecteds left?
      is.finished <- tail(populations$Infecteds, 1)
      
      # run the simulation
      while (is.finished > 0 & current.time < end.time ){
        populations <- SIR.stochastic(populations, beta, alpha, timestep)
        current.time <- tail(populations$Time, 1)
        is.finished <- tail(populations$Infecteds, 1)
      }
      populations$Sim <- loop
      populations$Sim <- as.factor(populations$Sim)
      all_pops <- rbind(all_pops, populations)
    }
    
    plot.populations.SIR.stochastic(all_pops, Nsims)
  })

  output$stochastic_model_average <- renderPlotly({
    
    # number of simulations to run
    Nsims <- input$Nsims_stochastic
    
    all_pops <- c()
    
    for(loop in 1:Nsims){
      
      # set up initial populations
      pop.size <- input$popSize_stochastic
      initial.infecteds <- input$initialInfecteds_stochastic
      initial.susceptibles <- pop.size - initial.infecteds
      initial.recovereds <- 0
      
      # transmission and recovery rates
      beta <- input$Beta_stochastic
      alpha <- input$alpha_stochastic
      
      # simulation times
      start.time <- 0
      end.time <- 100
      timestep <- input$timestep_stochastic
      
      # set up the initial population sizes and starting time
      populations <- data.frame(Time= start.time,
                                Susceptibles = initial.susceptibles,
                                Infecteds = initial.infecteds,
                                Recovereds = initial.recovereds)
      
      current.time <- tail(populations$Time, 1)
      
      # Is the experiment over? --- if there are no infecteds left
      is.finished <- tail(populations$Infecteds, 1)
      
      # run the simulation
      while (is.finished > 0 & current.time < end.time ){
        populations <- SIR.stochastic(populations, beta, alpha, timestep)
        current.time <- tail(populations$Time, 1)
        is.finished <- tail(populations$Infecteds, 1)
      }
      
      if(tail(populations$Time, 1) < end.time){
        final_pop <- c()
        final_pop <- data.frame("Time" = seq((tail(populations$Time, 1) + timestep), end.time, timestep))
        final_pop$Susceptibles <- tail(populations$Susceptibles, 1)
        final_pop$Infecteds <- tail(populations$Infecteds, 1)
        final_pop$Recovereds <- tail(populations$Recovereds, 1)
        populations <- rbind(populations, final_pop)
      }
      
      if(loop == 1){
        
        all_pops <- populations
        
      }else{
        
        all_pops <- all_pops + populations
        
      }
    }
    
    average_populations <- all_pops / loop
    plot.populations.SIR.stochastic_average(average_populations)
  })
  
  output$deterministic_model <- renderPlotly({
    
    # set up initial populations
    pop.size <- input$popSize_stochastic
    initial.infecteds <- input$initialInfecteds_stochastic
    initial.susceptibles <- pop.size - initial.infecteds
    initial.recovereds <- 0
    
    # transmission and recovery rates
    beta <- input$Beta_stochastic
    alpha <- input$alpha_stochastic
    
    # simulation times
    start.time <-0
    end.time <- 100
    timestep <- input$timestep_stochastic
    
    # set up the initial population sizes and starting time
    populations <- data.frame(Time= start.time,
                              Susceptibles = initial.susceptibles,
                              Infecteds = initial.infecteds,
                              Recovereds = initial.recovereds)
    current.time <- tail(populations$Time, 1)
    
    # run the simulation
    while (current.time < end.time){
      populations <- SIR(populations, beta, alpha, timestep)
      current.time <- tail(populations$Time, 1)}
    
    plot.populations.SIR(populations)
  })
  
  output$text_stochastic <- renderText({
    HTML("<h5>Here we use the Binomial distribution to determine the number of individuals which leave a given compartment.
    The probability that an individual will move from <i>S</i> to <i>I</i> at time <i>t</i> is
    <i>p(S\u2192I),<sub>t</sub></i> = 1 - <i>e<sup>-\u03B2I<sub>t</sub>/N<sub>t</sub></sup></i> and
  the probability that an individual will move from <i>I</i> to <i>R</i> at time <i>t</i> is
  <i>p(I\u2192R),<sub>t</sub></i> = 1 - <i>e<sup>-\u03B1</sup></i>.</h5>
    <br>
    <h5><b>Fig. A)</b> shows the outcome of ", input$Nsims_stochastic," individually run stochastic simulations</h5>
         <h5><b>Fig. B)</b> shows the average outcome of ", input$Nsims_stochastic," stochastic simulations</h5>
         <h5><b>Fig. C)</b> shows the deterministic outcome for the same parameters </h5>")
  })
  
  output$stochastic_model_text <- renderText({
    HTML("<h5><b>A)</b> Stochastic SIR with ", input$Nsims_stochastic, " simulations </h5>" )
  })
  
  output$stochastic_model__average_text <- renderText({
    HTML("<h5><b>B)</b> Stochastic SIR averaged over ", input$Nsims_stochastic, " simulations </h5>")
  })
  
  output$deterministic_model_text <- renderText({
    HTML("<h5><b>C)</b> Deterministic SIR</h5>")
  })
  
} # End Server

shinyApp(ui, server)
