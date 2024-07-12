# Shiny app for predation population model
# 09/07/2024 AIMILIOS PASCHALIS KOUNIS
### Dependencies ----
library(shiny)
library(bslib)
source("Growth_model_Predation.R")
### Define UI ----
ui <- page_fluid(
  layout_columns( col_widths = c(12, 12, 4, 4, 4, 12, 12), 
                  row_heights = list("auto","190px","240px","240px","240px","240px","240px"),
        card(
          layout_columns( col_widths = c(12,3,3,3),
          plotOutput("pred_model"),
          numericInput("time", "Timesteps", 100, min = 1),
          numericInput("res", "Initial resources for prey", 120, min = 0),
          numericInput("rgr", "Primary resource regeneration", 200, min = 0)
          )
        ),
        
        card("Modifiers",
             layout_columns(col_widths = c(3,3,3,3),
          sliderInput("growth_mod1",
                      "Prey birth modifier",
                      min = 0, max = 1, value = 1),
          sliderInput("death_mod1",
                      "Prey mortality modifier",
                      min = 0, max = 1, value = 1),
          sliderInput("growth_mod2",
                      "Predator birth modifier",
                      min = 0, max = 1, value = 1),
          sliderInput("death_mod2",
                      "Predator mortality modifier",
                      min = 0, max = 1, value = 1)
             )
          ),
        
        card("Prey initial population",
             layout_columns(col_widths = c(4,4,4,4,4,4), row_heights = list("80px","80px"),
             numericInput("ini_prey0", "Age 0", 20, min = 0),
             numericInput("ini_prey1", "Age 1", 20, min = 0),
             numericInput("ini_prey2", "Age 2", 20, min = 0),
             numericInput("ini_prey3", "Age 3", 20, min = 0),
             numericInput("ini_prey4", "Age 4", 20, min = 0),
             numericInput("ini_prey5", "Age 5", 20, min = 0)
             )
             ),
        card("Predator initial population",
             layout_columns( col_widths = c(4,4,4,4,4,4), row_heights = list("80px","80px"),
             numericInput("ini_pred0", "Age 0", 5, min = 0),
             numericInput("ini_pred1", "Age 1", 5, min = 0),
             numericInput("ini_pred2", "Age 2", 5, min = 0),
             numericInput("ini_pred3", "Age 3", 5, min = 0),
             numericInput("ini_pred4", "Age 4", 5, min = 0),
             numericInput("ini_pred5", "Age 5", 5, min = 0)
             )
             ),
        
        card("Birth rates",
          layout_columns(
            numericInput("gr1", "Prey",  value = 2, step = 0.1, min = 0),
            numericInput("gr2", "Predator",  value = 1.3, step = 0.1, min = 0)
          )
        ),
        
        card("Prey mortality rates",
          layout_columns(
            sliderInput("d1.1", "Age 0", min = 0, max = 1, value = 0.6),
            sliderInput("d1.2", "Age 1", min = 0, max = 1, value = 0.4),
            sliderInput("d1.3", "Age 2", min = 0, max = 1, value = 0.3),
            sliderInput("d1.4", "Age 3", min = 0, max = 1, value = 0.3),
            sliderInput("d1.5", "Age 4", min = 0, max = 1, value = 0.4),
            sliderInput("d1.6", "Age 5", min = 0, max = 1, value = 0.6),
          )
        ),
        
        card("Predator mortality rates",
          layout_columns(
            sliderInput("d2.1", "Age 0", min = 0, max = 1, value = 0.5),
            sliderInput("d2.2", "Age 1", min = 0, max = 1, value = 0.3),
            sliderInput("d2.3", "Age 2", min = 0, max = 1, value = 0.1),
            sliderInput("d2.4", "Age 3", min = 0, max = 1, value = 0.1),
            sliderInput("d2.5", "Age 4", min = 0, max = 1, value = 0.3),
            sliderInput("d2.6", "Age 5", min = 0, max = 1, value = 0.5),
          )
        ),
        
        card("Predation avoidance rates",
          layout_columns(
            sliderInput("av1", "Age 0", min = 0, max = 1, value = 0.9, step = 0.1),
            sliderInput("av2", "Age 1", min = 0, max = 1, value = 0.4, step = 0.1),
            sliderInput("av3", "Age 2", min = 0, max = 1, value = 0.2, step = 0.1),
            sliderInput("av4", "Age 3", min = 0, max = 1, value = 0.1, step = 0.1),
            sliderInput("av5", "Age 4", min = 0, max = 1, value = 0.2, step = 0.1),
            sliderInput("av6", "Age 5", min = 0, max = 1, value = 0.4, step = 0.1),
          )
        ),
        
        card("Predation efficiency rates",
          layout_columns(
            sliderInput("ef1", "Age 0", min = 0, max = 2, value = 0.1, step = 0.1),
            sliderInput("ef2", "Age 1", min = 0, max = 2, value = 0.2, step = 0.1),
            sliderInput("ef3", "Age 2", min = 0, max = 2, value = 0.8, step = 0.1),
            sliderInput("ef4", "Age 3", min = 0, max = 2, value = 1.3, step = 0.1),
            sliderInput("ef5", "Age 4", min = 0, max = 2, value = 1.4, step = 0.1),
            sliderInput("ef6", "Age 5", min = 0, max = 2, value = 0.1, step = 0.1),
          )
        )
  )
)

### Define server logic ----
server <- function(input,output){
  output$pred_model <- renderPlot({
    arg <- list("a" = c(input$ini_prey0,
                        input$ini_prey1,
                        input$ini_prey2,
                        input$ini_prey3,
                        input$ini_prey4,
                        input$ini_prey5),
                "b" = c(input$ini_pred0,
                        input$ini_pred1,
                        input$ini_pred2,
                        input$ini_pred3,
                        input$ini_pred4,
                        input$ini_pred5),
                "t" =  input$time, "rgr" = input$rgr, "res_prey" = input$res,
                "dmod1" = input$death_mod1, "dmod2" = input$death_mod2, 
                "gmod1" = input$growth_mod1, "gmod2" = input$growth_mod2,
                "gr1" = input$gr1, "gr2" = input$gr2,
                "d1" = c(input$d1.1,
                         input$d1.2,
                         input$d1.3,
                         input$d1.4,
                         input$d1.5,
                         input$d1.6),
                "d2" = c(input$d2.1,
                         input$d2.2,
                         input$d2.3,
                         input$d2.4,
                         input$d2.5,
                         input$d2.6),
                "ef" = c(input$ef1,
                         input$ef2,
                         input$ef3,
                         input$ef4,
                         input$ef5,
                         input$ef6),
                "av" = c(input$av1,
                         input$av2,
                         input$av3,
                         input$av4,
                         input$av5,
                         input$av6))
        do.call(pred_growth_function, arg)})
  }
### Call shinyApp----
shinyApp(ui = ui ,server = server)
