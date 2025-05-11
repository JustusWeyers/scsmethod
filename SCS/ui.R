#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  ### Title "Muskingum Application"
  
  shiny::fluidRow(
    shiny::titlePanel("SCS Method")
  ),
  
  
  ### Data input
  
  h2("Data input"),

  shiny::fluidRow(
    col_4(
      shinydashboard::box(
        title = "Design precipitation", width = 12,
        p(
          "Table 1: Fill-in table for share of precipitation duration and 
          amount."
        ),
        rhandsontable::rHandsontableOutput("ui_P")
      ),
      shinydashboard::box(
        title = "Soil classes", width = 12,
        p(
          "Table 2: Area of the corresponding soil classes [%]."
        ),
        rhandsontable::rHandsontableOutput("ui_soilclasses")
      )
    ),
    col_8(
      shinydashboard::box(
        title = "Landuse", width = 12,
        p("List 1: Landuse by SCS categories in ha."),
        col_12(
          shiny::uiOutput(outputId = "landuse_boxarray")
        ),
        col_6(
          shiny::uiOutput(outputId = "ui_select_landuse"),
        ),
        col_2(
          shiny::numericInput(
            inputId = "landuse_ha", 
            label = "Area [ha]", 
            value = 1, 
            min = 0, 
            width = "100%")
        ),
        col_2(
          tags$div(
            tags$label("Actions:", style="color: transparent"),
            tags$div(
              shiny::actionButton(
                inputId = "add_landuse", 
                label = "", 
                icon = shiny::icon("plus"), 
                width = "100%"
              ),
            )
          )
        ),
        col_2(
          tags$div(
            tags$label("Actions:", style="color: transparent"),
            tags$div(
              shiny::actionButton(
                inputId = "clear_landuse", 
                label = "", 
                icon = shiny::icon("trash"), 
                width = "100%"
              )
            )
          )
        )
      )
    )
  ),
  
  shiny::fluidRow(
    col_3(
      shiny::uiOutput("ui_dInput")
    ),
    col_3(
      shiny::uiOutput("ui_pInput")
    ),
    col_6(
      shinydashboard::box(
        title = "Actions", width = 12,
        shiny::actionButton(
          inputId = "clear_input", 
          label = "Clear input"
        ),
        shiny::actionButton(
          inputId = "demo_data", 
          label = "Use demo data"
        ),
        shiny::actionButton(
          inputId = "run", 
          label = "Run", 
          class = "btn-warning"
        )
      )
    )
  ),
  
  ### Calculations
  
  shiny::uiOutput("calculations")
  
)
