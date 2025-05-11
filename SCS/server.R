#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  ### Reactive values
  
  # Input values
  in_vals = shiny::reactiveValues(
    P = demo_P,
    soilclasses = demo_soilclasses,
    precipitation = 95,
    duration = 10,
    landuse_class = demo_landuse,
    landuse_area = demo_area
  )
  
  # Operating values
  vals = shiny::reactiveValues()
  
  ### Serverlogic: Input
  
  # Update in_vals precipitation data.frame
  shiny::observe({
    if (!is.null(input$ui_P)) {
      in_vals[["P"]] = rhandsontable::hot_to_r(input$ui_P)
    }
  })
  
  # Update in_vals soilclasses data.frame
  shiny::observe({
    if (!is.null(input$ui_soilclasses)) {
      in_vals[["soilclasses"]] = rhandsontable::hot_to_r(input$ui_soilclasses)
    }
  })
  
  # Load demodata
  shiny::observeEvent(input$demo_data, {
    in_vals$P = demo_P
    in_vals$soilclasses = demo_soilclasses
    in_vals$precipitation = 95
    in_vals$duration = 10
    in_vals$landuse_class = demo_landuse
    in_vals$landuse_area = demo_area
  })
  
  # Append a land use class and area
  shiny::observeEvent(input$add_landuse, {
    if (!(input$landuse %in% unlist(in_vals$landuse_class))) {
      in_vals$landuse_class = append(in_vals$landuse_class, input$landuse)
      in_vals$landuse_area = append(in_vals$landuse_area, input$landuse_ha)
    }
  })
  
  # Clear landuse list
  shiny::observeEvent(input$clear_landuse, {
    in_vals$landuse_class = c()
    in_vals$landuse_area = c()
  })

  # Clear input  
  shiny::observeEvent(input$clear_input, {
    in_vals$P = empty_df(r = 10, c = 2, cn = c("t", "P"))
    in_vals$soilclasses = empty_df(r = 4, c = 1, cn = c("area"))
    row.names(in_vals$soilclasses) = c("A", "B", "C", "D")
    in_vals$precipitation = 0
    in_vals$duration = 0
    in_vals$landuse_class = c()
    in_vals$landuse_area = c()
    
    vals$P = NULL
    vals$soil_classes = NULL
    soilA = NULL
    soilB = NULL
    soilC = NULL
    soilD = NULL
  })
  
  ### Serverlogic: Main functionality
  
  shiny::observeEvent(input$run, {
    if (TRUE) {
      vals$P = in_vals$P
      vals$soilclasses = in_vals$soilclasses
      
      vals$soilA = calculate_CurveNumber(
        sclass        = "A",
        pp            = in_vals$soilclasses[1,1],
        lu_class      = in_vals$landuse_class,
        lu            = in_vals$landuse_area,
        SCS_CN_values = scs_classification
      )
      
      vals$soilB = calculate_CurveNumber(
        sclass        = "B",
        pp            = in_vals$soilclasses[2,1],
        lu_class      = in_vals$landuse_class,
        lu            = in_vals$landuse_area,
        SCS_CN_values = scs_classification
      )
      
      vals$soilC = calculate_CurveNumber(
        sclass        = "C",
        pp            = in_vals$soilclasses[3,1],
        lu_class      = in_vals$landuse_class,
        lu            = in_vals$landuse_area,
        SCS_CN_values = scs_classification
      )
      
      vals$soilD = calculate_CurveNumber(
        sclass        = "D",
        pp            = in_vals$soilclasses[4,1],
        lu_class      = in_vals$landuse_class,
        lu            = in_vals$landuse_area,
        SCS_CN_values = scs_classification
      )
      
    }
  })
  
  dt = shiny::reactive(
    # Delta t in seconds
    unique(diff(vals$P$t)) * 3600
  )
  
  ### Output
  
  output$landuse_boxarray = shiny::renderUI({
    if (length(in_vals$landuse_class)> 0) {
      lapply(
        X = 1:length(in_vals$landuse_class), 
        FUN = arraybox, 
        class = in_vals$landuse_class, 
        area = in_vals$landuse_area
      )
    }
  })
  
  output$ui_P = rhandsontable::renderRHandsontable(
    rhandsontable::rhandsontable(
      data = in_vals[["P"]], 
      useTypes = TRUE, 
      stretchH = "all"
    )
  )
  
  output$ui_soilclasses = rhandsontable::renderRHandsontable(
    rhandsontable::rhandsontable(
      data = in_vals[["soilclasses"]], 
      useTypes = TRUE, 
      stretchH = "all",
    ) |>
    rhandsontable::hot_context_menu(
      allowRowEdit = FALSE, 
      allowColEdit = FALSE
    )
  )
  
  output$ui_dInput = shiny::renderUI(
    shinydashboard::box(
      solidHeader = FALSE, width = 12, title = "Duration [h]",
      shiny::numericInput(
        inputId = "p_duration", 
        label = NULL, 
        value = in_vals$duration, 
        min = 0
      )
    )
  )
  
  output$ui_pInput = shiny::renderUI(
    shinydashboard::box(
      solidHeader = FALSE, width = 12, title = "Precipitation [mm]",
      shiny::numericInput(
        inputId = "p", 
        label = NULL, 
        value = in_vals$precipitation, 
        min = 0
      )
    )
  )
  
  output$ui_select_landuse = shiny::renderUI(
    shiny::fluidRow(
      tags$style(
        type='text/css', 
        ".selectize-input { font-size: 10px; line-height: 20px;} 
        .selectize-dropdown { font-size: 10px; line-height: 10px; }"
      ),
      shiny::selectInput(
        inputId = "landuse",
        label = "Select landuse",
        choices = trimws(scs_classification$use),
        selected = FALSE,
        width = "100%"
      )
    )
  )
  
  output$soiltypeA_table = shiny::renderUI(
    if (!is.null(vals$soilA)) {
      shiny::tagList(
        p(
          "Table 3: Curve Number (CN) for soil class A. (A_LU: Area of landuse 
          class in ha, A: Area of soiltype A for related landuse in percent). 
          The CN is equal to the sum of 'A*CN'-column. All calculations have 
          been performed without rounded values."
        ),
        shiny::renderTable(
          sumrow(vals$soilA)
        )
      )
    }
  )
  
  output$soiltypeB_table = shiny::renderUI(
    if (!is.null(vals$soilB)) {
      shiny::tagList(
        p(
          "Table 4: Curve Number (CN) for soil class B. (A_LU: Area of landuse 
          class in ha, A: Area of soiltype B for related landuse in percent). 
          The CN is equal to the sum of 'A*CN'-column. All calculations have 
          been performed without rounded values."
        ),
        shiny::renderTable(
          sumrow(vals$soilB)
        )
      )
    }
  )
  
  output$soiltypeC_table = shiny::renderUI(
    if (!is.null(vals$soilC)) {
      shiny::tagList(
        p(
          "Table 5: Curve Number (CN) for soil class C. (A_LU: Area of landuse 
          class in ha, A: Area of soiltype C for related landuse in percent). 
          The CN is equal to the sum of 'A*CN'-column. All calculations have 
          been performed without rounded values."
        ),
        shiny::renderTable(
          sumrow(vals$soilC)
        )
      )
    }
  )
  
  output$soiltypeD_table = shiny::renderUI(
    if (!is.null(vals$soilD)) {
      shiny::tagList(
        p(
          "Table 6: Curve Number (CN) for soil class D. (A_LU: Area of landuse 
          class in ha, A: Area of soiltype D for related landuse in percent). 
          The CN is equal to the sum of 'A*CN'-column. All calculations have 
          been performed without rounded values."
        ),
        shiny::renderTable(
          sumrow(vals$soilD)
        )
      )
    }
  )
  
  output$calculations = shiny::renderUI(
    if (!is.null(vals$soilA)) {
      shiny::fluidRow(
        col_12(
          h2("Calculations"),
          col_6(
            shiny::uiOutput("soiltypeA_table")
          ),
          col_6(
            shiny::uiOutput("soiltypeB_table")
          ),
          col_6(
            shiny::uiOutput("soiltypeC_table")
          ),
          col_6(
            shiny::uiOutput("soiltypeD_table")
          )
        )
      )
    }
  )
  
}
