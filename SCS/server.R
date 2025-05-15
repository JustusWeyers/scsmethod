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
    landuse_area = demo_area,
    a = 0.05
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
    in_vals$a = 0.05
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
    in_vals$a = 0
    
    vals$P = NULL
    vals$soil_classes = NULL
    vals$soilA = NULL
    vals$soilB = NULL
    vals$soilC = NULL
    vals$soilD = NULL
  })
  
  ### Serverlogic: Main functionality
  
  shiny::observeEvent(input$run, {
    if (TRUE) {
      vals$P = in_vals$P
      
      vals$duration = input$duration
      vals$precipitation = input$precipitation
      vals$a = input$a

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
  
  CN_weighted = shiny::reactive(
    sum(
      sum(vals$soilA[, ncol(vals$soilA)]),
      sum(vals$soilB[, ncol(vals$soilB)]),
      sum(vals$soilC[, ncol(vals$soilC)]),
      sum(vals$soilD[, ncol(vals$soilD)])
    )
  )
  
  # Dry
  CN_I = shiny::reactive(
    CN_weighted()/(2.334-0.01334 * CN_weighted())
  )
  
  S_I = shiny::reactive(
    25.4 * (1000/CN_I()-10)
  )
  
  I_aI = shiny::reactive(
    vals$a * S_I()
  )
  
  P_effI = shiny::reactive({
    (vals$precipitation - vals$a * S_I())**2/(vals$precipitation - vals$a * S_I() + S_I())
  })
  
  # Normal
  CN_II = shiny::reactive(
    CN_weighted()
  )
  
  S_II = shiny::reactive(
    25.4 * (1000/CN_II()-10)
  )
  
  I_aII = shiny::reactive(
    vals$a * S_II()
  )
  
  P_effII = shiny::reactive(
    (vals$precipitation - vals$a * S_II())**2/(vals$precipitation - vals$a * S_II() + S_II())
  )
  
  # Wet
  CN_III = shiny::reactive(
    CN_weighted()/(0.4036 + 0.0059 * CN_weighted())
  )
  
  S_III = shiny::reactive(
    25.4 * (1000/CN_III()-10)
  )
  
  I_aIII = shiny::reactive(
    vals$a * S_III()
  )
  
  P_effIII = shiny::reactive(
    (vals$precipitation - vals$a * S_III())**2/(vals$precipitation - vals$a * S_III() + S_III())
  )
  
  # Selected CN, S and P_eff
  
  CN = shiny::reactive(
    c(CN_I = CN_I(), CN_II = CN_II(), CN_III = CN_III())[input$selectCN]
  )
  
  S = shiny::reactive(
    c(CN_I = S_I(), CN_II = S_II(), CN_III = S_III())[input$selectCN]
  )
  
  I_a = shiny::reactive(
    c(CN_I = I_aI(), CN_II = I_aII(), CN_III = I_aIII())[input$selectCN]
  )
  
  P_eff = shiny::reactive(
    c(CN_I = P_effI(), CN_II = P_effII(), CN_III = P_effIII())[input$selectCN]
  )
  
  P_eff_distribution = shiny::reactive(
    P_eff_dist(
      p = vals$P$P * vals$precipitation,
      t = vals$P$t * vals$duration,
      I_a = I_a(),
      S = S()
    )
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
        inputId = "duration", 
        label = NULL, 
        value = in_vals$duration, 
        min = 0
      )
    )
  )
  
  output$ui_pInput = shiny::renderUI(
    shinydashboard::box(
      solidHeader = FALSE, width = 12, title = "P [mm]",
      shiny::numericInput(
        inputId = "precipitation", 
        label = NULL, 
        value = in_vals$precipitation, 
        min = 0
      )
    )
  )
  
  output$ui_aInput = shiny::renderUI(
    shinydashboard::box(
      solidHeader = FALSE, width = 12, title = "Initial loss a",
      shiny::numericInput(
        inputId = "a", 
        label = NULL, 
        value = in_vals$a, 
        min = 0,
        max = 0.3
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
          h2("Soiltables"),
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
  
  output$curve_number = shiny::renderUI(
    if (!is.null(vals$soilA)) {
      shiny::fluidRow(
        col_12(
          h2("Characteristic values"),
          shinydashboard::box(
            solidHeader = TRUE, width = 12,
            col_3(
              shiny::HTML(
                paste(
                  "<b> Dry conditions (CN_I):</b>",
                  paste("<t>CN_I =", round(CN_I(), 3)),
                  paste("S_I =", round(S_I(), 3)),
                  paste("I_a,I =", round(I_aI(), 3)),
                  paste("P_eff,I =", round(P_effI(), 3)),
                  sep = "<br><br>"
                )
              )
            ),
            col_3(
              shiny::HTML(
                paste(
                  "<b> Normal conditions (CN_II):</b>",
                  paste("<t>CN_II =", round(CN_II(), 3)),
                  paste("S_II =", round(S_II(), 3)),
                  paste("I_a,II =", round(I_aII(), 3)),
                  paste("P_eff,II =", round(P_effII(), 3)),
                  sep = "<br><br>"
                )
              )
            ),
            col_3(
              shiny::HTML(
                paste(
                  "<b> Wet conditions (CN_III):</b>",
                  paste("<t>CN_III =", round(CN_III(), 3)),
                  paste("S_III =", round(S_III(), 3)),
                  paste("I_a,III =", round(I_aIII(), 3)),
                  paste("P_eff,III =", round(P_effIII(), 3)),
                  sep = "<br><br>"
                )
              )
            ),
            col_3(
              shiny::HTML(
                "<b>Select a CN to calculate the distribution of the effective 
                precipitation: </b><br><br>CN = "
              ),
              shiny::selectInput(
                inputId = "selectCN",
                label = NULL,
                choices = c("CN_I", "CN_II", "CN_III"),
                selected = "CN_III"
              )
            )
          )
        )
      )
    }
  )
  
  output$plot = shiny::renderPlot(
    PeffPlot(P_eff_distribution())
  )
  
  output$distr = shiny::renderUI(
    if (!is.null(vals$soilA)) {
      shiny::fluidRow(
        col_12(
          h2("P_eff distribution"),
          shinydashboard::box(
            solidHeader = TRUE, width = 12,
            col_6(
              shiny::renderTable({
                P_eff_distribution()
              })
              
            ),
            col_6(
              shiny::plotOutput("plot"),
              p("Blue: P_eff, Green: P - P_eff, Red: I_a")
            )
          )
        )
      )
    }
  )
  
}
