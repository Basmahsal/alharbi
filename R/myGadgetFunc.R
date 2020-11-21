myGadgetFunc <- function(dd, outcomes, predictors) {
  ui <- miniPage(gadgetTitleBar("health insurance"),
                 miniTabstripPanel(
                   miniTabPanel(
                     "Parameters",
                     icon = icon("sliders"),
                     miniContentPanel(
                       selectInput(
                         "country",
                         label = "select country",
                         choices = country_choices,
                         selected = c("Saudi Arabia", "United States", "United Kingdom"),
                         multiple = TRUE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL
                       ),
                       sliderInput(
                         "year",
                         label = h3("year"),
                         min = 1960,
                         max = 2020,
                         value = c(1960, 2020)
                       ),
                       selectInput(
                         "outcome",
                         label = "select an outcome",
                         choices = outcomes,
                         selected = outcomes[2],
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL

                       ),
                       selectInput(
                         "predicter",
                         label = "select a predicter",
                         choices = predicters,
                         selected = predicters[1],
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL
                       ))),
                   miniTabPanel(
                     "compare series",
                     icon = icon("area-chart"),
                     miniContentPanel(plotOutput("chartA", height = "100%"))
                   ),
                   miniTabPanel(
                     "regression analysis",
                     icon = icon("area-chart"),
                     miniContentPanel(verbatimTextOutput("chartD"))
                   )
                 )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.

    output$chartA<-     renderPlot({
      dd %>%
        filter(country %in% input$country) %>%
        filter(between(year, input$year[[1]], input$year[[2]])) %>%
        select(year, country, input$outcome, input$predicter) %>%
        na.omit() %>%
        ggplot(aes(
          x = year,
          y = !!sym(input$predicter)
          ,
          color = country
        )) +
        geom_smooth() +
        geom_point()
    })

    output$chartD <-  renderPrint({
      formula1 <-
        rlang::new_formula(sym(input$outcome), sym(input$predicter))
      #formula1
      linearMod <- lm(formula1, data = dd)
      summary(linearMod)
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <-
        stopApp(returnValue)
    })
  }

  runGadget(ui, server)
}

myGadgetFunc <- function(dd, outcomes, predictors) {
  ui <- miniPage(gadgetTitleBar("health insurance"),
                 miniTabstripPanel(
                   miniTabPanel(
                     "Parameters",
                     icon = icon("sliders"),
                     miniContentPanel(
                       selectInput(
                         "country",
                         label = "select country",
                         choices = country_choices,
                         selected = c("Saudi Arabia", "United States", "United Kingdom"),
                         multiple = TRUE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL
                       ),
                       sliderInput(
                         "year",
                         label = h3("year"),
                         min = 1960,
                         max = 2020,
                         value = c(1960, 2020)
                       ),
                       selectInput(
                         "outcome",
                         label = "select an outcome",
                         choices = outcomes,
                         selected = outcomes[2],
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL

                       ),
                       selectInput(
                         "predicter",
                         label = "select a predicter",
                         choices = predicters,
                         selected = predicters[1],
                         multiple = FALSE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL
                       ))),
                   miniTabPanel(
                     "compare series",
                     icon = icon("area-chart"),
                     miniContentPanel(plotOutput("chartA", height = "100%"))
                   ),
                   miniTabPanel(
                     "regression analysis",
                     icon = icon("area-chart"),
                     miniContentPanel(verbatimTextOutput("chartD"))
                   )
                 )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.

    output$chartA<-     renderPlot({
      dd %>%
        filter(country %in% input$country) %>%
        filter(between(year, input$year[[1]], input$year[[2]])) %>%
        select(year, country, input$outcome, input$predicter) %>%
        na.omit() %>%
        ggplot(aes(
          x = year,
          y = !!sym(input$predicter)
          ,
          color = country
        )) +
        geom_smooth() +
        geom_point()
    })

    output$chartD <-  renderPrint({
      formula1 <-
        rlang::new_formula(sym(input$outcome), sym(input$predicter))
      #formula1
      linearMod <- lm(formula1, data = dd)
      summary(linearMod)
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <-
        stopApp(returnValue)
    })
  }

  runGadget(ui, server)
}
