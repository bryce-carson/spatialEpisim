#--------------------------#
#     Server Components
#--------------------------#
server <- function(input, output, session) {
  globalValidator <- addRuleListToValidator(
    InputValidator$new(),
    filter(rules, is.na(expr))[, 2][[1]][[1]]
  )

  validatorsAndLambdas <-
    filter(rules, !is.na(expr)) |>
    rowwise() |>
    mutate(vld = list(addRuleListToValidator(InputValidator$new(), ruleList)),
           lambda = list(eval(bquote(\() eval(.(expr))))),
           .keep = "none")

  isolate(mapply(function(validator, lambda) {
                   validator$condition(lambda)
                   globalValidator$add_validator(validator)
                 },
            validatorsAndLambdas[[1]],
            validatorsAndLambdas[[2]]))

  ## TODO: these might be reduced to a single reactive if there is no use of
  ## selectedCountryCode elsewhere.
  selectedCountryDefaults <- reactive({
    filter(defaultParameters,
           ISONumeric == req(selectedCountryCode()),
           model == input$modelSelect) |>
      select(beta) |>
      as.numeric()
  })

  selectedCountryCode <- reactive({
    countrycode(req(input$selectedCountry), "country.name", "iso3c")
  })

  observe({ updateNumericInputs(selectedCountryDefaults(), session) })

  ## MAYBE TODO: adopt full reactivity like in Episim? That'd be appropriate,
  ## likely. Ashok wants reactivity in effect... achieving the desired effect
  ## without using actual reactivity is counterintuitive.
  observe({
    if(globalValidator$is_valid()) enable(id = "go") else disable(id = "go")
  }) |>
    bindEvent(input) # Watch all inputs.

  observe({
    ## MAYBE TODO: it would be nice to simply enable or disable vital dynamics
    ## as in Episim, rather than adjusting the value of muB or muD.
    ##
    ## Reset vital dynamics when not checked off
    if (req(input$muValue) == 0) {
      updateNumericInput(session, "muBirth", value = 0)
      updateNumericInput(session, "muDeath", value = 0)
    }
  })

  ## MAYBE FIXME: values is being used like a global storage space. It's not
  ## being used as a reactive value, changing somehow and being consumed by a
  ## reactive consumer (there is no call to values() in the whole project).
  ## NOTE: individual components of the reactive value are being used as
  ## reactive values, however.
  values <- reactiveValues()
  values$allow_simulation_run <- TRUE

  ## MAYBE FIXME: fileInputs is being used like a global storage space. It's not
  ## being used as a reactive value, changing somehow and being consumed by a
  ## reactive consumer (there is no call to fileInputs() in the whole project).
  ## NOTE: individual components of the reactive value are being used as
  ## reactive values, however.
  fileInputs <- reactiveValues(
    smStatus = NULL,
    latLonStatus = NULL,
    incidenceStatus = NULL
  )

  ## TODO: all of the supported countries should already have their data
  ## downloaded, this should not be performed while the server is running, and
  ## especially not for each user and every visit!
  susceptible <- reactive({
    shiny::validate(need(input$selectedCountry, "Country"),
                    need(input$agg, "Aggregation Factor"))
    createSusceptibleLayer(input$selectedCountry, input$agg) # RasterWorldPop.R
  })

  #==========================================================================#
  # World Pop Visualizer Components                                       ----
  #==========================================================================#
  #--------------------------------------------------------------------------#
  # Display the file inputs for generating the transmission path             #
  #--------------------------------------------------------------------------#
  output$transPathFileInputs <- renderUI({
    req(!is.null(input$selectedCountry) && input$selectedCountry != "")

    tagList(
      fileInput(inputId = "latLonData",
                label = strong("Upload Lat-Lon Data:"),
                placeholder = "Upload Lat-Lon data (.csv or .xls or .xlsx)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx")),
      fileInput(inputId = "incidenceData",
                label = strong("Upload Incidence/Death Data:"),
                placeholder = "Upload Incidence/Death data (.csv or .xls or .xlsx)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"))
    )
  })

  #--------------------------------------------------------------------------#
  # Dynamically generate a date slider that contains the dates for all the
  # observed data in the incidence/death file
  #--------------------------------------------------------------------------#
  output$transPathDateInput <- renderUI({
    req(iv_dataupload$is_valid() && input$appMode == "Visualizer")

    dateInfo <- colnames(transPathData())[4:length(colnames(transPathData()))]

    sliderTextInput(
      inputId = "transPathDate",
      label = strong("Date"),
      choices = dateInfo,
      selected = dateInfo[1],
      animate = animationOptions(interval = 250, loop = FALSE)
    )
  })


  output$resetButton <- renderUI({
    if (req(input$selectedCountry)) {
      actionButton(
        inputId = "visReset",
        label = "Reset Values",
        class = "act-btn"
      )
    }
  })

  output$leafletMap <- renderLeaflet({
    req(!is.null(input$selectedCountry))

    susc <- susceptible()$Susceptible
    level1Names <- NULL

    createLeafletPlot(input$selectedCountry, level1Names, susc)
  })

  output$croppedLeafletMap <- renderLeaflet({
    req(!is.null(input$selectedCountry) && !is.null(input$level1List))
    req(input$selectedCountry == level1Country())

    susc <- susceptible()$Susceptible
    level1Names <- input$level1List

  output$leafletMap <- renderLeaflet({
    shiny::validate(need(input$selectedCountry, "Country"),
                    need(susceptible(),
                         message = "Susceptible() is calculating..."))
    createLeafletPlot(input$selectedCountry, NULL, susceptible()$Susceptible)
  })

  output$terraOutputImage <- renderImage({ ## outputImage ----
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning

    if (input$selectedCountry == ""){
      list(src = "", width = 0, height = 0)
    } else {
      outfile <- tempfile(fileext = '.png')

      # createBasePlot(input$selectedCountry, 1, FALSE) # print the susceptible plot to www/

      # png(outfile, width = 800, height = 600)
      png(outfile, width = 1024, height = 768)
      createBasePlot(input$selectedCountry, susceptible()$Susceptible, TRUE)   # print the susceptible plot direct to UI
      dev.off()

      # list(src = outfile, contentType = 'image/png', width = 800, height = 600, alt = "Base plot image not found")
      list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Base plot image not found")
    }
  }, deleteFile = TRUE)

  output$transmission <- renderLeaflet({
    req(!is.null(input$selectedCountry))
    req(iv_dataupload$is_valid())

    level1Names <- NULL

    if(input$cropLev1 == TRUE){

      if(!is.null(input$level1List) && !("" %in% input$level1List)){
        level1Names <- input$level1List
      }
    }

    createLeafletBubblePlot(input$selectedCountry, level1Names, transPathData(), 1)
  })

  #--------------------------------------------------------------------------#
  # Proxy map for the leaflet plot to dynamically update the transmission
  # path data
  #--------------------------------------------------------------------------#
  observe({
    shiny::validate(need(input$transPathDate, "Trans. path"))

    # which date (column of data) to plot
    transDate <- input$transPathDate
    plotData <- transPathData()

    # To access a column inside the leafletProxy function the column name must
    # be called directly (can't use a variable storing the column name) so we
    # must set the column we want to a known name ("Current")
    colnames(plotData)[colnames(plotData) == transDate] <- "Current"

    labelText <- paste0(
      "Location: ", plotData$Location, "<br/>",
      "Count: ", plotData$Current, "<br/>") %>%
      lapply(htmltools::HTML)

    # To update the map, clear out the old markers and draw new ones using the
    # data from the newly selected date
    leafletProxy("transmission", data = plotData) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~ Longitude,
                       lat = ~ Latitude,
                       radius = ~ Current^0.35 * 2,
                       weight = 1,
                       opacity = 1,
                       color = ~ ifelse(Current > 0, "black", "transparent"),
                       fillColor = ~ ifelse(Current > 0,
                                            colorPalette(Current),
                                            "transparent"),
                       fillOpacity = 0.8,
                       label = labelText)
  })

  output$lollipop <- renderPlotly({
    req(iv_dataupload$is_valid()) ## MAYBE FIXME: shiny::validate?
    ggplotly(plotLolliChart(input$selectedCountry,
                            input$incidenceData$datapath))
  })

  output$timeSeriesOptions <- renderUI({
    plotTitle <- paste0("Time-Series Graph of Incidence/Death in ")
    if(input$selectedCountry %in% prependList) {
      plotTitle <- paste0(plotTitle, "the ")
    }
    plotTitle <- paste0(plotTitle, input$selectedCountry)

    plotOptionsMenuUI(
      id = "timeSeriesMenu",
      plotType = "Time-Series",
      title = plotTitle,
      xlab = "Date",
      ylab = "Number of Persons",
      colour = "#22031F",
      includeFlip = FALSE,
      includeGridlines = FALSE
    )
  })

  output$timeSeries <- renderPlotly({
    req(iv_dataupload$is_valid())
    req(!is.null(input[["timeSeriesMenu-Colour"]]))

    plotTimeSeries(file = input$incidenceData$datapath,
                   input[["timeSeriesMenu-Title"]],
                   input[["timeSeriesMenu-Xlab"]],
                   input[["timeSeriesMenu-Ylab"]],
                   input[["timeSeriesMenu-Colour"]],
                   input[["timeSeriesMenu-TSstyle"]]) |>
    ggplotly()
  })


  #---------------------------------------------------------------------------#
  # Combine the lat/long data with the observed infection into a single table #
  #---------------------------------------------------------------------------#
  transPathData <- reactive({
    req(iv_dataupload$is_valid() && input$appMode == "Visualizer")

    incidenceData <- openDataFile(input$incidenceData)
    latLonData <- openDataFile(input$latLonData)

    incidence <- as.data.frame(t(incidenceData))
    incidenceCols <- incidence[2,]
    incidence <- incidence[3:nrow(incidence),]
    colnames(latLonData) <- c("Location", "Latitude", "Longitude")
    colnames(incidence) <- incidenceCols

    plotData <- cbind(latLonData, lapply(incidence, as.numeric))
  })

  #--------------------------------------------------------------------------#
  # Checks to see that files have been uploaded (helper func)                #
  #--------------------------------------------------------------------------#
  ## FIXME: this needs to change. This is inapprorpiate to the purpose.
  observeEvent(input$latLonData, { fileInputs$latLonStatus <- 'uploaded' })
  observeEvent(input$incidenceData, { fileInputs$incidenceStatus <- 'uploaded' })

  observeEvent({
    input$selectedCountry
    input$appMode
  },
  {
    if(!is.null(input$selectedCountry) && input$selectedCountry != "" && input$appMode == "Visualizer") {
      show(id = "maptabPanels")
    } else {
      hide(id = "maptabPanels")
    }

    fileInputs$latLonStatus <- 'reset'
    fileInputs$incidenceStatus <- 'reset'
  })

  observe(priority = 100, {
    if(input$appMode == "Visualizer") {
      updateTabsetPanel(inputId = "vizTabSet", selected = "Leaflet Plot")
    }
  }) |>
    bindEvent(input$selectedCountry, input$appMode)

  observe(priority = 100, {
    if(input$cropLev1  == TRUE && input$appMode == "Visualizer" && !is.null(input$level1List)) {
      showTab(inputId = 'vizTabSet', target = 'Leaflet Cropped Plot')
    } else if((input$cropLev1  == FALSE && input$appMode == "Visualizer") || is.null(input$level1List)) {
      hideTab(inputId = 'vizTabSet', target = 'Leaflet Cropped Plot')
      updateTabsetPanel(inputId = "vizTabSet", selected = "Leaflet Plot")
    }
  }) |>
    bindEvent(input$cropLev1,
              input$selectedCountry,
              input$level1List,
              input$appMode)

  observeEvent(input$visReset, {

    updateCheckboxInput(
      inputId = "cropLev1",
      value = FALSE
    )
    updatePickerInput(
      inputId = "selectedCountry",
      selected = ""
    )

    fileInputs$latLonStatus <- 'reset'
    fileInputs$incidenceStatus <- 'reset'
  })

  observe({
    if(input$appMode == "Visualizer") {
      if(iv_dataupload$is_valid()) {
        showTab(inputId = 'vizTabSet', target = "Transmission Path")
        showTab(inputId = 'vizTabSet', target = "Lollipop Chart")
        showTab(inputId = 'vizTabSet', target = "Time-Series Graph")
      } else {
        hideTab(inputId = 'vizTabSet', target = "Transmission Path")
        hideTab(inputId = 'vizTabSet', target = "Lollipop Chart")
        hideTab(inputId = 'vizTabSet', target = "Time-Series Graph")
      }
    }
  })

  #==========================================================================#
  # Model Simulation Components                                           ----
  #==========================================================================#
  #--------------------------------------------------------------------------#
  # Output population base plot image to the app UI                          #
  #--------------------------------------------------------------------------#
  observeEvent(input$go, {
    req(globalValidator$is_valid())
    output$outputImage <- renderImage({

      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 768, height = 768)

      if(input$cropLev1) {
        req(input$level1List != "")
        isolate(createCroppedRaster(selectedCountry = input$selectedCountry,
                                    level1Region = input$level1List,
                                    susceptible()$Susceptible,
                                    directOutput = TRUE))
      } else {
        isolate(createBasePlot(selectedCountry = input$selectedCountry,
                               susceptible()$Susceptible,
                               directOutput = TRUE))  # print the susceptible plot direct to UI
      }

      dev.off()

      ## Adjust the dimensions of the base plot rendered in UI
      list(src = outfile,
           contentType = "image/png",
           width = 768,
           height = 768,
           alt = "Base plot image not found")
    }, deleteFile = TRUE)
  })

  #--------------------------------------------------------------------------#
  # Output IDE equations image to the app UI                                 #
  #--------------------------------------------------------------------------#
  observeEvent(input$go, {
    req(iv$is_valid())
    output$modelImg <- renderImage({
      return(list(src= "www/ModelEquations.png",
                  height = 400,
                  contentType = "image/png"))
    }, deleteFile = FALSE)
  })

  #--------------------------------------------------------------------------#
  # Output flowchart image to the app UI                                     #
  #--------------------------------------------------------------------------#
  observeEvent(input$go, {
    req(globalValidator$is_valid())
    output$flowchartImg <- renderImage({
      if (input$modelSelect == "SEIRD"){
        return(list(src= "www/SEIRD.png",
                    height = 400,
                    contentType = "image/png"))
      }
      else if (input$modelSelect == "SVEIRD"){
        return(list(src = "www/SVEIRD.png",
                    height = 400,
                    contentType = "image/png"))
      }
    }, deleteFile = FALSE)
  })

  #--------------------------------------------------------------------------#
  # Reset all parameter sliders, country selection, etc.                     #
  #--------------------------------------------------------------------------#
  observeEvent(input$resetAll, {
    reset("dashboard")
    disable(id = "go")

    values$allow_simulation_run <- FALSE

  })

  #--------------------------------------------------------------------------#
  # Checks to see that a new file has been uploaded (helper func)            #
  #--------------------------------------------------------------------------#
  observeEvent(input$seedData, {
    values$allow_simulation_run <- TRUE
    fileInputs$smStatus <- 'uploaded'
  })

  #--------------------------------------------------------------------------#
  # Check if all mandatory fields have a value                               #
  #--------------------------------------------------------------------------#
  # observe({
  #       mandatoryFilled <-
  #       vapply(fieldsMandatory,
  #              function(x) {
  #                !is.null(input[[x]]) && input[[x]] != ""
  #              },
  #              logical(1))
  #
  #       mandatoryFilled <- all(mandatoryFilled)
  #
  #     # enable/disable the submit button
  #     if (isolate(values$allow_simulation_run) == TRUE){
  #       shinyjs::toggleState(id = "go", condition = mandatoryFilled)
  #   }
  # })

  #--------------------------------------------------------------------------#
  #--------------------------------------------------------------------------#
  # observe({
  #   hoverDrop <-
  #     vapply(hoverDrop,
  #            function(x) {
  #              !is.null(input[[x]]) && input[[x]] != ""
  #            },
  #            logical(1))
  #   hoverDrop <- all(hoverDrop)
  #   # enable/disable the submit button
  #   if (isolate(values$allow_simulation_run) == TRUE){
  #     shinyjs::toggleClass(class = hoverDrop)
  #   }
  # })

  #--------------------------------------------------------------------------#
  # This static ui field is in server since other dynamic ui elements need it#
  #--------------------------------------------------------------------------#
  output$countryDropdown <- renderUI({
    pickerInput(
      inputId = "selectedCountry",
      label = (strong("Country")),
      choices = shortlist$Country,
      multiple = FALSE,
      selected = "Democratic Republic of Congo", #
      options = pickerOptions(
        actionsBox = TRUE,
        title = "Please select a country")
    )
  })

  #--------------------------------------------------------------------------#
  # Dynamically display the checkbox option to select for states/provinces   #
  #--------------------------------------------------------------------------#
  output$cropStateCheckbox <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(
        inputId = "cropLev1",
        label = strong("Crop State(s)/Province(s)"),
        value = TRUE)
    }
  })

  #--------------------------------------------------------------------------#
  # Checkbox for Data Assimilation                                           #
  #--------------------------------------------------------------------------#
  output$dataAssimCheckbox <- renderUI({
    validate(need(!is.null(input$selectedCountry), ""))

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(inputId = "dataAssim", label = strong("Include Bayesian data assimilation?"), value = FALSE)
    }
  })

  #--------------------------------------------------------------------------#
  # Create select box for choosing input country                             #
  #--------------------------------------------------------------------------#
  output$Level1Ui <- renderUI({
    req(!is.null(input$selectedCountry) && input$selectedCountry != "")
    validate(need(input$cropLev1 == TRUE, "")) # catches UI warning

    if (file.exists(paste0("gadm/", "gadm36_", toupper(selectedCountryCode()), "_1_sp.rds"))) {
      level1Options <<- readRDS(paste0("gadm/",
                                       "gadm36_",
                                       toupper(selectedCountryCode()),
                                       "_1_sp.rds"))$NAME_1
    } else {
      level1Options <<- getData("GADM",
                                download = TRUE,
                                level = 1,
                                country = toupper(selectedCountryCode()))$NAME_1
    }

    selectizeInput(inputId = "level1List",
                   label = NULL,
                   choices = level1Options,
                   selected = c("Ituri", "Nord-Kivu"),
                   multiple = TRUE,
                   options = list(placeholder = "Select state(s)/province(s)"))
  })

  level1Country <- reactiveVal({
    value = NULL
  })

  #--------------------------------------------------------------------------#
  # Radio button for SEIRD vs SVEIRD Model                                   #
  #--------------------------------------------------------------------------#
  output$modelRadio <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      radioButtons(inputId = "modelSelect",
                   label = strong("Epidemic Model"),
                   choiceValues = list("SEIRD","SVEIRD"),
                   choiceNames = list("SEIRD","SVEIRD"),
                   selected = "SVEIRD", #character(0), #
                   inline = TRUE,
                   width = "1000px")
    }
  })

  #--------------------------------------------------------------------------#
  # Radio button for Deterministic vs Stochastic Model                       #
  #--------------------------------------------------------------------------#
  output$stochasticRadio <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      radioButtons(inputId = "stochasticSelect",
                   label = strong("Model Stochasticity"),
                   choiceValues = list("Deterministic", "Stochastic"),
                   choiceNames = list("Deterministic", "Stochastic"),
                   selected = "Deterministic", #character(0), #
                   inline = TRUE,
                   width = "1000px")
    }
  })

  #--------------------------------------------------------------------------#
  # TODO: refactor numericInputs into single function                        #
  #--------------------------------------------------------------------------#
  output$alphaInput <- renderUI({
    alphaValue <- 0.00015 # 0.2100
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    ## DONE: no need for a new function, just use a package for reasonable conversions.
    library(countrycode)
    alphaValue <- filter(epiparms,
                         model == req(input$modelSelect),
                         ISONumeric == countrycode(req(input$selectedCountry),
                                                   origin = 'country.name',
                                                   destination = 'iso3c')) |>
      ## NOTE: the package must be specified here, or the following error
      ## occurs "No method 'select' for tbl_df". See
      ## https://stackoverflow.com/a/61428630/14211497.
      dplyr::select(alpha) |>
      as.numeric()
    stopifnot(length(alphaValue) == 1)

    numericInput("alpha",
                 HTML(paste("Daily Vaccination Rate (&#945)")),
                 alphaValue, 0, 1, 0.00001)
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#
  output$betaInput <- renderUI({
    req(input$modelSelect)
    betaValue <- 0.055 # 0.00001

    validate(need(input$selectedCountry, message = "A country must be selected."))

    ## WARN FIXME: the awful, nested if-else had the Democratic Republic of
    ## Congo mapped to "COD" for ISONumeric, while Uganda is also "COD". See the
    ## COW (correlates of war) country code details, and ask Ashok for more
    ## information.
    betaValue <- filter(epiparms,
                        ISONumeric == countrycode(input$modelSelect, "country.name", "iso3c"),
                        model == input$modelSellect) |>
      select(beta) |>
      as.numeric()

    stopifnot(length(betaValue) == 1)

    numericInput(inputId = "beta",
                 label = HTML(paste("Daily Exposure Rate (&#946)")),
                 value = betaValue, min = 0, max = 1, step = 0.00001)
  })

  output$gammaInput <- renderUI({
    shiny::validate(need(input$modelSelect, "Model"),
                    need(input$selectedCountry, "Country"))

    dplyr::filter(epiparms,
                  ISONumeric == selectedCountryCode(),
                  model == input$selectedCountry) |>
      dplyr::select("gamma") |>
      as.numeric() ->
      gamma

    stopifnot(length(gamma) == 1)

    numericInput(inputId = "gamma",
                 label = HTML(paste("Daily Infection Rate (&#947)")),
                 value = gamma, min = 0, max = 1, step = 0.00001)
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#
  output$sigmaInput <- renderUI({
    req(!is.null(input$modelSelect))
    sigmaValue <- 0.065

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"sigma"])
        } else if (input$selectedCountry == "Nigeria"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Uganda"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"sigma"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"sigma"])
        } else if (input$selectedCountry == "Nigeria"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Uganda"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"sigma"])}
      }

      numericInput(inputId = "sigma",
                   label = HTML(paste("Daily Recovery Rate (&#963)")),
                   value = sigmaValue, min = 0, max = 1, step = 0.00001)
    }
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#
  output$deltaInput <- renderUI({
    req(!is.null(input$modelSelect))
    deltaValue <- 0.0015

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"delta"])
        } else if (input$selectedCountry == "Nigeria"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Uganda"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"delta"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"delta"])
        } else if (input$selectedCountry == "Nigeria"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Uganda"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"delta"])}
      }

      numericInput(inputId = "delta",
                   label = HTML(paste("Daily Death Rate (&#948)")),
                   value = deltaValue, min = 0, max = 1, step = 0.00001)
    }
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#
  output$lambdaInput <- renderUI({
    req(!is.null(input$modelSelect))
    lambdaValue <- 15

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"lambda"])
        } else if (input$selectedCountry == "Nigeria"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"lambda"])}
        else if (input$selectedCountry == "Uganda"){
          lambdaValue <- 5}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"lambda"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"lambda"])
        } else if (input$selectedCountry == "Nigeria"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"lambda"])
        }
        else if (input$selectedCountry == "Uganda"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"lambda"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"lambda"])}
      }

      numericInput(inputId = "lambda",
                   label = HTML(paste("Distance Parameter (&#955)")),
                   value = lambdaValue,min = 1, max = 50, step = 1)
    }
  })

  #--------------------------------------------------------------------------#
  #                     Upload Seed Data                                     #
  #--------------------------------------------------------------------------#
  output$seedUpload <- renderUI({

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      fileInput(inputId = "seedData",
                label = "Upload Seed Data:",
                placeholder = "Upload seed data (.csv or .xls or .xlsx)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx")
      )
    }
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#
  output$startDateInput <- renderUI({
    req(!is.null(input$modelSelect))
    startDateInput <- Sys.Date() # NULL

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          startDateInput <- "2020-09-01" #filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"startDate"]
        } else if (input$selectedCountry == "Nigeria"){
          startDateInput <- "2020-09-01" #filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"startDate"]
        }
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          startDateInput <- "2021-09-01" #filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"startDate"]
        } else if (input$selectedCountry == "Nigeria"){
          startDateInput <- "2021-09-01" #filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"startDate"]
        }
      }
      if (input$selectedCountry == "Uganda") {
        startDateInput <- "2022-10-20"
      }
      else if (input$selectedCountry == "Democratic Republic of Congo") {
        startDateInput <- "2018-08-01"}

      dateInput('date', "Choose simulation start date:", value = startDateInput, max = Sys.Date(),
                format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                language = "en", width = NULL)
    }
  })

  #--------------------------------------------------------------------------#
  # numeric input for number of iterations                                   #
  #--------------------------------------------------------------------------#
  output$timestepInput <- renderUI({
    timestepValue <- 10
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (input$selectedCountry == "Czech Republic" || input$selectedCountry == "Nigeria"){timestepValue = 120}
    else if (input$selectedCountry == "Democratic Republic of Congo") {timestepValue = 440}
    else if (input$selectedCountry == "Uganda") {timestepValue = 63}

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "timestep",
                   label = "Number of Iterations (days)",
                   min = 1, max = 3650, value = timestepValue, step = 1)}
  }
  )

  #--------------------------------------------------------------------------#
  # Data Assimilation settings                                               #
  #--------------------------------------------------------------------------#
  output$dataAssimCmpts <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning

    checkboxGroupInput(inputId = "selectedCompartments",
                       "Select observable compartment(s)",
                       choices = c("V", "E", "I", "R", "D"),
                       selected = c("I"),
                       inline = TRUE
    )
  })
  showI <- reactive({
    "I" %in% input$selectedCompartments
  })

  showD <- reactive({
    "D" %in% input$selectedCompartments
  })

  output$dataAssimZones <- renderUI({
    validate(need(input$dataAssim == TRUE, ""),
             need(input$selectedCountry, message = "A country must be selected"))
    fileInput(inputId = "dataAssimZones",
              label = "Upload the lat/lon coordinates of reporting health zones (.csv or .xls or .xlsx)",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv",
                ".xls",
                ".xlsx"))
  })

  observeEvent(input$dataAssimZones, {
    ## FIXME: read but not store? Also, what if it is not a CSV?
    print(read.csv(input$dataAssimZones$datapath))
    print(as.character(input$dataAssimZones[1]))})

  output$dataAssimFileI <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
    if (showI()) {
      fileInput(inputId = "assimIData",
                label = ("Upload infection data to be assimilated with the model (.csv or .xls or .xlsx)"),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"))
    }
  })
  output$dataAssimFileD <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
    if (showD()) {
      fileInput(inputId = "assimDData",
                label = ("Upload death data to be assimilated with the model (.csv or .xls or .xlsx)"),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx")
      )
    }
  })

  #--------------------------------------------------------------------------#
  # Change the function which generates the Q matrix     #
  #--------------------------------------------------------------------------#
  output$varCovarFunc <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      selectInput(inputId = "covarianceSelect",
                  label = HTML("<span class='label-text'>Choose variance-covariance function:</span>"),
                  choices = list("DBD", "Balgovind", "Exponential", "Gaussian", "Spherical"),
                  selected = "DBD", #character(0), #
                  width = "1000px",
                  multiple = FALSE)
    }
  })

  #--------------------------------------------------------------------------#
  # Adjust parameter values for the variance=covariance function             #
  #--------------------------------------------------------------------------#

  output$selectRho <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "QCorrLength",
                   label = "Choose correlation length parameter for generating Q:",
                   value = 0.675,
                   step = 0.001,
                   min = 0)
    }
  })

  output$selectSigma <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "QVar",
                   label = "Choose variance parameter for generating Q:",
                   value = 0.55,
                   step = 0.01,
                   min = 0)
    }
  })

  output$selectNbhd <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "nbhd",
                   label = "Choose neighborhood parameter for generating Q:",
                   value = 3,
                   step = 1,
                   min = 0)
    }
  })

  output$selectPsiDiag <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "psidiag",
                   label = HTML(paste("Choose a value for the zero elements of &#936 to be set to:")),
                   value = 0.001,
                   step = 0.001,
                   min = 0)
    }
  })

  #--------------------------------------------------------------------------#
  # Change the recommended aggregation factor for slider dynamically         #
  #--------------------------------------------------------------------------#
  output$aggInput <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      sliderInput(inputId = "agg",
                  label = "Aggregation Factor",
                  min = 0, max = 100, step = 1, value = population$reco_rasterAgg[match(input$selectedCountry, population$Country)])
    }
  })

  lineThickness <- 1.5

  observeEvent(input$go, {
    req(globalValidator$is_valid())

    output$infectedExposedPlot <- renderPlotly({
      p <- makePlot(
        compartments = c("E", "I"),
        selectedCountry = input$selectedCountry,
        plotTitle = paste0("Time-series plot of Exposed and Infectious \n compartments in ", input$selectedCountry),
        xTitle = paste0("Day (from ", input$date, ")"),
        yTitle = "Compartment Value",
        lineThickness = lineThickness)
      ggplotly(p)
    })

    output$cumDeathsPlot <- renderPlotly({
      est_df <- as.data.frame(read_xlsx(paste0("www/MP4/COD_summary.xlsx")))
      obs_df <- as.data.frame(read_xlsx(paste0("observeddata/Ebola_Death_Data.xlsx")))
      cod_est_cum <- data.frame(x = ymd(est_df[,"Date"]), y = est_df[,"D"])
      cod_obs_df <- data.frame(x = ymd(obs_df[1:63,2]), y = rowSums(obs_df[1:63,3:ncol(obs_df)]))
      # cod_est_cum <- data.frame(x = ymd(est_df[,"Date"]), y = est_df[,"D"])
      cod_obs_cum <- data.frame(x = 1:nrow(cod_obs_df))
      cod_obs_cum[1,1] <- cod_obs_df[1,2]
      for(i in 2:nrow(cod_obs_df)){cod_obs_cum[i,1] <- cod_obs_cum[i-1,1] + cod_obs_df[i,2] }
      cod_obs_cum <- cbind(ymd(obs_df[1:63,2]),cod_obs_cum)
      colnames(cod_obs_cum) <- c("x", "y")


      q <- ggplot(cod_est_cum, aes(x, y)) +
        labs(title = "Estimated Vs. Observed Cumulative Deaths \nin the Democratic Republic of Congo",
             x = "Date",
             y = "Number of persons") +
        # scale_x_date(date_labels = "%d %b %Y") +
        geom_line(linewidth=2, color="black") +
        # geom_line(data = cod_obs_cum, aes(x, y), linewidth = 1.5, color = "black") +
        geom_point(data = cod_obs_cum, aes(x, y), color = "#18536F") +
        theme(
          plot.title = element_text(size = 18,
                                    face = "bold",
                                    margin = margin(0, 0, 25, 0),
                                    hjust = 0.5),
          axis.title.x = element_text(size = 14,
                                      face = "bold",
                                      margin = margin(25, 0, 0, 0)),
          axis.title.y = element_text(size = 14,
                                      face = "bold",
                                      margin = margin(0, 25, 0, 0)),
          axis.text.x.bottom = element_text(size = 14),
          axis.text.y.left = element_text(size = 14),
          axis.line = element_line(linewidth = 0.5),
          plot.margin = unit(c(1, 1, 1, 0),"cm"),
          legend.title = element_text(size = 10,
                                      face = "bold"),
          legend.box = "horizontal"
        ) +
        coord_cartesian(clip="off")

      ggplotly(q)
    })

    output$dailyIncidence <- renderPlotly({

      est_df <- as.data.frame(read_xlsx(paste0("www/MP4/COD_summary.xlsx")))
      cod_est_df <- data.frame(x = ymd(est_df[,"Date"]), y = est_df[,"I"])
      obs_df <- as.data.frame(read_xlsx(paste0("observeddata/Ebola_Incidence_Data.xlsx")))
      cod_obs_df <- data.frame(x = ymd(obs_df[1:63,2]), y = rowSums(obs_df[1:63,3:ncol(obs_df)]))

      p <- ggplot(cod_est_df, aes(x, y)) +
        labs(title = "Estimated vs. Observed Daily Incidence \nin the Democratic Republic of Congo",
             x = "Date",
             y = "Number of persons") +
        # scale_x_date(date_labels = "%d %b %Y") +
        geom_line(linewidth=2, color="red") +
        # geom_line(data = cod_obs_df, aes(x, y), linewidth = 1.5, color = "black") +
        geom_point(data = cod_obs_df, aes(x, y), color = "#18536F") +
        theme(
          plot.title = element_text(size = 18,
                                    face = "bold",
                                    margin = margin(0, 0, 25, 0),
                                    hjust = 0.5),
          axis.title.x = element_text(size = 14,
                                      face = "bold",
                                      margin = margin(25, 0, 0, 0)),
          axis.title.y = element_text(size = 14,
                                      face = "bold",
                                      margin = margin(0, 25, 0, 0)),
          axis.text.x.bottom = element_text(size = 14),
          axis.text.y.left = element_text(size = 14),
          axis.line = element_line(linewidth = 0.5),
          plot.margin = unit(c(1, 1, 1, 0),"cm"),
          legend.title = element_text(size = 10,
                                      face = "bold"),
          legend.box = "horizontal"
        ) +
        coord_cartesian(clip="off")

      ggplotly(p)
    })

    output$cumIPlot <- renderPlotly({
      est_df <- as.data.frame(read_xlsx(paste0("www/MP4/COD_summary.xlsx")))
      obs_df <- as.data.frame(read_xlsx(paste0("observeddata/Ebola_Incidence_Data.xlsx")))
      cod_est_df <- data.frame(x = ymd(est_df[,"Date"]), y = est_df[,"newI"])
      cod_obs_df <- data.frame(x = ymd(obs_df[1:63,2]), y = rowSums(obs_df[1:63,3:ncol(obs_df)]))
      cod_est_cum <- data.frame(x = ymd(est_df[,"Date"]), y = est_df[,"cumI"])
      cod_obs_cum <- data.frame(x = 1:nrow(cod_obs_df))
      cod_obs_cum[1,1] <- cod_obs_df[1,2]
      for(i in 2:nrow(cod_obs_df)){cod_obs_cum[i,1] <- cod_obs_cum[i-1,1] + cod_obs_df[i,2] }
      cod_obs_cum <- cbind(ymd(obs_df[1:63,2]),cod_obs_cum)
      colnames(cod_obs_cum) <- c("x", "y")


      q <- ggplot(cod_est_cum, aes(x, y)) +
        labs(title = "Estimated vs. Observed Cumulative Infections \nin the Democratic Republic of Congo",
             x = "Date",
             y = "Number of persons") +
        # scale_x_date(date_labels = "%d %b %Y") +
        geom_line(linewidth=2, color="red") +
        geom_point(data = cod_obs_cum, aes(x, y), color = "#18536F") +
        theme(
          plot.title = element_text(size = 18,
                                    face = "bold",
                                    margin = margin(0, 0, 25, 0),
                                    hjust = 0.5),
          axis.title.x = element_text(size = 14,
                                      face = "bold",
                                      margin = margin(25, 0, 0, 0)),
          axis.title.y = element_text(size = 14,
                                      face = "bold",
                                      margin = margin(0, 25, 0, 0)),
          axis.text.x.bottom = element_text(size = 14),
          axis.text.y.left = element_text(size = 14),
          axis.line = element_line(linewidth = 0.5),
          plot.margin = unit(c(1, 1, 1, 0),"cm"),
          legend.title = element_text(size = 10,
                                      face = "bold"),
          legend.box = "horizontal"
        ) +
        coord_cartesian(clip="off")

      ggplotly(q)
    })

    output$fullPlot <- renderPlotly({
      if (input$modelSelect == "SVEIRD"){

        p <- makePlot(compartments = c("S", "V", "E", "I", "R", "D"),
                 selectedCountry = input$selectedCountry,
                 plotTitle = paste0("Time-series plot of epidemic compartments \n in ", input$selectedCountry),
                 xTitle = paste0("Day (from ", input$date, ")"),
                 yTitle = "Compartment Value",
                 lineThickness = lineThickness)

      } else {
        p <- makePlot(compartments = c("S", "E", "I", "R", "D"),
                 selectedCountry = input$selectedCountry,
                 plotTitle = paste0("Time-series plot of epidemic compartments \n in ", input$selectedCountry),
                 xTitle = paste0("Day (from ", input$date, ")"),
                 yTitle = "Compartment Value",
                 lineThickness = lineThickness)
      }

      ggplotly(p)
    })
  })

  #--------------------------------------------------------------------------#
  # Generate seed data and have an option to download the file locally       #
  #--------------------------------------------------------------------------#

  output$seedDataButton <- renderUI({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      downloadButton('downloadData', label = "Generate Seed Data Template",
                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                     style = "length:800px")
    }
  })

  output$seedRadiusInput <- renderUI({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning


    if(!is.null(fileInputs$smStatus) && fileInputs$smStatus != 'reset') {
      radioButtons(inputId = "seedRadius",
                   label = strong("Insert infection data in"),
                   choiceNames = list("a single cell", "a Moore neighbourhood of cells"),
                   choiceValues = list(0, 1),
                   selected = 0, #character(0), #
                   inline = TRUE)
    }
  })

  observeEvent(input$selectedCountry, {
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      inputISO <- countrycode(input$selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha

      gadmFileName <- paste0("gadm36_", inputISO, "_1_sp.rds")  # name of the .rds file

      gadmFolder <- "gadm/" # .rds files should be stored in local gadm/ folder

      Level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))

      seedNames <- Level1Identifier$NAME_1
      seedCoords <- coordinates(Level1Identifier)
      seedVaxx <- c(0)
      seedExpo <- c(0)
      seedInfect <- c(0)
      seedRec <- c(0)
      seedDead <- c(0)
      seedCombine <- cbind(seedNames, seedCoords, seedVaxx, seedExpo, seedInfect, seedRec, seedDead)
      frameCombine <- data.frame(seedCombine)

      frameCombine <- frameCombine[c("seedNames", "V3", "V2", "seedVaxx", "seedExpo", "seedInfect", "seedRec", "seedDead")]

      colnames(frameCombine) <- c("Location", "lat", "lon", "InitialVaccinated", "InitialExposed", "InitialInfections", "InitialRecovered", "InitialDead")
      #print(frameCombine)

      isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c")
      sheetName <- sprintf("%s_initialSeedData", isoCode)

      output$downloadData <- downloadHandler(
        filename = function() {
          paste(sheetName, Sys.Date(), ".csv",  sep = "")
        },
        content = function(sheetName) {
          write.csv(frameCombine, sheetName, row.names = FALSE)
        }
      )
    }

    fileInputs$smStatus <- 'reset'
  })

  #--------------------------------------------------------------------------#
  # Multiple functionality when 'Run Simulation' is pressed                  #
  #--------------------------------------------------------------------------#
  observeEvent(input$go, {
    req(globalValidator$is_valid())
    isCropped <- input$cropLev1

    print(paste("Value of isCropped:", isCropped))

    rs <- createRasterStack(selectedCountry = input$selectedCountry,
                            rasterAgg = input$agg,
                            isCropped = isCropped,
                            level1Names = input$level1List,
                            susceptible = susceptible())

    # ============= TAB TO SHOW SEED DATA IN TABLE ===========
    data <- reactive({               # read seed data from .csv or .xlsx
      req(globalValidator$is_valid())
      req(input$seedData)
      ext <- tools::file_ext(input$seedData$datapath)
      seedData <- input$seedData
      if(ext == 'xlsx'){
        readxl::read_excel(input$seedData$datapath)
      } else {
        read.csv(input$seedData$datapath)
      }
    })

    ## seed data table ----
    output$tableSeed <- renderDT({ # print initial seed data to UI
      req(input$seedData)
      if(is.null(data())){
        return ()
      }
      datatable(data(),
                rownames = FALSE,
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = TRUE,
                  scrollX = TRUE))
    })

    ## output summary table ----
    output$outputSummary <- renderDT({ # print output summary to UI
      outputSummaryTable <- read_excel(paste0("www/MP4/", countrycode(input$selectedCountry, "country.name", "iso3c"), "_summary.xlsx"))
      datatable(outputSummaryTable,
                options = list(
                  autoWidth = FALSE,
                  scrollX = TRUE)) %>%
        formatRound(columns = 2:15, digits = 0)
    })

    output$dataPlot <- renderPlot({
      buildPlot()
    })

    # # Allow user to download the raster plot
    # output$downloadPlot <- downloadHandler(
    #     filename = function() {
    #         "susceptibleDataPlot.pdf"
    #     },
    #
    #     content = function(file) {
    #         pdf(file = file, width = 12, height = 12)
    #         print(buildPlot())
    #         dev.off()
    #     }
    # )

    # #Allow user to download the simulation summary data, simply save as csv
    # output$downloadData <- downloadHandler(
    #     filename = function() {
    #         "simulationSummary.csv"
    #     },
    #
    #     content = function(file) {
    #         write.table(x = cDatTable(),
    #                     file = file,
    #                     quote = FALSE, sep = ",", row.names = FALSE)
    #     }
    #  )

    # validate(need(!is.null(data()),'No csv uploaded.'))
    #
    # if(nrow(data())>1)
    # {
    #     return('Your csv has enough rows!')
    # }
    # else
    # {
    #     return('Your csv has not enough rows!')
    # }

    #---------------------------------------#
    # Compartmental model simulation begins #
    #---------------------------------------#

    #print(data())          # Prints the seed data

    #print(names(data()))   # Prints the column names of the seed data

    alpha <- ifelse(input$modelSelect == "SVEIRD", input$alpha, 0) # DO NOT DELETE
    beta  <- input$beta  # DO NOT DELETE
    gamma <- input$gamma # DO NOT DELETE
    sigma <- input$sigma # DO NOT DELETE
    delta <- input$delta # ifelse(input$modelSelect == "SEIR", 0, input$delta) # DO NOT DELETE

    eps <- 0.0000000000000001

    radius <- ifelse(input$lambda <= input$agg, 1, round(((input$lambda - input$agg)/input$agg) + eps) + 1)

    isDeterministic <- TRUE

    if(input$stochasticSelect == "Deterministic")
    {
      isDeterministic <- TRUE
    }
    else
    {
      isDeterministic <- FALSE
    }

    ### model function call ----
    SpatialCompartmentalModelWithDA(model = input$modelSelect,
                                    stack = rs,
                                    startDate = input$date,
                                    selectedCountry = input$selectedCountry,
                                    directOutput = FALSE,
                                    rasterAgg = input$agg,
                                    alpha, beta, gamma, sigma, delta,
                                    radius = radius,
                                    lambda = input$lambda,
                                    timestep = input$timestep,
                                    seedFile = input$seedData$datapath,
                                    seedRadius = as.numeric(input$seedRadius),
                                    deterministic = isDeterministic,
                                    isCropped = input$cropLev1,
                                    level1Names = input$level1List,
                                    DA = input$dataAssim,
                                    sitRepData = input$dataAssimZones$datapath,
                                    dataI = input$assimIData$datapath,
                                    dataD = input$assimDData$datapath,
                                    varCovarFunc = input$covarianceSelect,
                                    QVar = input$QVar,
                                    QCorrLength = input$QCorrLength,
                                    nbhd = input$nbhd,
                                    psiDiag = input$psidiag)

    row1  <- data.frame(Variable = "Country", Value = input$selectedCountry)
    row2  <- data.frame(Variable = "WorldPop Raster Dimension", Value = paste0(rs$nRows, " rows x ", rs$nCols, " columns = ", rs$nCells, " grid cells"))
    row3  <- data.frame(Variable = "Aggregation Factor", Value = input$agg)
    row4  <- data.frame(Variable = "Aggregated Raster Dimension", Value = paste0(nrow(rs$rasterStack), " rows x ", ncol(rs$rasterStack), " columns = ", ncell(rs$rasterStack), " grid cells"))
    row5  <- data.frame(Variable = "Compartmental Model", Value = input$modelSelect)
    row6  <- data.frame(Variable = "Model Parameters", Value = paste("Alpha:", alpha,"Beta:", beta,"Gamma:", gamma, "Sigma:", sigma,"Delta:", delta))
    row7  <- data.frame(Variable = "Average Distance Travelled/Day (in km)", Value = input$lambda)
    row8  <- data.frame(Variable = "Radius (1 = Moore neighbourhood)", Value = radius)
    row9  <- data.frame(Variable = "Uploaded Seed Data", Value = input$seedData$name)
    row10 <- data.frame(Variable = "Number of iterations (days)", Value = input$timestep)

    values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10)

    ## raster summary table ----
    output$summaryTable <- renderDT({
      datatable(values$df,
                rownames = FALSE,
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE))
    })

    #---------------------------------------#
    # Output seed plot image to the app UI  #
    #---------------------------------------#
    seedData <- reactive({
      if(globalValidator$is_valid()) read.csv(input$seedData$datapath, header = TRUE)
    })

    output$seedPlot <- renderLeaflet({
      if(globalValidator$is_valid())
        printCroppedBubbleSeedPlot(
          input$selectedCountry,
          input$seedData$datapath,
          level1Names = input$level1List,
          6) ## FIXME: What is six? Use an argument name.
    })

    #--------------------------------------------------------------------------#
    # Output the .mp4 video from www/ to the app UI                            #
    #--------------------------------------------------------------------------#
    ## mp4 video output ----
    output$outputVideo <- renderUI({
      tags$video(
        id = "video",
        type = "video/mp4",
        src = "MP4/Infected_MP4.mp4",  # TODO: dynamically change which mp4 is printed
        controls = "controls"
      )
    })
  })

  observeEvent(input$selectedCountry, {
    if(input$selectedCountry != "Democratic Republic of Congo") {
      updateSelectizeInput(
        inputId = "level1List",
        selected = ""
      )

      updateCheckboxInput(
        inputId = "cropLev1",
        value = FALSE
      )
    }
  })

  observeEvent(input$level1List, {
    level1Country(input$selectedCountry)
  })
  #--------------#
  # Tabset Panel #
  #--------------#
  observeEvent(input$resetAll,{
    hide(id = "tabsetContainer")
    fileInputs$smStatus <- 'reset'
  })

  observeEvent(!iv$is_valid(),{
    shinyjs::hide(id = "tabsetContainer")
  })

  observeEvent(input$go,{
    shinyjs::show(id = "tabsetContainer")
    updateTabsetPanel(inputId = 'tabSet', selected = 'Input Summary')
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  # output$downloadOutputSummary <- downloadHandler(
  #   filename = function() {"output.csv"},
  #   content = function(file){
  #     write.csv(data(), file, row.names = FALSE)
  #   }
  #
  # )

  observeEvent(is.null(input$seedData), {
    shinyjs::hide(id = "seedRadius")
  })

  observeEvent(input$seedData, {

  observeEvent(!globalValidator$is_valid(),{
    hide(id = "tabsetContainer")
  })

  observeEvent(input$go,{
    show(id = "tabsetContainer")
    updateTabsetPanel(inputId = 'tabSet', selected = 'Input Summary')
    runjs("window.scrollTo(0, 0)")
  })

  observeEvent(input$seedData, {
    if(is.null(input$seedData)) hide(id = "seedRadius")
  })

  observeEvent(input$seedData, {
    if(iv_seeddataupload$is_valid()) {
      hide(id = "downloadData")
      show(id = "seedRadius")
    } else {
      hide(id = "seedRadius")
      show(id = "downloadData")
    }
  })

}
