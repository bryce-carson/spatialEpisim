rules <- tribble(~expr, ~ruleList,
  expression(input$modelSelect == "SVEIRD"),
  list(alpha = sv_between(0, 1)),

  expression(input$cropLev1),
  list(level1List = c()),

  expression(input$appMode == "Visualizer"),
  list(seedData = c({
    ~ if (is.null(fileInputs$smStatus) || fileInputs$smStatus == "reset") "Required"
  })),

  expression(input$appMode == "Simulator"),
  list(
    latLonData = c({
      ~ if (is.null(fileInputs$latLonStatus) || fileInputs$incidenceStatus == "reset") "Required"
    }),
    incidenceData = c({
      ~ if (is.null(fileInputs$incidenceStatus) || fileInputs$incidenceStatus == "reset") "Required"
    })
  ),

  NA,
  list(
    beta = c(sv_between(0, 1, c(FALSE, TRUE))),
    gamma = c(sv_between(0, 1, c(FALSE, TRUE))),
    simga = c(sv_between(0, 1)),
    delta = c(sv_between(0, 1)),
    lambda = c(sv_integer(), sv_between(0, 1, c(FALSE, TRUE))),
    timestep = c(sv_integer(), sv_gt(0)),
    date = c(),
    selectedCountry = c()
  )
)
