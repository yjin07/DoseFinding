

server <- function(input, output, session) {
  # observeEvent(input$responseType, {
  #   if (input$responseType == 'Continuous') {
  #     updateSelectInput(session, "model", label = NULL,
  #                 choices = list("Sigmoid-Emax" = "sEmax", "Emax" = "emax", "Exponential" = "expo", "Beta" = "beta", "Linear" = "linear", "Linear-Log"="linearLog",
  #                 "Logistic" = "logistic", "Quadratic" = "quad"))
  #   } else {
  #     updateSelectInput(session, "model", label = NULL,
  #                 choices = list("Logistic" = "logistic"))
  #   }
  # })


  # ! ---------------------
  # ! Tab: simulate
  # ! ---------------------
  generate_and_render_simulate_data(output)

  observeEvent(input$generate, {
    generate_and_render_simulate_data(output)
  })

  # ! ---------------------
  # ! Tab: real data
  # ! ---------------------
  generate_and_render_real_data(output)


  myData <- reactiveVal()

  observeEvent(input$upload, {
    uploadChecking(input, myData)
  })

  observeEvent(myData(), {
    df <- myData()
    print("Values in myData:")

    generate_and_render_real_data(output, data = df)
  })

}
