library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(markdown)

library(logger)
library(here)
library(dplyr)
library(ggplot2)
library(patchwork)
library(binom)
library(pROC)
library(ResourceSelection)
library(nlme)


source(here("R/utils", "fitERMod.R"))
source(here("R/utils", "fitDERMod.R"))
# source(here("R/utils", "fitTGIMod.R"))         # * Deprecated

# source(here("webapp/R", "simulate.R"))         # * only used in the development version with `simulate` section
source(here("R", "sampling.R"))
source(here("R", "visualization.R"))
source(here("R", "results-der.R"))
source(here("R", "bootstrap-der.R"))
source(here("R", "results-dr.R"))
source(here("R", "bootstrap-dr.R"))
source(here("R", "results-tgi.R"))



uploadChecking <- function(input, myData) {
  if (is.null(input$file1)) {
    showModal(modalDialog(
      title = HTML("<b style='color:red;'>Error</b>"),
      "No file uploaded. Please upload a file first.",
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    req(input$file1)
    ext <- tools::file_ext(input$file1$datapath)
    data_type <- input$data_type

    if (!((data_type == "csv" && ext == "csv") || 
          (data_type == "txt" && ext == "txt") ||
          (data_type == "rdss" && ext %in% c("rds", "rda", "rdata")))) {
      showModal(modalDialog(
        title = HTML("<b style='color:red;'>Error</b>"),
        "File type does not match the selected data type. Please upload a file with the correct extension.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    } 
    
    df <- NULL
    if (data_type == "csv" && ext == "csv") {
      df <- tryCatch({
        read.csv(input$file1$datapath, header = TRUE)
      }, error = function(e) {
        return(NULL)
      })
    } else if (data_type == "txt" && ext == "txt") {
      df <- tryCatch({
        read.table(input$file1$datapath, header = TRUE, sep = "\t")
      }, error = function(e) {
        return(NULL)
      })
    } else if (data_type == "rdss" && ext %in% c("rds", "rda", "rdata")) {
      if (ext == "rds") {
        df <- tryCatch({
          readRDS(input$file1$datapath)
        }, error = function(e) {
          return(NULL)
        })
      } else if (ext %in% c("rda", "rdata")) {
        load(input$file1$datapath)
        df <- get(ls()[ls() != "file1"])  # 假设只加载一个对象
      }
    }

    if (is.null(df)) {
      showModal(modalDialog(
        title = HTML("<b style='color:red;'>Error</b>"),
        "Error reading the file. Please ensure it is properly formatted and has a header.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }

    required_columns <- c("Dose", "Exposure", "Response")
    if (!all(required_columns %in% colnames(df))) {
      showModal(modalDialog(
        title = HTML("<b style='color:red;'>Error</b>"),
        paste("The uploaded file must contain the following columns:", 
              paste(required_columns, collapse = ", ")),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }

    if ((all(df$Response %in% c(0,1)) && input$responseType != "Binary") || 
        (!all(df$Response %in% c(0,1)) && input$responseType != "Continuous")) {
      showModal(modalDialog(
        title = HTML("<b style='color:red;'>Error</b>"),
        "Response type does not match the uploaded data. Please select the correct response type.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = HTML("<b style='color:green;'>Success</b>"),
        "File uploaded successfully.",
        easyClose = TRUE,
        footer = NULL
      ))
      log_info("File uploaded successfully.")
      myData(df)
    }
  }
}



uploadCheckingTGI <- function(input, myData) {
  if (is.null(input$file_tgi)) {
    showModal(modalDialog(
      title = HTML("<b style='color:red;'>Error</b>"),
      "No file uploaded. Please upload a file first.",
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    req(input$file_tgi)
    ext <- tools::file_ext(input$file_tgi$datapath)
    data_type <- input$data_type_tgi

    if (!((data_type == "csv" && ext == "csv") || 
          (data_type == "txt" && ext == "txt") ||
          (data_type == "rdss" && ext %in% c("rds", "rda", "rdata")))) {
      showModal(modalDialog(
        title = HTML("<b style='color:red;'>Error</b>"),
        "File type does not match the selected data type. Please upload a file with the correct extension.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    } 
    
    df <- NULL
    if (data_type == "csv" && ext == "csv") {
      df <- tryCatch({
        read.csv(input$file_tgi$datapath, header = TRUE)
      }, error = function(e) {
        return(NULL)
      })
    } else if (data_type == "txt" && ext == "txt") {
      df <- tryCatch({
        read.table(input$file_tgi$datapath, header = TRUE, sep = "\t")
      }, error = function(e) {
        return(NULL)
      })
    } else if (data_type == "rdss" && ext %in% c("rds", "rda", "rdata")) {
      if (ext == "rds") {
        df <- tryCatch({
          readRDS(input$file_tgi$datapath)
        }, error = function(e) {
          return(NULL)
        })
      } else if (ext %in% c("rda", "rdata")) {
        load(input$file_tgi$datapath)
        df <- get(ls()[ls() != "file1"])  # 假设只加载一个对象
      }
    }

    if (is.null(df)) {
      showModal(modalDialog(
        title = HTML("<b style='color:red;'>Error</b>"),
        "Error reading the file. Please ensure it is properly formatted and has a header.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }

    required_columns <- c("Y0", "TIME", "Y", "SUBJID")
    if (!all(required_columns %in% colnames(df))) {
      showModal(modalDialog(
        title = HTML("<b style='color:red;'>Error</b>"),
        paste("The uploaded file must contain the following columns:", 
              paste(required_columns, collapse = ", ")),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    } else {
      showModal(modalDialog(
        title = HTML("<b style='color:green;'>Success</b>"),
        "File uploaded successfully.",
        easyClose = TRUE,
        footer = NULL
      ))
      log_info("File uploaded successfully.")
      myData(df)
    }
  }
}
