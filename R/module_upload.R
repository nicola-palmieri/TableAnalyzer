# ===============================================================
# 🧪 Table Analyzer — Upload Module (Stable wide-format version)
# ===============================================================

upload_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 1 — Upload data"),
      p("Choose whether to load the example dataset or upload your own Excel file."),
      hr(),
      with_help_tooltip(
        radioButtons(
          ns("data_source"),
          label = "Data source",
          choices = c(
            "Upload (long format)" = "long",
            "Upload (wide format)" = "wide",
            "Example dataset" = "example"
          ),
          selected = "long"
        ),
        "Decide whether to explore the built-in example data or load your own table."
      ),
      uiOutput(ns("layout_example")),
      uiOutput(ns("file_input")),
      uiOutput(ns("sheet_selector")),
      uiOutput(ns("replicate_col_input")),
      actionButton(ns("open_type_editor"), "Edit column types…"),
      uiOutput(ns("type_editor_modal"))
      
    ),
    mainPanel(
      width = 8,
      h4("Data preview"),
      verbatimTextOutput(ns("validation_msg")),
      DTOutput(ns("preview"))
    )
  )
}


upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactiveVal(NULL)
    editable_cols <- reactiveVal(NULL)

    render_validation <- function(text) {
      output$validation_msg <- renderText(text)
    }

    safe_call <- function(fun, ...) {
      tryCatch(list(result = fun(...), error = NULL),
               error = function(e) list(result = NULL, error = e))
    }

    process_loaded_data <- function(data, success_message = NULL,
                                    error_prefix = "Error preparing data") {
      processed <- safe_preprocess_uploaded_table(data)
      if (!is.null(processed$error)) {
        render_validation(format_safe_error_message(error_prefix, processed$error))
        return(FALSE)
      }

      data <- processed$result
      df(data)
      output$preview <- renderDT(
        data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          columnDefs = list(
            list(targets = "_all", className = "dt-nowrap")
          )
        ),
        class = "display nowrap"
      )
      if (!is.null(success_message)) {
        render_validation(success_message)
      }
      TRUE
    }

    handle_safe_result <- function(safe_result, error_prefix, success_message = NULL,
                                   prepare_error_prefix = "Error preparing data") {
      if (!is.null(safe_result$error)) {
        render_validation(format_safe_error_message(error_prefix, safe_result$error))
        return(FALSE)
      }
      process_loaded_data(safe_result$result, success_message, prepare_error_prefix)
    }

    # -----------------------------------------------------------
    # 1️⃣ Handle source selection
    # -----------------------------------------------------------
    observeEvent(input$data_source, {
      df(NULL)
      editable_cols(NULL)
      output$type_selectors <- renderUI(NULL)
      output$sheet_selector <- renderUI(NULL)
      output$preview <- renderDT(data.frame())

      if (input$data_source == "example") {
        path <- "data/toy_animal_trial_data_long.xlsx"
        validate(need(file.exists(path), "⚠️ Example dataset not found in data folder."))
        safe_result <- safe_call(readxl::read_excel, path)
        if (!handle_safe_result(
          safe_result,
          "Error preparing example dataset",
          "📂 Loaded built-in example dataset.",
          "Error preparing example dataset"
        )) {
          return()
        }
      } else {
        render_validation("Please upload an Excel file.")
      }
    }, ignoreInit = FALSE)

    output$file_input <- renderUI({
      req(input$data_source)
      if (input$data_source == "example") {
        return(NULL)
      }

      with_help_tooltip(
        fileInput(
          ns("file"),
          "Upload Excel file (.xlsx / .xls / .xlsm)",
          accept = c(".xlsx", ".xls", ".xlsm")
        ),
        "Provide the Excel workbook that stores your study measurements."
      )
    })

    output$replicate_col_input <- renderUI({
      req(input$data_source)
      if (input$data_source != "wide") {
        return(NULL)
      }

      with_help_tooltip(
        tagList(
          textInput(
            ns("replicate_col"),
            label = "Replicate column name",
            value = "Replicate",
            placeholder = "Replicate"
          )
        ),
        "Choose the column name that will store the second header row when wide data is reshaped."
      )
    })
    
    # -----------------------------------------------------------
    # 2️⃣ Example layout preview
    # -----------------------------------------------------------
    output$layout_example <- renderUI({
      req(input$data_source %in% c("long", "wide"))
      long_path <- "data/toy_animal_trial_data_long.xlsx"
      wide_path <- "data/toy_animal_trial_data_wide.xlsx"
      validate(need(file.exists(long_path) && file.exists(wide_path),
                    "❌ Example layout files not found in /data folder."))
      
      loader <- if (input$data_source == "long") {
        list(path = long_path, caption = "Long format — one row per measurement.")
      } else {
        list(
          path = wide_path,
          caption = "Wide format — two header rows (top: response, bottom: replicate).",
          fix_names = TRUE
        )
      }

      toy_result <- safe_call(readxl::read_excel, loader$path, n_max = 5)
      validate(need(
        is.null(toy_result$error),
        format_safe_error_message("Error loading example layout", toy_result$error)
      ))

      toy <- toy_result$result
      if (isTRUE(loader$fix_names)) {
        bad <- grepl("^\\.\\.\\.[0-9]+$", names(toy))
        names(toy)[bad] <- ""
      }
      caption <- loader$caption
      
      DT::datatable(
        toy,
        caption = htmltools::tags$caption(htmltools::tags$b(caption)),
        options = list(dom = "t", scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
    
    # -----------------------------------------------------------
    # 3️⃣ File upload → detect sheets
    # -----------------------------------------------------------
    observeEvent(input$file, {
      req(input$data_source != "example")
      ext <- tolower(tools::file_ext(input$file$name))
      validate(need(ext %in% c("xlsx", "xls", "xlsm"),
                    "❌ Invalid file type. Please upload .xlsx/.xls/.xlsm."))
      
      sheets_result <- safe_call(readxl::excel_sheets, input$file$datapath)
      validate(need(is.null(sheets_result$error), "❌ No readable sheets found in workbook."))

      sheets <- sheets_result$result

      render_validation(paste("✅ File loaded:", input$file$name))
      output$sheet_selector <- renderUI(
        with_help_tooltip(
          selectInput(ns("sheet"), "Select sheet", choices = sheets),
          "Pick the worksheet inside your Excel file that contains the data."
        )
      )
    }, ignoreInit = TRUE)
    
    # -----------------------------------------------------------
    # 4️⃣ Load selected sheet (handles both long & wide)
    # -----------------------------------------------------------
    reprocess_wide_upload <- function() {
      req(input$file, input$sheet)

      replicate_col <- input$replicate_col
      if (is.null(replicate_col) || !nzchar(trimws(replicate_col))) {
        replicate_col <- "Replicate"
      } else {
        replicate_col <- trimws(replicate_col)
      }

      safe_result <- safe_convert_wide_to_long(
        input$file$datapath,
        sheet = input$sheet,
        replicate_col = replicate_col
      )

      handle_safe_result(
        safe_result,
        "Error converting wide format",
        "✅ Wide format reshaped successfully."
      )
    }

    observeEvent(list(input$sheet, input$data_source), {
      req(input$file, input$sheet, input$data_source != "example")
      path <- input$file$datapath

      if (input$data_source == "wide") {
        reprocess_wide_upload()
        return()
      }

      long_result <- safe_call(readxl::read_excel, path, sheet = input$sheet)
      if (!handle_safe_result(
        long_result,
        "❌ Error loading sheet",
        "✅ Long format loaded successfully."
      )) {
        return()
      }
    })

    observeEvent(input$replicate_col, {
      req(input$file, input$sheet, input$data_source == "wide")
      reprocess_wide_upload()
    }, ignoreInit = TRUE)
    
    # -----------------------------------------------------------
    # 5️⃣ Column Type Editor (New, Modal-based)
    # -----------------------------------------------------------
    
    # Which columns are editable
    editable_cols <- reactiveVal(NULL)
    
    # Open modal when clicking the button
    observeEvent(input$open_type_editor, {
      data <- df()
      req(data)
      
      num_vars <- names(data)[vapply(data, is.numeric, logical(1))]
      editable_cols(num_vars)
      
      output$type_editor_modal <- renderUI({
        showModal(
          modalDialog(
            title = "Column type editor",
            size = "l",
            easyClose = TRUE,
            footer = modalButton("Close"),
            div(
              style = "padding-right: 10px;",
              fluidRow(
                lapply(seq_along(num_vars), function(i) {
                  col <- num_vars[[i]]
                  column(
                    width = 6,
                    selectInput(
                      ns(paste0("type_", col)),
                      label = col,
                      choices = c("Numeric", "Categorical"),
                      selected = if (is.numeric(data[[col]])) "Numeric" else "Categorical",
                      width = "100%"
                    )
                  )
                })
              )
            )
            
          )
        )
      })
    })
    
    
    # -----------------------------------------------------------
    # 6️⃣ Apply user type edits reactively
    # -----------------------------------------------------------
    observe({
      data <- df()
      cols <- editable_cols()
      req(data, cols)
      for (col in cols) {
        sel <- input[[paste0("type_", col)]] %||% "Numeric"
        if (sel == "Categorical") {
          data[[col]] <- auto_factor_order(as.character(data[[col]]))
        } else {
          data[[col]] <- suppressWarnings(as.numeric(as.character(data[[col]])))
        }
      }
      df(data)
    })
    
    # -----------------------------------------------------------
    # ✅ Return reactive data
    # -----------------------------------------------------------
    df
  })
}
