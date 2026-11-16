# "Data sets" tab of the main app (app.R): edit a data set from a CSV file and
# save it to a data frame. data_load_tab() is the tab UI and data_load_server()
# its server logic; app.R places the tab and calls the server function.
#
# Data reaches the simulation in two explicit steps: "Save to data frame" keeps
# an edited data set, then "Confirm data for simulation" assembles every data
# set (saved edits replacing the file's rows), converts it to the shape f()
# reads (R/model_data_adapter.R) and shows a summary to confirm. app.R takes the
# confirmed data from this function's return value and passes it to f() when
# "Run simulation" is pressed; confirming alone never runs a simulation.
#
# data/model_data_default.csv is loaded at startup; "Data file (CSV)" loads a
# different file in the same format. Saved edits are cleared when a new file is loaded.
#
# Choose an entity and a data set, edit cells in the grid, then click "Save to
# data frame". Only the selected entity's rows are shown and saved. The saved
# data frame is shown below the grid, can be downloaded as CSV, and is assigned
# to the global environment under the data set's measure and entity names
# (e.g. gross_capex_GVW) for inspection after the app closes.
#
# Saved edits are kept per entity and data set while the app runs: reselecting a data set
# shows its last saved version. "Reset to original" discards them and reloads
# the data set from the file.
#
# The saved data frame table has three filters on columns 3 to 9: for each,
# pick a column, then one or more values. Filters apply in order, so each one
# only offers values left by the filters before it, and a column used by one
# filter can't be picked in another. This only filters what is shown; the data
# frame in the global environment and the CSV download keep every row.
#
# install.packages(c("shiny", "excelR"))
# shiny and excelR are loaded by app.R (or by test/data_load_test.R)

# A data file holds several data sets. A data set is the rows sharing a
# measure value. row_type marks each row as "field" (a column-name row) or
# "data". measure, entity and year keep their names; column_* and value_*
# columns are named by the data set's field rows, one per column: column_1
# holds the file's column name and column_2 the name to use. The value_*
# columns must always hold numbers.

# Entity values as dropdown choices; missing or empty values become "(blank)"
# (an empty choice would act as the dropdown's placeholder)
entity_key <- function(x) {
  key <- as.character(x)
  key[is.na(key) | key == ""] <- "(blank)"
  key
}

# Entity names in a data source (from read_source()): the entity values of
# data rows
entity_choices_of <- function(src) {
  sort(unique(entity_key(src$data$entity[src$row_type == "data"])))
}

# Data set names in a data source (from read_source()): the measure values
# of data rows, only those of entity if given
dataset_choices_of <- function(src, entity = NULL) {
  keep <- src$row_type == "data"
  if (!is.null(entity)) keep <- keep & entity_key(src$data$entity) == entity
  unique(src$data$measure[keep])
}

# Reads a data file. Returns list(data, row_type, name): data is the file
# without its row_type column, which is kept separately as row_type; name
# labels the file in messages. Stops with a reason if the file isn't in the
# expected format
read_source <- function(path, name) {
  # UTF-8-BOM: a byte order mark would otherwise end up in the first column name
  data <- read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  missing <- setdiff(c("row_type", "measure", "entity", "column_1", "column_2"), names(data))
  if (length(missing) > 0) stop("missing column(s) ", paste(missing, collapse = ", "))
  if (!all(data$row_type %in% c("field", "data"))) stop("row_type must be 'field' or 'data'")
  src <- list(data = data[names(data) != "row_type"], row_type = data$row_type, name = name)
  if (length(dataset_choices_of(src)) == 0) stop("no data rows found")
  src
}

# Default file, relative to the repo root (the app's working directory, as for
# app.R's source("./R/...") calls)
default_path <- "data/model_data_default.csv"
default_source <- if (file.exists(default_path)) read_source(default_path, "model_data_default.csv")

# Columns the saved data frame table can be filtered on (by position: year and
# column_1 to column_6), and the number of filters
filter_cols <- 3:9
n_filters <- 3

# Input ids of filter i's column and values dropdowns
filter_col_id <- function(i) paste0("filter_col_", i)
filter_values_id <- function(i) paste0("filter_values_", i)

# Column values as filter choices; missing values become "(blank)"
filter_key <- function(x) {
  key <- as.character(x)
  key[is.na(key)] <- "(blank)"
  key
}

alert <- function(...) modalDialog(..., title = "Alert", easyClose = TRUE)

# Why a value that as.numeric() rejected isn't a number
why_not_numeric <- function(x) {
  chars <- unique(strsplit(gsub("[0-9.,+-]", "", x), "")[[1]])
  if (length(chars) == 0) return("not a valid number (check decimal points and signs)")
  chars[chars == " "] <- "space"
  paste("non-numeric characters:", paste(chars, collapse = " "))
}

# Converts the value columns of df (positions of the value_* columns in
# file_names) to numeric, dropping thousands separators. row_labels says where
# each row came from, for the error list. Returns list(df, errors); errors is
# NULL or one row per cell that can't be converted
convert_values <- function(df, row_labels, file_names) {
  errors <- NULL
  for (j in grep("^value_", file_names)) {
    x <- trimws(as.character(df[[j]]))
    x[x == ""] <- NA
    num <- suppressWarnings(as.numeric(gsub(",", "", x)))
    bad <- which(!is.na(x) & is.na(num))
    if (length(bad) > 0) {
      errors <- rbind(errors, data.frame(
        row = row_labels[bad],
        column = sprintf("%s (%s)", names(df)[j], file_names[j]),
        value = x[bad],
        reason = vapply(x[bad], why_not_numeric, "", USE.NAMES = FALSE),
        stringsAsFactors = FALSE
      ))
    }
    df[[j]] <- num
  }
  list(df = df, errors = errors)
}

# Alert listing the cells convert_values() couldn't convert
conversion_alert <- function(errors, intro, max_rows = 50) {
  shown_errors <- head(errors, max_rows)
  alert(
    p(intro),
    tags$table(
      class = "table table-condensed",
      tags$thead(tags$tr(lapply(c("Row", "Column", "Value", "Reason"), tags$th))),
      tags$tbody(lapply(seq_len(nrow(shown_errors)), function(i) {
        tags$tr(lapply(unlist(shown_errors[i, ]), tags$td))
      }))
    ),
    if (nrow(errors) > max_rows) p(sprintf("...and %d more.", nrow(errors) - max_rows)),
    size = "l"
  )
}

# Non-value columns: let R pick the type (read.csv typed columns over the
# whole file, so retype for this data set alone)
retype_other_cols <- function(df, file_names) {
  other <- grep("^value_", file_names, invert = TRUE)
  df[other] <- lapply(df[other], type.convert, as.is = TRUE)
  df
}

# Data rows of one data set in src (from read_source()) for one entity, named
# from its field rows, with the value columns converted to numeric. Returns
# list(df, alert): df is NULL and alert says why if the field rows don't give
# each column_* and value_* column exactly one name, or a value cell can't be
# converted
get_dataset <- function(name, entity, src) {
  data <- src$data
  variable <- grep("^(column|value)_", names(data))
  fields <- data[src$row_type == "field" & data$measure == name, , drop = FALSE]
  # Field rows may be repeated (e.g. one set per entity); identical repeats are
  # fine, only a column given different names counts as a duplicate
  fields <- fields[!duplicated(fields[c("column_1", "column_2")]), , drop = FALSE]
  if (!setequal(fields$column_1, names(data)[variable]) || anyDuplicated(fields$column_1)) {
    missing <- setdiff(names(data)[variable], fields$column_1)
    unknown <- setdiff(fields$column_1, names(data)[variable])
    dupes <- unique(fields$column_1[duplicated(fields$column_1)])
    details <- c(
      if (length(missing)) sprintf("no field row for: %s", paste(missing, collapse = ", ")),
      if (length(unknown)) sprintf("field rows name unknown columns: %s", paste(unknown, collapse = ", ")),
      if (length(dupes)) sprintf("columns given different names: %s", paste(dupes, collapse = ", "))
    )
    msg <- sprintf("Data set '%s' not loaded: column header details missing (%s).",
                   name, paste(details, collapse = "; "))
    return(list(df = NULL, alert = alert(msg)))
  }
  nms <- names(data)
  nms[variable] <- fields$column_2[match(names(data)[variable], fields$column_1)]
  rows <- which(src$row_type == "data" & data$measure == name & entity_key(data$entity) == entity)
  df <- data[rows, , drop = FALSE]
  names(df) <- make.unique(nms)
  rownames(df) <- NULL

  # +1 for the header line
  conv <- convert_values(df, paste(src$name, "line", rows + 1), names(data))
  if (!is.null(conv$errors)) {
    intro <- sprintf("Data set '%s' not loaded: these value cells can't be converted to numbers.", name)
    return(list(df = NULL, alert = conversion_alert(conv$errors, intro)))
  }
  list(df = retype_other_cols(conv$df, names(data)), alert = NULL)
}

# Key under which a data set's saved edits are stored: entity and data set name
stored_key <- function(entity, name) paste(entity, name, sep = " / ")

# Every data set of every entity in src (from read_source()) as one data frame
# with the file's column names, ready for model_data_to_dat(). A data set's
# saved edits (stored_edits, a list keyed by stored_key()) replace its rows
# from the file. Returns list(data, summary, errors): summary has one row per
# entity and data set (rows, edited); errors lists data sets that couldn't be
# loaded, in which case data is NULL
assemble_model_data <- function(src, stored_edits = list()) {

  file_column_names <- names(src$data)
  parts   <- list()
  summary <- NULL
  errors  <- character(0)

  for (entity in entity_choices_of(src)) {

    for (dataset_name in dataset_choices_of(src, entity)) {

      key    <- stored_key(entity, dataset_name)
      edited <- !is.null(stored_edits[[key]])

      if (edited) {
        df <- stored_edits[[key]]
      } else {
        loaded <- get_dataset(dataset_name, entity, src)
        if (is.null(loaded$df)) {
          errors <- c(errors, key)
          next
        }
        df <- loaded$df
      }

      # get_dataset() renames column_* / value_* from the field rows; the
      # adapter wants the file's names back. Same count and order, only
      # row_type was dropped
      names(df) <- file_column_names
      parts[[key]] <- df

      summary <- rbind(summary, data.frame(
        entity   = entity,
        dataset  = dataset_name,
        rows     = nrow(df),
        edited   = if (edited) "yes" else "",
        stringsAsFactors = FALSE
      ))
    }
  }

  data <- if (length(errors) == 0) do.call(rbind, unname(parts)) else NULL
  if (!is.null(data)) rownames(data) <- NULL

  list(data = data, summary = summary, errors = errors)
}

# Tab UI, placed in app.R's navbarPage
data_load_tab <- function() {
  tabPanel(
    "Data sets",
    br(),
    fluidRow(
      column(
        12,
        # Collapsible: <details> starts closed; clicking the heading opens it
        # Bootstrap 3 makes <summary> a plain block, hiding the browser's arrow,
        # so draw one: right-pointing when closed, down-pointing when open
        tags$style(HTML(
          "#about summary { cursor: pointer; }",
          "#about summary h4 { display: inline; }",
          "#about summary::before { content: '\\25B8'; margin-right: 6px; font-size: 18px; }",
          "#about details[open] summary::before { content: '\\25BE'; }"
        )),
        wellPanel(
          id = "about",
          tags$details(
            tags$summary(
              h4("About this tab"),
              tags$small(class = "text-muted", "(click to show / hide)")
            ),
            p(
              "Load a CSV data file (model_data_default.csv by default, or choose another with",
              "\"Select data file (CSV)\"), pick an entity and a data set, edit cells in the",
              "grid and click \"Save to data frame\"."
            ),
            tags$ul(
              tags$li(
                "Only the selected entity's rows are shown in the grid, saved and",
                "inspected. \"Select data set\" lists the data sets that entity has."
              ),
              tags$li(HTML(
                "The saved data frame appears below the grid. It is also assigned in R",
                "under the data set's measure and entity names (e.g.",
                "<code>gross_capex_GVW</code>) and can be downloaded as CSV."
              )),
              tags$li(
                "Saved edits are kept for each entity and data set while the app runs.",
                "\"Reset to original\" reloads the data set from the file. Loading a new",
                "file clears saved edits."
              ),
              tags$li(
                "The saved table can be filtered with up to three filters on columns 3 to 9.",
                "Each filter only offers values left by the ones before it. Filtering only",
                "changes what is displayed."
              ),
              tags$li(
                "Data reaches the simulation only after \"Confirm data for simulation\": this",
                "assembles every entity's data sets (saved edits replacing the file's rows) and",
                "shows a summary of what will be used. Edits in the grid that haven't been saved",
                "are not included. Confirming doesn't run a simulation - press \"Run simulation\"",
                "as usual afterwards."
              )
            ),
            h4("Data file rules"),
            tags$ul(
              tags$li(HTML(
                "The header row has the same columns as model_data_default.csv: <code>entity</code>,",
                "<code>row_type</code>, <code>measure</code>, <code>year</code>,",
                "<code>column_1</code> to <code>column_6</code> and <code>value_1</code> to",
                "<code>value_4</code>."
              )),
              tags$li(HTML(
                "<code>row_type</code> is either <code>data</code> (a data row) or",
                "<code>field</code> (a column-name row)."
              )),
              tags$li(HTML(
                "<code>measure</code> names the data set in both kinds of row.",
                "<code>measure</code>, <code>entity</code> and <code>year</code> keep",
                "their names."
              )),
              tags$li(HTML(
                "Each measure needs exactly one <code>field</code> row for each of",
                "<code>column_1</code> to <code>column_6</code> and <code>value_1</code> to",
                "<code>value_4</code>: <code>column_1</code> holds the file's column name",
                "and <code>column_2</code> the name to use for it.",
                "If they're missing or the count is wrong, the app shows",
                "\"Column header details missing\"."
              )),
              tags$li(HTML(
                "<code>value_&lt;n&gt;</code> columns must be numeric. Thousands separators",
                "(e.g. <code>1,234.5</code>) are removed. Any other non-numeric value",
                "stops the data set loading, or an edit saving, and the cells at fault are",
                "listed."
              ))
            )
          )
        ),
        hr(),
        h4("Load / amend data"),
        fileInput(
          "data_file", "Select data file (CSV)", accept = c(".csv", "text/csv"),
          placeholder = if (is.null(default_source)) "No file selected" else default_source$name
        ),
        fluidRow(
          column(3, selectInput("entity_select", "Select entity", choices = entity_choices_of(default_source))),
          column(3, selectInput(
            "dataset_select", "Select data set",
            choices = dataset_choices_of(default_source, entity_choices_of(default_source)[1])
          ))
        ),
        p("Data set rows from the data file. Edit cells, then save to update the data frame."),
        # excelR puts jexcel's "jexcel" class (right and bottom borders, meant for
        # the table) on the full-width output div too, drawing lines across the page
        tags$style("#dataset.jexcel { border: none; }"),
        excelOutput("dataset", height = "auto"),
        br(),
        br(),
        actionButton("dataset_save", "Save to data frame", class = "btn-primary"),
        actionButton("dataset_clear", "Reset to original"),
        actionButton("dataset_confirm", "Confirm data for simulation", class = "btn-success"),
        br(),
        br(),
        textOutput("model_data_status"),
        br()
      )
    ),
    hr(),
    fluidRow(
      column(
        12,
        h4("Inspect data"),
        fluidRow(lapply(seq_len(n_filters), function(i) {
          column(
            2,
            selectInput(filter_col_id(i), paste("Filter", i, "column"), choices = "(none)"),
            selectizeInput(
              filter_values_id(i), paste("Filter", i, "values"), choices = NULL, multiple = TRUE,
              options = list(placeholder = "All values")
            )
          )
        })),
        tableOutput("dataset_table"),
        downloadButton("dataset_download", "Download CSV")
      )
    )
  )
}

# Tab server logic, called from app.R's server function
data_load_server <- function(input, output, session) {
  # The loaded data file (from read_source()), plus a version number that
  # changes each time a file is loaded
  data_source <- reactiveVal(if (!is.null(default_source)) c(default_source, version = 1))

  saved <- reactiveValues(dataset = NULL)

  # Incremented by the reset button to force the grid to re-render
  reset <- reactiveValues(dataset = 0)

  # The selected data set as loaded from the file
  orig <- reactiveVal(NULL)

  # Last saved version of each data set, keyed by stored_key()
  stored <- reactiveValues()

  # Data confirmed for the simulation: dat is in the shape f() reads, info
  # describes it for the status line. NULL until the user confirms
  confirmed <- reactiveValues(dat = NULL, info = NULL)

  # Assembled and converted data waiting in the confirmation modal
  pending <- reactiveVal(NULL)

  # TRUE when a file was loaded or edits saved / reset since the last confirm
  dirty <- reactiveVal(FALSE)

  # The version shown in the grid: the stored one if saved, else orig()
  shown <- reactiveVal(NULL)

  # input$dataset is only sent after an edit and isn't cleared on re-render,
  # so track whether it belongs to the data set currently shown
  edited <- reactiveVal(FALSE)

  # File version, entity and name of the data set in the grid, so it isn't
  # loaded twice when the dropdowns then report the same selection
  loaded_key <- reactiveVal(NULL)

  load_dataset <- function(entity, name) {
    src <- data_source()
    if (is.null(src) || !isTruthy(entity) || !isTruthy(name)) return()
    key <- paste(src$version, stored_key(entity, name))
    if (identical(key, loaded_key())) return()
    loaded_key(key)

    res <- get_dataset(name, entity, src)
    if (!is.null(res$alert)) showModal(res$alert)
    df <- res$df
    orig(df)
    skey <- stored_key(entity, name)
    if (!is.null(df) && !is.null(stored[[skey]])) df <- stored[[skey]]
    shown(df)
    saved$dataset <- df
    edited(FALSE)
  }

  # Refreshes the data set choices for entity, keeping the selected data set
  # if the entity has it, and loads it
  select_entity <- function(entity) {
    src <- data_source()
    if (is.null(src) || !isTruthy(entity)) return()
    choices <- dataset_choices_of(src, entity)
    current <- input$dataset_select
    selected <- if (isTRUE(current %in% choices)) current else choices[1]
    updateSelectInput(session, "dataset_select", choices = choices, selected = selected)
    load_dataset(entity, selected)
  }

  observeEvent(input$entity_select, select_entity(input$entity_select))
  observeEvent(input$dataset_select, load_dataset(input$entity_select, input$dataset_select))

  # A new data file replaces the current one: clear saved edits (they belong
  # to the old file), refresh the entity and data set choices, keeping the
  # selections if the new file has them, and load the data set
  observeEvent(input$data_file, {
    new_source <- tryCatch(
      read_source(input$data_file$datapath, input$data_file$name),
      error = function(e) e
    )
    if (inherits(new_source, "error")) {
      showModal(alert(sprintf("File '%s' not loaded: %s", input$data_file$name, conditionMessage(new_source))))
      return()
    }
    old_version <- if (is.null(data_source())) 0 else data_source()$version
    data_source(c(new_source, version = old_version + 1))
    for (nm in names(stored)) stored[[nm]] <- NULL
    dirty(TRUE)

    entities <- entity_choices_of(new_source)
    current <- input$entity_select
    entity <- if (isTRUE(current %in% entities)) current else entities[1]
    updateSelectInput(session, "entity_select", choices = entities, selected = entity)
    select_entity(entity)
  })

  observeEvent(input$dataset, edited(TRUE))

  output$dataset <- renderExcel({
    reset$dataset
    df <- shown()
    req(df)
    excelTable(
      data = df,
      columns = data.frame(
        title = names(df),
        type = ifelse(sapply(df, is.numeric), "numeric", "text"),
        width = 110
      ),
      allowInsertRow = TRUE
    )
  })

  # When req() stops the render, htmlwidgets hides the grid, and Shiny by
  # default stops updating hidden outputs, so the grid would never come back
  outputOptions(output, "dataset", suspendWhenHidden = FALSE)

  observeEvent(input$dataset_save, {
    req(edited(), input$dataset)
    df <- excel_to_R(input$dataset)
    names(df) <- names(orig())
    file_names <- names(data_source()$data)
    conv <- convert_values(df, paste("grid row", seq_len(nrow(df))), file_names)
    if (!is.null(conv$errors)) {
      showModal(conversion_alert(conv$errors, "Not saved: these value cells can't be converted to numbers."))
      return()
    }
    df <- retype_other_cols(conv$df, file_names)
    saved$dataset <- df
    # Not written to shown(): that would redraw the grid mid-edit
    stored[[stored_key(input$entity_select, input$dataset_select)]] <- df
    dirty(TRUE)
  })

  observeEvent(input$dataset_clear, {
    stored[[stored_key(input$entity_select, input$dataset_select)]] <- NULL
    dirty(TRUE)
    shown(orig())
    # Forces a redraw even when shown() was already the original
    reset$dataset <- reset$dataset + 1
    saved$dataset <- orig()
    edited(FALSE)
  })

  # --- Saved data frame outputs --------------------------------------------
  # Data frame / file name: measure and entity of the data set, e.g. gross_capex_GVW
  saved_name <- function() paste(input$dataset_select, input$entity_select, sep = "_")

  observeEvent(saved$dataset, {
    assign(isolate(saved_name()), saved$dataset, envir = .GlobalEnv)
  })

  # Rows of df left after applying filters 1 to k in turn
  apply_filters <- function(df, k) {
    for (i in seq_len(k)) {
      col <- input[[filter_col_id(i)]]
      vals <- input[[filter_values_id(i)]]
      if (isTruthy(col) && col %in% names(df) && length(vals) > 0) {
        df <- df[filter_key(df[[col]]) %in% vals, , drop = FALSE]
      }
    }
    df
  }

  for (i in seq_len(n_filters)) {
    local({
      i <- i
      others <- setdiff(seq_len(n_filters), i)

      # Column choices: columns 3 to 9 of this data set (names differ between
      # data sets), minus those picked in the other filters. Keeps the selected
      # column if it's still available
      observeEvent(list(names(saved$dataset), lapply(others, function(j) input[[filter_col_id(j)]])), {
        taken <- unlist(lapply(others, function(j) input[[filter_col_id(j)]]))
        available <- setdiff(names(saved$dataset)[filter_cols], taken)
        current <- input[[filter_col_id(i)]]
        selected <- if (isTRUE(current %in% available)) current else "(none)"
        # "(none)" rather than "": selectize treats an empty value as a
        # placeholder, so it couldn't be picked again once a column was chosen
        updateSelectInput(session, filter_col_id(i), choices = c("(none)", available), selected = selected)
      }, ignoreNULL = FALSE)

      # Value choices: values of the selected column in the rows left by the
      # earlier filters. Drops selected values that are no longer there
      observeEvent(list(
        saved$dataset,
        lapply(seq_len(i), function(j) input[[filter_col_id(j)]]),
        lapply(seq_len(i - 1), function(j) input[[filter_values_id(j)]])
      ), {
        df <- saved$dataset
        col <- input[[filter_col_id(i)]]
        if (is.null(df) || !isTruthy(col) || !col %in% names(df)) {
          updateSelectizeInput(session, filter_values_id(i), choices = character(0))
          return()
        }
        df <- apply_filters(df, i - 1)
        choices <- filter_key(sort(unique(df[[col]]), na.last = TRUE))
        selected <- intersect(input[[filter_values_id(i)]], choices)
        updateSelectizeInput(session, filter_values_id(i), choices = choices, selected = selected)
      }, ignoreNULL = FALSE)
    })
  }

  # Row numbers are the rows of the saved data frame (and grid), so filtered
  # rows keep their original numbers
  output$dataset_table <- renderTable({
    df <- saved$dataset
    if (!is.null(df)) df <- apply_filters(df, n_filters)
    df
  }, rownames = TRUE)

  output$dataset_download <- downloadHandler(
    filename = function() paste0(saved_name(), ".csv"),
    content = function(file) write.csv(saved$dataset, file, row.names = FALSE)
  )

  # --- Confirm data for the simulation ------------------------------------
  # Step 1: assemble and convert, then show what would be used and ask
  observeEvent(input$dataset_confirm, {
    src <- data_source()
    if (is.null(src)) {
      showModal(alert("No data file loaded."))
      return()
    }

    assembled <- assemble_model_data(src, reactiveValuesToList(stored))
    if (length(assembled$errors) > 0) {
      showModal(alert(
        p("Data not confirmed: these data sets can't be loaded (select each one to see why)."),
        tags$ul(lapply(assembled$errors, tags$li))
      ))
      return()
    }

    dat      <- model_data_to_dat(assembled$data)
    warnings <- dat_check_for_f(dat)
    pending(list(dat = dat, summary = assembled$summary, file = src$name))

    summary <- assembled$summary
    showModal(modalDialog(
      title = "Confirm data for simulation",
      p(sprintf("File: %s. %d rows across %d data sets.", src$name, nrow(dat), nrow(summary))),
      p("Saved edits replace the file's rows for the data sets marked as edited.",
        "Edits in the grid that haven't been saved are not included."),
      if (length(warnings) > 0) tagList(
        p(tags$strong("Warnings"), " - the simulation may fail with this data:"),
        tags$ul(lapply(warnings, tags$li))
      ),
      tags$table(
        class = "table table-condensed",
        tags$thead(tags$tr(lapply(c("Entity", "Data set", "Rows", "Edited"), tags$th))),
        tags$tbody(lapply(seq_len(nrow(summary)), function(i) {
          tags$tr(lapply(unlist(summary[i, ]), tags$td))
        }))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("dataset_confirm_ok", "Confirm", class = "btn-success")
      ),
      size = "l",
      easyClose = TRUE
    ))
  })

  # Step 2: the user confirmed - this is what "Run simulation" will use from now on
  observeEvent(input$dataset_confirm_ok, {
    ready <- pending()
    req(ready)
    confirmed$dat  <- ready$dat
    confirmed$info <- list(
      file        = ready$file,
      time        = Sys.time(),
      n_rows      = nrow(ready$dat),
      edited_keys = with(ready$summary, paste(entity, dataset, sep = " / ")[edited == "yes"])
    )
    pending(NULL)
    dirty(FALSE)
    removeModal()
  })

  status_text <- reactive({
    info <- confirmed$info
    if (is.null(info)) {
      return("No data confirmed for the simulation - press \"Confirm data for simulation\" on the Data sets tab.")
    }
    text <- sprintf(
      "Simulation data: %s, confirmed %s, %d rows, %d edited data set(s).",
      info$file, format(info$time, "%H:%M:%S"), info$n_rows, length(info$edited_keys)
    )
    if (dirty()) text <- paste(text, "Changes made since then are not confirmed.")
    text
  })

  # One output per place the status is shown (Shiny needs distinct output ids)
  output$model_data_status          <- renderText(status_text())
  output$model_data_status_scenario <- renderText(status_text())

  list(
    dat  = reactive(confirmed$dat),
    info = reactive(confirmed$info)
  )
}
