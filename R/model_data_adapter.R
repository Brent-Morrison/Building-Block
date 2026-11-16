# -----------------------------------------------------------------------------------------------
# Adapter from the "model data" CSV schema (data/model_data_default.csv, loaded and edited on the
# "Data sets" tab in R/data_load.R) to the data frame shape f() in R/f.R reads.
#
# f() was written against data/price_subm_2023.csv, whose columns are
#   entity | year | lookup_key1 | lookup_key2 | lookup_key3 | lookup_key4 | ref_value1 | notes |
#   ref_value2 | ref_value3 | ref_value4
# and filters on lookup_key1 (the row type: "gross_capex", "Price", "loans", ...). The model data
# file carries the same information under generic column names:
#   entity | row_type | measure | year | column_1 .. column_6 | value_1 .. value_4
#
# Rather than rewriting every filter in f(), model_data_to_dat() maps the new columns onto the old
# ones (a positional mapping, verified row-for-row between the two default files):
#
#   model data        f() data frame
#   ---------------   --------------
#   measure           lookup_key1
#   entity, year      entity, year
#   column_1 / 2 / 3  lookup_key2 / 3 / 4   (tariff names are lookup_key2.lookup_key3.lookup_key4)
#   column_4          ref_value1            (capex: year operational, "2023-24" / "Ongoing")
#   column_5          notes
#   value_1 / 2 / 3   ref_value2 / 3 / 4    (note the shift by one)
#   column_6, value_4 dropped
#
# One structural difference: the model data file may hold the opening regulatory asset base as a
# single "rab" row per entity (value_2 = book value, value_3 = remaining life), whereas f() reads
# two rows, lookup_key1 == "rab_book_value" and "rab_remaining_life". The adapter splits each
# "rab" row into those two. Entities that already use the two-row form pass through unchanged.
#
# Pure functions, no Shiny dependency. Tested by test/model_data_adapter_test.R.
# -----------------------------------------------------------------------------------------------


# Columns the model data must have (after the Data sets tab has dropped row_type) and the f()
# columns they become, in get_data()'s column order
model_data_required_columns <- c(
  "entity", "measure", "year",
  "column_1", "column_2", "column_3", "column_4", "column_5",
  "value_1", "value_2", "value_3"
)

dat_column_order <- c(
  "entity", "year",
  "lookup_key1", "lookup_key2", "lookup_key3", "lookup_key4",
  "ref_value1", "notes",
  "ref_value2", "ref_value3", "ref_value4"
)

# lookup_key1 values f() filters on (R/f.R). "rab_book_value" and "rab_remaining_life" are
# produced by splitting "rab" rows when the file uses that form. The optional ones are summed
# by year in f() and an entity may simply have none (e.g. CW has no gov_cont rows); the
# required ones are read as single values or matrices and f() fails without them
dat_lookup_key1_required_by_f <- c(
  "gross_capex", "rab_book_value", "rab_remaining_life", "Price", "Quantity", "loans"
)
dat_lookup_key1_optional_for_f <- c("cust_cont", "gov_cont", "disp_proceeds")
dat_lookup_key1_used_by_f      <- c(dat_lookup_key1_required_by_f, dat_lookup_key1_optional_for_f)


# read.csv() leaves empty text cells as "" rather than NA, and f()'s filters were written against
# that. Keep the same convention so e.g. lookup_key2 == "" behaves the same for both sources
as_text_column <- function(x) {
  text <- as.character(x)
  text[is.na(text)] <- ""
  text
}


# Converts model data rows (data rows only, with the file's column names) to the data frame shape
# f() reads. Stops if a required column is missing
model_data_to_dat <- function(model_data) {

  # --- Validation ----------------------------------------------------------------------------
  missing_columns <- setdiff(model_data_required_columns, names(model_data))
  if (length(missing_columns) > 0) {
    stop("model data is missing column(s): ", paste(missing_columns, collapse = ", "))
  }

  # --- Column mapping ------------------------------------------------------------------------
  dat <- data.frame(
    entity      = as_text_column(model_data$entity),
    year        = as.integer(model_data$year),
    lookup_key1 = as_text_column(model_data$measure),
    lookup_key2 = as_text_column(model_data$column_1),
    lookup_key3 = as_text_column(model_data$column_2),
    lookup_key4 = as_text_column(model_data$column_3),
    ref_value1  = as_text_column(model_data$column_4),
    notes       = as_text_column(model_data$column_5),
    ref_value2  = suppressWarnings(as.numeric(model_data$value_1)),
    ref_value3  = suppressWarnings(as.numeric(model_data$value_2)),
    ref_value4  = suppressWarnings(as.numeric(model_data$value_3)),
    stringsAsFactors = FALSE
  )

  # --- Split single "rab" rows into the two rows f() reads -----------------------------------
  is_rab_row <- dat$lookup_key1 == "rab"

  if (any(is_rab_row)) {
    book_value_rows <- dat[is_rab_row, , drop = FALSE]
    book_value_rows$lookup_key1 <- "rab_book_value"
    book_value_rows$ref_value4  <- book_value_rows$ref_value3     # value_2 = book value

    remaining_life_rows <- dat[is_rab_row, , drop = FALSE]
    remaining_life_rows$lookup_key1 <- "rab_remaining_life"       # value_3 = remaining life,
                                                                  # already in ref_value4
    dat <- rbind(dat[!is_rab_row, , drop = FALSE], book_value_rows, remaining_life_rows)
  }

  # --- Output ----------------------------------------------------------------------------------
  dat <- dat[, dat_column_order]
  rownames(dat) <- NULL
  dat
}


# Checks a converted data frame has what f() needs for one entity. Returns a character vector of
# problems, empty when none. These are shown as warnings before the data is confirmed for use in
# the simulation - they don't block it, because f() may be run for an entity that is only
# partially populated while data is being built up.
#
# base_year is the year f() reads opening prices, quantities and the RAB from (R/f.R uses
# initial_fcast_yr - 1 = 2023)
dat_check_for_f <- function(dat, entity = "CW", base_year = 2023) {

  problems <- character(0)
  entity_rows <- dat[dat$entity == entity, , drop = FALSE]

  if (nrow(entity_rows) == 0) {
    return(sprintf("no rows for entity '%s'", entity))
  }

  # --- Row types f() filters on ----------------------------------------------------------------
  missing_row_types <- setdiff(dat_lookup_key1_required_by_f, unique(entity_rows$lookup_key1))
  if (length(missing_row_types) > 0) {
    problems <- c(problems, sprintf(
      "entity '%s' has no rows with lookup_key1 / measure: %s",
      entity, paste(missing_row_types, collapse = ", ")
    ))
  }

  # --- Base-year rows: prices, quantities and the opening RAB ----------------------------------
  base_year_rows <- entity_rows[entity_rows$year == base_year, , drop = FALSE]

  for (row_type in c("Price", "Quantity")) {
    if (sum(base_year_rows$lookup_key1 == row_type) == 0) {
      problems <- c(problems, sprintf("entity '%s' has no '%s' rows in %d", entity, row_type, base_year))
    }
  }

  for (row_type in c("rab_book_value", "rab_remaining_life")) {
    n_rows <- sum(base_year_rows$lookup_key1 == row_type)
    if (n_rows == 0) {
      problems <- c(problems, sprintf("entity '%s' has no '%s' row in %d", entity, row_type, base_year))
    } else if (n_rows > 1) {
      problems <- c(problems, sprintf(
        "entity '%s' has %d '%s' rows in %d; f() expects one", entity, n_rows, row_type, base_year
      ))
    }
  }

  # --- Numeric values ----------------------------------------------------------------------------
  n_missing_values <- sum(is.na(entity_rows$ref_value4[entity_rows$lookup_key1 %in% dat_lookup_key1_used_by_f]))
  if (n_missing_values > 0) {
    problems <- c(problems, sprintf(
      "entity '%s' has %d row(s) used by f() with an empty or non-numeric value_3 / ref_value4",
      entity, n_missing_values
    ))
  }

  problems
}
