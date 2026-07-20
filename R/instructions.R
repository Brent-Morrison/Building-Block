# -----------------------------------------------------------------------------------------------
# UI content for the "Instructions" tab (in-app user guide).
#
# Each guide topic is a separate function returning a tagList, so new topics can be added
# independently over time. instructions_tab() assembles all topics into the tab body.
# -----------------------------------------------------------------------------------------------


# Loan schedule ----------------------------------------------------------------------------------

instructions_loan_schedule <- function() {

  tagList(

    tags$p(
      "The model maintains a monthly loan schedule covering both the entity's existing debt ",
      "(the starting loan portfolio) and any new debt the model draws automatically during the ",
      "simulation to fund cash shortfalls."
    ),

    tags$h4("Existing loan portfolio"),
    tags$p(
      "Existing loans are read from the input CSV as rows with ",
      tags$code("lookup_key1 == \"loans\""), ". Each row is one loan:"
    ),
    tags$ul(
      tags$li(tags$code("lookup_key2"), " is the loan identifier (e.g. ", tags$code("loan1"), ")."),
      tags$li(tags$code("ref_value2"), " is the maturity date, encoded as ", tags$code("YYYYMM"),
              " (e.g. ", tags$code("202703"), " = March 2027)."),
      tags$li(tags$code("ref_value3"), " is the annual interest rate, as a percentage (e.g. ",
              tags$code("3.53"), " = 3.53%)."),
      tags$li(tags$code("ref_value4"), " is the loan principal."),
      tags$li(tags$code("lookup_key3"), ", ", tags$code("lookup_key4"), " and ",
              tags$code("ref_value1"), " are not used for loan rows and should be left as ",
              tags$code("na"), ".")
    ),
    tags$p(
      tags$strong("Assumption: "),
      "the CSV only records each loan's maturity date, not when it was drawn. The model assumes ",
      "every existing loan has a 10-year tenor and derives its start date as ",
      tags$em("maturity date minus 10 years"), ". If an entity's actual loans run for a different ",
      "term, the starting loan balance profile will not reflect that — see the TO DO note in ",
      tags$code("R/f.R"), " next to where the loan portfolio is built."
    ),

    tags$h4("Interest accrual and payment"),
    tags$p(
      "Interest is accrued monthly on each loan's outstanding balance using an actual/365 ",
      "day-count: ", tags$em("monthly interest = balance × rate ÷ 365 × days in month"),
      ". Accrued interest is paid in cash quarterly."
    ),

    tags$h4("Automatic new borrowing"),
    tags$p(
      "Each simulated month, if the model's cash balance would otherwise fall below the ",
      "configured cash buffer, it automatically draws a new 10-year loan sized to restore the ",
      "buffer. This lets the simulation keep running without manual intervention when a ",
      "scenario's opex/capex/price settings would otherwise put the entity into overdraft."
    ),
    tags$p(
      tags$strong("Known limitation: "),
      "the interest rate applied to these automatically drawn loans is currently a fixed 4% ",
      "placeholder rather than a configurable input, so scenario results that trigger automatic ",
      "borrowing will reflect that placeholder rate rather than a rate you have chosen."
    ),

    tags$h4("Repayment"),
    tags$p(
      "Each loan's balance runs off in the month it matures, and the outstanding balance is ",
      "repaid as a lump sum in cash the following month. This applies equally to existing loans ",
      "from the CSV and to loans the model draws automatically during the simulation."
    )
  )
}


# Input data: price_subm_2023.csv format ----------------------------------------------------------

instructions_csv_format <- function() {

  column_reference <- data.frame(
    Column      = c("entity", "year", "lookup_key1", "lookup_key2", "lookup_key3", "lookup_key4",
                     "ref_value1", "notes", "ref_value2", "ref_value3", "ref_value4"),
    Meaning     = c(
      "Water entity code (e.g. \"CW\" = Coliban Water).",
      "Financial year the row applies to.",
      "What kind of line item the row represents (formerly \"balance_type\") — see the table below.",
      "Meaning depends on lookup_key1: a capex/opex service category, or (for loans) the loan identifier.",
      "Meaning depends on lookup_key1: an asset category, or \"na\" where not applicable.",
      "Meaning depends on lookup_key1: a cost driver / tariff type (\"Fixed\" or contains \"Variable\" for Price/Quantity rows), or \"na\" where not applicable.",
      "Meaning depends on lookup_key1: the year an asset became operational, or \"na\"/\"999\" where not applicable.",
      "Free-text label, used by rab_book_value/rab_remaining_life rows to identify the matching asset group.",
      "Meaning depends on lookup_key1: an asset regulatory life in years, or (for loans) the maturity date as YYYYMM.",
      "Meaning depends on lookup_key1: an asset tax life in years, or (for loans) the interest rate as a percentage.",
      "The dollar (or, for Quantity rows, physical unit) value of the line item."
    ),
    stringsAsFactors = FALSE
  )

  row_type_reference <- data.frame(
    row_type = c(
      "gross_capex", "cust_cont", "gov_cont", "disp_proceeds", "rab_book_value",
      "rab_remaining_life", "loans", "Price", "Quantity", "opex categories*"
    ),
    key_columns = c(
      "lookup_key2, lookup_key3, lookup_key4, ref_value1, ref_value2, ref_value3, ref_value4",
      "lookup_key2, lookup_key3, lookup_key4, ref_value1, ref_value2, ref_value3, ref_value4",
      "lookup_key2, lookup_key3, lookup_key4, ref_value1, ref_value2, ref_value3, ref_value4",
      "ref_value4 only",
      "notes (asset group label), ref_value4",
      "notes (asset group label), ref_value4",
      "lookup_key2 (loan id), ref_value2 (maturity YYYYMM), ref_value3 (rate %), ref_value4 (principal)",
      "lookup_key2, lookup_key3, lookup_key4, ref_value4",
      "lookup_key2, lookup_key3, lookup_key4, ref_value4",
      "lookup_key2, ref_value4"
    ),
    not_applicable_convention = c(
      "n/a (all fields used)", "n/a (all fields used)", "n/a (all fields used)",
      "empty field", "empty field", "empty field", "lowercase \"na\"",
      "sentinel value 999", "sentinel value 999", "literal string \"NA\""
    ),
    example = c(
      "GVW,2024,gross_capex,Sewerage,Pipelines/network,Growth,2023-24,,90,90,3.38",
      "GVW,2024,cust_cont,Sewerage,Pipelines/network,Growth,Ongoing,,90,90,1.49",
      "SEW,2024,gov_cont,Recycled Water,Pipelines/network,Growth,2025-26,,50,50,12.529",
      "CW,2024,disp_proceeds,,,,,,,,2.15",
      "SEW,2023,rab_book_value,,,,,Buildings,,,91.87",
      "SEW,2023,rab_remaining_life,,,,,Buildings,,,38.28",
      "CW,2023,loans,loan1,na,na,na,,202703,3.53,50000",
      "CW,2023,Price,Recycled Water,Non-residential,Fixed,999,,999,999,942.01",
      "CW,2023,Quantity,Recycled Water,Non-residential,Fixed,999,,999,999,77",
      "GVW,2024,Corporate,Water,NA,NA,NA,,NA,NA,11.23"
    ),
    stringsAsFactors = FALSE
  )

  tagList(

    tags$p(
      "The main scenario input data is ", tags$code("data/price_subm_2023.csv"), ", loaded by ",
      tags$code("get_data()"), " into ", tags$code("dat_df"), " — this is the same object ",
      tags$code("f()"), " receives as its ", tags$code("dat"), " parameter. It holds per-entity ",
      "capex/opex/tariff/loan line items, one row per item per year. Its columns are named ",
      "generically (", tags$code("lookup_key1-4"), ", ", tags$code("ref_value1-4"), ") because ",
      "what each one holds depends on the row's ", tags$code("lookup_key1"), " (row type) — see ",
      "the tables below for what each column means for a given row type."
    ),

    tags$h4("Columns"),
    HTML(
      knitr::kable(column_reference, format = "html", row.names = FALSE,
                   table.attr = 'class="table table-striped table-bordered"') %>%
        kableExtra::kable_styling(full_width = FALSE, font_size = 12)
    ),

    tags$h4("lookup_key1 (row type) values"),
    tags$p(
      "Several of the columns above are overloaded — their meaning depends on the row's ",
      tags$code("lookup_key1"), ". This table shows which columns matter for each row type, along ",
      "with a real example row."
    ),
    HTML(
      knitr::kable(row_type_reference, format = "html", row.names = FALSE,
                   col.names = c("lookup_key1 value", "Key columns", "\"Not applicable\" shown as", "Example row"),
                   table.attr = 'class="table table-striped table-bordered"') %>%
        kableExtra::kable_styling(full_width = FALSE, font_size = 12)
    ),
    tags$p(
      tags$em(
        "* opex categories: Corporate, Treatment, Operations & Maintenance, Customer Service and ",
        "billing, Licence Fees, Other operating expenditure, External bulk charges (excl. ",
        "temporary purchases), GSL Payments, Environment Contribution."
      )
    ),

    tags$p(
      tags$strong("Important: "),
      "there is no single \"not applicable\" convention across the whole file — it varies by ",
      tags$code("lookup_key1"), " as shown above (an empty field, the literal string ",
      tags$code("\"NA\""), ", the sentinel value ", tags$code("999"), ", or lowercase ",
      tags$code("\"na\""), "). When adding rows, match the convention used by that row's own ",
      tags$code("lookup_key1"), ", not a blanket rule."
    ),

    tags$p(
      "For ", tags$code("Price"), " and ", tags$code("Quantity"), " rows specifically, the ",
      "combination of ", tags$code("lookup_key2"), "/", tags$code("lookup_key3"), "/",
      tags$code("lookup_key4"), " must follow the tariff-naming contract documented in ",
      tags$code("CLAUDE.md"), " (lookup_key2 values must not contain a \".\", must be spelled ",
      "identically across a tariff group, and lookup_key4 must be exactly \"Fixed\" or contain ",
      "\"Variable\") — this drives how the price path engine groups and prices tariffs."
    )
  )
}


# Tab assembly -------------------------------------------------------------------------------------

instructions_tab <- function() {

  fluidPage(
    theme = "simplex.min.css",

    tags$h2("Instructions"),
    tags$p("A user guide to how the model works and how to prepare its input data."),
    hr(),

    tags$h3("Loan schedule"),
    instructions_loan_schedule(),

    hr(),

    tags$h3("Input data: price_subm_2023.csv format"),
    instructions_csv_format()
  )
}
