# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A regulatory financial model (R / Shiny) implementing the "Building Block Model" (BBM) used by the
Victorian Essential Services Commission to set water utility prices. Given opex/capex forecasts and
cost-of-capital assumptions, it builds a regulatory asset base (RAB) roll-forward, computes the
revenue requirement, solves for prices that recover that revenue over 5-year periods, and produces
financial statements, KPIs and price schedules. See `README.md` for the regulatory background and
the block-diagram of the RAB → revenue requirement → price flow.

## Running the app

This is an R project (`Building-Block.Rproj`), not a package — there is no build/lint/test tool chain
(no `devtools`, `testthat`, `renv`, or CI). Work happens interactively in RStudio or via `Rscript`.

Launch the Shiny app from the project root:
```r
shiny::runApp()
# or open app.R in RStudio and click "Run App"
```
`app.R` sources `R/funs.R`, `R/price_path_engine.R`, `R/f.R` and compiles `R/trgt_days_cpp.cpp` via
`Rcpp::sourceCpp(..., rebuild = TRUE)` on every start — a C++ toolchain must be available (Rtools on
Windows).

There is no automated test runner. "Tests" in `test/` are standalone scripts you `source()` and eyeball
the printed/plotted output against — run them individually, e.g.:
```r
source("test/price_path_engine_test.R")   # exercises R/price_path_engine.R's 4 solve modes
source("test/trgt_days_test.R")     # validates the trgt_days_cpp / trgt_days_fast Rcpp path
```
`test/` and the repo root also contain reference Excel/PDF workbooks (AER templates, LRMC models,
price submission models) used to hand-check outputs — these are inputs to check against, not code.

## Architecture

### Core simulation: `R/f.R` — function `f()`
This is the heart of the model. Given an entity's historical financials (`dat`) plus scenario
parameters (opex quantities/prices/growth, capex by future period, ROE, cost of debt, inflation,
tariff mix), `f()`:
1. Builds forward capex/opex from `dat` + scenario deltas
2. Depreciates the opening RAB and new capex (accounting and regulatory bases)
3. Rolls the RAB forward (opening + capex − depreciation = closing)
4. Computes the revenue requirement (return on RAB + regulatory depreciation + opex)
5. Solves for the price path needed to recover that revenue (delegates to
   `R/price_path_engine.R` for the NPV-matching optimisation), selected via `price_path_mode`
   (one of the five `pp_mode_*` modes below) — **`desired_fixed` (the fixed/variable tariff-mix
   reallocation) is currently a no-op**: the UI input and `f()` parameter both still exist, but
   since the migration to `R/price_path_engine.R` it only emits a warning and has no effect. It
   needs rebuilding as a proper `pp_mode_*` before it does anything again.
6. Runs a **month-by-month double-entry accounting simulation** (see below) to produce a full
   trial balance, cashflow and loan schedule
7. Returns a list: `txns` (the transaction cube), `rab`, `price_delta`, `prices`, `rev_req`,
   `tariff_rev`, `loans`, `call` (echo of scenario args)

`f()` is called once per scenario row; `app.R` fans it out over an `expand.grid()` of parameter
combinations via `mapply(...)` to support both A/B scenario comparison and sensitivity sweeps
(the sensitivity parameter gets a `c(lower, base, upper)` vector, everything else stays scalar).

### Double-entry accounting core: `mat` (account × transaction-type × month array)
Inside `f()`, `mat` is a 3D array `[account_code, txn_code, month]` — a full monthly general ledger.
- **Accounts** come from `data/chart.csv` (chart of accounts: statement type, account group,
  account number/description).
- **Transaction types** come from `data/txn_type.csv`: each `txn_code` defines up to ~10
  debit/credit account pairs (`dr`/`cr`, `dr1`/`cr1`, …) that a posting of that type touches.
  `drcr()` in `R/funs.R` looks up the dr/cr account indices for a given txn code.
- The simulation loop walks months, posting transactions into `mat` cell-by-cell (revenue,
  expenses, capex, depreciation, interest, borrowings/repayments against a loan schedule,
  target-days-based debtor/creditor balances), summing to a closing balance each month.
- **`trgt_days`** (`R/funs.R`) / **`trgt_days_fast`** (`R/trgt_days_cpp.cpp` via Rcpp) compute the
  transfer/receipt needed each month to hit a target debtor/creditor "days outstanding" — this is
  called once per account per month so it's implemented in C++ for speed. The R and C++
  implementations must stay behaviourally identical; `test/trgt_days_test.R` is the check.
- Downstream reporting functions in `R/funs.R` (`tb`, `cf`, `rab`, `revreq`, `slr_fun`, and the
  ratio functions `cash_int_cover_fn`, `gearing_fn`, `int_fin_ratio_fn`, `current_ratio_fn`,
  `ret_on_asset_fn`) all read back out of this `mat` cube / the `f()` result list by indexing on
  account codes and txn codes (grep on account-code prefixes like `"^3"` = assets, `"^1|^2"` =
  P&L, per `data/reference.csv`'s account grouping).

### Price path optimisation: `R/price_path_engine.R`
Standalone, dependency-light module (only needs `nloptr`) usable independently of `f()`/Shiny —
see `test/price_path_engine_test.R` for direct usage examples. Every "mode" reduces to: build a
tariff × year matrix of price deltas from a small parameter vector `theta` (`delta_builder`),
turn that into prices/revenue (`pp_build_prices`, `pp_simulate_revenue`), and require some
function of NPV revenue (`pp_npv`, with mid-year discounting) to hit a target — solved via
`nloptr` (COBYLA) in `pp_solve_mode()` as a feasibility problem, not a true optimisation (the
objective is flat; equality/inequality constraints do the work). Five modes are implemented:
1. `pp_mode_uniform_all_years` — one price delta applied to every tariff, every year
2. `pp_mode_uniform_single_year` — one delta applied once (compounds forward thereafter)
3. `pp_mode_group_shares` — per-service-group deltas hitting specified NPV revenue shares
4. `pp_mode_lrmc_residual` — variable tariffs pinned to a closed-form LRMC glide path, fixed
   tariffs solved as the residual against the total revenue requirement
5. `pp_mode_lrmc_group_shares` — variable tariffs pinned to a closed-form LRMC glide path, fixed
   tariffs solved per group (one shared delta per group) to hit specified NPV revenue shares

`pp_solve_mode()` explicitly checks constraint violation after solving and warns rather than
trusting solver status, since COBYLA can silently return an infeasible point when equality/
inequality constraints conflict.

**Tariff-naming contract**: modes 3/5 (`pp_mode_group_shares`/`pp_mode_lrmc_group_shares`) and
mode 4/5's LRMC pinning rely on `group_labels`/`variable_mask` being derived from tariff naming
(`R/f.R`: `sub("\\..*", "", rownames(p0))` / `grepl("Variable", rownames(p0))`) rather than
configured separately. For that to stay correct, every tariff name (built as
`service.asset_category.cost_driver`) must have: no `.` inside `service` (would truncate the
group early); `service` spelled identically, case-sensitively, across all of a group's rows; and
`cost_driver` exactly `"Fixed"` or containing `"Variable"`, with no other segment containing the
substring `"Variable"` (anything not matched as variable is treated as fixed — there's no third
category). See the comment above `group_labels`/`variable_mask` in `R/f.R` for the full detail.

### Shiny app: `app.R`
`navbarPage` with five tabs: scenario input + KPI plots (side-by-side Scenario A/B), price path
constraints (per-scenario mode selector + parameters for whichever `pp_mode_*` is selected, via
`renderPricePathInputs(prefix)`), a financials table (`plot_fins`, year-filterable), tariff plots,
and CSV downloads (trial balance, RAB, revenue requirement). Scenario A and B are fully parallel
reactive chains (`simA_inputs`/`argsA`/`simA` and the `simB_inputs`/`argsB`/`simB` mirror) — when
changing one side's logic, check whether the other side needs the same change. All scenario
inputs are namespaced by prefix (`"a_"` and `"b_"`) via `renderInputs(prefix)`/
`renderPricePathInputs(prefix)`; growth/rate inputs arrive as percentages from the UI and are
divided by 100 before being passed into `f()`.

**Gotcha — character scenario parameters through `expand.grid()`/`switch()`:** `argsA`/`argsB`
build `f()`'s arguments via `expand.grid(stringsAsFactors = FALSE, ...)`. That flag is required:
`expand.grid()` defaults to `stringsAsFactors = TRUE`, which turns a single-value character column
(e.g. `price_path_mode`) into a one-level factor — and `switch()` on a factor dispatches by the
factor's *integer code* (always `1` for a one-level factor), not its label, so every mode would
silently run as whichever branch is listed first, regardless of the UI selection. This exact bug
shipped once already (fixed by adding `stringsAsFactors = FALSE` to both `expand.grid()` calls
*and* wrapping `R/f.R`'s dispatch as `switch(as.character(price_path_mode), ...)` as a second line
of defence). Any new character-valued parameter added to `f()` that flows through this pipeline
needs the same two safeguards.

### Data flow
`get_data()` (`R/funs.R`) loads four CSVs from `data/` (or the GitHub raw URLs as a fallback
`src`): `price_subm_2023.csv` (per-entity capex/opex line items by year — the main scenario
input data, entities are Victorian water utilities e.g. `"CW"` = Coliban Water), `reference.csv`
(account group lookups/labels), `chart.csv` (chart of accounts with account-group balances),
`txn_type.csv` (transaction-type dr/cr account mapping, described above). `f()` currently hardcodes
`ent_parm <- "CW"` to select the entity from `dat`.

### File structure
When creating new functions, test files to follow this convention:
- function      >> ./R/***.R, 
- test function >> ./test/***_test.R  

### Coding Style: Readability Over Brevity
Optimize all code for a human reader debugging it months from now, not for token efficiency or line count.
- Use descriptive names; avoid abbreviations and clever one-liners.
- Break complex expressions into named intermediate variables.
- Structure functions into clear phases (validation, preparation, computation, output), with section-header comments where useful.
- Prefer explicit loops and control flow over dense functional chains (nested map/reduce) or deep nesting; use early returns instead.
- Comment on *why*, not what the syntax already shows.
- Use generous whitespace/indentation to signal structure.

When editing existing code: preserve its current style, make incremental/reviewable changes, and never refactor purely for conciseness.

When presenting code: briefly state the approach, show the code, then note any notable implementation decisions or readability trade-offs.

General rule: write production-quality, maintainable code — not code golf.


### Root-level loose files
`plots.R`, `price_path.R`, `scratch.R`, `temp1.R`, `temp2.R` at the repo root are standalone/scratch
scripts — not `source()`d by `app.R` or anything else. Don't assume they're wired into the app; check
before relying on functions defined there.
