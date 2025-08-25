# Building-Block

## Background
The building block model (BBM) is a regulatory framework that ensures a firm earns a revenue stream with a present value equal to the present value of its expenditure stream[^1].

The Essential Services Commission (ESC) in Victoria regulates water businesses using the BBM to determine the maximum allowable revenue that a water utility can recover from customers through tariffs.

In the context of Victorian water utilities, the BBM is a forward-looking cost-of-service framework that ensures that utilities can recover efficient costs of providing water and sewerage services (including a fair return on investment), while protecting customers from monopoly pricing.

## Project
This project prepares a regulatory financial model that implements the BBM and returns comprehensive financial projections, pricing schedules and key performance indicators.  

The model takes account of uncertainty performing Monte Carlo simulations and/or scenario analysis for numerous parameters (eg., interest rates, customer growth, capital expenditure).  

Modelling steps are as follows: 

1. Load entity initial period financial statements 
2. Process future capex and opex data with scenario adjustments
3. Calculate asset depreciation schedules
4. Builds a regulatory asset base (RAB) reconciliation
5. Computes revenue requirements (return + opex + depreciation)
6. Determines prices to match revenue requirements across 5-year periods
7. Process non-regulated income and expenditure
8. Return comprehensive financial projections, pricing schedules and KPI's

The regulatory building block component of this is represented below:

```mermaid
flowchart TD
    A(Opening RAB) -->B(Depreciation on<br/>opening RAB)
    C(Regulatory capex) -->D(Depreciation<br/>on capex)
    I(Regulatory<br>opex) ----> J(Revenue requirement) 
    B --> E(Closing RAB)
    D --> E
    A --> E
    C --> E
    B ----> H(Regulatory<br>depreciation)
    D ----> H
    E --> F(Regulatory<br>rate of return)
    F --> G(Return on RAB)
    H ----> J(Revenue requirement) 
    G ----> J
    J -.-> K(Prices)
    J -.-> L(Quantities)
```

## Other uses
While this project focuses on Victorian water utilities regulated by the ESC, the BBM is the standard regulatory framework used in many regulated infrastructure industries internationally. 

The table below (courtesy of ChatGPT), summarises application of the BBM across industries and jurisdictions.

| **Jurisdiction / Sector**                   | **Regulator(s)**                                               | **Application of BBM**                                 | **Key Features / Differences**                                                                                                                                        |
| ------------------------------------------- | -------------------------------------------------------------- | ------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Victoria - Water**                        | Essential Services Commission (ESC)                            | Price determinations for urban & rural water utilities | PREMO framework (Performance, Risk, Engagement, Management, Outcomes) adds incentive overlay; RAB indexed to CPI                                                      |
| **Australia - Energy Networks**             | Australian Energy Regulator (AER)                              | Transmission & distribution of electricity and gas     | Extensive incentive schemes: Efficiency Benefit Sharing Scheme (EBSS), Capital Expenditure Sharing Scheme (CESS), Service Target Performance Incentive Scheme (STPIS) |
| **Australia - Transport (Rail & Airports)** | State access regulators; ACCC (airports in past)               | Rail access charges, airport services                  | BBM applied to access pricing; some regimes have light-handed monitoring rather than full revenue caps                                                                |
| **Australia - Telecoms (historical)**       | ACCC                                                           | Fixed-line access services (Telstra)                   | BBM applied pre-NBN; now moving towards different wholesale frameworks                                                                                                |
| **UK - Water (Ofwat)**                      | Ofwat                                                          | Water and sewerage utilities                           | RPI-X / CPI-X regulation; strong use of outcome delivery incentives (ODIs)                                                                                            |
| **UK - Energy (Ofgem)**                     | Ofgem                                                          | Electricity & gas networks                             | RIIO model (Revenue = Incentives + Innovation + Outputs) builds on BBM with strong innovation incentives                                                              |
| **UK - Telecoms (Ofcom)**                   | Ofcom                                                          | Wholesale access pricing                               | BBM applied to Openreach network (BT Group); hybrid with competitive elements                                                                                         |
| **New Zealand - Energy**                    | Commerce Commission                                            | Electricity distribution & transmission, gas pipelines | BBM introduced in 2010; similar to AER model but adapted to smaller market                                                                                            |
| **European Union - Energy & Water**         | National regulators (e.g., ERSE in Portugal, CRE in France)    | Energy networks, water pricing                         | EU requires transparent cost-based methods; most adopt BBM/RAB regulation                                                                                             |
| **United States - Energy, Water, Telecoms** | State Public Utility Commissions (PUCs), FERC (federal energy) | Rate-of-return regulation (historical test years)      | Functionally similar to BBM but **ex post**; based on historic costs rather than forecast efficiency                                                                  |
| **Canada - Energy & Water**                 | Provincial utility boards                                      | Rate-of-return regulation                              | Similar to US; ex post, less use of incentive schemes                                                                                                                 |
| **Latin America (Chile, Peru, Brazil)**     | Sectoral regulators                                            | Electricity & water utilities                          | BBM applied; often ex ante and benchmarked against "model company" assumptions                                                                                        |
| **Asia (Singapore, Hong Kong, India)**      | National regulators                                            | Energy & water                                         | BBM or hybrid RAB-based methods; often used to attract infrastructure investment                                                                                      |


**In summary**
- Australia & UK: Ex ante, forecast-based BBM with efficiency and service incentives.
- US & Canada: Ex post, historic "rate-of-return" model (building block in substance but backward-looking).
- Europe & elsewhere: Mix of BBM and RAB-based methods, often mandated by law to ensure cost-reflective, transparent pricing.


[^1]: [Wikipedia](https://en.wikipedia.org/wiki/Building_block_model#Basis)
