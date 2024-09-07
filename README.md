# Building-Block
The building block model, when applied correctly and consistently over time, ensures that the firm earns a revenue stream with a present value equal to the present value of its expenditure stream. 


```mermaid
flowchart TD
    A(Opening RAB) -->B(Depn opening RAB)
    C(Regulatory capex) -->D(Depn capex)
    I(Regulatory opex) ----> J(Revenue requirement) 
    B --> E(Closing RAB)
    D --> E(Closing RAB)
    B ----> H(Regulatory depn)
    D ----> H(Regulatory depn)
    E --> F(Required rate of return)
    F --> G(Return on RAB)
    H ----> J(Revenue requirement) 
    G ----> J(Revenue requirement) 
```
