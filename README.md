# Building-Block
The building block model, when applied correctly and consistently over time, ensures that the firm earns a revenue stream with a present value equal to the present value of its expenditure stream[^1].


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
    E --> F(Requlatory<br>rate of return)
    F --> G(Return on RAB)
    H ----> J(Revenue requirement) 
    G ----> J
    J -.-> K(Prices)
    J -.-> L(Quantities)
```

[^1]: [Wikipedia](https://en.wikipedia.org/wiki/Building_block_model#Basis)
