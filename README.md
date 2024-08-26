# R_GenSAopt_Bat_LCOE
Optimization tool for industrial BESS application. Those files can optimize energy storage services for Brazilian industrial companies.

**Main files:** GenSA_LCOE_high.R and GenSA_LCOE_medium.R
- They execute the optimization tool for a High-demand or a Medium-demand company
- It is divided in steps to find the best technical condition seaking minimum LCOE for each scenario:
    Business As Usual (BAU), PV generation w/o BESS, PV Generation + New Lithium Battery BESS, PV Generation + SLB Battery BESS.

**Execution files:** LCOE_BAU_High.R; LCOE_NLIB_High.R; LCOE_SLB_High.R; LCOE_SLB_LFP_High.R; LCOE_BAU_Medium.R; LCOE_NLIB_Medium.R; LCOE_SLB_LFP_Medium.R; LCOE_SLB_Medium.R
- Main file will read those files to execute the LCOE calculation function.
- It contains the primmary data of the respective scenarios
- Do not edit data

**Testing files:** LCOE_BAU_High_test.R; LCOE_NLIB_High_test.R; LCOE_SLB_High_test.R; LCOE_SLB_LFP_High_test.R; LCOE_BAU_Medium_test.R; LCOE_NLIB_Medium_test.R; LCOE_SLB_LFP_Medium_test.R; LCOE_SLB_Medium_test.R
- It calculate the LCOE based on primmary data;
- Use those files to change data and calculte the LCOE and to try different options;
