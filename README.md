# dieter-coupling-remind
This is a github repository for [DIETER model](https://gitlab.com/diw-evu/dieter_public/dietergms), as part of model coupling between [IAM REMIND](https://github.com/remindmodel/remind) and energy system model DIETER.

This DIETER corresponds to [coupled version of REMIND](https://github.com/cchrisgong/remind-coupling-dieter/). This github repository should be cloned under 
remind-coupling-dieter/scripts/iterative/ as a submodule.
Run python DIETER_rewrite.py inside the repository to create parallel models for each year before running the model.

The latest version v1.0 include features:
- DIETER receives information such as total (usable) power demand, costs (CAPEX, OPEX, fuel cost, CO2 costs), lifetime of plants, efficiency etc, from REMIND
- Generators' capacity lower bound in DIETER coupled to REMIND pre-investment capacities (standing capacity before investment in one REMIND period)
- DIETER makes the year-long investment and dispatch given the technical informations in one REMIND period (usually 5 years), and returns market value for all generation technologies, as well as their capacity factors. These are then returned to the next iteration REMIND. Market values are reflected by markups (difference between electricity price and market value), which are imposed as a tax/subsidy in REMIND. Markups are taken directly from DIETER. Market values in REMIND = electricity price in REMIND + markup in DIETER
- DIETER informs REMIND the peak hourly residual load demand
- DIETER also informs REMIND on curtailment as a ratio to usable power
- A split of coal types into lignite and hard coal in DIETER 

Limitations:
- No ramping costs are included in DIETER, wind offshore is turned off
- No storage, H2 or DSM are yet implemented. Operational reserves are excluded for simplicity
- Fuel price from REMIND (which are marginals of primary fuel balance equations) can be volatile, so currently this is fixed to REMIND's first iteration result when passed to DIETER, then smoothed over 3 REMIND periods (usually 15 years). REMIND budget is handled similarly, though no intertemporal smoothing
- When capacity lower bound in DIETER is slightly less than the REMIND pre-investment capacities, the run is more stable. This is due to the fact that the two models differ in electricity price. This results in DIETER favoring wind in later years than REMIND (see attached graph on generation mix in 2045 for Germany)


Note:
dataprocessing folder only contains plotting scripts for standalone DIETER which Chris wrote for Gunnar's lectures. For plots used to process DIETER results of the coupled runs, it is in the REMIND repo (https://github.com/cchrisgong/remind-coupling-dieter) under dataprocessing/DIETER_plots



