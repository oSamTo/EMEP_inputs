## CREATING EMEP-ACTM INPUT FILES

######################################################################################
#### **Creating emission input files for the EMEP atmospheric model for the UK (0.01&deg;) and EU/Globe (0.1&deg;)**
######################################################################################

### Versions currently included: v4.45 , v5.0

Use **R/run_setup.R** to input run requirements, e.g. 

   * EMEP version required
   * emissions year
   * inventory year
   * spatial distribution year
   * pollutants of choice
   * annual, monthly or daily data
   * temporal schema (EMEP model, custom...)
   * scenario names (if required)
   * alternate sources of emissions (particularly for scenarios)

QAQC file produced for each pollutant & emissions year. 

Use **comparisons/run_compare.R** to compare two sets of inputs (totals etc.)


*Info:*
----------------

1. EU emissions data; 

   * gridded data taken from HTAPv3 (CAMS) as submitted to EMEP/CEIP. 
   * data is 0.1&deg; for GNFR sectors
   * data is masked using "Emissions_mask.tif" - this removes UK terrestrial cells

2. Non-EU & Global emissions data;

   * Not currently incorporated.

3. UK emissions data;

   * 1km and 0.01&deg; emissions surfaces are created for UK & Eire in the 'Inventory Processor' (https://github.com/oSamTo/inventory_processor)
   * Data is processed from the NAEI, MapEire and EMEP
   * This workflow takes that data and creates EMEP4UK input files, masked to "Emissions_mask.tif"


-----------------------------------------------------------------------------------------------------------------


_All UK emissions data processed and stored on JASMIN:_

/gws/nopw/j04/ceh_generic/inventory_processor


