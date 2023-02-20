## CREATING EMEP-ACTM INPUT FILES

######################################################################################
#### **Creating emission input files for the EMEP atmospheric model for the UK (0.01&deg;) and EU/Globe (0.1&deg;)**
######################################################################################

Oct '23: This workflow creates inputs for EMEP v.xxxxx

*Info:*
----------------

1. EU emissions data; 

   * gridded data taken from HTAPv3 (CAMS) web address here
   * data is 0.1&deg; for GNFR sectors
   * data is masked using "Emissions_mask.tif" - this removes UK terrestrial cells

2. Non-EU & Global emissions data;
   * Info here

3. UK emissions data;

   * 1km and 0.01&deg; emissions surfaces are created for UK & Eire in the 'UK Emissions Model' (https://github.com/oSamTo/UK_emissions_model)
   * Data is processed from the NAEI, MapEire, EMEP and E-PRTR
   * This workflow takes that data and creates EMEP4UK input files, masked to "Emissions_mask.tif"


-----------------------------------------------------------------------------------------------------------------


_All UK emissions data processed and stored in:_

//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/Emissions_grids_plain


