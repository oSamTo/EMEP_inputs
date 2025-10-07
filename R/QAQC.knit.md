---
params:
 project: NA
 scenario: NA
 y: NA
 dt_poll: NA
 species: NA
 uk_folname: NA
 eu_folname: NA
 map_yr_uk: NA
 naei_inv: NA
 emep_inv: NA
 v_EMEP_sec: NA
 dt_sec: NA
 time_dim: NA
 emep_version: NA
 uk_agg_schema: NA
 eu_agg_schema: NA
 tp_scheme: NA
 l_fname_uk_sums: NA
 l_fname_eu_sums: NA
 l_uk_maps : NA
 l_eu_maps : NA
 dt_month_uk: NA
 dt_month_eu: NA
title: "EMEP4UK input files QAQC document - UK/EIRE & EU"
author: "Sam Tomlinson"
date: "2025-10-07"


header-includes: 
 \usepackage{geometry}
 \geometry{top=1cm,left=1cm,bottom=1cm,right=1cm}
 
output:
  pdf_document:
     fig_caption: true
     extra_dependencies: ["threeparttable", "booktabs"]
---

# UK/EIRE INPUT FILES

## METADATA

**Project:** NFC

**Scenario:** BASE

**Git Repo:** https://github.com/oSamTo/EMEP_inputs

**JASMIN dir:** /gws/ssde/j25b/ceh_generic/samtom

**Output dir:** EMEP_inputs/outputs/NFC/BASE/EMEP4UKv5.0/inv2025/UKEIRE/annual/TPannual_allISO

**EMEP version:** v5.0

\vspace{\baselineskip}

Input files processed to 0.01$^\circ$ x 0.01$^\circ$ resolution, in WGS84. 

\vspace{\baselineskip}


Table: Metadata of inputs

| Area | Species | Inv_Year | Emis_Year | temp_res | orig_sec | orig_res | orig_crs |
|:----:|:-------:|:--------:|:---------:|:--------:|:--------:|:--------:|:--------:|
|  UK  |   sox   |   2025   |   2023    |  annual  |   SNAP   |   1km    |   BNG    |
| Eire |   sox   |   2025   |   2023    |  annual  |   GNFR   |   1km    |   TM65   |
| SEA  |   sox   | as above | as above  |   N/A    |   N/A    |   N/A    |   N/A    |



Notes: 

* Emissions from the UK and Eire are processed to consistent sector categories (see 
/data/lookups/EMEP_sectors.csv) and masked to a UK+10km mask. EMEP/CEIP 
emissions take precedence outside of this mask. 

* The 10km strip from the land territory into the sea is combined (UK + Eire) to a 'SEA' ISO 
code, for all sectors.

* Emissions maps are pre-processed from 1km x 1km BNG (UK) and 1km x 1km Irish 
Grid TM65 (Eire) into WGS84 lat lon, at 0.01$^\circ$ x 0.01$^\circ$ resolution. 

  * This is done in /gws/ssde/j25b/ceh_generic/inventory_processor

\newpage

## MAP: TOTAL EMISSION ANNUM^-1^

\begin{figure}[h]

{\centering \includegraphics[width=0.7\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/UKEIRE/annual/TPannual_allISO/plots/e2023/sox_UKEIRE_2023emis_2023map_2025inv_1_UKTOTMAP} 

}

\caption{UK/Eire total sox emissions annum-1 (Non-UK/Eire emissions for context).}\label{fig:unnamed-chunk-2}
\end{figure}

\vspace{\baselineskip}


Table: Annual emissions of sox per ISO.

|  Area   | Species | Total_kt | mean_t |  max_t  |
|:-------:|:-------:|:--------:|:------:|:-------:|
|   UK    |   sox   |  91.17   |  0.26  | 5101.40 |
|  Eire   |   sox   |   7.28   |  0.07  | 359.76  |
|   SEA   |   sox   |   3.98   |  0.04  |  50.43  |
| Overall |   sox   |  102.43  |  0.18  | 5101.40 |

* N.B. UK and Eire totals in Table 2 are only on-land emissions.

\newpage

## PROCESSING FLOW

The processing in Figure \ref{fig:fig-proc1} shows total emissions as taken 
from the maps (inv_spatial), the inventory table (inv_table), the scaled maps 
(spatial_scaled), and the area aggregated totals using the mask. 

N.B. the map used is not necessarily the same as the emissions year (e.g. MapEire from 2019).

\begin{figure}[h]

{\centering \includegraphics[width=0.6\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/UKEIRE/annual/TPannual_allISO/plots/e2023/sox_UKEIRE_2023emis_2023map_2025inv_2_UKINVBAR} 

}

\caption{Emissions processing from source to area-scaled totals.}\label{fig:fig-proc1}
\end{figure}

Total sox in UK & Eire, processed: 114.2 kt + 7.4 kt = 121.6 kt. Spatial breakdown :

* UK = 91.2 kt

* Eire = 7.3 kt

* SEA = 4 kt

* Outwith 10km = 19.2 kt

**Overall: Of 121.6 kt processed, 102.4 kt are input to .nc file, and 19.2 kt are masked (dropped).**

Figure \ref{fig:fig-proc2} shows the data progressing to NetCDF inputs - 'outwith' data (data outside 10km buffer) is dropped. 

\begin{figure}[h]

{\centering \includegraphics[width=0.6\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/UKEIRE/annual/TPannual_allISO/plots/e2023/sox_UKEIRE_2023emis_2023map_2025inv_3_UKNCBAR} 

}

\caption{Area-scaled toals to NetCDF input data.}\label{fig:fig-proc2}
\end{figure}

Emissions coming from processed inventories to NetCDF inputs; 'outwith' emissions (emissions outside 10km sea buffer) are masked and not used for UK/Eire (replaced by EMEP-EU).

\newpage

## ALTERNATIVE EMISSIONS - INPUT

**Input files:**

UK: No alternative (non-inventory) sources of emissions were used for sox.


Table: No alternative input files

|   Area   | GNFR | source_diff | source_pt | alt_emis_t | emis_delta_t |
|:--------:|:----:|:-----------:|:---------:|:----------:|:------------:|
| No files |  NA  |     NA      |    NA     |     NA     |      NA      |

Eire: No alternative (non-inventory) sources of emissions were used for sox.


Table: No alternative input files

|   Area   | GNFR | source_diff | source_pt | alt_emis_t | emis_delta_t |
|:--------:|:----:|:-----------:|:---------:|:----------:|:------------:|
| No files |  NA  |     NA      |    NA     |     NA     |      NA      |


\begin{figure}[h]

{\centering \includegraphics[width=0.7\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/UKEIRE/annual/TPannual_allISO/plots/e2023/sox_UKEIRE_2023emis_2023map_2025inv_3b_UKALTTOTBAR} 

}

\caption{Inventory emissions vs altered emissions - TOTAL (if any).}\label{fig:fig-altemistot}
\end{figure}


```
## [1] "No sectoral changes."
```


\newpage

## MAP: SECTORAL EMISSIONS ANNUM^-1^ - NC FILE

### UK and Eire temporal schema: annual


\begin{figure}[h]

{\centering \includegraphics[width=0.75\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/UKEIRE/annual/TPannual_allISO/plots/e2023/sox_UKEIRE_2023emis_2023map_2025inv_6_UKANNSECMAP} 

}

\caption{UK/Eire total sox emissions sector-1 annum-1}\label{fig:unnamed-chunk-5}
\end{figure}


Table: sox emissions (kt) per sector per ISO (red values have alternative data sources) (Totals match Processing Flow totals)

|                Sector|   GB   |  IE   |  SEA  | ncTotal |   OW   |  Total  |
|---------------------:|:------:|:-----:|:-----:|:-------:|:------:|:-------:|
|         A_PublicPower| 26.747 | 0.71  |   0   | 27.457  | 1.625  | 29.082  |
|            B_Industry| 41.851 | 1.465 |   0   | 43.316  |   0    | 43.316  |
| C_OtherStationaryComb| 15.032 | 4.935 |   0   | 19.967  |   0    | 19.967  |
|            D_Fugitive|   0    |   0   |   0   |  0.000  |   0    |  0.000  |
|            E_Solvents| 0.796  |   0   |   0   |  0.796  |   0    |  0.796  |
|       F_RoadTransport| 0.397  | 0.04  |   0   |  0.437  |   0    |  0.437  |
|            G_Shipping|   0    | 0.025 | 0.057 |  0.082  |  0.03  |  0.112  |
|            H_Aviation|   0    | 0.095 |   0   |  0.095  |   0    |  0.095  |
|             I_Offroad| 5.455  | 0.003 | 3.925 |  9.383  | 17.503 | 26.886  |
|               J_Waste| 0.888  | 0.002 |   0   |  0.890  |   0    |  0.890  |
|       K_AgriLivestock|   0    |   0   |   0   |  0.000  |   0    |  0.000  |
|           L_AgriOther|   0    |   0   |   0   |  0.000  |   0    |  0.000  |
|               M_Other|   0    |   0   |   0   |  0.000  |   0    |  0.000  |
|                 Total| 91.166 | 7.275 | 3.982 | 102.423 | 19.158 | 121.581 |

\newpage

## EMISSIONS MONTH^-1^

### UK and Eire temporal schema: annual

Source: EMEP v5.0 schema: data/temporal/EMEP4UKv5.0/

\begin{figure}[h]

{\centering \includegraphics[width=0.6\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/UKEIRE/annual/TPannual_allISO/plots/e2023/sox_UKEIRE_2023emis_2023map_2025inv_4_UKMONTOTLINE} 

}

\caption{Emissions of sox month-1}\label{fig:fig-montot}
\end{figure}

Total monthly emissions of sox in UK, Eire and the SEA buffer. 
As annual emissions input into NetCDF, using model profiles.



Table: Total monthly sox emissions (kt)

|  Area|  M1   |  M2  |  M3   |  M4  |  M5  |  M6  |  M7  |  M8  |  M9  | M10  | M11  | M12  | Total  |
|-----:|:-----:|:----:|:-----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:------:|
|    IE| 0.87  | 0.83 | 0.84  | 0.78 | 0.62 | 0.54 | 0.33 | 0.29 | 0.40 | 0.54 | 0.59 | 0.66 |  7.29  |
|   SEA| 0.30  | 0.33 | 0.36  | 0.35 | 0.33 | 0.34 | 0.33 | 0.31 | 0.32 | 0.34 | 0.34 | 0.32 |  3.97  |
|    UK| 8.84  | 8.73 | 9.49  | 8.67 | 6.99 | 7.08 | 6.33 | 5.62 | 6.50 | 7.38 | 7.51 | 8.13 | 91.27  |
| Total| 10.01 | 9.89 | 10.69 | 9.80 | 7.94 | 7.96 | 6.99 | 6.22 | 7.22 | 8.26 | 8.44 | 9.11 | 102.53 |

\newpage

## MAP: EMISSIONS MONTH^-1^

Also see Table 5 above for totals. 

\begin{figure}[h]

{\centering \includegraphics[width=0.85\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/UKEIRE/annual/TPannual_allISO/plots/e2023/sox_UKEIRE_2023emis_2023map_2025inv_7_UKMONTOTMAP} 

}

\caption{UK/Eire total sox emissions month-1}\label{fig:unnamed-chunk-8}
\end{figure}

\newpage

## SECTORAL EMISSIONS MONTH^-1^

### UK and Eire temporal schema: annual

Source: EMEP v5.0 schema: data/temporal/EMEP4UKv5.0/

\begin{figure}[h]

{\centering \includegraphics[width=0.8\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/UKEIRE/annual/TPannual_allISO/plots/e2023/sox_UKEIRE_2023emis_2023map_2025inv_5_UKMONSECLINE} 

}

\caption{Emissions (kt) of sox month-1 sector-1}\label{fig:unnamed-chunk-9}
\end{figure}

N.B. as UK data starts in SNAP sectors & Eire data in GNFR sectors, UK & Eire sectoral monthly emissions are sometimes flat

\vspace{\baselineskip}


Table: Monthly sox emissions (kt) per sector (UK)

|                Sector| Area |  M1  |  M2  |  M3  |  M4  |  M5  |  M6  |  M7  |  M8  |  M9  | M10  | M11  | M12  | Total |
|---------------------:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:-----:|
|         A_PublicPower|  UK  | 2.68 | 2.51 | 2.77 | 2.48 | 1.86 | 2.03 | 2.00 | 1.67 | 1.94 | 2.13 | 2.11 | 2.57 | 26.75 |
|            B_Industry|  UK  | 3.49 | 3.65 | 3.91 | 3.70 | 3.40 | 3.45 | 3.29 | 3.08 | 3.27 | 3.54 | 3.58 | 3.50 | 41.86 |
| C_OtherStationaryComb|  UK  | 2.09 | 1.96 | 2.15 | 1.83 | 1.10 | 0.94 | 0.41 | 0.28 | 0.66 | 1.05 | 1.18 | 1.44 | 15.09 |
|            D_Fugitive|  UK  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|            E_Solvents|  UK  | 0.06 | 0.06 | 0.06 | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.06 | 0.06 | 0.79  |
|       F_RoadTransport|  UK  | 0.03 | 0.03 | 0.03 | 0.03 | 0.03 | 0.03 | 0.03 | 0.03 | 0.03 | 0.04 | 0.03 | 0.03 | 0.37  |
|            G_Shipping|  UK  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|            H_Aviation|  UK  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|             I_Offroad|  UK  | 0.42 | 0.45 | 0.49 | 0.48 | 0.45 | 0.47 | 0.45 | 0.42 | 0.44 | 0.47 | 0.47 | 0.44 | 5.45  |
|               J_Waste|  UK  | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.07 | 0.84  |
|       K_AgriLivestock|  UK  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|           L_AgriOther|  UK  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|               M_Other|  UK  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|                 Total|  -   | 8.84 | 8.73 | 9.48 | 8.66 | 6.98 | 7.06 | 6.32 | 5.62 | 6.48 | 7.37 | 7.50 | 8.11 | 91.15 |

\vspace{\baselineskip}


Table: Monthly sox emissions (kt) per sector (EIRE)

|                Sector| Area |  M1  |  M2  |  M3  |  M4  |  M5  |  M6  |  M7  |  M8  |  M9  | M10  | M11  | M12  | Total |
|---------------------:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:-----:|
|         A_PublicPower|  IE  | 0.06 | 0.06 | 0.06 | 0.06 | 0.06 | 0.06 | 0.06 | 0.06 | 0.05 | 0.06 | 0.06 | 0.06 | 0.71  |
|            B_Industry|  IE  | 0.12 | 0.12 | 0.13 | 0.13 | 0.12 | 0.12 | 0.12 | 0.10 | 0.11 | 0.13 | 0.13 | 0.13 | 1.46  |
| C_OtherStationaryComb|  IE  | 0.67 | 0.64 | 0.64 | 0.58 | 0.42 | 0.35 | 0.14 | 0.12 | 0.22 | 0.34 | 0.39 | 0.47 | 4.98  |
|            D_Fugitive|  IE  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|            E_Solvents|  IE  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|       F_RoadTransport|  IE  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|            G_Shipping|  IE  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|            H_Aviation|  IE  | 0.01 | 0.01 | 0.01 | 0.01 | 0.01 | 0.01 | 0.01 | 0.01 | 0.01 | 0.01 | 0.01 | 0.01 | 0.12  |
|             I_Offroad|  IE  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|               J_Waste|  IE  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|       K_AgriLivestock|  IE  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|           L_AgriOther|  IE  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|               M_Other|  IE  | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00  |
|                 Total|  -   | 0.86 | 0.83 | 0.84 | 0.78 | 0.61 | 0.54 | 0.33 | 0.29 | 0.39 | 0.54 | 0.59 | 0.67 | 7.27  |

\newpage

# EU INPUTS FILES

## METADATA

**Project:** NFC

**Git Repo:** https://github.com/oSamTo/EMEP_inputs

**JASMIN dir:** /gws/ssde/j25b/ceh_generic/samtom/EMEP_inputs

**Output dir:** outputs/NFC/BASE/EMEP4UKv5.0/inv2025/EU/annual/TPannual_allISO

**EMEP version:** v5.0

\vspace{\baselineskip}

Input files processed to 0.1$^\circ$ x 0.1$^\circ$ resolution, in WGS84. 

\vspace{\baselineskip}


Table: Metadata of inputs

|  Area   | Species | Inv_Year | Emis_Year | temp_res | orig_sec | orig_res | orig_crs |
|:-------:|:-------:|:--------:|:---------:|:--------:|:--------:|:--------:|:--------:|
| EU ISOs |   sox   |   2025   |   2023    |  annual  |   GNFR   |   0.1    |  WGS84   |

**EMEP ISO codes:** AL, AT, BE, BG, DK, FI, FR, GR, HU, IS, IE, IT, LU, NL, NO, PL, PT, RO, ES, SE, CH, TR, GB, BAS, NOS, ATL, MED, BLS, BY, UA, MD, EE, LV, LT, CZ, SK, SI, HR, BA, MK, KZ, GE, CY, AM, MT, LI, DE, RU, MC, NOA, KG, AZ, RS, ME, UZ, TM, CAS, TJ, RUE, AST

Notes: 

* Emissions from the EU (and wider) domain remain in GNFR sectors (see 
/data/lookups/EMEP_sectors.csv) with UK+10km data masked out. EMEP/CEIP 
emissions take precedence outside of this mask. 

* Emissions data are provided in WGS84 lat lon, at 0.1$^\circ$ x 0.1$^\circ$ resolution. 

  * This is done in /gws/ssde/j25b/ceh_generic/inventory_processor

\newpage

## MAP: TOTAL EMISSION ANNUM^-1^


\begin{figure}[h]

{\centering \includegraphics[width=0.9\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/EU/annual/TPannual_allISO/plots/e2023/sox_EU_2023emis_2023map_2025inv_8_EUTOTMAP} 

}

\caption{EU total sox emissions annum-1 (UK/Eire emissions for context).}\label{fig:unnamed-chunk-13}
\end{figure}

\vspace{\baselineskip}


Table: Annual emissions of sox per ISO.

| Area | Species | Total_kt | mean_t |  max_t   |
|:----:|:-------:|:--------:|:------:|:--------:|
|  EU  |   sox   | 10748.8  | 20.62  | 230125.8 |

* N.B. EU totals in table above have UK & Eire masked out.

\newpage

## PROCESSING FLOW

Figure \ref{fig:fig-procEU1} shows total EMEP emissions as taken 
from EMEP/CEIP (inventory_data) and emissions post processing (processed_data). 

\begin{figure}[h]

{\centering \includegraphics[width=0.35\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/EU/annual/TPannual_allISO/plots/e2023/sox_EU_2023emis_2023map_2025inv_9_EUINVBAR} 

}

\caption{Emissions processing from source.}\label{fig:fig-procEU1}
\end{figure}

Total sox lost in EU domain, following processing: 102 kt. Spatial breakdown :


Table: Emissions lost in EMEP domain per ISO.

|  ISO  | inventory_kt | processed_kt | lost_kt | proc_ratio |
|:-----:|:------------:|:------------:|:-------:|:----------:|
|  ATL  |    112.9     |    110.6     |  -2.3   |   0.980    |
|  GB   |     95.1     |     4.4      |  -90.7  |   0.046    |
|  IE   |     7.4      |     0.0      |  -7.4   |   0.000    |
|  NOS  |     25.7     |     24.0     |  -1.7   |   0.934    |
| Total |    241.1     |    139.0     | -102.1  |   1.960    |


Figure \ref{fig:fig-procEU2} shows the data progressing to NetCDF inputs - emissions inside 10km sea buffer are masked and not used for EMEP-EU.

\begin{figure}[h]

{\centering \includegraphics[width=0.6\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/EU/annual/TPannual_allISO/plots/e2023/sox_EU_2023emis_2023map_2025inv_10_EUNCBAR} 

}

\caption{Processed toals to NetCDF input data.}\label{fig:fig-procEU2}
\end{figure}

\newpage

## MAP: SECTORAL EMISSIONS ANNUM^-1^

### EU temporal schema: annual

Source: EMEP v5.0 schema: data/temporal/EMEP4UKv5.0/

\begin{figure}[h]

{\centering \includegraphics[width=0.85\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/EU/annual/TPannual_allISO/plots/e2023/sox_EU_2023emis_2023map_2025inv_13_EUANNSECMAP} 

}

\caption{EMEP-EU total sox emissions sector-1 annum-1}\label{fig:unnamed-chunk-14}
\end{figure}


Table: sox emissions (kt) per sector in EU, plus UK proximal ISOs

|        Sector         |    EU    |  FR   |  NL   |  BE   |   DE   |  ES   |  NOS  |  ATL   |
|:---------------------:|:--------:|:-----:|:-----:|:-----:|:------:|:-----:|:-----:|:------:|
|     A_PublicPower     | 5239.86  | 5.63  | 1.83  | 0.92  | 69.01  | 3.92  | 0.00  |  0.00  |
|      B_Industry       | 2524.76  | 52.43 | 14.42 | 15.77 | 125.03 | 47.24 | 0.00  |  0.00  |
| C_OtherStationaryComb | 2118.01  | 9.35  | 0.47  | 1.73  | 15.16  | 14.39 | 0.00  |  0.00  |
|      D_Fugitive       |  227.10  | 7.82  | 0.00  | 3.03  |  5.29  | 21.87 | 0.00  |  0.00  |
|      E_Solvents       |  33.67   | 0.01  | 0.04  | 0.01  |  0.10  | 0.02  | 0.00  |  0.00  |
|    F_RoadTransport    |  98.45   | 0.79  | 0.17  | 0.10  |  0.75  | 0.30  | 0.00  |  0.00  |
|      G_Shipping       |  404.26  | 0.51  | 0.01  | 0.07  |  0.31  | 3.18  | 24.02 | 110.55 |
|      H_Aviation       |   6.67   | 0.68  | 0.25  | 0.10  |  0.58  | 0.52  | 0.00  |  0.00  |
|       I_Offroad       |  37.31   | 0.67  | 0.26  | 0.03  |  0.29  | 0.61  | 0.00  |  0.00  |
|        J_Waste        |   7.14   | 0.32  | 0.06  | 0.01  |  0.10  | 1.56  | 0.00  |  0.00  |
|    K_AgriLivestock    |   0.00   | 0.00  | 0.00  | 0.00  |  0.00  | 0.00  | 0.00  |  0.00  |
|      L_AgriOther      |  20.07   | 0.06  | 0.00  | 0.00  |  0.00  | 0.01  | 0.00  |  0.00  |
|        M_Other        |  31.49   | 0.00  | 0.00  | 0.00  |  0.00  | 0.00  | 0.00  |  0.00  |
|         Total         | 10748.79 | 78.27 | 17.51 | 21.77 | 216.62 | 93.62 | 24.02 | 110.55 |

\newpage

## EMISSIONS MONTH^-1^

### EMEP-EU temporal schema: annual

Source: EMEP v5.0 schema: data/temporal/EMEP4UKv5.0/

\begin{figure}[h]

{\centering \includegraphics[width=0.6\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/EU/annual/TPannual_allISO/plots/e2023/sox_EU_2023emis_2023map_2025inv_11_EUMONTOTLINE} 

}

\caption{Emissions of sox month-1}\label{fig:fig-montotEU}
\end{figure}

Total monthly emissions of sox in EU and ISOs nearest to UK/Eire. 
As annual emissions input into NetCDF, using model profiles.



Table: Total monthly sox emissions (kt)

| ISO|   M1   |   M2   |   M3   |  M4   |  M5   |  M6   |  M7   |  M8   |  M9   |  M10  |  M11  |  M12   |  Total  |
|---:|:------:|:------:|:------:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:------:|:-------:|
|  EU| 1058.4 | 1056.7 | 1004.4 | 930.2 | 839.1 | 791.9 | 759.5 | 740.7 | 775.0 | 862.1 | 932.9 | 1008.9 | 10759.8 |
|  FR|  8.2   |  8.6   |  7.5   |  7.3  |  6.3  |  5.6  |  5.3  |  4.8  |  5.0  |  6.0  |  6.6  |  7.2   |  78.4   |
|  NL|  1.6   |  1.6   |  1.6   |  1.5  |  1.4  |  1.4  |  1.3  |  1.2  |  1.3  |  1.5  |  1.5  |  1.6   |  17.5   |
|  BE|  2.0   |  2.1   |  2.1   |  2.0  |  1.8  |  1.8  |  1.5  |  1.4  |  1.6  |  1.8  |  1.9  |  1.9   |  21.9   |
|  DE|  22.1  |  22.4  |  21.2  | 19.2  | 16.7  | 15.5  | 14.2  | 13.2  | 14.6  | 17.4  | 19.3  |  21.1  |  216.9  |
|  ES|  9.9   |  10.3  |  8.7   |  7.9  |  7.6  |  7.1  |  6.9  |  6.0  |  6.1  |  7.1  |  7.7  |  8.5   |  93.8   |
| NOS|  2.0   |  2.0   |  2.0   |  2.0  |  2.0  |  2.0  |  2.0  |  2.0  |  2.0  |  2.0  |  2.0  |  2.0   |  24.0   |
| ATL|  9.2   |  9.2   |  9.2   |  9.2  |  9.2  |  9.2  |  9.2  |  9.2  |  9.2  |  9.2  |  9.2  |  9.2   |  110.4  |

\newpage

## SECTORAL EMISSIONS MONTH^-1^

### EMEP-EU temporal schema: annual

Source: EMEP v5.0 schema: data/temporal/EMEP4UKv5.0/

\begin{figure}[h]

{\centering \includegraphics[width=0.7\linewidth]{../outputs/NFC/BASE/EMEP4UKv5.0/inv2025/EU/annual/TPannual_allISO/plots/e2023/sox_EU_2023emis_2023map_2025inv_12_EUMONSECLINE} 

}

\caption{Emissions (kt) of sox month-1 sector-1}\label{fig:unnamed-chunk-17}
\end{figure}


Table: Monthly sox emissions (kt) per sector per ISO

|                Sector| ISO |   M1   |   M2   |   M3   |  M4   |  M5   |  M6   |  M7   |  M8   |  M9   |  M10  |  M11  |  M12   |  Total  |
|---------------------:|:---:|:------:|:------:|:------:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:------:|:-------:|
|         A_PublicPower| EU  | 510.7  | 485.1  | 457.1  | 447.6 | 412.9 | 386.2 | 378.6 | 375.3 | 393.4 | 436.1 | 466.6 | 493.3  | 5242.9  |
|            B_Industry| EU  | 226.6  | 233.7  | 233.7  | 219.3 | 207.6 | 202.2 | 188.8 | 175.5 | 187.6 | 208.6 | 218.4 | 224.7  | 2526.7  |
| C_OtherStationaryComb| EU  | 250.7  | 266.8  | 241.3  | 190.3 | 145.7 | 130.3 | 119.5 | 118.2 | 121.7 | 144.1 | 175.5 | 219.7  | 2123.8  |
|            D_Fugitive| EU  |  18.8  |  18.9  |  18.9  | 18.9  | 18.9  | 18.9  | 18.9  | 18.9  | 18.9  | 18.9  | 18.9  |  18.9  |  226.7  |
|            E_Solvents| EU  |  2.6   |  2.7   |  2.7   |  2.8  |  2.8  |  2.9  |  2.9  |  2.9  |  2.9  |  2.9  |  2.7  |  2.7   |  33.5   |
|       F_RoadTransport| EU  |  7.6   |  7.8   |  8.0   |  8.2  |  8.4  |  8.4  |  8.4  |  8.4  |  8.4  |  8.6  |  8.4  |  7.9   |  98.5   |
|            G_Shipping| EU  |  33.3  |  33.5  |  33.8  | 33.9  | 33.9  | 34.0  | 33.9  | 33.3  | 33.5  | 33.9  | 33.8  |  33.5  |  404.3  |
|            H_Aviation| EU  |  0.5   |  0.5   |  0.6   |  0.6  |  0.6  |  0.6  |  0.6  |  0.5  |  0.5  |  0.6  |  0.6  |  0.5   |   6.7   |
|             I_Offroad| EU  |  2.8   |  3.0   |  3.1   |  3.2  |  3.2  |  3.3  |  3.2  |  3.0  |  3.1  |  3.2  |  3.1  |  2.9   |  37.1   |
|               J_Waste| EU  |  0.6   |  0.6   |  0.6   |  0.6  |  0.6  |  0.6  |  0.6  |  0.6  |  0.6  |  0.6  |  0.6  |  0.6   |   7.2   |
|       K_AgriLivestock| EU  |  0.0   |  0.0   |  0.0   |  0.0  |  0.0  |  0.0  |  0.0  |  0.0  |  0.0  |  0.0  |  0.0  |  0.0   |   0.0   |
|           L_AgriOther| EU  |  1.4   |  1.4   |  2.0   |  2.2  |  1.8  |  1.7  |  1.5  |  1.5  |  1.6  |  1.8  |  1.7  |  1.5   |  20.1   |
|               M_Other| EU  |  2.6   |  2.6   |  2.6   |  2.6  |  2.6  |  2.6  |  2.6  |  2.6  |  2.6  |  2.6  |  2.6  |  2.6   |  31.2   |
|                 Total|  -  | 1058.2 | 1056.6 | 1004.4 | 930.2 | 839.0 | 791.7 | 759.5 | 740.7 | 774.8 | 861.9 | 932.9 | 1008.8 | 10758.7 |

\newpage

# TIME SERIES



\newpage

## EMISSIONS SECTORAL CLASSIFICATIONS

### UK data starts in SNAP sectors, Eire data in GNFR sectors. 

* All UK SNAP03 & SNAP04 goes to B_Industry.

* All UK SNAP08 goes to I_Offroad. 

* All UK SNAP10 goes to K_AgriLivestock. 


Table: Sectoral lookup table, SNAP <--> GNFR

|              GNFRlong| SNAP |  sec  | s1 | s2 | EMEP_sec |name                         |
|---------------------:|:----:|:-----:|:--:|:--:|:--------:|:----------------------------|
|         A_PublicPower|  1   | sec01 | 1  | 1  |    1     |PublicPower                  |
|            B_Industry|  3   | sec02 | 3  | 3  |    2     |Industry                     |
| C_OtherStationaryComb|  2   | sec03 | 2  | 2  |    3     |OtherStationaryComb          |
|            D_Fugitive|  5   | sec04 | 5  | 5  |    4     |Fugitive                     |
|            E_Solvents|  6   | sec05 | 6  | 2  |    5     |Solvents                     |
|       F_RoadTransport|  7   | sec06 | 7  | 2  |    6     |RoadTransport                |
|            G_Shipping|  8   | sec07 | 8  | 8  |    7     |Shipping                     |
|            H_Aviation|  8   | sec08 | 8  | 7  |    8     |Aviation                     |
|             I_Offroad|  8   | sec09 | 8  | 2  |    9     |Offroad                      |
|               J_Waste|  9   | sec10 | 9  | 6  |    10    |Waste                        |
|       K_AgriLivestock|  10  | sec11 | 10 | 2  |    11    |AgriLivestock                |
|           L_AgriOther|  10  | sec12 | 10 | 2  |    12    |AgriOther                    |
|               M_Other|  11  | sec13 | 5  | 5  |    13    |Other                        |
|           O_AviCruise|  NA  |       | NA | NA |    NA    |AviationCruise               |
|         P_IntShipping|  8   |       | NA | NA |    NA    |InternationalShipping        |
|              Q_LULUCF|  NA  |       | NA | NA |    NA    |LULUCF                       |
|                      |  1   | sec14 | 1  | 1  |    1     |PublicPower_Point            |
|                      |  1   | sec15 | 1  | 3  |    1     |PublicPower_Area             |
|                      |  7   | sec16 | 7  | 2  |    16    |RoadTransportExhaustGasoline |
|                      |  7   | sec17 | 7  | 2  |    17    |RoadTransportExhaustDiesel   |
|                      |  7   | sec18 | 7  | 2  |    18    |RoadTransportExhaustLPGgas   |
|                      |  7   | sec19 | 7  | 2  |    19    |RoadTransportNonExhaustOther |

