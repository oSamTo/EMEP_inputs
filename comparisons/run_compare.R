##########################################################
####                                                  ####
####     THIS RUN FILE WILL COMPARE TWO DIFFERENT     ####
####    INPUT FILES FOR EMEP; RELATIVE AND ABSOLUTE   ####
####              HTML REPORT VERSION                 ####
####                                                  ####
##########################################################

i_a <- as.numeric(commandArgs(trailingOnly = TRUE)[1]) # array number
# i_a <- 6

## Need to nominate 2 runs to compare.
# as input files have changing folder structure and filename vocabularly over
# the years, it's not possible to year variables etc so just use the whole
# folder name and find the required file(s).

# state whether to compare and write annual and/or monthly.
# For now, monthly is paused. Any development will work from factor files.
comp_annual <- TRUE
comp_month <- FALSE # 15/05/26 : not available.

# state the domain.
domain <- "UKEIRE" # only UKEIRE / EU / GLOBAL

# the named domain sources the correct functions
source(paste0("comparisons/compare_functions_html_v5.R"))

# need to make sure the folder name is changed in line with the desired
# emissions year (if relevant, for example for older v4.45 inputs).

## comparisons always made FROM --> TO (i.e higher in 'to' = +ve value)
folname_from <- "outputs/NFCv2/BASE/UKEIRE/NAEI/i2025/EMEP4UKv5.0/annual/TPannual/allISO" # nolint
names(folname_from) <- "from"
emis_yr_from <- 2023
map_yr_from <- 2023

folname_to <- "outputs/SNAPSPLIT/SN10/UKEIRE/NAEI/i2025/EMEP4UKv5.0/annual/TPannual/allISO" # nolint
names(folname_to) <- "to"
emis_yr_to <- 2023
map_yr_to <- 2023

# if the domain isn't in the filepath, stop process and check
if (grepl(domain, folname_from) && grepl(domain, folname_to)) {
    print("Domain check ok.")
} else {
    stop(paste0("Domain ", domain, " is not in at least one of the filepaths!"))
}

## Do an output for one nominated pollutant.
# (as from: dt_poll[, emep_model]
v_pollutant <- c("nox", "nh3", "sox", "pm25", "pmco", "co", "voc")
p <- v_pollutant[i_a]

# process the annual and/or monthly HTML report
if (comp_annual) {
    print(paste0(
        format(Sys.time(), "%Y-%m-%d %X"),
        ": Processing ",
        p,
        ": annual"
    ))
    output_comparison_annual(
        pollutant = p,
        domain,
        folname_from,
        emis_yr_from,
        map_yr_from,
        folname_to,
        emis_yr_to,
        map_yr_to,
        array_id = i_a
    )
}

if (comp_month) {
    print(paste0(
        format(Sys.time(), "%Y-%m-%d %X"),
        ": Processing ",
        p,
        ": monthly"
    ))
    output_comparison_monthly(
        pollutant = p,
        domain,
        folname_from,
        emis_yr_from,
        map_yr_from,
        folname_to,
        emis_yr_to,
        map_yr_to,
        array_id = i_a
    )
}

print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ": END."
))
