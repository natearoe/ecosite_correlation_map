# Title: SSURGO major component ecosite report 
# Desc: This script pulls ecosite data for all major components in SSURGO
# Process: 
# 1. Pulls all major components from SDA
# 2. Pulls mukey, coiid, ecositeid, ecosite correlation date (recwlupdated), 
#     and areastatus from NASIS web report by areasymbol
# 3. Join NASIS data to SDA by mukey and coiid
# 4. Determine % of coiids with ecosite correlation for all SSURGO major comps
# 5. Determine % of coiids with ecosite correlation for PES comps

# Note: this process requires access to NASIS

# Library -----------------------------------------------------------------
library(future)
library(future.apply)
library(httr)
library(xml2)
library(rvest)
library(dplyr)
library(soilDB)


# SDA query ---------------------------------------------------------------


# query SDA for areasymbol, mukey, compkind for all major coiids
areasymbols_all_query <- "SELECT l.areasymbol, m.mukey, c.nasiscoiid, c.majcompflag, c.compkind
          FROM legend AS l
          INNER JOIN mapunit AS m ON m.lkey = l.lkey
          INNER JOIN component AS c on c.mukey = m.mukey
          WHERE l.areasymbol != 'US'
            AND
          c.majcompflag = 'Yes'" 

# run query using secret function that does not limit rows
# x <- soilDB:::.SDA_query_FOR_JSON_AUTO(areasymbols_all_query)
# 
# save(x, file = "C:/Users/Nathan.Roe/Documents/ES_maps/sda_results/sda_ssa_mukey_coiid_compkind_nationwide.rda")

load("C:/Users/Nathan.Roe/Documents/ES_maps/sda_results/sda_ssa_mukey_coiid_compkind_nationwide.rda")

# unnest
x_long <- tidyr::unnest(x, cols = c(m)) |> tidyr::unnest(cols = c(c))

#write.csv(x_long, "C:/Users/Nathan.Roe/Documents/ES_maps/sda_results/sda_comps.csv")
#x_long <- read.csv("C:/Users/Nathan.Roe/Documents/ES_maps/sda_results/sda_comps.csv")

# Web report (rep) ---------------------------------------------------------

# # function to run web report with error handling (partially written by chatGPT)
# get_nasis_table <- function(web_report, areasymbol, max_tries = 3, wait_seconds = 5) {
#   
#   attempt <- 1
#   
#   while (attempt <= max_tries) {
#     cat("Areasymbol:", areasymbol, "- attempt", attempt, "/n")
#     
#     res <- try({
#       url <- paste0(web_report, areasymbol)
#       # parseWebReport(url)
#       page <- xml2::read_html(url)
#       web_data <- page |> rvest::html_node("table") |>
#         rvest::html_table(header = TRUE) |> 
#         mutate(ecositeid = as.character(ecositeid))
#       web_data
#     }, silent = TRUE)
#     
#     if (!inherits(res, "try-error")) {
#       return(res) # success, break out of function
#     }
#     
#     # if we got here, an error happened
#     warning(sprintf("Error for %s on attempt %d: %s",
#                     areasymbol, attempt, as.character(attr(res, "condition"))))
#     
#     attempt <- attempt + 1
#     if (attempt <= max_tries) Sys.sleep(wait_seconds)
#   }
#   
#   # after max_tries, give up but don't kill the whole job
#   cat("Giving up on", areasymbol, "after", max_tries, "attempts/n")
#   return(NULL)
# }
# 
# # set parallelization plan
# plan(multisession) 
# 
# # apply over unique areasymbols
# areasymbol_unique <- unique(x$areasymbol)
# 
# # run future in parallel
# wr_list <- future_lapply(
#   X = areasymbol_unique, # or areasymbol, whatever your vector is called
#   FUN = function(x){
#     get_nasis_table(web_report = "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-EScoecoCorrBySSA&area_var=",
#                     areasymbol = areasymbol_unique)
#   }
# )
# 
# # rbind lists together
# wr_df <- bind_rows(wr_list)
# 
# write.csv(wr_df, "C:/Users/Nathan.Roe/Documents/ES_maps/sda_results/nasis_coiids_repdmu.csv")


# function to run web report with error handling (partially written by chatGPT)
get_nasis_table <- function(areasymbol, max_tries = 3, wait_seconds = 5) {
  
  attempt <- 1
  
  while (attempt <= max_tries) {
    cat("Areasymbol:", areasymbol, "- attempt", attempt, "/n")
    
    res <- try({
      url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=DSP-EScoecoCorrBySSA&area_var=", areasymbol)
      # parseWebReport(url)
      page <- xml2::read_html(url)
      web_data <- page |> rvest::html_node("table") |>
        rvest::html_table(header = TRUE) |> 
        mutate(ecositeid = as.character(ecositeid),
               ecositeid = if_else(is.na(ecositeid), "Not assigned", ecositeid))
      web_data
    }, silent = TRUE)
    
    if (!inherits(res, "try-error")) {
      return(res) # success, break out of function
    }
    
    # if we got here, an error happened
    warning(sprintf("Error for %s on attempt %d: %s",
                    areasymbol, attempt, as.character(attr(res, "condition"))))
    
    attempt <- attempt + 1
    if (attempt <= max_tries) Sys.sleep(wait_seconds)
  }
  
  # after max_tries, give up but don't kill the whole job
  cat("Giving up on", areasymbol, "after", max_tries, "attempts/n")
  return(NULL)
}

# set parallelization plan
plan(multisession) 

# apply over unique areasymbols
areasymbol_unique <- unique(x$areasymbol)

# run future in parallel
wr_list <- future_lapply(
  X = areasymbol_unique, # or areasymbol, whatever your vector is called
  FUN = get_nasis_table
)

# rbind lists together
wr_df <- bind_rows(wr_list)

write.csv(wr_df, "C:/Users/Nathan.Roe/Documents/ES_maps/sda_results/nasis_coiids_repdmu.csv")

# Join SDA & NASIS ----------------------------------------------------------

# join SDA and NASIS
full_df <- x_long |> dplyr::left_join(wr_df, by = join_by(nasiscoiid == coiid,
                                                    mukey == lmapunitiid))

# the NAs in this join are the result of the coiid not being found in NASIS.
# this is because the NASIS web report only looks at repdmus. SDA is a static product
# that is updates annually, but NASIS is continually being updated. The coiids 
# that are not found in NASIS are from non-repdmus - NASIS has been changed since
# the last SDA update.
# to deal with this we will run a similar web report for the areasymbols that 
# contain a coiid that was not found.

# records with NAs (as described above)
full_df_non_rep <- full_df |> dplyr::filter(is.na(ecositeid))


# Web report (non-rep) ----------------------------------------------


# function to run web report with error handling (partially written by chatGPT)
get_nasis_table_nonrep <- function(areasymbol, max_tries = 3, wait_seconds = 5) {
  
  attempt <- 1
  
  while (attempt <= max_tries) {
    cat("Areasymbol:", areasymbol, "- attempt", attempt, "/n")
    
    res <- try({
      url <- paste0("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=EScoecoCorrBySSA_restricted&area_var=", areasymbol)
      # parseWebReport(url)
      page <- xml2::read_html(url)
      web_data <- page |> rvest::html_node("table") |>
        rvest::html_table(header = TRUE) |> 
        mutate(ecositeid = as.character(ecositeid),
               ecositeid = if_else(is.na(ecositeid), "Not assigned", ecositeid))
      web_data
    }, silent = FALSE)
    
    if (!inherits(res, "try-error")) {
      return(res) # success, break out of function
    }
    
    # if we got here, an error happened
    warning(sprintf("Error for %s on attempt %d: %s",
                    areasymbol, attempt, as.character(attr(res, "condition"))))
    
    attempt <- attempt + 1
    if (attempt <= max_tries) Sys.sleep(wait_seconds)
  }
  
  # after max_tries, give up but don't kill the whole job
  cat("Giving up on", areasymbol, "after", max_tries, "attempts/n")
  return(NULL)
}

# run future in parallel
wr_list_nonrep <- future_lapply(
  
  X = full_df_non_rep$areasymbol |> unique(), # or areasymbol, whatever your vector is called
  FUN = get_nasis_table_non_rep
  
)

# rbind lists together
wr_df_nonrep <- bind_rows(wr_list_nonrep)

write.csv(wr_df_nonrep, "C:/Users/Nathan.Roe/Documents/ES_maps/sda_results/nasis_coiids_nonrepdmu.csv")

# Join rep & non-rep ---------------------------------------------------------

full_df_non_rep_joined <- full_df_non_rep |> 
  # remove ecositeid, recwlupdated, ssastatus to prevent duplication in join
  dplyr::select(-ecositeid, -recwlupdated, -ssastatus) |> 
  # 
  dplyr::left_join(wr_df_nonrep, by = join_by(nasiscoiid == coiid,
                                                          mukey == lmapunitiid))


# rbind full_df without NAs and the new non-rep data
final <- rbind(full_df |> dplyr::filter(!is.na(ecositeid)),
               full_df_non_rep_joined)

# Calc summaries ----------------------------------------------------------

# all ssurgo
all_ssurgo_no_eco <- nrow(final |> dplyr::filter(ecositeid == "" | is.na(ecositeid)))

total_ssurgo_corrs <- nrow(final) - all_ssurgo_no_eco

all_ssurgo_corr_perc <- 100 * (1 - all_ssurgo_no_eco/nrow(final))

# pes components (not misc. areas, not out-of-date)
pes_final <- final |> dplyr::filter(compkind != "Miscellaneous area" & ssastatus != 1)

pes_no_eco <- nrow(pes_final |> dplyr::filter(ecositeid == "" | is.na(ecositeid)))

pes_corr_perc <- 100 * (1 - pes_no_eco/nrow(pes_final))

# numb&perc comps misc

numb_coiids_misc <- nrow(final |> dplyr::filter(compkind == "Miscellaneous area"))
perc_coiids_misc <- 100*numb_coiids_misc/nrow(final)

# numb&perc ssa out of date

numb_coiids_ssa_ood <- nrow(final |> dplyr::filter(ssastatus == 1))
perc_coiids_ssa_ood <- 100*numb_coiids_ssa_ood/nrow(final)


report <- data.frame("ssurgo_coiid_with_corr" = total_ssurgo_corrs,
                     "ssurgo_coiid_corr_perc" = all_ssurgo_corr_perc,
                     "pes_coiid_corr_perc" = pes_corr_perc,
                     "ssurgo_coiids_misc" = numb_comps_misc,
                     "perc_ssurgo_coiids_misc" = perc_comps_misc,
                     "ssurgo_coiids_ssa_ood" = numb_coiids_ssa_ood,
                     "perc_ssurgo_coiids_ssa_ood" = perc_coiids_ssa_ood
                     )

report <- report |> t()

write.csv(report, "C:/Users/Nathan.Roe/Documents/ES_maps/sda_results/coiid_ecosite_corr_report.csv")

# Save data ---------------------------------------------------------------

write.csv(final, "C:/Users/Nathan.Roe/Documents/ES_maps/sda_results/sda_nasis_ecosite_corr.csv")

