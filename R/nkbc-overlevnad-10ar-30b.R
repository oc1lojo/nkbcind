#' @export
nkbc30b <- list(
  code = "nkbc30b",
  kortnamn = "nkbc_overlevnad_10ar_30b",
  lab = c(
    sv = "Observerad 10-årsöverlevnad",
    en = "Observed 10-year survival"
  ),
  pop = c(
    sv = "alla anmälda fall",
    en = "all reported cases"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      lubridate::year(a_diag_dat) <= lubridate::year(lubridate::today()) - 11
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(
      x,
      surv_time = lubridate::interval(a_diag_dat, VITALSTATUSDATUM_ESTIMAT) / lubridate::years(1),

      # Dikotomt utfall för (naiv) skattning av överlevnadsfunktionen
      outcome = dplyr::case_when(
        surv_time > 10 ~ TRUE,
        surv_time <= 10 & VITALSTATUS %in% 1 ~ FALSE,
        surv_time <= 10 & VITALSTATUS %in% c(0, 2, NA) ~ NA,
        TRUE ~ NA
      )
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  geo_units_vars = "region", # OBS Enbart redovisning på sjukvårdsregionsnivå
  other_vars = c("a_pat_alder", "d_invasiv", "d_trigrp"),
  other_vars_inca = c("a_pat_alder", "d_invasiv", "d_trigrp", "d_tstad", "d_nstad", "d_mstad"),
  comment =
    c(
      sv = "Observera att analysen inte är justerad för skillnader i case-mix, socioekonomi, samsjuklighet etc.",
      en = "Note that the analysis is not adjusted for differences in case mix, socioeconomics, comorbidity, etc."
    ),
  om_indikatorn = list(
    sv = paste(
      "Total överlevnad betraktas som det viktigaste utfallsmåttet.",
      "Dödsorsakerna kan vara andra än bröstcancer.",
      "Observerad 10-årsöverlevnad anger de bröstcancerfall som överlevt 10 år efter diagnos.",
      "Observera att analysen inte är justerad för skillnader i case-mix, socioekonomi, samsjuklighet etc.."
    ),
    en = paste(
      "Overall survival is considered the most important outcome measure.",
      "The causes of death can be other than breast cancer.",
      "Observed 10-year survival indicates the breast cancer cases that survived 10 years after diagnosis.",
      "Note that the analysis is not adjusted for differences in case mix, socioeconomics, comorbidity, etc."
    )
  ),
  vid_tolkning = NULL,
  inkl_beskr_overlevnad_10ar = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc30b) <- "nkbcind"
