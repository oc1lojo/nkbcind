nkbc02 <- list(
  code = "nkbc02",
  kortnamn = "nkbc_omv_kontaktssk_02",
  lab = "Patienten har erbjudits, i journalen dokumenterad, kontaktsjuksköterska",
  lab_short = "Kontaktsjuksköterska",
  pop = "alla anmälda fall",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # kontaktsjuksköterska tillkom mitten av 2014
      lubridate::year(a_diag_dat) >= 2015
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Hantera missing
      outcome = as.logical(ifelse(a_omv_kssk_Varde %in% c(0, 1), a_omv_kssk_Varde, NA))
    )
  },
  target_values = c(95, 99),
  period_dat_var = "a_diag_dat",
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = "Enligt den Nationella  Cancerstrategin (SOU 2009:11) ska alla cancerpatienter erbjudas en kontaktsjuksköterska.",
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc02) <- "nkbcind"
