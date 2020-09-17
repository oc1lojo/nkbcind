#' @export
nkbc02 <- list(
  code = "nkbc02",
  kortnamn = "nkbc_omv_kontaktssk_02",
  lab = c(
    sv = "Patienten har erbjudits, i journalen dokumenterad, kontaktsjuksköterska",
    en = "The patient was offered, as documented in the medical record, a contact nurse"
  ),
  lab_short = c(
    sv = "Kontaktsjuksköterska",
    en = "Contact nurse"
  ),
  pop = c(
    sv = "alla anmälda fall",
    en = "all reported cases"
  ),
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
  om_indikatorn = list(
    sv = paste(
      "Enligt den Nationella  Cancerstrategin (SOU 2009:11) ska alla cancerpatienter erbjudas en kontaktsjuksköterska.",
      "RCC i samverkan publicerade 2019 en nationell beskrivning av kontaktsjuksköterskans uppdrag."
    ),
    en = paste(
      "According to the National Cancer Strategy, all cancer patients should be offered a contact nurse.",
      "In 2019, the Regional Cancer Centres published a national description of the contact nurse’s assignment."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc02) <- "nkbcind"
