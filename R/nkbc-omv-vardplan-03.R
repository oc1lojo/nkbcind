#' @export
nkbc03 <- list(
  code = "nkbc03",
  kortnamn = "nkbc_omv_vardplan_03",
  lab = c(
    sv = "Individuell vårdplan (Min Vårdplan) har upprättats i samråd med patienten",
    en = "Individual care plan was prepared in consultation with the patient "
  ),
  lab_short = c(
    sv = "Min vårdplan",
    en = "Individual care plan"
  ),
  pop = c(
    sv = "alla anmälda fall",
    en = "all reported cases"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # min vp tillkom mitten av 2014
      lubridate::year(a_diag_dat) >= 2015
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Hantera missing
      outcome = as.logical(ifelse(a_omv_indivplan_Varde %in% c(0, 1), a_omv_indivplan_Varde, NA))
    )
  },
  target_values = c(95, 99),
  period_dat_var = "a_diag_dat",
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = "En individuell skriftlig vårdplan, kallad Min vårdplan, ska tas fram för varje patient med cancer enligt den  Nationella Cancerstrategin (SOU 2009:11).",
    en = "An individual written care plan should be prepared for each patient with cancer according to the National Cancer Strategy."
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc03) <- "nkbcind"
