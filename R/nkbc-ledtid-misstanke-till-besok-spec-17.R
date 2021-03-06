#' @export
nkbc17 <- list(
  code = "nkbc17",
  kortnamn = "nkbc_ledtid_misstanke_till_besok_spec_17",
  lab = c(
    sv = "Välgrundad misstanke om cancer till första besök i specialiserad vård",
    en = "Well-founded suspicion of cancer to first specialized care visit"
  ),
  pop = c(
    sv = "alla anmälda fall",
    en = "all reported cases"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast fall med år från 2013 (1:a kontakt tillkom 2013)
      lubridate::year(a_diag_dat) >= 2013
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      d_a_diag_misscadat = lubridate::ymd(coalesce(a_diag_misscadat, a_diag_kontdat)),
      outcome = as.numeric(lubridate::ymd(a_diag_besdat) - d_a_diag_misscadat),
      outcome = ifelse(outcome < 0, 0, outcome)
    )
  },
  prop_within_value = 5,
  target_values = 80,
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  other_vars_inca = c("a_pat_alder", "d_invasiv", "d_a_planbeh_typ"),
  om_indikatorn = list(
    sv = paste(
      "Standardiserade vårdförlopp infördes 2015 för att säkra utredning och start av behandling till patienter i rimlig tid oberoende var patienten söker vård.",
      "Startpunkten för SVF har tolkats olika av vårdgivare varför ledtiden skall tolkas med försiktighet."
    ),
    en = paste(
      "Standardized care processes were introduced in 2015 to ensure investigation and start of treatment for patients in a reasonable time, regardless of where the patient seeks care.",
      "Different caregivers have defined start of the standardized care process differently which should be remembered at interpretation."
    )
  ),
  vid_tolkning = NULL,
  inkl_beskr_missca = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc17) <- "nkbcind"
