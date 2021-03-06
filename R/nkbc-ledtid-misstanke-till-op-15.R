#' @export
nkbc15 <- list(
  code = "nkbc15",
  kortnamn = "nkbc_ledtid_misstanke_till_op_15",
  lab = c(
    sv = "Välgrundad misstanke om cancer till operation",
    en = "Well-founded suspicion of cancer to surgery"
  ),
  pop = c(
    sv = "primärt opererade fall utan fjärrmetastaser vid diagnos",
    en = "primarily operated cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast fall med år från 2013 (1:a kontakt tillkom 2013)
      lubridate::year(a_diag_dat) >= 2013,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast primär opereration (planerad om utförd ej finns)
      d_prim_beh_Varde == 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      d_a_diag_misscadat = lubridate::ymd(coalesce(a_diag_misscadat, a_diag_kontdat)),
      outcome = as.numeric(lubridate::ymd(op_kir_dat) - d_a_diag_misscadat),
      outcome = ifelse(outcome < 0, 0, outcome)
    )
  },
  prop_within_value = 28,
  target_values = 80,
  period_dat_var = "op_kir_dat",
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = paste(
      "Standardiserade vårdförlopp infördes 2015 för att säkra utredning och start av behandling till patienter i rimlig tid oberoende var patienten söker vård.",
      "För bröstcancer är tiden från välgrundad misstanke till start av behandling 28 kalenderdagar.",
      "Av patienter som diagnosticerats med bröstcancer bör 80% ha startat behandling inom denna tidsperiod.",
      "För ett antal patienter krävs mer avancerade utredningsmetoder för att nå diagnos vilket kan förlänga tiden till behandlingsstart.",
      "Startpunkten för SVF har tolkats olika av vårdgivare varför ledtiden skall tolkas med försiktighet."
    ),
    en = paste(
      "Standardized care processes were introduced in 2015 to ensure investigation and start of treatment for patients in a reasonable time, regardless of where the patient seeks care.",
      "The indicator states the proportion of cases that reach the target level of 28 calendar days from suspicion of cancer to surgery.",
      "The target should be reached in 80% of cases (some patients may require additional investigation delaying lead time to surgery).",
      "Different caregivers have defined start of the standardized care process differently which should be remembered at interpretation."
    )
  ),
  vid_tolkning = NULL,
  inkl_beskr_missca = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc15) <- "nkbcind"
