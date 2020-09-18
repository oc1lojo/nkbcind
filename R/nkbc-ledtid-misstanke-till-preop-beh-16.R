#' @export
nkbc16 <- list(
  code = "nkbc16",
  kortnamn = "nkbc_ledtid_misstanke_till_preop_beh_16",
  lab = c(
    sv = "Välgrundad misstanke om cancer till preoperativ onkologisk behandling",
    en = "Well-founded suspicion of cancer to preoperative oncological treatment"
  ),
  pop = c(
    sv = "opererade fall utan fjärrmetastaser vid diagnos med preoperativ onkologisk behandling",
    en = "operated cases without distant metastasis at diagnosis with preoperative oncological treatment"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast fall med år från 2013 (1:a kontakt tillkom 2013)
      lubridate::year(a_diag_dat) >= 2013,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast preop onk behandling (planerad om utförd ej finns)
      d_prim_beh_Varde == 2,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      d_a_diag_misscadat = lubridate::ymd(coalesce(a_diag_misscadat, a_diag_kontdat)),
      d_pre_onk_dat = pmin(lubridate::ymd(pre_kemo_dat),
        lubridate::ymd(pre_rt_dat),
        lubridate::ymd(pre_endo_dat),
        na.rm = TRUE
      ),

      outcome = as.numeric(lubridate::ymd(d_pre_onk_dat) - d_a_diag_misscadat),

      outcome = ifelse(outcome < 0, 0, outcome)
    )
  },
  prop_within_value = 28,
  target_values = 80,
  period_dat_var = "d_pre_onk_dat",
  sjhkod_var = "pre_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = c(
      "Preoperativ onkologisk behandling kan vara cytostatika, antikroppsbehandling, endokrin behandling eller strålbehandling.",
      paste(
        "Standardiserade vårdförlopp infördes 2015 för att säkra utredning och start av behandling till patienter i rimlig tid oberoende var patienten söker vård.",
        "För bröstcancer är tiden från välgrundad misstanke till start av behandling 28 kalenderdagar.",
        "Av patienter som diagnosticerats med bröstcancer bör 80% ha startat behandling inom denna tidsperiod.",
        "För ett antal patienter krävs mer avancerade utredningsmetoder för att nå diagnos vilket kan förlänga tiden till behandlingsstart.",
        "Startpunkten för SVF har tolkats olika av vårdgivare varför ledtiden skall tolkas med försiktighet."
      )
    ),
    en = c(
      "Preoperative oncological treatment includes, solely or in combination, chemotherapy, antibody treatment, endocrine treatment or radiation therapy.",
      paste(
        "Standardized care processes were introduced in 2015 to ensure investigation and start of treatment for patients in a reasonable time, regardless of where the patient seeks care.",
        "The indicator states the proportion of cases that reach the target level of 28 calendar days from suspicion of cancer to start of preoperative oncological treatment.",
        "The target should be reached in 80% of cases (some patients may require more advanced investigation methods are required to reach a diagnosis, which can extend the time to start treatment).",
        "Different caregivers have defined start of the standardized care process differently which should be remembered at interpretation."
      )
    )
  ),
  vid_tolkning = list(
    sv = paste(
      "Andelen preoperativt behandlade patienter varierar i landet.",
      "Många patienter som startar preoperativ onkologisk behandling ingår i behandlingsstudier med krav på specificerade undersökningar före start vilket kan förlänga ledtiden.",
      "Siffrorna skall därför tolkas med viss försiktighet."
    ),
    en = paste(
      "The proportion of preoperatively treated patients varies in the country.",
      "Many patients who start preoperative oncological treatment are included in treatment studies with requirements for specified examinations before start, which can extend the lead time.",
      "The figures should therefore be interpreted with some caution."
    )
  ),
  inkl_beskr_missca = TRUE,
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc16) <- "nkbcind"
