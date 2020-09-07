nkbc16 <- list(
  code = "nkbc16",
  kortnamn = "nkbc_ledtid_misstanke_till_preop_beh_16",
  lab = "Välgrundad misstanke om cancer till preoperativ onkologisk behandling",
  pop = "opererade fall utan fjärrmetastaser vid diagnos med preoperativ onkologisk behandling",
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
  om_indikatorn =
    c(
      "Preoperativ onkologisk behandling kan vara cytostatika, antikroppsbehandling, endokrin behandling eller strålbehandling.",
      paste(
        "Standardiserade vårdförlopp infördes 2015 för att säkra utredning och start av behandling till patienter i rimlig tid oberoende var patienten söker vård.",
        "För bröstcancer är tiden från välgrundad misstanke till start av behandling 28 kalenderdagar.",
        "Av patienter som diagnosticerats med bröstcancer bör 80% ha startat behandling inom denna tidsperiod.",
        "För ett antal patienter krävs mer avancerade utredningsmetoder för att nå diagnos vilket kan förlänga tiden till behandlingsstart.",
        "Startpunkten för SVF har tolkats olika av vårdgivare varför ledtiden skall tolkas med försiktighet."
      )
    ),
  vid_tolkning =
    paste(
      "Andelen preoperativt behandlade patienter varierar i landet.",
      "Många patienter som startar preoperativ onkologisk behandling ingår i behandlingsstudier med krav på specificerade undersökningar före start vilket kan förlänga ledtiden.",
      "Siffrorna skall därför tolkas med viss försiktighet."
    ),
  inkl_beskr_missca = TRUE,
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc16) <- "nkbcind"
