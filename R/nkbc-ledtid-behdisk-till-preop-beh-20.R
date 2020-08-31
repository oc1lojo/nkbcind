nkbc20 <- list(
  code = "nkbc20",
  kortnamn = "nkbc_ledtid_behdisk_till_preop_beh_20",
  lab = "Första behandlingsdiskussion till preoperativ onkologisk behandling",
  pop = "opererade fall utan fjärrmetastaser vid diagnos med preoperativ onkologisk behandling",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

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
      d_pre_onk_dat = pmin(as.Date(pre_kemo_dat),
        as.Date(pre_rt_dat),
        as.Date(pre_endo_dat),
        na.rm = TRUE
      ),

      outcome = as.numeric(lubridate::ymd(d_pre_onk_dat) - lubridate::ymd(a_planbeh_infopatdat)),
      outcome = ifelse(outcome < 0, 0, outcome)
    )
  },
  prop_within_value = 14,
  target_values = 80,
  sjhkod_var = "pre_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn =
    c(
      "Preoperativ onkologisk behandling kan vara cytostatika, antikroppsbehandling, endokrin behandling eller strålbehandling.",
      "Standardiserade vårdförlopp infördes 2015 för att säkra utredning och start av behandling till patienter i rimlig tid oberoende var patienten söker vård."
    ),
  vid_tolkning =
    paste(
      "Andelen preoperativt behandlade patienter varierar i landet.",
      "Många patienter som startar preoperativ onkologisk behandling ingår i behandlingsstudier med krav på specificerade undersökningar före start vilket kan förlänga ledtiden.",
      "Siffrorna skall därför tolkas med viss försiktighet."
    ),
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc20) <- "nkbcind"
