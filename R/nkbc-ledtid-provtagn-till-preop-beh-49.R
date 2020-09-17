#' @export
nkbc49 <- list(
  code = "nkbc49",
  kortnamn = "nkbc_ledtid_provtagn_till_preop_beh_49",
  lab = c(
    sv = "Provtagningsdatum till preoperativ onkologisk behandling",
    en = "Biopsy date to preoperative oncological treatment"
  ),
  pop = c(
    sv = "opererade fall utan fjärrmetastaser vid diagnos med preoperativ onkologisk behandling",
    en = "operated cases without distant metastasis at diagnosis with preoperative oncological treatment"
  ),
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
      d_pre_onk_dat = pmin(lubridate::ymd(pre_kemo_dat),
        lubridate::ymd(pre_rt_dat),
        lubridate::ymd(pre_endo_dat),
        na.rm = TRUE
      ),

      outcome = as.numeric(lubridate::ymd(d_pre_onk_dat) - lubridate::ymd(a_diag_dat)),

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
        "Handläggningstiden från provtagning som ger cancerdiagnos till start av preoperativ onkologisk behandling bör vara kort och oberoende av var patienten söker vård.",
        "Både ledtidens start och slut är tydliga och väl definierade vilket underlättar vid jämförelse."
      )
    ),
    en = c(
      "Preoperative oncological treatment includes, solely or in combination, chemotherapy, antibody treatment, endocrine treatment or radiation therapy.",
      paste(
        "The processing time from cancer diagnosis to the start of preoperative oncological treatment should be short and independent of where the patient seeks care.",
        "Both the start and end of the lead time are clear and well defined, which facilitates comparison"
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
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc49) <- "nkbcind"
