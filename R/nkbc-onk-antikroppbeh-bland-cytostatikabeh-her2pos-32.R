nkbc32 <- list(
  code = "nkbc32",
  kortnamn = "nkbc_onk_antikroppbeh_bland_cytostatikabeh_her2pos_32",
  lab = "Antikroppsbehandling bland cytostatikabehandlade",
  pop = "opererade, cytostatikabehandlade HER2 positiva invasiva fall utan fjärrmetastaser vid diagnos",
  pop_short = "opererade, cytostatikabehandlade HER2+ invasiva fall utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast invasiv cancer
      d_invasiv == "Invasiv cancer",

      # Endast cytostatikabehandlade
      d_kemo == TRUE,

      # HER2+ (amplifiering eller 3+).
      d_her2_Varde == 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Går på det som finns, pre eller postop. Om det ena saknas antas samma som finns för det andra.
      outcome = as.logical(pmax(post_antikropp_Varde, pre_antikropp_Varde, na.rm = TRUE))
    )
  },
  target_values = c(90, 95),
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_onk_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = "Vid HER2-positiv invasiv bröstcancer rekommenderas behandling med antikroppsbehandling i kombination efter eller med cytostatika, under förutsättning att patienten kan tolerera det sistnämnda.",
  vid_tolkning = "Både preoperativ och postoperativ antikropps- och cytostatikabehandling är medtaget i beräkningen.",
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc32) <- "nkbcind"
