#' @export
nkbc32 <- list(
  code = "nkbc32",
  kortnamn = "nkbc_onk_antiher2beh_bland_cytostatikabeh_her2pos_32",
  lab = c(
    sv = "Anti-HER2-riktad behandling",
    en = "Anti-HER2 treatment"
  ),
  lab_short_w_pop = c(
    sv = "Anti-HER2-riktad behandling bland cytostatikabehandlade",
    en = "Anti-HER2 treatment mong chemotherapy-treated"
  ),
  pop = c(
    sv = "opererade, cytostatikabehandlade HER2-positiva invasiva fall utan fjärrmetastaser vid diagnos",
    en = "operated, chemotherapy-treated HER2-positive invasive cases without distant metastasis at diagnosis"
  ),
  pop_short = c(
    sv = "opererade, cytostatikabehandlade HER2+ invasiva fall utan fjärrmetastaser vid diagnos",
    en = "operated, chemotherapy-treated HER2+ invasive cases without distant metastasis at diagnosis"
  ),
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
  other_vars_inca = c("a_pat_alder", "d_pnstat", "d_er"),
  om_indikatorn = list(
    sv = "Vid HER2-positiv invasiv bröstcancer rekommenderas anti-HER2-riktad behandling i kombination med cytostatika, under förutsättning att patienten kan tolerera det sistnämnda.",
    en = "In HER2 positive invasive breast cancer, anti-HER2 treatment in combination with chemotherapy is recommended, provided that the patient can tolerate the latter."
  ),
  vid_tolkning = list(
    sv = "Både preoperativ och postoperativ anti-HER2-riktad och cytostatikabehandling är medtaget i beräkningen.",
    en = "Both preoperative and postoperative anti-HER2 treatment/chemotherapy are included in the calculation."
  ),
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc32) <- "nkbcind"
