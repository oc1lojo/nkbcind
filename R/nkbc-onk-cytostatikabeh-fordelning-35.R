#' @export
nkbc35 <- list(
  code = "nkbc35",
  kortnamn = "nkbc_onk_cytostatikabeh_fordelning_35",
  lab = c(
    sv = "Cytostatikabehandling, pre- respektive postoperativt",
    en = "Chemotherapy, pre- and postoperative, respectively"
  ),
  pop = c(
    sv = "opererade, invasiva fall utan fjärrmetastaser vid diagnos",
    en = "operated, invasive cases without distant metastasis at diagnosis"
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

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Pre eller postoperativ
      outcome = factor(
        dplyr::case_when(
          post_kemo_Varde == 1 & pre_kemo_Varde == 1 ~ 1,
          pre_kemo_Varde == 1 ~ 0,
          post_kemo_Varde == 1 ~ 2,
          post_kemo_Varde == 0 | pre_kemo_Varde == 0 ~ 3
        ),
        levels = c(0, 1, 2, 3),
        labels = c(
          "Enbart preoperativ",
          "Både pre- och postoperativ",
          "Enbart postoperativ",
          "Ingen"
        )
      ),
      outcome_en = factor(
        dplyr::case_when(
          post_kemo_Varde == 1 & pre_kemo_Varde == 1 ~ 1,
          pre_kemo_Varde == 1 ~ 0,
          post_kemo_Varde == 1 ~ 2,
          post_kemo_Varde == 0 | pre_kemo_Varde == 0 ~ 3
        ),
        levels = c(0, 1, 2, 3),
        labels = c(
          "Preoperative only",
          "Both pre- and postoperative",
          "Postoperative only",
          "None"
        )
      )
    )
  },
  sjhkod_var = "d_onk_sjhkod",
  other_vars = c("a_pat_alder", "d_tstad", "d_nstad", "d_trigrp"),
  om_indikatorn = list(
    sv = "Pre- eller postoperativ cytostatikabehandling rekommenderas i allmänhet vid bröstcancer med spridning till axillens lymfkörtlar, om tumören har svag hormonell känslighet och/eller andra riskfaktorer.",
    en = "Pre- or postoperative chemotherapy is generally recommended in breast cancer with lymph node metastasis, if the tumour has weak hormonal sensitivity and/or if other risk factors are present."
  ),
  vid_tolkning = list(
    sv = c(
      "Tumörstorlek och spridning till lymfkörtlar är kliniskt diagnostiserat.",
      "För fall med preoperativ onkologisk behandling är östrogenreceptoruttryck hämtat från biopsi före behandling, i övriga fall från operation."
    ),
    en = c(
      "Evaluation of tumour size and spread to lymph nodes is based on clinical diagnosis.",
      "For cases with preoperative oncological treatment, oestrogen receptor content is based on pretreatment biopsy, and otherwise postoperatively from the surgical specimen."
    )
  ),
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc35) <- "nkbcind"
