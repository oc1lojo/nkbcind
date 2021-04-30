#' @export
nkbc09g <- list(
  code = "nkbc09g",
  kortnamn = "nkbc_pop_n_09g",
  lab = c(
    sv = "Spridning till lymfkörtlarna vid operation",
    en = "Spread to lymph nodes at surgery"
  ),
  pop = c(
    sv = "opererade fall utan fjärrmetastaser vid diagnos",
    en = "operated cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Enbart opererade
      !is.na(op_kir_dat),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = d_pnstat,
      outcome_en = d_pnstat_en
    )
  },
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_screening", "d_invasiv"),
  other_vars_inca = c("a_pat_alder", "d_screening", "d_invasiv", "d_prim_beh"),
  om_indikatorn = list(
    sv = paste(
      "Kännedom om tumörspridning till armhålans lymfkörtlar ger prognostisk information inför val av postoperativ onkologisk behandling.",
      "Tumörspridning fastställs genom mikroskopisk analys av sentinel node och/eller övriga lymfkörtlar i armhålan."
    ),
    en = paste(
      "Knowledge about tumour spread to the axillary lymph nodes provides guidance for postoperative treatment decisions and information on prognosis.",
      "The diagnosis is determined by microscopical analysis of the sentinel node and/or other lymph nodes in the armpit"
    )
  ),
  vid_tolkning = list(
    sv = c(
      "I populationen ingår både primärt opererade och opererade efter påbörjad/genomförd preoperativ onkologisk behandling.",
      "Spridning till lymfkörtlar i armhålan definieras som metastas > 0.2 mm."
    ),
    en = c(
      "The population includes both primarily operated and operated patients after started/completed preoperative oncological treatment.",
      "A tumor infiltrate of >0.2 mm in the lymph node is considered as lymph node involvement (spread to lymph nodes)."
    )
  ),
  teknisk_beskrivning = NULL
)
class(nkbc09g) <- "nkbcind"
