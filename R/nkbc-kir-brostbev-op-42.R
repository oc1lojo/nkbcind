#' @export
nkbc42 <- list(
  code = "nkbc42",
  kortnamn = "nkbc_kir_brostbev_op_42",
  lab = c(
    sv = "Bröstbevarande operation",
    en = "Breast-conserving surgery"
  ),
  pop = c(
    sv = "opererade fall med bröstkirurgi (fall med enbart axillkirurgi exkluderade) utan fjärrmetastas vid diagnos",
    en = "operated cases with breast surgery (cases with only axillary surgery excluded) without distant metastasis at diagnosis"
  ),
  pop_short = c(
    sv = "opererade fall utan fjärrmetastas vid diagnos",
    en = "operated cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Opererade fall
      !is.na(op_kir_dat),

      # Exkludera enbart axillkirurgi eller missing
      op_kir_brost_Varde %in% c(1, 2, 4),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = dplyr::case_when(
        op_kir_brost_Varde %in% 1 ~ TRUE,
        op_kir_brost_Varde %in% c(2, 4) ~ FALSE,
        TRUE ~ NA
      )
    )
  },
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  other_vars_inca = c("a_pat_alder", "d_invasiv", "d_prim_beh"),
  om_indikatorn = list(
    sv = paste(
      "Bröstbevarande ingrepp i kombination med strålbehandling är standardingreppet förmajoriteten av tidigt upptäckta bröstcancrar.",
      "Bl.a. tumörens storlek och lokalisation liksom bröstets storlek spelar roll vid val av operationsmetod."
    ),
    en = paste(
      "Breast-conserving surgery in combination with radiation therapy is the standard procedure for most early-detected breast cancers.",
      "The size and location of the tumour as well as the size of the breast affects the choice of type of surgery."
    )
  ),
  vid_tolkning = list(
    sv = "Case-mix liksom lokala terapitraditioner, exempelvis andel preoperativt onkologiskt behandlade, påverkar andelen bröstbevarande kirurgi.",
    en = "Case-mix as well as local therapy traditions, for example the proportion of preoperatively oncologically treated, affects the proportion of breast-conserving surgery."
  ),
  teknisk_beskrivning = NULL
)
class(nkbc42) <- "nkbcind"
