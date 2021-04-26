#' @export
nkbc09h <- list(
  code = "nkbc09h",
  kortnamn = "nkbc_pop_t_09h",
  lab = c(
    sv = "Tumörstorlek vid operation",
    en = "Tumour size at surgery"
  ),
  lab_short = c(
    sv = "Tumörstorlek",
    en = "Tumour size"
  ),
  pop = c(
    sv = "primärt opererade invasiva fall utan fjärrmetastaser vid diagnos",
    en = "primarily operated cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Enbart primärt opererade
      !is.na(op_kir_dat),
      d_prim_beh_Varde %in% 1,

      # Enbart invasiv cancer
      d_invasiv == "Invasiv cancer",

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome1 = as.numeric(op_pad_invstl),
      outcome2 = d_op_pad_invstl_kat,
      outcome2_en = d_op_pad_invstl_kat_en
    )
  },
  outcome = c("outcome1", "outcome2"),
  outcome_title = list(
    sv = c("Tumörstorlek", "Tumörstorlek, kategorier"),
    en = c("Tumour size", "Tumour size, categories")
  ),
  prop_within_unit = "mm",
  prop_within_value = 20,
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_screening", "d_trigrp"),
  om_indikatorn = list(
    sv = paste(
      "Tumörstorlek baserad på mikroskopisk analys av preparat från primär operation, d.v.s. operation utan föregående onkologisk behandling.",
      "Uppgiften om tumörstorlek ger prognostisk information inför val av postoperativ onkologisk behandling"
    ),
    en = paste(
      "Tumour size based on microscopical analysis after primary surgery, i.e. surgery without prior oncological treatment.",
      "Provides prognostic information for the decision of postoperative oncological treatment."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09h) <- "nkbcind"
