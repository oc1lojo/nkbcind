#' @export
nkbc28 <- list(
  code = "nkbc28",
  kortnamn = "nkbc_onk_stralbeh_efter_brostbev_op_28",
  lab = c(
    sv = "Strålbehandling efter bröstbevarande operation",
    en = "Radiotherapy after breast-conserving surgery"
  ),
  pop = c(
    sv = "invasiva fall med bröstbevarande operation utan fjärrmetastaser vid diagnos",
    en = "invasive cases with breast-conserving surgery without distant metastasis at diagnosis"
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

      # Endast bröstbevarande operation
      op_kir_brost_Varde == 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = as.logical(post_rt_Varde)
    )
  },
  target_values = c(90, 95),
  sjhkod_var = "post_inr_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = list(
    sv = paste(
      "Strålbehandling efter bröstbevarande operation minskar risken för lokalt återfall.",
      "Vid betydande samsjuklighet får nyttan med strålbehandling vägas mot potentiella nackdelar."
    ),
    en = paste(
      "Radiotherapy after breast-conserving surgery reduces the risk of local recurrence.",
      "In case of significant comorbidity, the benefits of radiotherapy should be weighed against potential harm."
    )
  ),
  vid_tolkning = NULL,
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc28) <- "nkbcind"
