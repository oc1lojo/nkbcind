#' @export
nkbc53 <- list(
  code = "nkbc53",
  kortnamn = "nkbc_kir_brostbev_op_sma_tum_inv_53",
  lab = c(
    sv = "Bröstbevarande operation vid små invasiva tumörer",
    en = "Breast-conserving surgery for small invasive tumours"
  ),
  pop = c(
    sv = "primärt opererade fall med invasiv cancer <=30 mm utan fjärrmetastaser vid diagnos",
    en = "primarily operated cases with invasive cancer <=30 mm without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Extent infördes mitten av 2014
      lubridate::year(a_diag_dat) >= 2015,

      # Opererade fall
      !is.na(op_kir_dat),

      # Endast primär opereration (planerad om utförd ej finns)
      d_prim_beh_Varde == 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10,

      # Exkludera fall som ej op i bröstet eller missing
      op_kir_brost_Varde %in% c(1, 2, 4),

      # Invasiv cancer
      d_invasiv %in% "Invasiv cancer",

      # Extent <= 30mm
      d_max_extent <= 30
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = ifelse(op_kir_brost_Varde == 1, TRUE, FALSE)
    )
  },
  target_values = c(70, 85),
  period_dat_var = "op_kir_dat",
  sjhkod_var = "op_inr_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = list(
    sv = paste(
      "Ett bröstbevarande ingrepp och strålbehandling är standardingrepp för majoriteten av tidigt upptäckta bröstcancrar.",
      "Tumörens egenskaper, form och storlek på bröstet spelar roll vid val av operationsmetod."
    ),
    en = paste(
      "Breast-conserving surgery and radiation therapy are standard procedures for the majority of early-detected breast cancers.",
      "The size and location of the tumour as well as the size of the breast affects the choice of type of surgery."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc53) <- "nkbcind"
