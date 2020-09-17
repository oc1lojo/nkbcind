#' @export
nkbc07 <- list(
  code = "nkbc07",
  kortnamn = "nkbc_kir_direktrek_07",
  lab = c(
    sv = "Omedelbara rekonstruktioner",
    en = "Immediate reconstructions"
  ),
  lab_short_w_pop = c(
    sv = "Omedelbara rekonstruktioner vid mastektomi",
    en = "Immediate reconstructions at mastectomy"
  ),
  pop = c(
    sv = "fall med mastektomi eller subkutan mastektomi utan fjärrmetastaser vid diagnos",
    en = "cases with mastectomy or subcutaneous mastectomy without distant metastasis at diagnosis"
  ),
  pop_short = c(
    sv = "mastektomerade fall utan fjärrmetastaser vid diagnos",
    en = "mastectomized cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast mastektomi och subkutan mastektomi
      op_kir_brost_Varde %in% c(2, 4),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Hantera missing
      outcome = as.logical(ifelse(op_kir_onkoplastik_Varde %in% c(0, 1), op_kir_onkoplastik_Varde, NA))
    )
  },
  target_values = c(15, 20),
  period_dat_var = "op_kir_dat",
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = paste(
      "Omedelbar rekonstruktion innebär att en bröstform återskapas i samband med att bröstet opereras bort.",
      "Bröstrekonstruktion kan också göras i ett senare skede (senrekonstruktion)."
    ),
    en = paste(
      "Immediate reconstruction means that a breast shape is recreated in connection with primary surgery.",
      "Breast reconstruction can also be done at a later stage (late reconstruction)."
    )
  ),
  vid_tolkning = list(
    sv = "Vissa enheter remitterar fall aktuella för omedelbar rekonstruktion till annan enhet (med kompetens för bröstrekonstruktion) vilket kan påverka statistiken.",
    en = "Some units refer cases for immediate reconstruction to another unit (with competence for breast reconstruction), which may affect the statistics."
  ),
  teknisk_beskrivning = NULL
)
class(nkbc07) <- "nkbcind"
