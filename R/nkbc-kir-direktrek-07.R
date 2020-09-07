#' @export
nkbc07 <- list(
  code = "nkbc07",
  kortnamn = "nkbc_kir_direktrek_07",
  lab_long = "Omedelbara rekonstruktioner vid mastektomi",
  lab = "Omedelbara rekonstruktioner",
  pop = "fall med mastektomi eller subkutan mastektomi utan fjärrmetastaser vid diagnos",
  pop_short = "mastektomerade fall utan fjärrmetastaser vid diagnos",
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
  om_indikatorn =
    paste(
      "Omedelbar rekonstruktion innebär att en bröstform återskapas i samband med att bröstet opereras bort.",
      "Bröstrekonstruktion kan också göras i ett senare skede (senrekonstruktion)."
    ),
  vid_tolkning = "Vissa enheter remitterar fall aktuella för omedelbar rekonstruktion till annan enhet (med kompetens för bröstrekonstruktion) vilket kan påverka statistiken.",
  teknisk_beskrivning = NULL
)
class(nkbc07) <- "nkbcind"
