#' @export
nkbc05 <- list(
  code = "nkbc05",
  kortnamn = "nkbc_diag_mdk_efter_op_05",
  lab = c(
    sv = "Multidisciplinär konferens efter operation",
    en = "Multidisciplinary team conference (MDT) after surgery"
  ),
  pop = c(
    sv = "opererade fall utan fjärrmetastaser vid diagnos",
    en = "operated cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast opererade
      !is.na(op_kir_dat),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Hantera missing
      outcome = as.logical(ifelse(op_mdk_Varde %in% c(0, 1), op_mdk_Varde, NA))
    )
  },
  target_values = c(90, 99),
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = paste(
      "Anger andelen fall som diskuterats på MDK där definierade specialister och professioner deltar och formulerar behandlingsrekommendationer.",
      "Att MDK genomförs har betydelse för jämlik och kunskapsstyrd vård samt kvalitetssäkring."
    ),
    en = paste(
      "Indicates the proportion of cases discussed at MDT where dedicated specialists and professionals participate and decide on treatment recommendations.",
      "The implementation of a multidisciplinary discussion is important for maintaining high quality standard of care."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc05) <- "nkbcind"
