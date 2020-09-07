#' @export
nkbc05 <- list(
  code = "nkbc05",
  kortnamn = "nkbc_diag_mdk_efter_op_05",
  lab = "Multidisciplinär konferens efter operation",
  pop = "opererade fall utan fjärrmetastaser vid diagnos",
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
  om_indikatorn =
    paste(
      "Anger andelen fall som diskuterats på MDK där definierade specialister och professioner deltar och formulerar behandlingsrekommendationer.",
      "Att MDK genomförs har betydelse för jämlik och kunskapsstyrd vård samt kvalitetssäkring."
    ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc05) <- "nkbcind"
