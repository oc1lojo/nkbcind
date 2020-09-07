#' @export
nkbc06 <- list(
  code = "nkbc06",
  kortnamn = "nkbc_diag_klar_innan_op_06",
  lab = "Fastställd diagnos innan operation",
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
      outcome = as.logical(ifelse(a_diag_preopmorf_Varde %in% c(0, 1), a_diag_preopmorf_Varde, NA))
    )
  },
  target_values = c(85, 90),
  period_dat_var = "op_kir_dat",
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = "En fastställd diagnos innan behandlingsstart är viktigt för planering och genomförande av behandling och undvikande av omoperationer.",
  vid_tolkning =
    paste(
      "För att undvika alltför långa utredningstider kan det ibland vara nödvändigt att operera patienten innan diagnosen är helt fastställd.",
      "Fastställd diagnos måste vägas mot tidsåtgång."
    ),
  teknisk_beskrivning = NULL
)
class(nkbc06) <- "nkbcind"
