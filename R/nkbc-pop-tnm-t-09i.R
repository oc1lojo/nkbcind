#' @export
nkbc09i <- list(
  code = "nkbc09i",
  kortnamn = "nkbc_pop_tnm_t_09i",
  lab = c(
    sv = "Tumörstorlek (klinisk) vid diagnos",
    en = "Tumour size (clinical) at diagnosis"
  ),
  lab_short = c(
    sv = "Tumörstorlek",
    en = "Tumour size"
  ),
  pop = c(
    sv = "alla anmälda fall",
    en = "all reported cases"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = d_tstad,
      outcome_en = d_tstad_en
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  other_vars_inca = c("a_pat_alder", "d_screening", "d_invasiv"),
  om_indikatorn = list(
    sv = paste(
      "Kännedom om tumörens utbredning i bröstet påverkar val av primär behandling, typ av kirurgi och val av postoperativ onkologisk behandling.",
      "Måttet grundas på bilddiagnostik och klinisk undersökning.",
      "OBS att detta mått är en uppskattning och kan ändras vid mikroskopisk analys av preparat efter kirurgi och ev. förbehandling."
    ),
    en = paste(
      "The estimated tumour size influences the choice of primary treatment and type of surgery.",
      "Estimated tumour size is based on imaging and clinical examination.",
      "NOTE that this estimated size can be altered after microscopical analysis of the tumour post-surgery or after neoadjuvant treatment."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09i) <- "nkbcind"
