#' @export
nkbc09i <- list(
  code = "nkbc09i",
  kortnamn = "nkbc_pop_tnm_t_09i",
  lab = "Tumörstorlek (klinisk) vid diagnos",
  lab_short = "Tumörstorlek",
  pop = "alla anmälda fall",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = d_tstad
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn =
    paste(
      "Kännedom om tumörens utbredning i bröstet påverkar val av primär behandling, typ av kirurgi och val av postoperativ onkologisk behandling.",
      "Måttet grundas på bilddiagnostik och klinisk undersökning.",
      "OBS att detta mått är en uppskattning och kan ändras vid mikroskopisk analys av preparat efter kirurgi och ev. förbehandling."
    ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09i) <- "nkbcind"
