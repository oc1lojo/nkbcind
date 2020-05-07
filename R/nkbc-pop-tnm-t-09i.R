nkbc09i <- list(
  code = "nkbc09i",
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
  om_indikatorn = "Kännedom om tumörens utbredning i bröstet påverkar val av primär behandling, typ av kirurgi och val av postoperativ onkologisk behandling. Grundas på bilddiagnostik och klinisk undersökning.",
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09i) <- "nkbcind"
