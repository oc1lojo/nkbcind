#' @export
nkbc09b <- list(
  code = "nkbc09b",
  kortnamn = "nkbc_pop_alder_09b",
  lab = "Ålder vid diagnos",
  lab_short = "Ålder",
  pop = "alla anmälda fall",
  filter_pop = function(x, ...) {
    dplyr::filter(x)
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = as.numeric(a_pat_alder)
    )
  },
  prop_within_unit = "år",
  prop_within_value = 65,
  sjhkod_var = "a_inr_sjhkod",
  other_vars = "d_invasiv",
  om_indikatorn = "Bröstcancer förekommer i alla åldrar, men är vanligare i hög ålder.",
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09b) <- "nkbcind"
