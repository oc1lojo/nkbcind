#' @export
nkbc09b <- list(
  code = "nkbc09b",
  kortnamn = "nkbc_pop_alder_09b",
  lab = c(
    sv = "Ålder vid diagnos",
    en = "Age at diagnosis"
  ),
  lab_short = c(
    sv = "Ålder",
    en = "Age"
  ),
  pop = c(
    sv = "alla anmälda fall",
    en = "all reported cases"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(x)
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = as.numeric(a_pat_alder)
    )
  },
  prop_within_unit = c("år", "years"),
  prop_within_value = 65,
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("d_screening", "d_invasiv"),
  om_indikatorn = list(
    sv = "Bröstcancer förekommer i alla åldrar, men är vanligare i hög ålder.",
    en = "Breast cancer occurs at all ages but is more common in old age."
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09b) <- "nkbcind"
