#' @export
nkbc09a <- list(
  code = "nkbc09a",
  kortnamn = "nkbc_pop_kon_09a",
  lab = c(
    sv = "Kön",
    en = "Sex"
  ),
  pop = c(
    sv = "alla anmälda fall",
    en = "all reported cases"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(x) # ingen filtrering
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = factor(KON_VALUE,
        levels = c(1, 2),
        labels = c("Män", "Kvinnor")
      ),
      outcome_en = factor(KON_VALUE,
        levels = c(1, 2),
        labels = c("Men", "Women")
      )
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = "Bröstcancer förekommer både hos män och kvinnor men är betydligt vanligare hos kvinnor.",
    en = "Breast cancer occurs in both men and women but is significantly more common in women."
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09a) <- "nkbcind"
