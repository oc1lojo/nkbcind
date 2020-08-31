nkbc09a <- list(
  code = "nkbc09a",
  kortnamn = "nkbc_pop_kon_09a",
  lab = "Kön vid diagnos",
  lab_short = "Kön",
  pop = "alla anmälda fall",
  filter_pop = function(x, ...) {
    dplyr::filter(x) # ingen filtrering
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = factor(KON_VALUE,
        levels = c(1, 2),
        labels = c("Män", "Kvinnor")
      )
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = "Bröstcancer förekommer både hos män och kvinnor men är betydligt vanligare hos kvinnor.",
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09a) <- "nkbcind"
