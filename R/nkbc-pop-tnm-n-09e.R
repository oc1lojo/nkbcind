nkbc09e <- list(
  code = "nkbc09e",
  kortnamn = "nkbc_pop_tnm_n_09e",
  lab = "Spridning till lymfkörtlarna vid diagnos",
  lab_short = "Spridning till lymfkörtlarna",
  pop = "alla anmälda fall",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x
      # Endast invasiv cancer
      # invasiv == "Invasiv cancer", Bortselekterat pga om väljer enbart invasiv
      # cancer så tas alla med uppgift saknas på invasiv bort. Dock några fel? reg
      # in situ och N1 men men...
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = d_nstad
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn =
    paste(
      "Kännedom om tumörspridning till armhålans lymfkörtlar ger vägledning för behandling och information om prognos.",
      "Före kirurgi grundas bedömningen på bilddiagnostik, klinisk undersökning och vävnadsprov/cellprov."
    ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09e) <- "nkbcind"
