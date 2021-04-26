#' @export
nkbc09e <- list(
  code = "nkbc09e",
  kortnamn = "nkbc_pop_tnm_n_09e",
  lab = c(
    sv = "Spridning till lymfkörtlarna vid diagnos",
    en = "Spread to lymph nodes at diagnosis"
  ),
  lab_short = c(
    sv = "Spridning till lymfkörtlarna",
    en = "Spread to lymph nodes at diagnosis"
  ),
  pop = c(
    sv = "alla anmälda fall",
    en = "all reported cases"
  ),
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
      outcome = d_nstad,
      outcome_en = d_nstad_en
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_screening", "d_invasiv"),
  om_indikatorn = list(
    sv = paste(
      "Kännedom om tumörspridning till armhålans lymfkörtlar ger vägledning för behandling och information om prognos.",
      "Före kirurgi grundas bedömningen på bilddiagnostik, klinisk undersökning och vävnadsprov/cellprov."
    ),
    en = paste(
      "Knowledge of tumour spread to the axillary lymph nodes provides guidance for treatment decisions and information on prognosis.",
      "The assessment before surgery is based on imaging, clinical examination and tissue/cell samples."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09e) <- "nkbcind"
