#' @export
nkbc09c <- list(
  code = "nkbc09c",
  kortnamn = "nkbc_pop_invasiv_09c",
  lab = c(
    sv = "Invasivitet vid diagnos",
    en = "Invasive cancer at diagnosis"
  ),
  lab_short = c(
    sv = "Invasivitet",
    en = "Invasive cancer"
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
      outcome = d_invasiv,
      outcome_en = d_invasiv_en
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = "a_pat_alder",
  other_vars_inca = c("a_pat_alder", "d_screening", "d_a_planbeh_typ"),
  om_indikatorn = list(
    sv = paste(
      "Invasiv cancer innebär att cancercellerna kan sprida sig via lymfsystemet eller blodbanan till andra organ.",
      "Cancer in situ, en icke fullt utvecklad bröstcancer, innebär att cancercellerna växer enbart inuti bröstets utförsgångar.",
      "Cancer in situ kan inte ge upphov till spridning/fjärrmetastaser."
    ),
    en = paste(
      "An invasive cancer can cause spread through the lymph and blood vessels.",
      "In the non-invasive cancer, e.g. cancer in situ, the malignant cells are present in the milk ducts but lack the ability to infiltrate and metastasize."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09c) <- "nkbcind"
