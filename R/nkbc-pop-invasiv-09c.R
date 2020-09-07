#' @export
nkbc09c <- list(
  code = "nkbc09c",
  kortnamn = "nkbc_pop_invasiv_09c",
  lab = "Invasivitet vid diagnos",
  lab_short = "Invasivitet",
  pop = "alla anmälda fall",
  filter_pop = function(x, ...) {
    dplyr::filter(x)
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = d_invasiv
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = "a_pat_alder",
  other_vars_inca = c("a_pat_alder", "d_a_planbeh_typ"),
  om_indikatorn =
    paste(
      "Invasiv cancer innebär att cancercellerna kan sprida sig via lymfsystemet eller blodbanan till andra organ.",
      "Cancer in situ, en icke fullt utvecklad bröstcancer, innebär att cancercellerna växer enbart inuti bröstets utförsgångar.",
      "Cancer in situ kan inte ge upphov till spridning/fjärrmetastaser."
    ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09c) <- "nkbcind"
