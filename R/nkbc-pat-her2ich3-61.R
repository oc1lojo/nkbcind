#' @export
nkbc61 <- list(
  code = "nkbc61",
  kortnamn = "nkbc_pat_her2ich3_61",
  lab =
    c(
      sv = "IHC HER2 3+"
    ),
  pop =
    c(
      sv = "alla anmälda invasiva fall"
    ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast invasiv cancer
      d_invasiv_Varde %in% 1
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(
      x,
      outcome = d_her2ihc_Varde == 3
    )
  },
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_pat_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc61) <- "nkbcind"
