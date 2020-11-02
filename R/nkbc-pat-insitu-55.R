#' @export
nkbc55 <- list(
  code = "nkbc55",
  kortnamn = "nkbc_pat_insitu_55",
  lab = c(
    sv = "Enbart cancer in situ"
  ),
  pop =
    c(
      sv = "alla anmälda fall"
    ),
  filter_pop = function(x, ...) {
    dplyr::filter(x)
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = d_invasiv_Varde == 2
    )
  },
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_pat_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc55) <- "nkbcind"
