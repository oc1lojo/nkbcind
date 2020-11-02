#' @export
nkbc58 <- list(
  code = "nkbc58",
  kortnamn = "nkbc_pat_her2pos_58",
  lab =
    c(
      sv = "HER2-positivitet"
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
      outcome = d_her2_Varde == 1
    )
  },
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_pat_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc58) <- "nkbcind"
