#' @export
nkbc62 <- list(
  code = "nkbc62",
  kortnamn = "nkbc_pat_her2ich2amp_62",
  lab =
    c(
      sv = "Ampl (ISH) HER2"
    ),
  lab_short_w_pop =
    c(
      sv = "Ampl (ISH) HER2 bland IHC HER2 2+"
    ),
  pop =
    c(
      sv = "invasiva fall som är IHC HER2 2+"
    ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast invasiv cancer
      d_invasiv_Varde %in% 1,

      #  IHC HER2 2+
      d_her2ihc_Varde %in% 2

      # # Ampl (ISH) HER2 värde finns
      # d_her2ish_Varde %in% c(1, 2)
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(
      x,
      outcome = d_her2ish_Varde == 1
    )
  },
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_pat_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc62) <- "nkbcind"
