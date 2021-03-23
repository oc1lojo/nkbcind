#' @export
nkbc56 <- list(
  code = "nkbc56",
  kortnamn = "nkbc_pat_erpos_56",
  lab = c(
    sv = "ER-positivitet"
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
      outcome = d_er_Varde == 1
    )
  },
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_pat_sjhkod",
  other_vars = "a_pat_alder",
  other_vars_inca = c("a_pat_alder", "d_pr", "d_her2"),
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc56) <- "nkbcind"
