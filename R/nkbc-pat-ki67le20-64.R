#' @export
nkbc64 <- list(
  code = "nkbc64",
  kortnamn = "nkbc_pat_ki67le20_64",
  lab =
    c(
      sv = "Ki67 <= 20 %"
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
      outcome = d_pad_ki67proc <= 20
    )
  },
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_pat_sjhkod",
  other_vars = c("a_pat_alder", "d_prim_beh"),
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc64) <- "nkbcind"
