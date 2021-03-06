#' @export
nkbc66 <- list(
  code = "nkbc66",
  kortnamn = "nkbc_pat_ngh1_66",
  lab =
    c(
      sv = "NHG 1"
    ),
  lab_short_w_pop =
    c(
      sv = "NHG 1 bland primärt opererade"
    ),
  pop =
    c(
      sv = "primärt opererade invasiva fall"
    ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast invasiv cancer
      d_invasiv_Varde %in% 1,

      #
      d_prim_beh_Varde %in% 1,

      # Opererade
      !is.na(op_kir_dat)
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(
      x,
      outcome = op_pad_nhg_Varde == 1
    )
  },
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_pat_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc66) <- "nkbcind"
