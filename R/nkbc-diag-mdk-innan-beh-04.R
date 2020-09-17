#' @export
nkbc04 <- list(
  code = "nkbc04",
  kortnamn = "nkbc_diag_mdk_innan_beh_04",
  lab = c(
    sv = "Multidisciplinär konferens inför behandlingsstart",
    en = "Multidisciplinary team conference (MDT) before start of treatment"
  ),
  pop = c(
    sv = "alla anmälda fall",
    en = "all reported cases"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(x) # ingen filtrering
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Hantera missing
      outcome = as.logical(ifelse(a_mdk_Varde %in% c(0, 1), a_mdk_Varde, NA))
    )
  },
  target_values = c(90, 99),
  period_dat_var = "a_diag_dat",
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = paste(
      "Anger andelen fall som diskuterats på MDK där definierade specialister och professioner deltar och formulerar behandlingsrekommendationer.",
      "Att MDK genomförs har betydelse för jämlik och kunskapsstyrd vård samt kvalitetssäkring."
    ),
    en = paste(
      "Indicates the proportion of cases discussed at MDT where dedicated specialists and professionals participate and decide on treatment recommendations.",
      "The implementation of a multidisciplinary discussion is important for maintaining high quality standard of care."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc04) <- "nkbcind"
