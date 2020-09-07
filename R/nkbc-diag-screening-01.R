#' @export
nkbc01 <- list(
  code = "nkbc01",
  kortnamn = "nkbc_diag_screening_01",
  lab = "Screeningupptäckt bröstcancer",
  pop = "kvinnor i åldrarna 40–74 år vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Ålder 40-74 år vid diagnos
      a_pat_alder <= 74,
      a_pat_alder >= 40,

      # Enbart kvinnor
      KON_VALUE == 2
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Hantera missing
      outcome = as.logical(ifelse(a_diag_screening_Varde %in% c(0, 1), a_diag_screening_Varde, NA))
    )
  },
  target_values = c(60, 70),
  period_dat_var = "a_diag_dat",
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = "Mammografiscreening erbjuds alla kvinnor mellan 40–74 år.",
  vid_tolkning =
    paste(
      "Definitionen av \"screeningupptäckt fall\" kan enligt erfarenhet tolkas olika vilket kan påverka siffrorna.",
      "Här avses enbart de fall som diagnostiserats i samband med en kallelse till den regionsorganiserade screeningmammografin."
    ),
  teknisk_beskrivning = NULL
)
class(nkbc01) <- "nkbcind"
