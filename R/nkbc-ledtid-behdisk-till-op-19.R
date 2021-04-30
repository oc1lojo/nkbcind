#' @export
nkbc19 <- list(
  code = "nkbc19",
  kortnamn = "nkbc_ledtid_behdisk_till_op_19",
  lab = c(
    sv = "Första behandlingsdiskussion till operation"
  ),
  pop = c(
    sv = "primärt opererade fall utan fjärrmetastaser vid diagnos"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast opererade
      !is.na(op_kir_dat),

      # Endast primär opereration (planerad om utförd ej finns)
      d_prim_beh_Varde == 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = as.numeric(lubridate::ymd(op_kir_dat) - lubridate::ymd(a_planbeh_infopatdat)),
      outcome = ifelse(outcome < 0, 0, outcome)
    )
  },
  prop_within_value = 14,
  target_values = 80,
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = "Standardiserade vårdförlopp infördes 2015 för att säkra utredning och start av behandling till patienter i rimlig tid oberoende var patienten söker vård."
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc19) <- "nkbcind"
