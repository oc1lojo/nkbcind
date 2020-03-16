nkbc51 <- list(
  code = "nkbc51",
  lab = "Första behandlingsdiskussion till primär behandling",
  pop = "opererade fall utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    filter(
      x,
      # Reg av given onkologisk behandling
      year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    mutate(x,
      d_pre_onk_dat = pmin(ymd(pre_kemo_dat),
        ymd(pre_rt_dat),
        ymd(pre_endo_dat),
        na.rm = TRUE
      ),
      d_prim_beh_dat = case_when(
        d_prim_beh_Varde == 1 ~ ymd(op_kir_dat),
        d_prim_beh_Varde == 2 ~ d_pre_onk_dat,
        TRUE ~ ymd(NA_character_)
      ),
      outcome = as.numeric(d_prim_beh_dat - ymd(a_planbeh_infopatdat)),

      outcome = ifelse(outcome < 0, 0, outcome)
    )
  },
  prop_within_value = 14,
  target_values = 80,
  sjhkod_var = "d_prim_beh_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv", "d_prim_beh"),
  om_indikatorn = "Standardiserat vårdförlopp infördes 2016 för att säkra utredning och vård till patienter i rimlig och säker tid.",
  inkl_beskr_onk_beh = TRUE,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc51) <- "nkbcind"
