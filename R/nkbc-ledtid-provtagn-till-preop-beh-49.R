nkbc49 <- list(
  code = "nkbc49",
  lab = "Provtagningsdatum till preoperativ onkologisk behandling",
  pop = "opererade fall utan fjärrmetastaser vid diagnos med preoperativ onkologisk behandling",
  filter_pop = function(x, ...) {
    filter(
      x,
      # Reg av given onkologisk behandling
      year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast preop onk behandling (planerad om utförd ej finns)
      d_prim_beh_Varde == 2,

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

      outcome = as.numeric(ymd(d_pre_onk_dat) - ymd(a_diag_dat)),

      outcome = ifelse(outcome < 0, 0, outcome)
    )
  },
  prop_within_value = 28,
  period_dat_var = "d_pre_onk_dat",
  sjhkod_var = "pre_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc49) <- "nkbcind"
