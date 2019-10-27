# Jfr https://bitbucket.org/cancercentrum/nkbc-onlinerapporter/src/2018-05-19/nkbc38.R#lines-194

nkbc38 <- list(
  code = "nkbc38",
  lab = "Fullföljt pre-och postoperativ cytostatikabehandling enligt behandlingsplan",
  pop = "opererade, cytostatikabehandlade fall utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    filter(
      x,
      # Reg av given onkologisk behandling
      year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast cytostatikabehandlade
      d_kemo == TRUE,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    mutate(x,
      # Går på det värde som finns
      pre_kemo_enlplan_Varde = ifelse(pre_kemo_enlplan_Varde %in% c(0,1), pre_kemo_enlplan_Varde, NA),
      post_kemo_enlplan_Varde = ifelse(post_kemo_enlplan_Varde %in% c(0,1), post_kemo_enlplan_Varde, NA),
      d_kemo_enlplan_Varde = pmin(post_kemo_enlplan_Varde, pre_kemo_enlplan_Varde, na.rm = TRUE),

      # Hantera missing
      outcome = as.logical(ifelse(d_kemo_enlplan_Varde %in% c(0, 1), d_kemo_enlplan_Varde, NA))
    )
  },
  sjhkod_var = "d_onk_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc38) <- "nkbcind"

filter_nkbc38_pop <- nkbc38$filter_pop
mutate_nkbc38_outcome <- nkbc38$mutate_outcome
