#' @export
nkbc23 <- list(
  code = "nkbc23",
  kortnamn = "nkbc_ledtid_op_till_stralbeh_23",
  lab = "Operation till strålbehandling",
  pop = "primärt opererade fall utan fjärrmetastaser vid diagnos som inte fått postoperativ cytostatikabehandling",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast primär opereration (planerad om utförd ej finns)
      d_prim_beh_Varde == 1,

      # Ej postop cytostatikabeh
      !(post_kemo_Varde %in% 1),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = as.numeric(lubridate::ymd(post_rt_dat) - lubridate::ymd(op_kir_dat)),
      outcome = ifelse(outcome < 0, 0, outcome)
    )
  },
  prop_within_value = 42,
  target_values = 80,
  sjhkod_var = "post_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn =
    paste(
      "Handläggningstiden från operation och PAD-svar till start av postoperativ strålbehandling bör vara rimlig och oberoende av var patienten söker vård.",
      "Ledtidens start och slut är tydliga och väl definierade vilket underlättar vid jämförelse.",
      "Vid postoperativ cytostatikabehandling ges denna oftast före strålbehandling."
    ),
  vid_tolkning = "Operationsdatum är datum för första operation, vilket innebär att tiden från sista operation till start av strålbehandling kan vara kortare än den som redovisas.",
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc23) <- "nkbcind"
