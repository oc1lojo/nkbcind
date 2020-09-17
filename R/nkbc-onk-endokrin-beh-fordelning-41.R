#' @export
nkbc41 <- list(
  code = "nkbc41",
  kortnamn = "nkbc_onk_endokrin_beh_fordelning_41",
  lab = c(
    sv = "Endokrin behandling, pre- respektive postoperativt",
    en = "Endocrine treatment, pre- and postoperative, respectively"
  ),
  pop = c(
    sv = "opererade östrogenreceptorpositiva invasiva fall utan fjärrmetastaser vid diagnos",
    en = "operated oestrogen receptor positive invasive cases without distant metastasis at diagnosis"
  ),
  pop_short = c(
    sv = "opererade ER+ invasiva fall utan fjärrmetastaser vid diagnos",
    en = "operated ER+ invasive cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast invasiv cancer
      d_invasiv == "Invasiv cancer",

      # ER+
      d_er_Varde == 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Pre eller postoperativ
      outcome = factor(
        dplyr::case_when(
          post_endo_Varde == 1 & pre_endo_Varde == 1 ~ 1,
          pre_endo_Varde == 1 ~ 0,
          post_endo_Varde == 1 ~ 2,
          post_endo_Varde == 0 | pre_endo_Varde == 0 ~ 3
        ),
        levels = c(0, 1, 2, 3),
        labels = c(
          "Enbart preoperativ",
          "Både pre- och postoperativ",
          "Enbart postoperativ",
          "Ingen"
        )
      ),
      outcome_en = factor(
        dplyr::case_when(
          post_endo_Varde == 1 & pre_endo_Varde == 1 ~ 1,
          pre_endo_Varde == 1 ~ 0,
          post_endo_Varde == 1 ~ 2,
          post_endo_Varde == 0 | pre_endo_Varde == 0 ~ 3
        ),
        levels = c(0, 1, 2, 3),
        labels = c(
          "Preoperative only",
          "Both pre- and postoperative",
          "Postoperative only",
          "None"
        )
      )
    )
  },
  sjhkod_var = "d_onk_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = list(
    sv = "Redovisar andelen fall som fått preoperativ respektive postoperativ endokrin behandling eller bägge.",
    en = "States the proportion of cases that have received preoperative and postoperative endocrine treatment or both."
  ),
  vid_tolkning = list(
    sv = paste(
      "Redovisade data avser påbörjad behandling.",
      "Studier visar att ca 70% av patienterna avbryter eller gör längre avbrott i sin endokrina behandling p.g.a. biverkningar."
    ),
    en = paste(
      "Reported data refers to started treatment.",
      "Studies show that about 70% of patients discontinue or make longer interruptions in their endocrine treatment due to side effects."
    )
  ),
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc41) <- "nkbcind"
