#' @export
nkbc31 <- list(
  code = "nkbc31",
  kortnamn = "nkbc_onk_endokrin_beh_maluppfyllelse_31",
  lab = c(
    sv = "Endokrin behandling, måluppfyllelse",
    en = "Endocrine treatment"
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
      # Går på det som finns, pre eller postop. Om det ena saknas antas samma som finns för det andra.
      outcome = as.logical(pmax(post_endo_Varde, pre_endo_Varde, na.rm = TRUE))
    )
  },
  target_values = c(85, 90),
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_onk_sjhkod",
  other_vars = c("a_pat_alder", "d_op_pad_invstl_diko10"),
  om_indikatorn = list(
    sv = paste(
      "Endokrin behandling bör erbjudas till alla patienter med östrogenreceptorpositiv (ER+) bröstcancer.",
      "För patienter med mycket låg risk för återfall (tumör <=10 mm av luminal A-typ utan spridning till lymfkörtlarna) kan man avstå från endokrin behandling förutsatt att patienten är informerad om balansen mellan risk och nytta.",
      "Vid betydande samsjuklighet får nyttan med endokrin behandling vägas mot potentiella nackdelar."
    ),
    en = paste(
      "Endocrine therapy should be offered to all patients with oestrogen receptor positive (ER+) breast cancer.",
      "For patients with a very low risk of recurrence (tumour <= 10 mm of luminal A-type without axillary metastases), endocrine therapy may be refrained provided the patient is informed of the balance between benefits and potential harm."
    )
  ),
  vid_tolkning = list(
    sv = paste(
      "Både preoperativ och postoperativ endokrin behandling är medtaget i beräkningen.",
      "Redovisade data avser påbörjad behandling.",
      "Studier visar att ca 70% av patienterna avbryter eller gör längre avbrott i sin endokrina behandling p.g.a. biverkningar."
    ),
    en = paste(
      "Both preoperative and postoperative endocrine treatment are taken into account.",
      "Data refers to started treatment.",
      "Studies show that about 70% of patients discontinue or make longer interruptions in their endocrine treatment due to side effects."
    )
  ),
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc31) <- "nkbcind"
