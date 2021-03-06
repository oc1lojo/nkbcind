#' @export
nkbc46 <- list(
  code = "nkbc46",
  kortnamn = "nkbc_onk_cytostatikabeh_bland_erneg_maluppfyllelse_46",
  lab = c(
    sv = "Cytostatikabehandling, måluppfyllelse",
    en = "Chemotherapy"
  ),
  pop = c(
    sv = paste(
      "opererade östrogenreceptornegativa invasiva fall med större tumörer eller spridning till lymfkörtlar",
      "(T2-T4 eller cN+ för opererade  fall efter preoperativ onkologisk behandling, tumörstorlek >10 mm eller pN+ för primärt opererade fall)",
      "utan fjärrmetastaser vid diagnos"
    ),
    en = paste(
      "operated oestrogen receptor negative invasive cases with larger tumors or spread to lymph nodes",
      "(T2-T4 or cN+ for operated cases after preoperative oncological treatment, tumor size >10 mm or pN+ for primary operated cases)",
      "without distant metastasis at diagnosis"
    )
  ),
  pop_short = c(
    sv = "opererade ER- invasiva fall med större tumörer eller spridning till lymfkörtlar utan fjärrmetastaser vid diagnos",
    en = "operated ER- invasive cases with larger tumors or spread to lymph nodes without distant metastasis at diagnosis"
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

      # ER-
      d_er_Varde == 2,

      # större tumörer eller spridning till lymfkörtlar
      (
        # Om operation efter påbörjad/genomförd preop onk beh: T2-T4 (>20mm) eller cN+
        d_prim_beh_Varde == 2 &
          (a_tnm_tklass_Varde %in% c(20, 30, 42, 44, 45, 46) | a_tnm_nklass_Varde %in% c(10, 20, 30))
      ) | (
        # Om primär operation: tumörstorlek > 10 mm eller pN+
        d_prim_beh_Varde == 1 &
          (op_pad_invstl > 10 | op_pad_lglmetant > 0)
      ),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = dplyr::case_when(
        d_prim_beh_Varde == 1 ~ as.logical(post_kemo_Varde),
        d_prim_beh_Varde == 2 ~ as.logical(pre_kemo_Varde) | post_kemo_Varde %in% 1,
        TRUE ~ NA
      )
    )
  },
  target_values = c(80, 90),
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_onk_sjhkod",
  other_vars = c("a_pat_alder", "d_prim_beh"),
  om_indikatorn = list(
    sv = "Pre- eller postoperativ cytostatikabehandling rekommenderas i allmänhet vid bröstcancer med spridning till axillens lymfkörtlar, om tumören har svag hormonell känslighet och/eller andra riskfaktorer.",
    en = "Pre- or postoperative chemotherapy is generally recommended in breast cancer with lymph node metastasis, if the tumour has weak hormonal sensitivity and / or if other risk factors are present."
  ),
  vid_tolkning = NULL,
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc46) <- "nkbcind"
