#' @export
mutate_nkbcind_d_vars <- function(x, ...) {
  dplyr::mutate(x,

    # Upptäckssätt
    d_screening = factor(
      tidyr::replace_na(a_diag_screening_Varde, 99),
      levels = c(1, 0, 99),
      labels = c("Screeningupptäckt", "Kliniskt upptäckt", "Uppgift saknas")
    ),

    # Primär behandling
    d_prim_beh = factor(
      tidyr::replace_na(d_prim_beh_Varde, 99),
      levels = c(1, 2, 99),
      labels = c(
        "Primär operation",
        "Preoperativ onkologisk behandling",
        "Uppgift saknas"
      )
    ),
    d_prim_beh_en = factor(
      tidyr::replace_na(d_prim_beh_Varde, 99),
      levels = c(1, 2, 99),
      labels = c(
        "Primary surgery",
        "Preoperative oncological treatment",
        "Missing"
      )
    ),

    # Planerad åtgärd
    d_a_planbeh_typ = factor(
      tidyr::replace_na(a_planbeh_typ_Varde, 99),
      levels = c(1:8, 99),
      labels = c(
        "Primär operation",
        "Preoperativ onkologisk behandling eller konservativ behandling",
        "Ej operation eller fjärrmetastaser vid diagnos",
        "Preoperativ onkologisk behandling, cytostatika + ev. annan behandling (operation planeras)",
        "Preoperativ onkologisk behandling, endast endokrin terapi (operation planeras)",
        "Konservativ behandling (oklart om operation blir aktuell/ operation planeras ej)",
        "Fjärrmetastaserande sjukdom",
        "Ingen behandling (patienten avböjt/ annat skäl)",
        "Uppgift saknas"
      )
    ),
    d_a_planbeh_typ_en = factor(
      tidyr::replace_na(a_planbeh_typ_Varde, 99),
      levels = c(1:8, 99),
      labels = c(
        "Primary surgery",
        "Preoperative oncological treatment or conservative treatment",
        "No surgery or distant metastasis at diagnosis",
        "Preoperative oncological treatment, chemotherapy + any other treatment (surgery is planned)",
        "Preoperative oncological treatment, endocrine treatment only (surgery is planned)",
        "Conservative treatment (unclear if surgery becomes relevant/ surgery is not planned)",
        "Metastatic disease",
        "No treatment (patient declined/ other reason)",
        "Missing"
      )
    ),

    # Invasivitet
    d_invasiv = factor(
      tidyr::replace_na(d_invasiv_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Invasiv cancer", "Enbart cancer in situ", "Uppgift saknas")
    ),
    d_invasiv_en = factor(
      tidyr::replace_na(d_invasiv_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Invasive cancer", "Cancer in situ only", "Missing")
    ),

    # Histologisk grad (invasiv) eller kärnatypigrad (cancer in situ)
    d_op_pad_nhg = factor(
      dplyr::case_when(
        op_pad_nhg_Varde %in% c(97, 98, NA) ~ 99L,
        TRUE ~ op_pad_nhg_Varde
      ),
      levels = c(1, 2, 3, 99),
      labels = c("Grad 1", "Grad 2", "Grad 3", "Uppgift saknas")
    ),
    d_op_pad_nhg_en = factor(
      dplyr::case_when(
        op_pad_nhg_Varde %in% c(97, 98, NA) ~ 99L,
        TRUE ~ op_pad_nhg_Varde
      ),
      levels = c(1, 2, 3, 99),
      labels = c("Grade 1", "Grade 2", "Grade 3", "Missing")
    ),

    # ER-status
    d_er = factor(
      tidyr::replace_na(d_er_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Positiv", "Negativ", "Uppgift saknas")
    ),
    d_er_en = factor(
      tidyr::replace_na(d_er_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Positive", "Negative", "Missing")
    ),

    # PgR-status
    d_pr = factor(
      tidyr::replace_na(d_pr_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Positiv", "Negativ", "Uppgift saknas")
    ),
    d_pr_en = factor(
      tidyr::replace_na(d_pr_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Positive", "Negative", "Missing")
    ),

    # HER2-status
    d_her2 = factor(
      tidyr::replace_na(d_her2_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Positiv", "Negativ", "Uppgift saknas")
    ),
    d_her2_en = factor(
      tidyr::replace_na(d_her2_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Positive", "Negative", "Missing")
    ),

    # HER2 IHC
    d_her2ihc_Varde = dplyr::case_when(
      d_prim_beh_Varde == 1 ~ op_pad_her2_Varde,
      d_prim_beh_Varde %in% c(2, 3) ~ a_pad_her2_Varde
    ),

    # HER2 ISH
    d_her2ish_Varde = dplyr::case_when(
      d_prim_beh_Varde == 1 ~ op_pad_her2ish_Varde,
      d_prim_beh_Varde %in% c(2, 3) ~ a_pad_her2ish_Varde
    ),

    # Biologisk subtyp
    d_trigrp = factor(
      tidyr::replace_na(d_trigrp_Varde, 99),
      levels = c(3, 2, 1, 99),
      labels = c("Trippelnegativ", "HER2-positiv", "Luminal", "Uppgift saknas")
    ),
    d_trigrp_en = factor(
      tidyr::replace_na(d_trigrp_Varde, 99),
      levels = c(3, 2, 1, 99),
      labels = c("Triple negative", "HER2 positive", "Luminal", "Missing")
    ),

    # Ki67
    d_pad_ki67proc = dplyr::case_when(
      d_prim_beh_Varde == 1 ~ op_pad_ki67proc,
      d_prim_beh_Varde %in% c(2, 3) ~ a_pad_ki67proc
    ),

    # Klinisk T-stadium (TNM), dikotomiserad
    d_tstad = factor(
      dplyr::case_when(
        a_tnm_tklass_Varde == 0 ~ 1,
        a_tnm_tklass_Varde == 5 ~ 1,
        a_tnm_tklass_Varde == 10 ~ 1,
        a_tnm_tklass_Varde == 20 ~ 2,
        a_tnm_tklass_Varde == 30 ~ 2,
        a_tnm_tklass_Varde == 42 ~ 2,
        a_tnm_tklass_Varde == 44 ~ 2,
        a_tnm_tklass_Varde == 45 ~ 2,
        a_tnm_tklass_Varde == 46 ~ 2,
        a_tnm_tklass_Varde == 50 ~ 99,
        is.na(a_tnm_tklass_Varde) ~ 99
      ),
      levels = c(1, 2, 99),
      labels = c("<=20mm (T0/Tis/T1)", ">20mm (T2-T4)", "Uppgift saknas")
    ),
    d_tstad_en = factor(
      dplyr::case_when(
        a_tnm_tklass_Varde == 0 ~ 1,
        a_tnm_tklass_Varde == 5 ~ 1,
        a_tnm_tklass_Varde == 10 ~ 1,
        a_tnm_tklass_Varde == 20 ~ 2,
        a_tnm_tklass_Varde == 30 ~ 2,
        a_tnm_tklass_Varde == 42 ~ 2,
        a_tnm_tklass_Varde == 44 ~ 2,
        a_tnm_tklass_Varde == 45 ~ 2,
        a_tnm_tklass_Varde == 46 ~ 2,
        a_tnm_tklass_Varde == 50 ~ 99,
        is.na(a_tnm_tklass_Varde) ~ 99
      ),
      levels = c(1, 2, 99),
      labels = c("<=20mm (T0/Tis/T1)", ">20mm (T2-T4)", "Missing")
    ),

    # Klinisk N-stadium (TNM), dikotomiserad
    d_nstad = factor(
      dplyr::case_when(
        a_tnm_nklass_Varde == 0 ~ 1,
        a_tnm_nklass_Varde == 10 ~ 2,
        a_tnm_nklass_Varde == 20 ~ 2,
        a_tnm_nklass_Varde == 30 ~ 2,
        a_tnm_nklass_Varde == 40 ~ 99,
        is.na(a_tnm_nklass_Varde) ~ 99
      ),
      levels = c(1, 2, 99),
      labels = c("Nej (cN-)", "Ja (cN+)", "Uppgift saknas")
    ),
    d_nstad_en = factor(
      dplyr::case_when(
        a_tnm_nklass_Varde == 0 ~ 1,
        a_tnm_nklass_Varde == 10 ~ 2,
        a_tnm_nklass_Varde == 20 ~ 2,
        a_tnm_nklass_Varde == 30 ~ 2,
        a_tnm_nklass_Varde == 40 ~ 99,
        is.na(a_tnm_nklass_Varde) ~ 99
      ),
      levels = c(1, 2, 99),
      labels = c("No (cN-)", "Yes (cN+)", "Missing")
    ),

    # Klinisk M-stadium (TNM)
    d_mstad = factor(
      dplyr::case_when(
        a_tnm_mklass_Varde == 0 ~ 1,
        a_tnm_mklass_Varde == 10 ~ 2,
        a_tnm_mklass_Varde == 20 ~ 99,
        is.na(a_tnm_mklass_Varde) ~ 99
      ),
      levels = c(1, 2, 99),
      labels = c("Nej (M0)", "Ja (M1)", "Uppgift saknas")
    ),
    d_mstad_en = factor(
      dplyr::case_when(
        a_tnm_mklass_Varde == 0 ~ 1,
        a_tnm_mklass_Varde == 10 ~ 2,
        a_tnm_mklass_Varde == 20 ~ 99,
        is.na(a_tnm_mklass_Varde) ~ 99
      ),
      levels = c(1, 2, 99),
      labels = c("No (M0)", "Yes (M1)", "Missing")
    ),

    # Tumörstorlek vid (primär) operation, kategorier
    d_op_pad_invstl_kat =
      cut(
        dplyr::if_else(d_prim_beh_Varde %in% 1, op_pad_invstl, NA_integer_),
        breaks = c(-Inf, 20, 50, Inf),
        labels = c("<=20 mm", "21-50 mm", ">50 mm")
      ) %>%
        forcats::fct_explicit_na(na_level = "Uppgift saknas"),
    d_op_pad_invstl_kat_en =
      cut(
        dplyr::if_else(d_prim_beh_Varde %in% 1, op_pad_invstl, NA_integer_),
        breaks = c(-Inf, 20, 50, Inf),
        labels = c("<=20 mm", "21-50 mm", ">50 mm")
      ) %>%
        forcats::fct_explicit_na(na_level = "Missing"),

    # Tumörstorlek vid (primär) operation, dikotomiserad med brytpunkt 10 mm
    d_op_pad_invstl_diko10 = cut(
      dplyr::if_else(d_prim_beh_Varde %in% 1, op_pad_invstl, NA_integer_),
      breaks = c(-Inf, 10, Inf),
      labels = c("<=10 mm", ">10 mm")
    ) %>%
      forcats::fct_explicit_na(na_level = "Uppgift saknas"),
    d_op_pad_invstl_diko10_en = cut(
      dplyr::if_else(d_prim_beh_Varde %in% 1, op_pad_invstl, NA_integer_),
      breaks = c(-Inf, 10, Inf),
      labels = c("<=10 mm", ">10 mm")
    ) %>%
      forcats::fct_explicit_na(na_level = "Missing"),

    # Max extent
    d_max_extent = pmax(op_pad_extentx, op_pad_extenty, na.rm = TRUE),

    # Patologisk N-stadium
    d_pnstat =
      factor(
        dplyr::case_when(
          op_pad_lglusant > 0 & op_pad_lglmetant == 0 ~ "Nej (pN-)",
          op_pad_lglmetant > 0 ~ "Ja (pN+)",
          TRUE ~ "Uppgift saknas"
        ),
        levels = c("Nej (pN-)", "Ja (pN+)", "Uppgift saknas")
      ),
    d_pnstat_en =
      factor(
        dplyr::case_when(
          op_pad_lglusant > 0 & op_pad_lglmetant == 0 ~ "No (pN-)",
          op_pad_lglmetant > 0 ~ "Yes (pN+)",
          TRUE ~ "Missing"
        ),
        levels = c("No (pN-)", "Yes (pN+)", "Missing")
      ),
    d_pn = cut(op_pad_lglmetant, c(1, 4, 100),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("1-3 metastaser", "=> 4 metastaser")
    ),
    d_pn_en = cut(op_pad_lglmetant, c(1, 4, 100),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("1-3 metastases", "=> 4 metastases")
    ),

    # Opererande sjukhus, och om detta saknas, anmälande sjukhus
    d_opans_sjhkod = dplyr::coalesce(
      op_inr_sjhkod,
      a_inr_sjhkod
    ),

    # Opererande sjukhus för primärt opererade fall, annars anmälande sjukhus
    d_pat_sjhkod = dplyr::case_when(
      d_prim_beh_Varde == 1 ~ op_inr_sjhkod,
      d_prim_beh_Varde %in% c(2, 3) ~ a_inr_sjhkod
    ),

    # Sjukhus ansvarigt för primär behandling
    d_prim_beh_sjhkod = dplyr::case_when(
      d_prim_beh_Varde == 1 ~ op_inr_sjhkod,
      d_prim_beh_Varde == 2 ~ pre_inr_sjhkod
    ),

    # Sjukhus där onkologisk behandling ges
    d_onk_sjhkod = dplyr::coalesce(
      post_inr_sjhkod,
      pre_inr_sjhkod
    ),

    # Rapporterande sjukhus där onkologisk behandling ges, och om detta saknas, sjukhus ansvarigt för rapportering av onkologisk behandling, sjukhus för onkologisk behandling, anmälande sjukhus
    d_onkpostans_sjhkod = dplyr::coalesce(
      post_inr_sjhkod,
      op_onk_sjhkod,
      a_onk_rappsjhkod,
      a_onk_sjhkod,
      a_inr_sjhkod
    ),
    d_onkpreans_sjhkod = dplyr::coalesce(
      pre_inr_sjhkod,
      op_onk_sjhkod,
      a_onk_rappsjhkod,
      a_onk_sjhkod,
      a_inr_sjhkod
    ),

    # Sjukhus ansvarigt för rapportering av uppföljning, och om detta saknas,
    # sjukhus för onkologisk behandling, sjukhus ansvarigt för rapportering av
    # onkologisk behandling, opererande sjukhus, anmälande sjukhus
    d_uppfans_sjhkod = dplyr::coalesce(
      op_uppf_sjhkod,
      a_uppf_sjhkod,
      a_onk_sjhkod,
      op_onk_sjhkod,
      a_onk_rappsjhkod,
      a_kir_sjhkod,
      a_inr_sjhkod
    ),

    # Kemoterapi
    d_kemo = as.logical(pmax(post_kemo_Varde, pre_kemo_Varde, na.rm = TRUE)),

    # LKF-region för att imputera om region för sjukhus saknas
    d_region_lkf = dplyr::case_when(
      REGION_NAMN == "Region Sthlm/Gotland" ~ 1L,
      REGION_NAMN == "Region Uppsala/Örebro" ~ 2L,
      REGION_NAMN == "Region Sydöstra" ~ 3L,
      REGION_NAMN == "Region Syd" ~ 4L,
      REGION_NAMN == "Region Väst" ~ 5L,
      REGION_NAMN == "Region Norr" ~ 6L
    ),

    # Vitalstatus
    d_vitalstatus = factor(VITALSTATUS,
      levels = c(0, 1, 2),
      labels = c("Levande", "Avlidna", "Uppgift saknas")
    ),
    d_vitalstatus_en = factor(VITALSTATUS,
      levels = c(0, 1, 2),
      labels = c("Alive", "Diseased", "Missing")
    )
  )
}
