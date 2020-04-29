mutate_nkbcind_d_vars <- function(x, ...) {
  dplyr::mutate(x,

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

    # Planerad åtgärd
    a_planbeh_typ = factor(
      a_planbeh_typ_Varde,
      levels = c(1, 2, 3),
      labels = c(
        "Primär operation",
        "Preoperativ onkologisk behandling eller konservativ behandling",
        "Ej operation eller fjärrmetastaser vid diagnos"
      )
    ),
    d_a_planbeh_typ = factor(
      tidyr::replace_na(a_planbeh_typ_Varde, 99),
      levels = c(1, 2, 3, 99),
      labels = c(
        "Primär operation",
        "Preoperativ onkologisk behandling eller konservativ behandling",
        "Ej operation eller fjärrmetastaser vid diagnos",
        "Uppgift saknas"
      )
    ),

    # Invasivitet
    d_invasiv = factor(
      tidyr::replace_na(d_invasiv_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Invasiv cancer", "Enbart cancer in situ", "Uppgift saknas")
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

    # ER
    d_er = factor(
      tidyr::replace_na(d_er_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Positiv", "Negativ", "Uppgift saknas")
    ),

    # Biologisk subtyp
    d_trigrp = factor(
      d_trigrp_Varde,
      levels = c(3, 2, 1, 99),
      labels = c("Trippel negativ", "HER2 positiv", "Luminal", "Uppgift saknas")
    ),

    # T
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
        is.na(a_tnm_tklass_Varde) ~ 99,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 99),
      labels = c("<=20mm (T0/T1)", ">20mm (T2-T4)", "Uppgift saknas")
    ),

    # N
    d_nstad = factor(
      dplyr::case_when(
        a_tnm_nklass_Varde == 0 ~ 1,
        a_tnm_nklass_Varde == 10 ~ 2,
        a_tnm_nklass_Varde == 20 ~ 2,
        a_tnm_nklass_Varde == 30 ~ 2,
        a_tnm_nklass_Varde == 40 ~ 99,
        is.na(a_tnm_nklass_Varde) ~ 99,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 99),
      labels = c("Nej (cN-)", "Ja (cN+)", "Uppgift saknas")
    ),

    # M
    d_mstad = factor(
      dplyr::case_when(
        a_tnm_mklass_Varde == 0 ~ 1,
        a_tnm_mklass_Varde == 10 ~ 2,
        a_tnm_mklass_Varde == 20 ~ 99,
        is.na(a_tnm_mklass_Varde) ~ 99,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 99),
      labels = c("Nej (M0)", "Ja (M1)", "Uppgift saknas")
    ),

    # Tumörstorlek vid operation, kategorier
    d_op_pad_invstl_kat =
      cut(
        dplyr::if_else(d_prim_beh_Varde %in% 1, op_pad_invstl, NA_integer_),
        breaks = c(-Inf, 20, 50, Inf),
        labels = c("<=20 mm", "20-50 mm", ">50 mm")
      ) %>%
        forcats::fct_explicit_na(na_level = "Uppgift saknas"),

    # Tumörstorlek vid operation, dikotomiserad med brytpunkt 10 mm
    d_op_pad_invstl_diko10 = cut(
      dplyr::if_else(d_prim_beh_Varde %in% 1, op_pad_invstl, NA_integer_),
      breaks = c(-Inf, 10, Inf),
      labels = c("<=10 mm", ">10 mm")
    ) %>%
      forcats::fct_explicit_na(na_level = "Uppgift saknas"),

    # pN
    d_pn = cut(op_pad_lglmetant, c(1, 4, 100),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("1-3 metastaser", "=> 4 metastaser")
    ),

    d_pnstat =
      factor(
        dplyr::case_when(
          op_pad_lglmetant == 0 ~ "Nej (pN-)",
          op_pad_lglmetant > 0 ~ "Ja (pN+)",
          TRUE ~ "Uppgift saknas"
        ),
        levels = c("Nej (pN-)", "Ja (pN+)", "Uppgift saknas")
      ),

    d_max_extent = pmax(op_pad_extentx, op_pad_extenty, na.rm = TRUE),

    d_kemo = as.logical(pmax(post_kemo_Varde, pre_kemo_Varde, na.rm = TRUE)),

    d_vitalstatus = factor(VITALSTATUS,
      levels = c(0, 1, 2),
      labels = c("Levande", "Avlidna", "Uppgift saknas")
    )
  )
}
