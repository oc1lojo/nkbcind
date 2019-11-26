mutate_nkbcind_d_vars <- function(x, ...) {
  mutate(x,

    # Primär behandling
    d_prim_beh = factor(
      replace_na(d_prim_beh_Varde, 99),
      levels = c(1, 2, 99),
      labels = c(
        "Primär operation",
        "Preoperativ onkologisk behandling",
        "Uppgift saknas"
      )
    ),

    # Invasivitet
    d_invasiv = factor(
      replace_na(d_invasiv_Varde, 99),
      levels = c(1, 2, 99),
      labels = c("Invasiv cancer", "Enbart cancer in situ", "Uppgift saknas")
    ),

    # ER
    d_er = factor(
      replace_na(d_er_Varde, 99),
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
      case_when(
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
      case_when(
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
      case_when(
        a_tnm_mklass_Varde == 0 ~ 1,
        a_tnm_mklass_Varde == 10 ~ 2,
        a_tnm_mklass_Varde == 20 ~ 99,
        is.na(a_tnm_mklass_Varde) ~ 99,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 99),
      labels = c("Nej (M0)", "Ja (M1)", "Uppgift saknas")
    ),

    # pN
    d_pn = cut(op_pad_lglmetant, c(1, 4, 100),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("1-3 metastaser", "=> 4 metastaser")
    ),

    d_pnstat =
      factor(
        case_when(
          op_pad_lglmetant == 0 ~ "Nej (pN-)",
          op_pad_lglmetant > 0 ~ "Ja (pN+)",
          TRUE ~ "Uppgift saknas"
        ),
        levels = c("Nej (pN-)", "Ja (pN+)", "Uppgift saknas")
      ),

    d_max_extent = pmax(op_pad_extentx, op_pad_extenty, na.rm = TRUE),

    d_kemo = as.logical(pmax(post_kemo_Varde, pre_kemo_Varde, na.rm = TRUE))
  )
}