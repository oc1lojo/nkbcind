varbesk_other_vars <- tibble::enframe(
  c(
    a_pat_alder = "Ålder vid diagnos",
    d_prim_beh = "Primär behandling",
    op_pad_invstl = "Tumörstorlek",
    d_tstad = "Tumörstorlek",
    d_op_pad_invstl_kat = "Tumörstorlek",
    d_nstad = "Spridning till lymfkörtlar",
    d_invasiv = "Invasivitet vid diagnos",
    d_pn = "Spridning till lymfkörtlar",
    d_op_pad_nhg = "Grad",
    d_er = "Östrogenreceptor (ER)",
    d_trigrp = "Biologisk subtyp",
    d_op_kir_brost_kat = "Slutresultat bröstingrepp",
    d_vitalstatus = "Vitalstatus"
  ),
  name = "var",
  value = "label"
)
