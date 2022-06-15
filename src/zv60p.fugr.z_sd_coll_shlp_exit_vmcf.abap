function Z_SD_COLL_SHLP_EXIT_VMCF.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCR_TAB_T OPTIONAL
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR_T
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------

  data:
    lr_badi_com_se type ref to badi_sd_com_se.

  try.
*     Get BAdI handle
      get badi lr_badi_com_se.
    catch cx_badi_not_implemented.
*     This should not occur due to fallback class but to be save...
      clear lr_badi_com_se.
    catch cx_badi_multiply_implemented.
*     This appears to be very unlikely but to be save...
      clear lr_badi_com_se.
  endtry.

  if lr_badi_com_se is bound.
    call badi lr_badi_com_se->com_se_f4_help_exit
      changing
        cs_shlp        = shlp
        cs_callcontrol = callcontrol
        ct_shlp        = shlp_tab[]
        ct_record      = record_tab[].
  endif.

endfunction.
