*----------------------------------------------------------------------*
***INCLUDE LV60PF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_tooltip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_tooltip .

* Setzen Tooltips für die Icons
  data: tooltips type ref to cl_salv_tooltips,
        ls_exc type ALV_S_QINF.
  ls_exc-TYPE        = cl_salv_tooltip=>c_type_icon.
  ls_exc-FIELDNAME   = 'STATF'.
  ls_exc-TABNAME     = 'GT_FVKDFI'.
  ls_exc-VALUE       = ICON_CREATE.
  ls_exc-TEXT        = TEXT-CL1.
  append ls_exc to gt_exc.

  ls_exc-TYPE        = cl_salv_tooltip=>c_type_icon.
  ls_exc-FIELDNAME   = 'STATF'.
  ls_exc-TABNAME     = 'GT_FVKDFI'.
  ls_exc-VALUE       = ICON_OKAY.
  ls_exc-TEXT        = TEXT-CL2.
  append ls_exc to gt_exc.

  ls_exc-TYPE        = cl_salv_tooltip=>c_type_icon.
  ls_exc-FIELDNAME   = 'STATF'.
  ls_exc-TABNAME     = 'GT_FVKDFI'.
  ls_exc-VALUE       = ICON_CANCEL.
  ls_exc-TEXT        = TEXT-CL3.
  append ls_exc to gt_exc.

  ls_exc-TYPE        = cl_salv_tooltip=>c_type_icon.
  ls_exc-FIELDNAME   = 'STATF'.
  ls_exc-TABNAME     = 'GT_FVKDFI'.
  ls_exc-VALUE       = ICON_LED_GREEN.
  ls_exc-TEXT        = TEXT-CL4.
  append ls_exc to gt_exc.

  ls_exc-TYPE        = cl_salv_tooltip=>c_type_icon.
  ls_exc-FIELDNAME   = 'STATF'.
  ls_exc-TABNAME     = 'GT_FVKDFI'.
  ls_exc-VALUE       = ICON_LED_RED.
  ls_exc-TEXT        = TEXT-CL5.
  append ls_exc to gt_exc.

endform.                    " set_tooltip
