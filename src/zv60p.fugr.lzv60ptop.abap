FUNCTION-POOL ZV60P.                    "MESSAGE-ID ..
TABLES : tvsa.
TABLES : rv45a.
TABLES : fplt.
TYPE-POOLS: shlp.
INCLUDE rvreuse_global_data     .
INCLUDE vbrkdata                .
INCLUDE vbrpdata.               .
INCLUDE <icon>.
DATA : BEGIN OF gt_shorttext OCCURS 10.
DATA : line(132).
DATA : END OF gt_shorttext.
DATA : BEGIN OF gts_shorttext OCCURS 10.
DATA : line(132).
DATA : END OF gts_shorttext.

DATA : BEGIN OF gt_longtext OCCURS 10.
DATA : msgid LIKE vbfs-msgid.
DATA : msgno LIKE vbfs-msgno.
DATA : index LIKE sy-index.
DATA : line(132).
DATA : END OF gt_longtext.
DATA : BEGIN OF gts_longtext OCCURS 10.
DATA : msgid LIKE vbfs-msgid.
DATA : msgno LIKE vbfs-msgno.
DATA : index LIKE sy-index.
DATA : line(132).
DATA : END OF gts_longtext.
DATA: treelist LIKE snodetext OCCURS 50 WITH HEADER LINE.
DATA: gt_vbfs  LIKE vbfs      OCCURS 10 WITH HEADER LINE.
DATA: gt_vbfs_summed  LIKE vbfs      OCCURS 10 WITH HEADER LINE.
DATA: gt_vmcfa  LIKE vmcfao      OCCURS 10 WITH HEADER LINE.
DATA : gt_fplt  LIKE fplt   OCCURS 10 WITH HEADER LINE.
DATA : gt_fvbsk LIKE vbskfo OCCURS 10 WITH HEADER LINE.
DATA : gt_rvbsk LIKE vbskro OCCURS 10 WITH HEADER LINE.
DATA : gt_lvbsk LIKE vbsklo OCCURS 10 WITH HEADER LINE.
DATA : gt_cvbsk LIKE vbsklo OCCURS 10 WITH HEADER LINE.            "AIP
DATA : gt_wvbsk LIKE vbskwo OCCURS 10 WITH HEADER LINE.
DATA : gt_svbsk LIKE vbskso OCCURS 10 WITH HEADER LINE.
DATA : gt_gvbsk LIKE vbskgo OCCURS 10 WITH HEADER LINE.
DATA : gt_tvbsk LIKE vbskgo OCCURS 10 WITH HEADER LINE.
DATA:  gt_vbss  LIKE vbsso  OCCURS 10 WITH HEADER LINE.
DATA:  gt_fvbss LIKE vbssfo OCCURS 10 WITH HEADER LINE.
DATA:  gt_fvkdfi LIKE zvkdfif OCCURS 10 WITH HEADER LINE.
*DATA:  gt_fvkdfi LIKE vkdfif OCCURS 10 WITH HEADER LINE.
*DATA:  gt_zfvkdfi LIKE zvkdfif OCCURS 10 WITH HEADER LINE.
DATA:  gt_fvkdfign LIKE vkdfifgn OCCURS 10 WITH HEADER LINE.
DATA : gd_default_data LIKE rv60a.
DATA : gd_vbfs_called_by(1).
DATA : gd_invoice_list(1).
DATA : gd_vbfs_cb_program LIKE sy-repid.
DATA : gd_vbfs_cb_user_command(30).
DATA : gd_vbfs_cb_gui_status(30).
DATA : gd_vbfs_s_cb_program LIKE sy-repid.
DATA : gd_vbfs_s_cb_user_command(30).
DATA : gd_vbfs_cb_top_of_page(30).
DATA : gd_vbfs_s_cb_top_of_page(30).
DATA : gd_vbfs_s_cb_gui_status(30).
DATA : gd_vbsk_cb_program LIKE sy-repid.
DATA : gd_vbsk_cb_user_command(30).
DATA : gd_vbsk_cb_status(30).
DATA : gd_vmcfa_cb_program LIKE sy-repid.
DATA : gd_vmcfa_cb_user_command(30).
DATA : gd_vmcfa_cb_status(30).
DATA : gd_vbsk_tabname  TYPE slis_tabname.
DATA : gd_vbsk_smart    TYPE c.
DATA: BEGIN OF lt_role_split OCCURS 2,
        vbeln LIKE vbpa-kunnr,
        parvw LIKE vbpa-parvw,
        vtext LIKE tpart-vtext,
      END OF lt_role_split.

DATA: BEGIN OF gt_par_field_split OCCURS 2,
        vbeln_f LIKE vbpa-vbeln,
        vbeln_s LIKE vbpa-vbeln,
        parvw   LIKE vbpa-parvw,
        field   LIKE dntab-fieldtext,
        value_f(20),
        value_s(20),
      END OF gt_par_field_split.

DATA: BEGIN OF gt_par_nametab OCCURS 2.
        INCLUDE STRUCTURE dntab.
DATA: END   OF gt_par_nametab.

DATA: BEGIN OF gt_header_field_split OCCURS 2,
        vbeln_f LIKE vbpa-vbeln,
        vbeln_s LIKE vbpa-vbeln,
        field   LIKE dntab-fieldtext,
        value_f(20),
        value_s(20),
      END OF gt_header_field_split.

DATA: BEGIN OF gt_item_info OCCURS 2,
        vbeln_f LIKE vbrp-vbeln,
        posnr_f LIKE vbrp-posnr,
        vbeln_s LIKE vbrp-vbeln,
        posnr_s LIKE vbrp-posnr,
        field   LIKE dntab-fieldtext,
        value_f(20),
        value_s(20),
      END OF gt_item_info.

DATA: BEGIN OF gt_header_nametab OCCURS 2.
        INCLUDE STRUCTURE dntab.
DATA: END   OF gt_header_nametab.

DATA: BEGIN OF gt_item_nametab OCCURS 2.
        INCLUDE STRUCTURE dntab.
DATA: END   OF gt_item_nametab.

DATA: BEGIN OF vbrk_ausnahme_tab,
        feld1(5) VALUE 'VBELN',
        feld2(5) VALUE 'NETWR',
        feld3(5) VALUE 'KNUMV',
        feld4(5) VALUE 'VBELN',
        feld5(5) VALUE 'ERNAM',
        feld6(5) VALUE 'ERDAT',
        feld7(5) VALUE 'ERZET',
        feld8(5) VALUE 'KURRF',
        feld9(5) VALUE 'AEDAT',
        feld10(5) VALUE 'BELNR',
        feld11(5) VALUE 'RFBSK',
        feld12(5) VALUE 'MRNKZ',
        feld13(5) VALUE 'SFAKN',
        feld14(5) VALUE 'MWSBK',
        feld15(5) VALUE 'FKTYP',
      END OF vbrk_ausnahme_tab.
DATA: BEGIN OF par_ausnahme_tab,
        feld1(5) VALUE 'VBELN',
      END OF par_ausnahme_tab.

DATA : gt_vbpa LIKE vbpa OCCURS 10 WITH HEADER LINE.
DATA : role_split_indicator TYPE c.
DATA : par_field_split_indicator TYPE c.
DATA : header_field_split_indicator TYPE c.
DATA : ld_key_f LIKE vbrk-vbeln.
DATA : ld_key_s LIKE vbrk-vbeln.

DATA : gt_vbrp_f LIKE vbrp OCCURS 10 WITH HEADER LINE.
DATA : gt_vbrp_s LIKE vbrp OCCURS 10 WITH HEADER LINE.
DATA : gd_with_item_info.
DATA : gd_posinfo.
INCLUDE tabcdata.
*DATA:  XVBRK_TABIX_ZEILE_1 LIKE SY-TABIX, "Tabix der ersten Zeile
*DATA:  XVBRK_TABIX_AKTUELL LIKE SY-TABIX, "laufender Tabix der Seite
*DATA:  XVBRK_LOOPC         LIKE SY-TABIX. "Anzahl LOOP-Zeilen
DATA:  vdics_tabix_zeile_1 LIKE sy-tabix. "Tabix der ersten Zeile
DATA:  vdics_tabix_aktuell LIKE sy-tabix. "laufender Tabix der Seite
DATA:  vdics_loopc         LIKE sy-tabix. "Anzahl LOOP-Zeilen
TABLES:  rv60a.
TABLES: vdics.
TABLES:  tvfk.
TABLES: tvfkt.
*TABLES: VBRK.
TABLES: tvap.
TABLES: tvak.
TABLES: tvakt.
TABLES: vbak.
TABLES: vbap.
TABLES: likp.
TABLES: lips.
TABLES: tvlk.
TABLES: tvlkt.
DATA : fcode(20).
DATA : gt_old_vdics LIKE vdics OCCURS 10 WITH HEADER LINE.
DATA : gd_old_vbeln LIKE vdics-vbeln.
DATA : vdics_tabix LIKE sy-tabix.
DATA : tc_selline LIKE sy-stepl.
DATA : vdics_int_vbeln LIKE vbrk-vbeln.
DATA : vbapf_int_vbeln LIKE vbrk-vbeln.
DATA : gt_vbapf LIKE vbapf OCCURS 10 WITH HEADER LINE.
DATA : gd_fkart LIKE rv60a-fkart.
DATA : gt_tvcpf LIKE tvcpf OCCURS 10 WITH HEADER LINE.
DATA : gd_vbtyp LIKE vbrk-vbtyp.
DATA : gd_vbeln LIKE vbrk-vbeln.
DATA : gd_sammg LIKE vbsk-sammg.
DATA : gd_item_tabix LIKE sy-tabix.
DATA : BEGIN OF gt_legend OCCURS 10,
         statf LIKE vkdfif-statf,
         text(50).
DATA : END OF gt_legend.
DATA : gd_hotspot_mode(1).
DATA : BEGIN OF gt_fkrelk_tab OCCURS 10,
         pstyv LIKE tvap-pstyv,
END OF gt_fkrelk_tab.
DATA: gt_exc TYPE TABLE OF alv_s_qinf.

* Global field used by callback subroutine for VF04, VF24... report optimizations
DATA gd_opt_enabled TYPE abap_bool.

ENHANCEMENT-POINT LV60PTOP_01 SPOTS ES_SAPLV60P STATIC.
