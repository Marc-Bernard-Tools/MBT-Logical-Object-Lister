************************************************************************
* /MBTOOLS/BW_TLOGO_LISTER
* MBT Logical Object Lister
*
* This tool lists all logical object types (TLOGO) for SAP BW. You can
* sort objects in three ways, display object properties, and check if
* objects are compatible with SAP BW/4HANA.
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

REPORT /mbtools/bw_tlogo_lister.

CONSTANTS:
  c_version TYPE string VALUE '1.0.0'.

TABLES:
  rstlogoprop, objh, objs, icon.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS:
  so_tlogo FOR rstlogoprop-tlogo.
PARAMETERS:
  p_subobj AS CHECKBOX DEFAULT 'X',
  p_bpc    AS CHECKBOX MODIF ID bpc.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
PARAMETERS:
  p_bw  RADIOBUTTON GROUP g3 DEFAULT 'X',
  p_b4h RADIOBUTTON GROUP g3,
  p_bw4 RADIOBUTTON GROUP g3.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS:
  p_bytext RADIOBUTTON GROUP g2 DEFAULT 'X',
  p_byname RADIOBUTTON GROUP g2,
  p_bysequ RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME.
PARAMETERS:
  p_prop  AS CHECKBOX,
  p_cache TYPE c NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b4.

*&---------------------------------------------------------------------*

CONSTANTS:
  c_ujt_invisible_types TYPE funcname VALUE 'UJT_TLOGO_TYPE_DETAILS'.

DATA:
  gr_tree       TYPE REF TO /mbtools/cl_tree,
  g_t_tree      TYPE rsawbn_t_tree,
  g_t_tlogo     TYPE rs_t_tlogo,
  g_t_bpc       TYPE rs_t_tlogo,
  g_t_blacklist TYPE rs_t_tlogo.

*&---------------------------------------------------------------------*

INCLUDE /mbtools/bc_tree_t01.    " Control Definitions

INCLUDE /mbtools/bc_tree_t02.    " Control PBO and PAI

*&---------------------------------------------------------------------*
*&      Form  prepare_tree
*&---------------------------------------------------------------------*
FORM prepare_tree.

* Get all TLOGOs (except for old CompositeProvider which is local only)
  SELECT tlogo FROM rstlogoprop INTO TABLE g_t_tlogo
    WHERE tlogo <> 'COPR'.

* Get hidden BPC TLOGOs
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname           = c_ujt_invisible_types
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.
  IF sy-subrc = 0.
    CALL FUNCTION c_ujt_invisible_types
      IMPORTING
        e_t_tlogo_invisible = g_t_bpc.
  ENDIF.

* Get tree model
  DATA:
    lr_tree_model TYPE REF TO cl_rsawbn_tree_model_fl_lsys.

  CREATE OBJECT lr_tree_model.

  lr_tree_model->create_tree( ).
  lr_tree_model->set_view( 'M' ).

  g_t_tree = lr_tree_model->get_tree( ).

ENDFORM.                    "prepare_tree

*&---------------------------------------------------------------------*
*&      Form  process_main
*&---------------------------------------------------------------------*
FORM process_main USING
  VALUE(i_tlogo)   TYPE rstlogo
  VALUE(i_domname) TYPE domname
  VALUE(i_level)   TYPE i.

  TYPES:
    BEGIN OF ys_value,
      domvalue_l TYPE domvalue_l,
      valpos     TYPE valpos,
      appval     TYPE ddappval,
      ddtext     TYPE val_text,
    END OF ys_value.

  DATA:
    lr_level   TYPE REF TO /mbtools/cl_tree_level,
    l_valtab   TYPE tabname,
    l_valfld   TYPE fieldname,
    l_txttab   TYPE tabname,
    l_txtfld   TYPE fieldname,
    l_field    TYPE fieldname,
    l_len      TYPE intlen,
    l_maxlen   TYPE i,
    l_columns  TYPE string,
    l_tables   TYPE string,
    l_where    TYPE string,
    l_order    TYPE string,
    l_valpos   TYPE valpos,
    ls_value   TYPE ys_value,
    lt_value   TYPE TABLE OF ys_value,
    l_dummy    TYPE string,
    l_title    TYPE c LENGTH 80,
    l_hidden   TYPE rs_bool,
    l_no_b4h   TYPE rs_bool,
    l_type     TYPE rspc_type,
    l_tlogo    TYPE rstlogo,
    l_rstxtlg  TYPE rstxtlg,
    l_cubetype TYPE rscubetype,
    l_iobjtp   TYPE rsd_iobjtp,
    l_deftp    TYPE rzd1_deftp,
    l_srctype  TYPE rsa_srctype.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

* Check if domain has value table
  IF NOT i_domname IS INITIAL.
    SELECT SINGLE entitytab FROM dd01l INTO l_valtab
      WHERE domname = i_domname AND as4local = 'A'.
    IF sy-subrc = 0 AND l_valtab IS INITIAL.
*     Get fix values from domain
      SELECT a~domvalue_l a~valpos a~appval b~ddtext INTO TABLE lt_value
        FROM dd07l AS a LEFT OUTER JOIN dd07t AS b
        ON a~domname = b~domname AND a~valpos = b~valpos AND b~ddlanguage = sy-langu
        WHERE a~domname = i_domname AND a~as4local = 'A'.
    ELSE.
*     Get values from table
      SELECT SINGLE fieldname FROM dd03l INTO l_valfld
        WHERE tabname = l_valtab AND rollname = i_domname AND as4local = 'A'.
      IF sy-subrc = 0.
        SELECT SINGLE tabname FROM dd08l INTO l_txttab
          WHERE checktable = l_valtab AND frkart = 'TEXT'.
        IF sy-subrc = 0.
          l_maxlen = 0.
          SELECT fieldname intlen FROM dd03l INTO (l_field, l_len)
            WHERE tabname = l_txttab AND inttype = 'C' AND as4local = 'A'.
            IF l_len > l_maxlen.
              l_txtfld = l_field.
              l_maxlen = l_len.
            ENDIF.
          ENDSELECT.
          IF sy-subrc = 0.
            l_columns = 'p~&1 t~&2'.
            REPLACE '&1' WITH l_valfld INTO l_columns.
            REPLACE '&2' WITH l_txtfld INTO l_columns.
            l_tables  = '&1 AS p JOIN &2 AS t ON p~&3 = t~&4'.
            REPLACE '&1' WITH l_valtab INTO l_tables.
            REPLACE '&2' WITH l_txttab INTO l_tables.
            REPLACE '&3' WITH l_valfld INTO l_tables.
            REPLACE '&4' WITH l_valfld INTO l_tables.
            l_where   = 'langu = ''&1'''.
            REPLACE '&1' WITH sy-langu INTO l_where.
            l_order = 'p~&1'.
            REPLACE '&1' WITH l_valfld INTO l_order.
          ENDIF.
        ENDIF.
        IF l_columns IS INITIAL.
          l_columns = l_valfld.
          l_tables  = l_valtab.
          l_where   = ''.
        ENDIF.
        l_valpos = 0.
        SELECT (l_columns) FROM (l_tables)
          INTO (ls_value-domvalue_l, ls_value-ddtext)
          WHERE (l_where) ORDER BY (l_order).
          ADD 1 TO l_valpos.
          ls_value-valpos = l_valpos.
          APPEND ls_value TO lt_value.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDIF.

* Additional cases (missing in domain values)
  CASE i_tlogo.
    WHEN rs_c_tlogo-infocube.
*      ls_value-domvalue_l = rsd_c_cubetype-spo.
*      ls_value-valpos     = 100.
*      ls_value-ddtext     = 'Semantic Partitioned Object'.
*      COLLECT ls_value INTO lt_value.
    WHEN rs_c_tlogo-infoobject.
*      ls_value-domvalue_l = rsd_c_objtp-attribute.
*      ls_value-valpos     = 100.
*      ls_value-ddtext     = 'Attribute'.
*      COLLECT ls_value INTO lt_value.
*      ls_value-domvalue_l = rsd_c_objtp-meta.
*      ls_value-valpos     = 101.
*      ls_value-ddtext     = 'Meta-InfoObject'.
*      COLLECT ls_value INTO lt_value.
    WHEN rs_c_tlogo-element.
      ls_value-domvalue_l = rzd1_c_deftp-sheet.
      ls_value-valpos     = 100.
      ls_value-ddtext     = 'Sheet'.
      COLLECT ls_value INTO lt_value.
      ls_value-domvalue_l = rzd1_c_deftp-cell.
      ls_value-valpos     = 101.
      ls_value-ddtext     = 'Cell'.
      COLLECT ls_value INTO lt_value.
      ls_value-domvalue_l = rzd1_c_deftp-exception.
      ls_value-valpos     = 102.
      ls_value-ddtext     = 'Exception'.
      COLLECT ls_value INTO lt_value.
      ls_value-domvalue_l = rzd1_c_deftp-condition.
      ls_value-valpos     = 103.
      ls_value-ddtext     = 'Condition'.
      COLLECT ls_value INTO lt_value.
    WHEN rs_c_tlogo-process_variant.
      DELETE lt_value WHERE domvalue_l CP 'Y*' OR domvalue_l CP 'Z*'.
  ENDCASE.

* Rename SAP InfoSets to avoid confusion with BW InfoSets
  LOOP AT lt_value INTO ls_value WHERE domvalue_l BETWEEN 'AQ' AND 'AQZZ'.
    CONCATENATE 'SAP' ls_value-ddtext INTO ls_value-ddtext SEPARATED BY space.
    MODIFY lt_value FROM ls_value.
  ENDLOOP.

* Sort by description, technical name, or transport order
  CASE rs_c_true.
    WHEN p_bytext.
      SORT lt_value BY ddtext.
    WHEN p_byname.
      SORT lt_value BY domvalue_l.
    WHEN p_bysequ.
      SORT lt_value BY valpos.
  ENDCASE.

  LOOP AT lt_value INTO ls_value.

    IF i_tlogo IS INITIAL.
      CHECK ls_value-domvalue_l IN so_tlogo.
    ENDIF.

    lr_level->value = ls_value-domvalue_l.

    lr_level->text = ls_value-ddtext.

*   Clean-up some texts
    IF lr_level->text IS INITIAL.
      lr_level->text = 'No text'.
    ELSEIF lr_level->text CS '(->'.
      SPLIT lr_level->text AT '(' INTO lr_level->text l_dummy.
    ELSEIF lr_level->text CS 'Configuration for'.
      REPLACE 'Configuration for' WITH '' INTO lr_level->text.
    ENDIF.

*   Get icon
    CASE i_tlogo.
      WHEN rs_c_tlogo-infocube.
        l_title = 'Sub-object'.
        l_cubetype = lr_level->value.

        CALL METHOD cl_rso_repository=>get_tlogo_icon
          EXPORTING
            i_tlogo    = rs_c_tlogo-infocube
            i_cubetype = l_cubetype
          RECEIVING
            r_icon     = lr_level->icon.

*       Cases that are not handled properly
        IF p_cache IS INITIAL.
          CASE l_cubetype.
            WHEN rsd_c_cubetype-virtual.
              lr_level->icon = icon_biw_virtual_info_provider.
            WHEN rsd_c_cubetype-hybrid.
              lr_level->icon = icon_write_file.
          ENDCASE.
        ENDIF.

        CALL METHOD cl_rso_repository=>get_tlogo_description
          EXPORTING
            i_tlogo       = rs_c_tlogo-infocube
*           i_cubetype    = l_cubetype
          RECEIVING
            r_description = l_rstxtlg.

      WHEN rs_c_tlogo-infoobject.
        l_title = 'Sub-object'.
        l_iobjtp = lr_level->value.

        CALL METHOD cl_rso_repository=>get_tlogo_icon
          EXPORTING
            i_tlogo  = rs_c_tlogo-infoobject
            i_iobjtp = l_iobjtp
          RECEIVING
            r_icon   = lr_level->icon.

        CALL METHOD cl_rso_repository=>get_tlogo_description
          EXPORTING
            i_tlogo       = rs_c_tlogo-infoobject
            i_iobjtp      = l_iobjtp
          RECEIVING
            r_description = l_rstxtlg.

      WHEN rs_c_tlogo-element.
        l_title = 'Sub-object'.
        l_deftp = lr_level->value.

        CALL METHOD cl_rso_repository=>get_tlogo_icon
          EXPORTING
            i_tlogo              = rs_c_tlogo-element
            i_query_element_type = l_deftp
          RECEIVING
            r_icon               = lr_level->icon.

*       Cases that are not handled properly
        IF p_cache IS INITIAL.
          CASE l_deftp.
            WHEN rzd1_c_deftp-exception.
              lr_level->icon = icon_bw_exception_monitor.
            WHEN rzd1_c_deftp-condition.
              lr_level->icon = icon_summarize.
          ENDCASE.
        ENDIF.

        CALL METHOD cl_rso_repository=>get_tlogo_description
          EXPORTING
            i_tlogo              = rs_c_tlogo-element
            i_query_element_type = l_deftp
          RECEIVING
            r_description        = l_rstxtlg.

      WHEN rs_c_tlogo-logsys.
        l_title = 'Sub-object'.
        l_srctype = lr_level->value.

        CALL METHOD cl_rsar_srctype=>get_icon
          EXPORTING
            i_srctype = l_srctype
          RECEIVING
            e_icon    = lr_level->icon.

        CALL METHOD cl_rsar_srctype=>get_description
          EXPORTING
            i_srctype     = l_srctype
          RECEIVING
            e_description = l_rstxtlg.

      WHEN rs_c_tlogo-process_variant.

        l_title = 'Sub-object'.

        SELECT SINGLE icon FROM rsprocesstypes INTO lr_level->icon
          WHERE type = lr_level->value.
        IF sy-subrc <> 0.
          lr_level->icon = icon_dummy.
        ENDIF.

        SELECT SINGLE description FROM rsprocesstypest INTO l_rstxtlg
          WHERE langu = sy-langu AND type = lr_level->value.
        IF sy-subrc <> 0.
          l_rstxtlg = 'Unknown process type'.
        ENDIF.

      WHEN OTHERS.
        l_title = 'Object'.
        l_tlogo = lr_level->value.

        CALL METHOD cl_rso_repository=>get_tlogo_icon
          EXPORTING
            i_tlogo = l_tlogo
          RECEIVING
            r_icon  = lr_level->icon.

        CALL METHOD cl_rso_repository=>get_tlogo_description
          EXPORTING
            i_tlogo       = l_tlogo
          RECEIVING
            r_description = l_rstxtlg.
    ENDCASE.

*   Check for hidden, local or obsolete objects
    CLEAR l_hidden.

    IF NOT l_tlogo IS INITIAL.
      READ TABLE g_t_bpc TRANSPORTING NO FIELDS
        WITH TABLE KEY tlogo = l_tlogo.
      IF sy-subrc = 0.
        IF p_bpc = rs_c_false.
          CONTINUE.
        ENDIF.
        IF lr_level->text(3) <> 'BPC'.
          CONCATENATE 'BPC' lr_level->text INTO lr_level->text SEPARATED BY space.
        ENDIF.
        l_hidden = rs_c_true.
        lr_level->text   = lr_level->text && ' [hidden]'.
      ELSE.
        READ TABLE g_t_tlogo TRANSPORTING NO FIELDS
          WITH TABLE KEY tlogo = l_tlogo.
        IF sy-subrc <> 0.
          lr_level->text = lr_level->text && ' [local]'.
        ENDIF.
      ENDIF.
    ENDIF.

*   Check B4H mode or SAP BW/4HANA compatibility
    IF p_b4h = rs_c_true.
      CLEAR l_no_b4h.

      CASE i_tlogo.
        WHEN rs_c_tlogo-infocube.
          l_no_b4h = rs_c_true.
        WHEN rs_c_tlogo-logsys.
          IF NOT lr_level->value CA 'OHF'. "ODP, HANA, File
            l_no_b4h = rs_c_true.
          ENDIF.
        WHEN rs_c_tlogo-process_variant.
          l_type = lr_level->value.

          IF l_type <> 'NDB_MERGE'.
            CALL FUNCTION 'RSSM_PROCESS_NEEDED'
              EXPORTING
                i_type         = l_type
                i_simulate_b4h = rs_c_true
              IMPORTING
                e_hide         = l_no_b4h.
          ENDIF.
        WHEN OTHERS.
          READ TABLE g_t_blacklist TRANSPORTING NO FIELDS
            WITH TABLE KEY tlogo = l_tlogo.
          IF sy-subrc = 0.
            l_no_b4h = rs_c_true.
          ENDIF.
      ENDCASE.

      IF l_no_b4h = rs_c_true.
        lr_level->text   = lr_level->text && ' [not supported in B4H mode]'.
        l_hidden = rs_c_true.
        lr_level->icon   = icon_dummy.
      ENDIF.

    ELSEIF p_bw4 = rs_c_true.

      CASE i_tlogo.
        WHEN rs_c_tlogo-logsys.
          IF NOT lr_level->value CA 'OHF'. "ODP, HANA, File
            CONTINUE. ">>>
          ENDIF.
        WHEN rs_c_tlogo-process_variant.
          l_type = lr_level->value.

          IF l_type <> 'NDB_MERGE'.
            CALL FUNCTION 'RSSM_PROCESS_NEEDED'
              EXPORTING
                i_type         = l_type
                i_simulate_b4h = rs_c_true
              IMPORTING
                e_hide         = l_no_b4h.

            IF l_no_b4h = rs_c_true.
              CONTINUE. ">>>
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          READ TABLE g_t_blacklist TRANSPORTING NO FIELDS
            WITH TABLE KEY tlogo = l_tlogo.
          IF sy-subrc = 0.
            CONTINUE. ">>>
          ENDIF.
      ENDCASE.
    ENDIF.

*TO-DO: Mark 3.x objects
*
*    DATA: l_image TYPE tv_image.
*
*    CALL METHOD cl_rsawbn_obj_service=>get_obsolete_icon
*      EXPORTING
*        i_tlogo  = l_tlogo
*      RECEIVING
*        re_image = l_image.
*    IF NOT l_image IS INITIAL.
*      CONCATENATE '@HO@' lr_level->text INTO lr_level->text SEPARATED BY space.
*    ENDIF.

*   Add to output
    CALL METHOD gr_tree->add_detail
      EXPORTING
        i_icon   = lr_level->icon
        i_title  = l_title
        i_text   = lr_level->text
        i_value  = lr_level->value
        i_hidden = l_hidden
        i_level  = lr_level->level.

*   Reset cache
    IF p_cache IS INITIAL.
      CASE i_tlogo.
        WHEN rs_c_tlogo-infocube.
          CALL METHOD cl_rso_repository=>get_tlogo_icon
            EXPORTING
              i_tlogo    = 'ZZZZ'
              i_cubetype = 'Z'
            RECEIVING
              r_icon     = l_dummy.

        WHEN rs_c_tlogo-infoobject.
          CALL METHOD cl_rso_repository=>get_tlogo_icon
            EXPORTING
              i_tlogo  = 'ZZZZ'
              i_iobjtp = 'ZZZ'
            RECEIVING
              r_icon   = l_dummy.

        WHEN rs_c_tlogo-element.
          CALL METHOD cl_rso_repository=>get_tlogo_icon
            EXPORTING
              i_tlogo              = 'ZZZZ'
              i_query_element_type = 'ZZZ'
            RECEIVING
              r_icon               = l_dummy.

        WHEN OTHERS.
          CALL METHOD cl_rso_repository=>get_tlogo_icon
            EXPORTING
              i_tlogo = 'ZZZZ'
            RECEIVING
              r_icon  = l_dummy.
      ENDCASE.
    ENDIF.

    lr_level->next( ).

    IF p_prop = rs_c_true.
      IF l_tlogo IS INITIAL.
        PERFORM process_icon USING l_tlogo lr_level->icon lr_level->level.
      ELSE.
        PERFORM process_properties USING l_tlogo lr_level->icon l_rstxtlg lr_level->level.
      ENDIF.
    ENDIF.

    IF p_subobj = rs_c_true.
      CASE l_tlogo.
        WHEN rs_c_tlogo-infocube.
          PERFORM process_main USING l_tlogo 'RSCUBETYPE' lr_level->level.
        WHEN rs_c_tlogo-infoobject.
          PERFORM process_main USING l_tlogo 'RSIOBJTP' lr_level->level.
        WHEN rs_c_tlogo-element.
          PERFORM process_main USING l_tlogo 'RSZDEFTP' lr_level->level.
        WHEN rs_c_tlogo-logsys.
          PERFORM process_main USING l_tlogo 'RSSRCTYPE_BW' lr_level->level.
        WHEN rs_c_tlogo-process_variant.
          PERFORM process_main USING l_tlogo 'RSPC_TYPE' lr_level->level.
        WHEN rs_c_tlogo-analysis_process.
          PERFORM process_anpr USING lr_level->level.
      ENDCASE.
    ENDIF.

    lr_level->back( ).

  ENDLOOP.

ENDFORM.                    "process_main

*&---------------------------------------------------------------------*
*&      Form  process_properties
*&---------------------------------------------------------------------*
FORM process_properties USING
  VALUE(i_tlogo)   TYPE rstlogo
  VALUE(i_icon)    TYPE icon_d
  VALUE(i_text)    TYPE rstxtlg
  VALUE(i_level)   TYPE i.

  DATA:
    lr_level   TYPE REF TO /mbtools/cl_tree_level,
    l_tabname  TYPE tabname,
    l_funcname TYPE funcname,
    l_clsname  TYPE seoclsname.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

  CALL METHOD gr_tree->add_sub_node
    EXPORTING
      i_icon  = icon_icon_list
      i_title = 'Properties'.

  lr_level->next( ).

* BI Content
  SELECT SINGLE tlogo_d FROM rstlogoprop INTO lr_level->value
    WHERE tlogo = i_tlogo.
  IF NOT lr_level->value IS INITIAL.
    CALL METHOD cl_rsrq_lookup=>get_text_from_domain
      EXPORTING
        i_domain = 'RSTLOGO_D'
        i_value  = lr_level->value
      IMPORTING
        e_text   = lr_level->text.

    CALL METHOD gr_tree->add_detail
      EXPORTING
        i_icon  = i_icon
        i_title = 'BI Content Object'
        i_text  = lr_level->text
        i_value = lr_level->value
        i_level = lr_level->level.
  ENDIF.

* Description
  CALL METHOD gr_tree->add_detail
    EXPORTING
      i_icon  = i_icon
      i_title = 'Respository Description'
      i_text  = i_text
      i_value = ''
      i_level = lr_level->level.

* Icon
  SELECT SINGLE id name FROM icon INTO (lr_level->value, lr_level->text)
    WHERE id = i_icon.
  IF sy-subrc = 0.
    CALL METHOD gr_tree->add_detail
      EXPORTING
        i_icon  = i_icon
        i_title = 'Respository Icon'
        i_text  = lr_level->text
        i_value = lr_level->value
        i_level = lr_level->level.
  ENDIF.

* Object
  SELECT SINGLE ddtext FROM objt INTO lr_level->text
    WHERE objectname = i_tlogo AND language = sy-langu.
  IF sy-subrc = 0.
    CALL METHOD gr_tree->add_detail
      EXPORTING
        i_icon  = ''
        i_title = 'Object Description'
        i_text  = lr_level->text
        i_value = ''
        i_level = lr_level->level.
  ENDIF.

  SELECT SINGLE objecttype FROM objh INTO lr_level->value
    WHERE objectname = i_tlogo.
  IF sy-subrc = 0.
    CALL METHOD cl_rsrq_lookup=>get_text_from_domain
      EXPORTING
        i_domain = 'OB_TYP'
        i_value  = lr_level->value
      IMPORTING
        e_text   = lr_level->text.

    CALL METHOD gr_tree->add_detail
      EXPORTING
        i_icon  = ''
        i_title = 'Object Type'
        i_text  = lr_level->text
        i_value = lr_level->value
        i_level = lr_level->level.
  ENDIF.

* Package
  SELECT SINGLE b~devclass INTO lr_level->value
    FROM objs AS a JOIN tadir AS b
    ON a~tabname = b~obj_name
    WHERE a~objectname = i_tlogo AND a~prim_table = 'X'
      AND b~pgmid = 'R3TR' AND b~object = 'TABL'.
  IF sy-subrc = 0.
    SELECT SINGLE ctext FROM tdevct INTO lr_level->text
      WHERE devclass = lr_level->value AND spras = sy-langu.
    IF sy-subrc <> 0.
      lr_level->text = 'No text'.
    ENDIF.

    CALL METHOD gr_tree->add_detail
      EXPORTING
        i_icon  = icon_package_standard
        i_title = 'Package'
        i_text  = lr_level->text
        i_value = lr_level->value
        i_level = lr_level->level
        i_type  = 'DEVC'.
  ENDIF.

* Primary table
  SELECT SINGLE tabname FROM objs INTO l_tabname
    WHERE objectname = i_tlogo AND prim_table = 'X'.
  IF sy-subrc = 0.
    PERFORM write_table
      USING l_tabname 'Primary Table' lr_level->level.
  ENDIF.

* Dependent tables
  SELECT tabname FROM objs INTO l_tabname
    WHERE objectname = i_tlogo AND prim_table = ''
    ORDER BY tabname.

    PERFORM write_table
      USING l_tabname 'Dependent Table' lr_level->level.

  ENDSELECT.

* ABAP Class
  SELECT SINGLE class FROM rstlogoprop INTO l_clsname
    WHERE tlogo = i_tlogo.

  PERFORM write_class
    USING l_clsname 'ABAP Class' lr_level->level.

* ABAP Collaction Class
  SELECT SINGLE class_coll FROM rstlogoprop INTO l_clsname
    WHERE tlogo = i_tlogo.

  PERFORM write_class
    USING l_clsname 'ABAP Class (Collection)' lr_level->level.

* ABAP Function
  l_funcname = 'RSO_' && i_tlogo && '_MAINTAIN'.

  PERFORM write_function
    USING l_funcname 'ABAP Function' lr_level->level.

* Transport Functions
  l_funcname = 'RS_' && i_tlogo && '_BEFORE_EXPORT'.

  PERFORM write_function
    USING l_funcname 'Before Export' lr_level->level.

  l_funcname = 'RS_' && i_tlogo && '_AFTER_IMPORT'.

  PERFORM write_function
    USING l_funcname 'After Import' lr_level->level.

  lr_level->back( ).

ENDFORM.                    "process_properties

*&---------------------------------------------------------------------*
*&      Form  process_icon
*&---------------------------------------------------------------------*
FORM process_icon USING
  VALUE(i_tlogo)   TYPE rstlogo
  VALUE(i_icon)    TYPE icon_d
  VALUE(i_level)   TYPE i.

  DATA:
    lr_level TYPE REF TO /mbtools/cl_tree_level.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

* Icon
  SELECT SINGLE id name FROM icon INTO (lr_level->value, lr_level->text)
    WHERE id = i_icon.
  IF sy-subrc = 0.
    CALL METHOD gr_tree->add_detail
      EXPORTING
        i_icon  = i_icon
        i_title = 'Icon'
        i_text  = lr_level->text
        i_value = lr_level->value
        i_level = lr_level->level.
  ENDIF.

ENDFORM.                    "process_icon

*&---------------------------------------------------------------------*
*&      Form  process_anpr
*&---------------------------------------------------------------------*
FORM process_anpr USING
  VALUE(i_level)   TYPE i.

  DATA:
    lr_level TYPE REF TO /mbtools/cl_tree_level,
    lr_appl  TYPE REF TO cl_rsan_fct_appl_type,
    ls_group TYPE cl_rsan_fct_appl_type=>ys_appltoolgroup.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

  lr_appl = cl_rsan_fct_appl_type=>get_appl_type( 'GENERIC' ).

  lr_level->next( ).

  LOOP AT lr_appl->th_appltoolgroups INTO ls_group.

    PERFORM process_anpr_group USING ls_group lr_level->level.

  ENDLOOP.

  lr_level->back( ).

ENDFORM.                    "process_anpr

*&---------------------------------------------------------------------*
*&      Form  process_anpr_group
*&---------------------------------------------------------------------*
FORM process_anpr_group USING
  VALUE(is_group)  TYPE cl_rsan_fct_appl_type=>ys_appltoolgroup
  VALUE(i_level)   TYPE i.

  DATA:
    lr_level TYPE REF TO /mbtools/cl_tree_level,
    ls_tool  TYPE cl_rsan_fct_appl_type=>ys_appltool,
    l_hidden TYPE rs_bool.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

  lr_level->text = is_group-text.

  CLEAR l_hidden.

  CASE is_group-toolgroup.
    WHEN 'DS'.
      lr_level->icon = icon_bw_apd_target.
    WHEN 'DST'.
      lr_level->icon = icon_bw_apd_transformation.
    WHEN 'DT'.
      lr_level->icon = icon_bw_apd_source.
    WHEN OTHERS.
      lr_level->icon = ''.
      BREAK-POINT.
  ENDCASE.

  IF p_b4h = rs_c_true.
    l_hidden = rs_c_true.
    lr_level->text   = lr_level->text && ' [not supported in B4H mode]'.
    lr_level->icon   = icon_dummy.
  ENDIF.

  CALL METHOD gr_tree->add_detail
    EXPORTING
      i_icon   = lr_level->icon
      i_title  = 'Tool Group'
      i_text   = lr_level->text
      i_value  = is_group-toolgroup
      i_level  = lr_level->level
      i_hidden = l_hidden.

  lr_level->next( ).

  LOOP AT is_group-ts_applfunc INTO ls_tool.

    PERFORM process_anpr_tool USING ls_tool lr_level->level.

  ENDLOOP.

  lr_level->back( ).

ENDFORM.                    "process_anpr_group

*&---------------------------------------------------------------------*
*&      Form  process_anpr_tool
*&---------------------------------------------------------------------*
FORM process_anpr_tool USING
  VALUE(is_tool)   TYPE cl_rsan_fct_appl_type=>ys_appltool
  VALUE(i_level)   TYPE i.

  DATA:
    lr_level TYPE REF TO /mbtools/cl_tree_level,
    l_hidden TYPE rs_bool.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

  REPLACE '.GIF' WITH '' INTO is_tool-tool_image.
  IF is_tool-tool_image CS '/SAP/PUBLIC/BC/ICONS/'.
    REPLACE '/SAP/PUBLIC/BC/ICONS/S_' WITH '' INTO is_tool-tool_image.
    CONDENSE is_tool-tool_image NO-GAPS.
  ELSEIF is_tool-tool_image CS '/SAP/BW/EI/APD/ICONS/'.
    REPLACE '/SAP/BW/EI/APD/ICONS/' WITH '' INTO is_tool-tool_image.
    CONDENSE is_tool-tool_image NO-GAPS.

    CASE is_tool-tool_image.
      WHEN 'BWDATABASETABLE'.
        is_tool-tool_image = 'BWDATA'.
      WHEN 'BWASSOCIATIONANALYSIS'.
        is_tool-tool_image = 'BWASAN'.
      WHEN 'BWCLUSTERANALYSIS'.
        is_tool-tool_image = 'BWCLUS'.
      WHEN 'BWCOLUMNTOROW'.
        is_tool-tool_image = 'BWCORO'.
      WHEN 'BWDATAMINING'.
        is_tool-tool_image = 'BWDMTH'.
      WHEN 'BWDECISIONTREE'.
        is_tool-tool_image = 'BWDETR'.
      WHEN 'BWREGRESSIONANALYSIS'.
        is_tool-tool_image = 'BWREGR'.
      WHEN 'BWROWTOCOLUMN'.
        is_tool-tool_image = 'BWROCO'.
      WHEN 'BWSOURCECRM'.
        is_tool-tool_image = 'B_BWSG'.
      WHEN 'BWWEIGHTEDSCORETABLE'.
        is_tool-tool_image = 'BWWSTA'.
      WHEN 'DST_REGRESSION'.
        is_tool-tool_image = 'BWREGR'.
      WHEN OTHERS.
        is_tool-tool_image = ''.
        BREAK-POINT.
    ENDCASE.
  ENDIF.
  CONCATENATE '@' is_tool-tool_image(6) '@' INTO is_tool-tool_image.
  CONDENSE is_tool-tool_image NO-GAPS.

  SELECT SINGLE id FROM icon INTO lr_level->icon
    WHERE internal = is_tool-tool_image.
  IF sy-subrc <> 0.
    lr_level->icon = ''.
  ENDIF.

  lr_level->text = is_tool-text.

  IF p_b4h = rs_c_true.
    l_hidden = rs_c_true.
    lr_level->text = lr_level->text && ' [not supported in B4H mode]'.
    lr_level->icon = icon_dummy.
  ENDIF.

  CALL METHOD gr_tree->add_detail
    EXPORTING
      i_icon   = lr_level->icon
      i_title  = 'Tool'
      i_text   = lr_level->text
      i_value  = is_tool-tool
      i_level  = lr_level->level
      i_hidden = l_hidden.

  IF p_prop = rs_c_true.
    PERFORM process_icon USING 'ANPR' lr_level->icon lr_level->level.
  ENDIF.

  lr_level->back( ).

ENDFORM.                    "process_anpr_tool

*&---------------------------------------------------------------------*
*&      Form  write_table
*&---------------------------------------------------------------------*
FORM write_table USING
  VALUE(i_table)   TYPE tabname
  VALUE(i_title)   TYPE rstxtlg
  VALUE(i_level)   TYPE i.

  DATA:
    lr_level   TYPE REF TO /mbtools/cl_tree_level,
    l_rollname TYPE rollname.

  CHECK NOT i_table IS INITIAL.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

  SELECT SINGLE tabname FROM dd02l INTO lr_level->value
    WHERE tabname = i_table.
  IF sy-subrc = 0.
    SELECT SINGLE ddtext FROM dd02t INTO lr_level->text
      WHERE tabname = i_table AND as4local = 'A'
        AND ddlanguage = sy-langu.
    IF sy-subrc <> 0.
      lr_level->text = 'No text'.
    ENDIF.

    CALL METHOD gr_tree->add_detail
      EXPORTING
        i_icon  = icon_database_table
        i_title = i_title
        i_text  = lr_level->text
        i_value = lr_level->value
        i_level = lr_level->level
        i_type  = 'TABL'.

*    SELECT fieldname rollname FROM dd03l INTO (lr_level->value,l_rollname)
*      WHERE tabname = i_table AND as4local = 'A'
*        AND ( rollname IN ('PROGRAMM','PROGRAM','PROGNAME','ClASSNAME','SEOCLSNAME','FUNCNAME')
*         OR   domname  IN ('PROGRAMM','PROGRAM','PROGNAME','ClASSNAME','SEOCLSNAME','FUNCNAME') ).
*
*      SELECT SINGLE ddtext FROM dd04t INTO lr_level->text
*        WHERE rollname = l_rollname AND as4local = 'A'
*          AND ddlanguage = sy-langu.
*      IF sy-subrc <> 0.
*        SELECT SINGLE ddtext FROM dd03t INTO lr_level->text
*          WHERE tabname = i_table AND fieldname = lr_level->value AND as4local = 'A'
*            AND ddlanguage = sy-langu.
*        IF sy-subrc <> 0.
*          lr_level->text = 'No text'.
*        ENDIF.
*      ENDIF.
*
*      CALL METHOD gr_tree->add_detail
*        EXPORTING
*          i_icon  = icon_led_red
*          i_title = 'ABAP-related Field'
*          i_text  = lr_level->text
*          i_value = lr_level->value
*          i_level = lr_level->level
*          i_type  = ''.
*    ENDSELECT.

  ENDIF.

ENDFORM.                    "write_table

*&---------------------------------------------------------------------*
*&      Form  write_function
*&---------------------------------------------------------------------*
FORM write_function USING
  VALUE(i_funct)   TYPE funcname
  VALUE(i_title)   TYPE rstxtlg
  VALUE(i_level)   TYPE i.

  DATA:
    lr_level TYPE REF TO /mbtools/cl_tree_level.

  CHECK NOT i_funct IS INITIAL.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

  SELECT SINGLE funcname FROM tfdir INTO lr_level->value
    WHERE funcname = i_funct.
  IF sy-subrc = 0.
    SELECT SINGLE stext FROM tftit INTO lr_level->text
      WHERE funcname = i_funct AND spras = sy-langu.
    IF sy-subrc <> 0.
      lr_level->text = 'No text'.
    ENDIF.

    CALL METHOD gr_tree->add_detail
      EXPORTING
        i_icon  = icon_abap
        i_title = i_title
        i_text  = lr_level->text
        i_value = lr_level->value
        i_level = lr_level->level
        i_type  = 'FUNC'.
  ENDIF.

ENDFORM.                    "write_function

*&---------------------------------------------------------------------*
*&      Form  write_class
*&---------------------------------------------------------------------*
FORM write_class USING
  VALUE(i_class)   TYPE seoclsname
  VALUE(i_title)   TYPE rstxtlg
  VALUE(i_level)   TYPE i.

  DATA:
    lr_level TYPE REF TO /mbtools/cl_tree_level.

  CHECK NOT i_class IS INITIAL.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

  SELECT SINGLE clsname FROM seoclass INTO lr_level->value
    WHERE clsname = i_class.
  IF sy-subrc = 0.
    SELECT SINGLE descript FROM seoclasstx INTO lr_level->text
      WHERE clsname = i_class AND langu = sy-langu.
    IF sy-subrc <> 0.
      lr_level->text = 'No text'.
    ENDIF.

    CALL METHOD gr_tree->add_detail
      EXPORTING
        i_icon  = icon_abap
        i_title = i_title
        i_text  = lr_level->text
        i_value = lr_level->value
        i_level = lr_level->level
        i_type  = 'CLAS'.
  ENDIF.

ENDFORM.                    "write_class

*&---------------------------------------------------------------------*
*&      Form  tlogo_blacklist
*&---------------------------------------------------------------------*
FORM prepare_tlogo_blacklist.

* See BW 7.50: CL_RS_B4HANA_UTIL=>_FILL_TLOGO_BLACKLIST
  APPEND 'AGGR' TO g_t_blacklist.
  APPEND 'DAGR' TO g_t_blacklist.
  APPEND 'ANMO' TO g_t_blacklist.
  APPEND 'DANM' TO g_t_blacklist.
  APPEND 'ANPR' TO g_t_blacklist.
  APPEND 'DANP' TO g_t_blacklist.
  APPEND 'ANSO' TO g_t_blacklist.
  APPEND 'DANS' TO g_t_blacklist.
  APPEND 'AABC' TO g_t_blacklist.
  APPEND 'AADT' TO g_t_blacklist.
  APPEND 'AAPP' TO g_t_blacklist.
  APPEND 'AAPS' TO g_t_blacklist.
  APPEND 'ABPC' TO g_t_blacklist.
  APPEND 'ABPF' TO g_t_blacklist.
  APPEND 'ABRU' TO g_t_blacklist.
  APPEND 'ACGA' TO g_t_blacklist.
  APPEND 'ACGP' TO g_t_blacklist.
  APPEND 'ACGS' TO g_t_blacklist.
  APPEND 'ACLB' TO g_t_blacklist.
  APPEND 'ACTR' TO g_t_blacklist.
  APPEND 'ADAF' TO g_t_blacklist.
  APPEND 'ADEE' TO g_t_blacklist.
  APPEND 'ADEI' TO g_t_blacklist.
  APPEND 'ADEL' TO g_t_blacklist.
  APPEND 'ADIM' TO g_t_blacklist.
  APPEND 'ADMC' TO g_t_blacklist.
  APPEND 'ADMD' TO g_t_blacklist.
  APPEND 'ADMF' TO g_t_blacklist.
  APPEND 'ADMG' TO g_t_blacklist.
  APPEND 'ADML' TO g_t_blacklist.
  APPEND 'ADMP' TO g_t_blacklist.
  APPEND 'ADMS' TO g_t_blacklist.
  APPEND 'ADTG' TO g_t_blacklist.
  APPEND 'AFLC' TO g_t_blacklist.
  APPEND 'AFLD' TO g_t_blacklist.
  APPEND 'AFLE' TO g_t_blacklist.
  APPEND 'AFLG' TO g_t_blacklist.
  APPEND 'AJUT' TO g_t_blacklist.
  APPEND 'AKPI' TO g_t_blacklist.
  APPEND 'AMBR' TO g_t_blacklist.
  APPEND 'ARTP' TO g_t_blacklist.
  APPEND 'ASPD' TO g_t_blacklist.
  APPEND 'ASPF' TO g_t_blacklist.
  APPEND 'ASPR' TO g_t_blacklist.
  APPEND 'ATEM' TO g_t_blacklist.
  APPEND 'ATPF' TO g_t_blacklist.
  APPEND 'AWSS' TO g_t_blacklist.
  APPEND 'BAOE' TO g_t_blacklist.
  APPEND 'BITM' TO g_t_blacklist.
  APPEND 'DBIT' TO g_t_blacklist.
  APPEND 'BIXP' TO g_t_blacklist.
  APPEND 'DBIX' TO g_t_blacklist.
  APPEND 'BRSE' TO g_t_blacklist.
  APPEND 'DBRS' TO g_t_blacklist.
  APPEND 'BTMP' TO g_t_blacklist.
  APPEND 'DBTM' TO g_t_blacklist.
  APPEND 'CRWB' TO g_t_blacklist.
  APPEND 'DCRW' TO g_t_blacklist.
  APPEND 'CUBE' TO g_t_blacklist.
  APPEND 'DCUB' TO g_t_blacklist.
  APPEND 'DDAS' TO g_t_blacklist.
  APPEND 'DDDA' TO g_t_blacklist.
  APPEND 'DMMO' TO g_t_blacklist.
  APPEND 'DDMM' TO g_t_blacklist.
  APPEND 'ENHO' TO g_t_blacklist.
  APPEND 'ERPT' TO g_t_blacklist.
  APPEND 'DRPT' TO g_t_blacklist.
  APPEND 'HIER' TO g_t_blacklist.
  APPEND 'DHIE' TO g_t_blacklist.
  APPEND 'HYBR' TO g_t_blacklist.
  APPEND 'DHYB' TO g_t_blacklist.
  APPEND 'INSP' TO g_t_blacklist.
  APPEND 'IOBC' TO g_t_blacklist.
  APPEND 'DIOC' TO g_t_blacklist.
  APPEND 'ISCS' TO g_t_blacklist.
  APPEND 'DSCS' TO g_t_blacklist.
  APPEND 'ISET' TO g_t_blacklist.
  APPEND 'DISE' TO g_t_blacklist.
  APPEND 'ISFS' TO g_t_blacklist.
  APPEND 'SHFS' TO g_t_blacklist.
  APPEND 'ISIG' TO g_t_blacklist.
  APPEND 'DISG' TO g_t_blacklist.
  APPEND 'SHIG' TO g_t_blacklist.
  APPEND 'ISIP' TO g_t_blacklist.
  APPEND 'SHIP' TO g_t_blacklist.
  APPEND 'ISMP' TO g_t_blacklist.
  APPEND 'SHMP' TO g_t_blacklist.
  APPEND 'ISTD' TO g_t_blacklist.
  APPEND 'DSTD' TO g_t_blacklist.
  APPEND 'ISTS' TO g_t_blacklist.
  APPEND 'SHTR' TO g_t_blacklist.
  APPEND 'ITEM' TO g_t_blacklist.
  APPEND 'DITM' TO g_t_blacklist.
  APPEND 'KPCE' TO g_t_blacklist.
  APPEND 'DKPC' TO g_t_blacklist.
  APPEND 'KPDF' TO g_t_blacklist.
  APPEND 'DKPD' TO g_t_blacklist.
  APPEND 'LPOA' TO g_t_blacklist.
  APPEND 'LPOD' TO g_t_blacklist.
  APPEND 'MPRO' TO g_t_blacklist.
  APPEND 'DMPR' TO g_t_blacklist.
  APPEND 'ODPE' TO g_t_blacklist.
  APPEND 'ODSO' TO g_t_blacklist.
  APPEND 'DODS' TO g_t_blacklist.
  APPEND 'RAPA' TO g_t_blacklist.
  APPEND 'RASE' TO g_t_blacklist.
  APPEND 'PSA ' TO g_t_blacklist.
  APPEND 'SPOK' TO g_t_blacklist.
  APPEND 'THEM' TO g_t_blacklist.
  APPEND 'THED' TO g_t_blacklist.
  APPEND 'TMPL' TO g_t_blacklist.
  APPEND 'DTMP' TO g_t_blacklist.
  APPEND 'UPDR' TO g_t_blacklist.
  APPEND 'DUPD' TO g_t_blacklist.
  APPEND 'WWIB' TO g_t_blacklist.
  APPEND 'DWIB' TO g_t_blacklist.
  APPEND 'WWPA' TO g_t_blacklist.
  APPEND 'DWPA' TO g_t_blacklist.
  APPEND 'XCLS' TO g_t_blacklist.
  APPEND 'DXCL' TO g_t_blacklist.
  APPEND 'XLWB' TO g_t_blacklist.
  APPEND 'DXLW' TO g_t_blacklist.
  APPEND 'RDAC' TO g_t_blacklist.
  APPEND 'EREL' TO g_t_blacklist.

ENDFORM.                    "prepare_tlogo_blacklist

*&---------------------------------------------------------------------*

INITIALIZATION.

* Is BPC available?
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname           = c_ujt_invisible_types
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    LOOP AT SCREEN.
      IF screen-group1 = 'BPC'.
*        screen-active = '0'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*&---------------------------------------------------------------------*

START-OF-SELECTION.

  CREATE OBJECT gr_tree.

* Add top node
  CALL METHOD gr_tree->add_top_node
    EXPORTING
      i_icon  = icon_folder
      i_title = 'BW Logical Objects'.

* Prepare processing
  PERFORM prepare_tree.

  PERFORM prepare_tlogo_blacklist.

* Process sub nodes
  PERFORM process_main USING '' 'RSTLOGO' 0.

* Expand complete tree
  gr_tree->expand_all( ).

* Output as ALV tree control
  CALL SCREEN 100.
