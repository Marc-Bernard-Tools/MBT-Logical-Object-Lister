************************************************************************
* /MBTOOLS/CL_BW_TLOGO_LISTER
* MBT Logical Object Lister
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
class /MBTOOLS/CL_BW_TLOGO_LISTER definition
  public
  final
  create public .

public section.

  interfaces ZIF_APACK_MANIFEST .
  interfaces /MBTOOLS/IF_MANIFEST .

  types:
    ty_tlogos    TYPE RANGE OF rstlogoprop-tlogo .

  constants C_VERSION type STRING value '1.0.0' ##NO_TEXT.
  constants C_TITLE type STRING value 'MBT Logical Object Lister' ##NO_TEXT.
  constants C_DESCRIPTION type STRING value 'Display the metadata of SAP BW, SAP BPC, or SAP BW/4HANA object models' ##NO_TEXT.
  constants C_DOWNLOAD_ID type I value 3635 ##NO_TEXT.
  constants C_UJT_INVISIBLE_TYPES type FUNCNAME value 'UJT_TLOGO_TYPE_DETAILS' ##NO_TEXT.

  methods CONSTRUCTOR .
  methods INITIALIZE
    importing
      !I_TLOGOS type TY_TLOGOS
      !I_BW type ABAP_BOOL
      !I_B4H type ABAP_BOOL
      !I_BW4 type ABAP_BOOL
      !I_PROP type ABAP_BOOL
      !I_BYTEXT type ABAP_BOOL
      !I_BYNAME type ABAP_BOOL
      !I_BYSEQU type ABAP_BOOL
      !I_CACHE type ABAP_BOOL
      !I_SUBOBJ type ABAP_BOOL
      !I_BPC type ABAP_BOOL .
  methods PBO .
  methods PAI
    importing
      !I_OK_CODE type SY-UCOMM .
  methods SCREEN .
  PROTECTED SECTION.

private section.

  aliases APACK_MANIFEST
    for ZIF_APACK_MANIFEST~DESCRIPTOR .
  aliases MBT_MANIFEST
    for /MBTOOLS/IF_MANIFEST~DESCRIPTOR .

  data MR_TOOL type ref to /MBTOOLS/CL_TOOLS .
  data MR_TREE type ref to /MBTOOLS/CL_TREE .
  data M_TLOGOS type TY_TLOGOS .
  data M_BW type ABAP_BOOL .
  data M_B4H type ABAP_BOOL .
  data M_BW4 type ABAP_BOOL .
  data M_PROP type ABAP_BOOL .
  data M_BYTEXT type ABAP_BOOL .
  data M_BYNAME type ABAP_BOOL .
  data M_BYSEQU type ABAP_BOOL .
  data M_CACHE type ABAP_BOOL .
  data M_SUBOBJ type ABAP_BOOL .
  data M_BPC type ABAP_BOOL .
  data MT_TREE type RSAWBN_T_TREE .
  data MT_TLOGO type RS_T_TLOGO .
  data MT_BPC type RS_T_TLOGO .
  data MT_BLACKLIST type RS_T_TLOGO .

  methods PREPARE_TREE .
  methods PROCESS_MAIN
    importing
      !I_TLOGO type RSTLOGO
      !I_DOMNAME type DOMNAME
      !I_LEVEL type I .
  methods PROCESS_ICON
    importing
      !I_TLOGO type RSTLOGO
      !I_ICON type ICON_D
      !I_LEVEL type I .
  methods PROCESS_PROPERTIES
    importing
      !I_TLOGO type RSTLOGO
      !I_ICON type ICON_D
      !I_TEXT type RSTXTLG
      !I_LEVEL type I .
  methods PROCESS_ANPR
    importing
      !I_LEVEL type I .
  methods PROCESS_ANPR_GROUP
    importing
      !IS_GROUP type CL_RSAN_FCT_APPL_TYPE=>YS_APPLTOOLGROUP
      !I_LEVEL type I .
  methods PROCESS_ANPR_TOOL
    importing
      !IS_TOOL type CL_RSAN_FCT_APPL_TYPE=>YS_APPLTOOL
      !I_LEVEL type I .
  methods WRITE_TABLE
    importing
      !I_TABLE type TABNAME
      !I_TITLE type RSTXTLG
      !I_LEVEL type I .
  methods WRITE_FUNCTION
    importing
      !I_FUNCT type FUNCNAME
      !I_TITLE type RSTXTLG
      !I_LEVEL type I .
  methods WRITE_CLASS
    importing
      !I_CLASS type SEOCLSNAME
      !I_TITLE type RSTXTLG
      !I_LEVEL type I .
  methods PREPARE_TLOGO_BLACKLIST .
ENDCLASS.



CLASS /MBTOOLS/CL_BW_TLOGO_LISTER IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mr_tool EXPORTING i_tool = me.

    apack_manifest = mr_tool->apack_manifest.
    mbt_manifest   = mr_tool->mbt_manifest.
  ENDMETHOD.


  METHOD initialize.

    CREATE OBJECT mr_tree.

    m_tlogos  = i_tlogos.
    m_bw      = i_bw.
    m_b4h     = i_b4h.
    m_bw4     = i_bw4.
    m_prop    = i_prop.
    m_bytext  = i_bytext.
    m_byname  = i_byname.
    m_bysequ  = i_bysequ.
    m_cache   = i_cache.
    m_subobj  = i_subobj.
    m_bpc     = i_bpc.

    mr_tree->add_top_node(
      i_icon  = icon_folder
      i_title = 'BW Logical Objects' ).

    " Prepare processing
    prepare_tree( ).

    prepare_tlogo_blacklist( ).

    " Process sub nodes
    process_main( i_tlogo = '' i_domname = 'RSTLOGO' i_level = 0 ).

    " Expand complete tree
    mr_tree->expand_all( ).

  ENDMETHOD.


  METHOD pai.
    mr_tree->pai( i_ok_code = i_ok_code ).
  ENDMETHOD.


  METHOD pbo.
    mr_tree->display( ).
  ENDMETHOD.


  METHOD prepare_tlogo_blacklist.

* See BW 7.50: CL_RS_B4HANA_UTIL=>_FILL_TLOGO_BLACKLIST
    APPEND 'AGGR' TO mt_blacklist.
    APPEND 'DAGR' TO mt_blacklist.
    APPEND 'ANMO' TO mt_blacklist.
    APPEND 'DANM' TO mt_blacklist.
    APPEND 'ANPR' TO mt_blacklist.
    APPEND 'DANP' TO mt_blacklist.
    APPEND 'ANSO' TO mt_blacklist.
    APPEND 'DANS' TO mt_blacklist.
    APPEND 'AABC' TO mt_blacklist.
    APPEND 'AADT' TO mt_blacklist.
    APPEND 'AAPP' TO mt_blacklist.
    APPEND 'AAPS' TO mt_blacklist.
    APPEND 'ABPC' TO mt_blacklist.
    APPEND 'ABPF' TO mt_blacklist.
    APPEND 'ABRU' TO mt_blacklist.
    APPEND 'ACGA' TO mt_blacklist.
    APPEND 'ACGP' TO mt_blacklist.
    APPEND 'ACGS' TO mt_blacklist.
    APPEND 'ACLB' TO mt_blacklist.
    APPEND 'ACTR' TO mt_blacklist.
    APPEND 'ADAF' TO mt_blacklist.
    APPEND 'ADEE' TO mt_blacklist.
    APPEND 'ADEI' TO mt_blacklist.
    APPEND 'ADEL' TO mt_blacklist.
    APPEND 'ADIM' TO mt_blacklist.
    APPEND 'ADMC' TO mt_blacklist.
    APPEND 'ADMD' TO mt_blacklist.
    APPEND 'ADMF' TO mt_blacklist.
    APPEND 'ADMG' TO mt_blacklist.
    APPEND 'ADML' TO mt_blacklist.
    APPEND 'ADMP' TO mt_blacklist.
    APPEND 'ADMS' TO mt_blacklist.
    APPEND 'ADTG' TO mt_blacklist.
    APPEND 'AFLC' TO mt_blacklist.
    APPEND 'AFLD' TO mt_blacklist.
    APPEND 'AFLE' TO mt_blacklist.
    APPEND 'AFLG' TO mt_blacklist.
    APPEND 'AJUT' TO mt_blacklist.
    APPEND 'AKPI' TO mt_blacklist.
    APPEND 'AMBR' TO mt_blacklist.
    APPEND 'ARTP' TO mt_blacklist.
    APPEND 'ASPD' TO mt_blacklist.
    APPEND 'ASPF' TO mt_blacklist.
    APPEND 'ASPR' TO mt_blacklist.
    APPEND 'ATEM' TO mt_blacklist.
    APPEND 'ATPF' TO mt_blacklist.
    APPEND 'AWSS' TO mt_blacklist.
    APPEND 'BAOE' TO mt_blacklist.
    APPEND 'BITM' TO mt_blacklist.
    APPEND 'DBIT' TO mt_blacklist.
    APPEND 'BIXP' TO mt_blacklist.
    APPEND 'DBIX' TO mt_blacklist.
    APPEND 'BRSE' TO mt_blacklist.
    APPEND 'DBRS' TO mt_blacklist.
    APPEND 'BTMP' TO mt_blacklist.
    APPEND 'DBTM' TO mt_blacklist.
    APPEND 'CRWB' TO mt_blacklist.
    APPEND 'DCRW' TO mt_blacklist.
    APPEND 'CUBE' TO mt_blacklist.
    APPEND 'DCUB' TO mt_blacklist.
    APPEND 'DDAS' TO mt_blacklist.
    APPEND 'DDDA' TO mt_blacklist.
    APPEND 'DMMO' TO mt_blacklist.
    APPEND 'DDMM' TO mt_blacklist.
    APPEND 'ENHO' TO mt_blacklist.
    APPEND 'ERPT' TO mt_blacklist.
    APPEND 'DRPT' TO mt_blacklist.
    APPEND 'HIER' TO mt_blacklist.
    APPEND 'DHIE' TO mt_blacklist.
    APPEND 'HYBR' TO mt_blacklist.
    APPEND 'DHYB' TO mt_blacklist.
    APPEND 'INSP' TO mt_blacklist.
    APPEND 'IOBC' TO mt_blacklist.
    APPEND 'DIOC' TO mt_blacklist.
    APPEND 'ISCS' TO mt_blacklist.
    APPEND 'DSCS' TO mt_blacklist.
    APPEND 'ISET' TO mt_blacklist.
    APPEND 'DISE' TO mt_blacklist.
    APPEND 'ISFS' TO mt_blacklist.
    APPEND 'SHFS' TO mt_blacklist.
    APPEND 'ISIG' TO mt_blacklist.
    APPEND 'DISG' TO mt_blacklist.
    APPEND 'SHIG' TO mt_blacklist.
    APPEND 'ISIP' TO mt_blacklist.
    APPEND 'SHIP' TO mt_blacklist.
    APPEND 'ISMP' TO mt_blacklist.
    APPEND 'SHMP' TO mt_blacklist.
    APPEND 'ISTD' TO mt_blacklist.
    APPEND 'DSTD' TO mt_blacklist.
    APPEND 'ISTS' TO mt_blacklist.
    APPEND 'SHTR' TO mt_blacklist.
    APPEND 'ITEM' TO mt_blacklist.
    APPEND 'DITM' TO mt_blacklist.
    APPEND 'KPCE' TO mt_blacklist.
    APPEND 'DKPC' TO mt_blacklist.
    APPEND 'KPDF' TO mt_blacklist.
    APPEND 'DKPD' TO mt_blacklist.
    APPEND 'LPOA' TO mt_blacklist.
    APPEND 'LPOD' TO mt_blacklist.
    APPEND 'MPRO' TO mt_blacklist.
    APPEND 'DMPR' TO mt_blacklist.
    APPEND 'ODPE' TO mt_blacklist.
    APPEND 'ODSO' TO mt_blacklist.
    APPEND 'DODS' TO mt_blacklist.
    APPEND 'RAPA' TO mt_blacklist.
    APPEND 'RASE' TO mt_blacklist.
    APPEND 'PSA ' TO mt_blacklist.
    APPEND 'SPOK' TO mt_blacklist.
    APPEND 'THEM' TO mt_blacklist.
    APPEND 'THED' TO mt_blacklist.
    APPEND 'TMPL' TO mt_blacklist.
    APPEND 'DTMP' TO mt_blacklist.
    APPEND 'UPDR' TO mt_blacklist.
    APPEND 'DUPD' TO mt_blacklist.
    APPEND 'WWIB' TO mt_blacklist.
    APPEND 'DWIB' TO mt_blacklist.
    APPEND 'WWPA' TO mt_blacklist.
    APPEND 'DWPA' TO mt_blacklist.
    APPEND 'XCLS' TO mt_blacklist.
    APPEND 'DXCL' TO mt_blacklist.
    APPEND 'XLWB' TO mt_blacklist.
    APPEND 'DXLW' TO mt_blacklist.
    APPEND 'RDAC' TO mt_blacklist.
    APPEND 'EREL' TO mt_blacklist.

  ENDMETHOD.


  METHOD prepare_tree.

* Get all TLOGOs (except for old CompositeProvider which is local only)
    SELECT tlogo FROM rstlogoprop INTO TABLE mt_tlogo
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
          e_t_tlogo_invisible = mt_bpc.
    ENDIF.

* Get tree model
    DATA:
      lr_tree_model TYPE REF TO cl_rsawbn_tree_model_fl_lsys.

    CREATE OBJECT lr_tree_model.

    lr_tree_model->create_tree( ).
    lr_tree_model->set_view( 'M' ).

    mt_tree = lr_tree_model->get_tree( ).

  ENDMETHOD.


  METHOD process_anpr.

    DATA:
      lr_level TYPE REF TO /mbtools/cl_tree_level,
      lr_appl  TYPE REF TO cl_rsan_fct_appl_type,
      ls_group TYPE cl_rsan_fct_appl_type=>ys_appltoolgroup.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

    lr_appl = cl_rsan_fct_appl_type=>get_appl_type( 'GENERIC' ).

    lr_level->next( ).

    LOOP AT lr_appl->th_appltoolgroups INTO ls_group.

      process_anpr_group( is_group = ls_group i_level = lr_level->level ).

    ENDLOOP.

    lr_level->back( ).

  ENDMETHOD.


  METHOD process_anpr_group.

    DATA:
      lr_level    TYPE REF TO /mbtools/cl_tree_level,
      lt_applfunc TYPE STANDARD TABLE OF cl_rsan_fct_appl_type=>ys_appltool WITH DEFAULT KEY,
      ls_tool     TYPE cl_rsan_fct_appl_type=>ys_appltool,
      l_hidden    TYPE rs_bool.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

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

    IF m_b4h = rs_c_true.
      l_hidden = rs_c_true.
      lr_level->text   = lr_level->text && ' [not supported in B4H mode]'.
      lr_level->icon   = icon_dummy.
    ENDIF.

    CALL METHOD mr_tree->add_detail
      EXPORTING
        i_icon   = lr_level->icon
        i_title  = 'Tool Group'
        i_text   = lr_level->text
        i_value  = is_group-toolgroup
        i_level  = lr_level->level
        i_hidden = l_hidden.

    lr_level->next( ).

    lt_applfunc = is_group-ts_applfunc.

    CASE rs_c_true.
      WHEN m_bytext.
        SORT lt_applfunc BY text.
      WHEN m_byname.
        SORT lt_applfunc BY tool.
      WHEN m_bysequ.
        " keep it
    ENDCASE.

    LOOP AT lt_applfunc INTO ls_tool.

      process_anpr_tool( is_tool = ls_tool i_level = lr_level->level ).

    ENDLOOP.

    lr_level->back( ).

  ENDMETHOD.


  METHOD process_anpr_tool.

    DATA:
      lr_level TYPE REF TO /mbtools/cl_tree_level,
      ls_tool  LIKE is_tool,
      l_hidden TYPE rs_bool.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

    ls_tool = is_tool.

    REPLACE '.GIF' WITH '' INTO ls_tool-tool_image.
    IF ls_tool-tool_image CS '/SAP/PUBLIC/BC/ICONS/'.
      REPLACE '/SAP/PUBLIC/BC/ICONS/S_' WITH '' INTO ls_tool-tool_image.
      CONDENSE ls_tool-tool_image NO-GAPS.
    ELSEIF ls_tool-tool_image CS '/SAP/BW/EI/APD/ICONS/'.
      REPLACE '/SAP/BW/EI/APD/ICONS/' WITH '' INTO ls_tool-tool_image.
      CONDENSE ls_tool-tool_image NO-GAPS.

      CASE ls_tool-tool_image.
        WHEN 'BWDATABASETABLE'.
          ls_tool-tool_image = 'BWDATA'.
        WHEN 'BWASSOCIATIONANALYSIS'.
          ls_tool-tool_image = 'BWASAN'.
        WHEN 'BWCLUSTERANALYSIS'.
          ls_tool-tool_image = 'BWCLUS'.
        WHEN 'BWCOLUMNTOROW'.
          ls_tool-tool_image = 'BWCORO'.
        WHEN 'BWDATAMINING'.
          ls_tool-tool_image = 'BWDMTH'.
        WHEN 'BWDECISIONTREE'.
          ls_tool-tool_image = 'BWDETR'.
        WHEN 'BWREGRESSIONANALYSIS'.
          ls_tool-tool_image = 'BWREGR'.
        WHEN 'BWROWTOCOLUMN'.
          ls_tool-tool_image = 'BWROCO'.
        WHEN 'BWSOURCECRM'.
          ls_tool-tool_image = 'B_BWSG'.
        WHEN 'BWWEIGHTEDSCORETABLE'.
          ls_tool-tool_image = 'BWWSTA'.
        WHEN 'DST_REGRESSION'.
          ls_tool-tool_image = 'BWREGR'.
        WHEN OTHERS.
          ls_tool-tool_image = ''.
          BREAK-POINT.
      ENDCASE.
    ENDIF.
    CONCATENATE '@' ls_tool-tool_image(6) '@' INTO ls_tool-tool_image.
    CONDENSE ls_tool-tool_image NO-GAPS.

    SELECT SINGLE id FROM icon INTO lr_level->icon
      WHERE internal = ls_tool-tool_image.
    IF sy-subrc <> 0.
      lr_level->icon = ''.
    ENDIF.

    lr_level->text = ls_tool-text.

    IF m_b4h = rs_c_true.
      l_hidden = rs_c_true.
      lr_level->text = lr_level->text && ' [not supported in B4H mode]'.
      lr_level->icon = icon_dummy.
    ENDIF.

    CALL METHOD mr_tree->add_detail
      EXPORTING
        i_icon   = lr_level->icon
        i_title  = 'Tool'
        i_text   = lr_level->text
        i_value  = ls_tool-tool
        i_level  = lr_level->level
        i_hidden = l_hidden.

    IF m_prop = rs_c_true.
      process_icon( i_tlogo = 'ANPR' i_icon = lr_level->icon i_level = lr_level->level ).
    ENDIF.

    lr_level->back( ).

  ENDMETHOD.


  METHOD process_icon.

    DATA:
      lr_level TYPE REF TO /mbtools/cl_tree_level.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

* Icon
    SELECT SINGLE id name FROM icon INTO (lr_level->value, lr_level->text)
      WHERE id = i_icon.
    IF sy-subrc = 0.
      CALL METHOD mr_tree->add_detail
        EXPORTING
          i_icon  = i_icon
          i_title = 'Icon'
          i_text  = lr_level->text
          i_value = lr_level->value
          i_level = lr_level->level.
    ENDIF.

  ENDMETHOD.


  METHOD process_main.

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

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

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
      WHEN m_bytext.
        SORT lt_value BY ddtext.
      WHEN m_byname.
        SORT lt_value BY domvalue_l.
      WHEN m_bysequ.
        SORT lt_value BY valpos.
    ENDCASE.

    LOOP AT lt_value INTO ls_value.

      IF i_tlogo IS INITIAL.
        CHECK ls_value-domvalue_l IN m_tlogos.
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
          IF m_cache IS INITIAL.
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
*             i_cubetype    = l_cubetype
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
          IF m_cache IS INITIAL.
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
        READ TABLE mt_bpc TRANSPORTING NO FIELDS
          WITH TABLE KEY tlogo = l_tlogo.
        IF sy-subrc = 0.
          IF m_bpc = rs_c_false.
            CONTINUE.
          ENDIF.
          IF lr_level->text(3) <> 'BPC'.
            CONCATENATE 'BPC' lr_level->text INTO lr_level->text SEPARATED BY space.
          ENDIF.
          l_hidden = rs_c_true.
          lr_level->text   = lr_level->text && ' [hidden]'.
        ELSE.
          READ TABLE mt_tlogo TRANSPORTING NO FIELDS
            WITH TABLE KEY tlogo = l_tlogo.
          IF sy-subrc <> 0.
            lr_level->text = lr_level->text && ' [local]'.
          ENDIF.
        ENDIF.
      ENDIF.

*   Check B4H mode or SAP BW/4HANA compatibility
      IF m_b4h = rs_c_true.
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
            READ TABLE mt_blacklist TRANSPORTING NO FIELDS
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

      ELSEIF m_bw4 = rs_c_true.

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
            READ TABLE mt_blacklist TRANSPORTING NO FIELDS
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
      CALL METHOD mr_tree->add_detail
        EXPORTING
          i_icon   = lr_level->icon
          i_title  = l_title
          i_text   = lr_level->text
          i_value  = lr_level->value
          i_hidden = l_hidden
          i_level  = lr_level->level.

*   Reset cache
      IF m_cache IS INITIAL.
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

      IF m_prop = rs_c_true.
        IF l_tlogo IS INITIAL.
          process_icon( i_tlogo = l_tlogo i_icon = lr_level->icon i_level = lr_level->level ).
        ELSE.
          process_properties( i_tlogo = l_tlogo i_icon = lr_level->icon i_text = l_rstxtlg i_level = lr_level->level ).
        ENDIF.
      ENDIF.

      IF m_subobj = rs_c_true.
        CASE l_tlogo.
          WHEN rs_c_tlogo-infocube.
            process_main( i_tlogo = l_tlogo i_domname = 'RSCUBETYPE' i_level = lr_level->level ).
          WHEN rs_c_tlogo-infoobject.
            process_main( i_tlogo = l_tlogo i_domname = 'RSIOBJTP' i_level = lr_level->level ).
          WHEN rs_c_tlogo-element.
            process_main( i_tlogo = l_tlogo i_domname = 'RSZDEFTP' i_level = lr_level->level ).
          WHEN rs_c_tlogo-logsys.
            process_main( i_tlogo = l_tlogo i_domname = 'RSSRCTYPE_BW' i_level = lr_level->level ).
          WHEN rs_c_tlogo-process_variant.
            process_main( i_tlogo = l_tlogo i_domname = 'RSPC_TYPE' i_level = lr_level->level ).
          WHEN rs_c_tlogo-analysis_process.
            process_anpr( i_level = lr_level->level ).
        ENDCASE.
      ENDIF.

      lr_level->back( ).

    ENDLOOP.

  ENDMETHOD.


  METHOD process_properties.

    DATA:
      lr_level   TYPE REF TO /mbtools/cl_tree_level,
      l_tabname  TYPE tabname,
      l_funcname TYPE funcname,
      l_clsname  TYPE seoclsname.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

    CALL METHOD mr_tree->add_sub_node
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

      CALL METHOD mr_tree->add_detail
        EXPORTING
          i_icon  = i_icon
          i_title = 'BI Content Object'
          i_text  = lr_level->text
          i_value = lr_level->value
          i_level = lr_level->level.
    ENDIF.

* Description
    CALL METHOD mr_tree->add_detail
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
      CALL METHOD mr_tree->add_detail
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
      CALL METHOD mr_tree->add_detail
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

      CALL METHOD mr_tree->add_detail
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

      CALL METHOD mr_tree->add_detail
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
      write_table( i_table = l_tabname i_title = 'Primary Table' i_level = lr_level->level ).
    ENDIF.

* Dependent tables
    SELECT tabname FROM objs INTO l_tabname
      WHERE objectname = i_tlogo AND prim_table = ''
      ORDER BY tabname.

      write_table( i_table = l_tabname i_title = 'Dependent Table' i_level = lr_level->level ).

    ENDSELECT.

* ABAP Class
    SELECT SINGLE class FROM rstlogoprop INTO l_clsname
      WHERE tlogo = i_tlogo.

    write_class( i_class = l_clsname i_title = 'ABAP Class' i_level = lr_level->level ).

* ABAP Collaction Class
    SELECT SINGLE class_coll FROM rstlogoprop INTO l_clsname
      WHERE tlogo = i_tlogo.

    write_class( i_class = l_clsname i_title = 'ABAP Class (Collection)' i_level = lr_level->level ).

* ABAP Function
    l_funcname = 'RSO_' && i_tlogo && '_MAINTAIN'.

    write_function( i_funct = l_funcname i_title = 'ABAP Function' i_level = lr_level->level ).

* Transport Functions
    l_funcname = 'RS_' && i_tlogo && '_BEFORE_EXPORT'.

    write_function( i_funct = l_funcname i_title = 'Before Export' i_level = lr_level->level ).

    l_funcname = 'RS_' && i_tlogo && '_AFTER_IMPORT'.

    write_function( i_funct = l_funcname i_title = 'After Import' i_level = lr_level->level ).

    lr_level->back( ).

  ENDMETHOD.


  METHOD screen .

    " Is BPC available?
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = c_ujt_invisible_types
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      LOOP AT SCREEN.
        IF screen-group1 = 'BPC'.
          " screen-active = '0'.
          screen-input = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD write_class.

    DATA:
      lr_level TYPE REF TO /mbtools/cl_tree_level.

    CHECK NOT i_class IS INITIAL.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

    SELECT SINGLE clsname FROM seoclass INTO lr_level->value
      WHERE clsname = i_class.
    IF sy-subrc = 0.
      SELECT SINGLE descript FROM seoclasstx INTO lr_level->text
        WHERE clsname = i_class AND langu = sy-langu.
      IF sy-subrc <> 0.
        lr_level->text = 'No text'.
      ENDIF.

      CALL METHOD mr_tree->add_detail
        EXPORTING
          i_icon  = icon_abap
          i_title = i_title
          i_text  = lr_level->text
          i_value = lr_level->value
          i_level = lr_level->level
          i_type  = 'CLAS'.
    ENDIF.

  ENDMETHOD.


  METHOD write_function.

    DATA:
      lr_level TYPE REF TO /mbtools/cl_tree_level.

    CHECK NOT i_funct IS INITIAL.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

    SELECT SINGLE funcname FROM tfdir INTO lr_level->value
      WHERE funcname = i_funct.
    IF sy-subrc = 0.
      SELECT SINGLE stext FROM tftit INTO lr_level->text
        WHERE funcname = i_funct AND spras = sy-langu.
      IF sy-subrc <> 0.
        lr_level->text = 'No text'.
      ENDIF.

      CALL METHOD mr_tree->add_detail
        EXPORTING
          i_icon  = icon_abap
          i_title = i_title
          i_text  = lr_level->text
          i_value = lr_level->value
          i_level = lr_level->level
          i_type  = 'FUNC'.
    ENDIF.

  ENDMETHOD.


  METHOD write_table.

    DATA:
      lr_level   TYPE REF TO /mbtools/cl_tree_level,
      l_rollname TYPE rollname.

    CHECK NOT i_table IS INITIAL.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

    SELECT SINGLE tabname FROM dd02l INTO lr_level->value
      WHERE tabname = i_table.
    IF sy-subrc = 0.
      SELECT SINGLE ddtext FROM dd02t INTO lr_level->text
        WHERE tabname = i_table AND as4local = 'A'
          AND ddlanguage = sy-langu.
      IF sy-subrc <> 0.
        lr_level->text = 'No text'.
      ENDIF.

      CALL METHOD mr_tree->add_detail
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
*      CALL METHOD mr_tree->add_detail
*        EXPORTING
*          i_icon  = icon_led_red
*          i_title = 'ABAP-related Field'
*          i_text  = lr_level->text
*          i_value = lr_level->value
*          i_level = lr_level->level
*          i_type  = ''.
*    ENDSELECT.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
