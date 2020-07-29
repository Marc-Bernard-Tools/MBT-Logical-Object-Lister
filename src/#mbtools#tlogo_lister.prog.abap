REPORT /mbtools/tlogo_lister.
************************************************************************
* MBT Logical Object Lister
*
* This tool lists all logical object types (TLOGO) for SAP BW. You can
* sort objects in three ways, display object properties, and check if
* objects are compatible with SAP BW/4HANA.
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

TABLES:
  sscrfields, rstlogoprop.

*-----------------------------------------------------------------------

* Main
SELECTION-SCREEN:
  BEGIN OF SCREEN 200 AS SUBSCREEN,
    BEGIN OF BLOCK b200 WITH FRAME,
      COMMENT /1(77) scr_t200,
      COMMENT /1(77) scr_t201,
    END OF BLOCK b200,
    BEGIN OF BLOCK b210 WITH FRAME.
SELECT-OPTIONS:
  s_tlogo FOR rstlogoprop-tlogo.
PARAMETERS:
  p_subobj AS CHECKBOX DEFAULT 'X',
  p_bpc    AS CHECKBOX MODIF ID bpc.
SELECTION-SCREEN:
END OF BLOCK b210,
BEGIN OF BLOCK b220 WITH FRAME.
PARAMETERS:
  p_bw  RADIOBUTTON GROUP g3 DEFAULT 'X' MODIF ID bw4,
  p_b4h RADIOBUTTON GROUP g3 MODIF ID bw4,
  p_bw4 RADIOBUTTON GROUP g3 MODIF ID bw4.
SELECTION-SCREEN:
END OF BLOCK b220,
BEGIN OF BLOCK b230 WITH FRAME.
PARAMETERS:
  p_bytext RADIOBUTTON GROUP g2 DEFAULT 'X',
  p_byname RADIOBUTTON GROUP g2,
  p_bysequ RADIOBUTTON GROUP g2.
SELECTION-SCREEN:
END OF BLOCK b230,
BEGIN OF BLOCK b240 WITH FRAME.
PARAMETERS:
  p_prop  AS CHECKBOX,
  p_cache TYPE c NO-DISPLAY.
SELECTION-SCREEN:
END OF BLOCK b240,
END OF SCREEN 200.

*-----------------------------------------------------------------------

* About
SELECTION-SCREEN:
  BEGIN OF SCREEN 900 AS SUBSCREEN,
    BEGIN OF BLOCK b900 WITH FRAME,
      COMMENT /1(50) scr_t900,
      COMMENT 60(25) scr_t901,
      SKIP,
      COMMENT /1(77) scr_t902,
    END OF BLOCK b900,
    BEGIN OF BLOCK b910 WITH FRAME,
      PUSHBUTTON /1(55) b_docu USER-COMMAND docu,
      SKIP,
      PUSHBUTTON /1(55) b_tool USER-COMMAND tool,
      SKIP,
      PUSHBUTTON /1(55) b_home USER-COMMAND home,
    END OF BLOCK b910,
  END OF SCREEN 900.

*-----------------------------------------------------------------------

* Header
SELECTION-SCREEN:
  BEGIN OF BLOCK scr_header,
    SKIP,
    SKIP,
    COMMENT /3(77) scr_t001 FOR FIELD s_tlogo,
    SKIP,
  END OF BLOCK scr_header,
  BEGIN OF TABBED BLOCK scr_tab FOR 23 LINES,
    TAB (40) scr_tab2 USER-COMMAND scr_push2 DEFAULT SCREEN 0200,
    TAB (40) scr_tab9 USER-COMMAND scr_push9 DEFAULT SCREEN 0900,
  END OF BLOCK scr_tab.

*-----------------------------------------------------------------------

CONSTANTS:
  c_title TYPE string VALUE /mbtools/cl_tool_bw_lol=>c_tool-title.

DATA:
  gv_ok_code TYPE sy-ucomm,
  go_tool    TYPE REF TO /mbtools/cl_tools,
  go_screen  TYPE REF TO /mbtools/cl_screen,
  go_app     TYPE REF TO /mbtools/cl_bw_tlogo_lister.

*-----------------------------------------------------------------------

MODULE pbo_100 OUTPUT.

  go_screen->banner( iv_show = abap_false ).

  go_app->pbo( ).

ENDMODULE.                 " PBO_0100  OUTPUT

MODULE pai_100 INPUT.

  go_app->pai( CHANGING cv_ok_code = gv_ok_code ).

ENDMODULE.                 " PAI_0100  INPUT

INITIALIZATION.

  IF /mbtools/cl_switches=>is_active( c_title ) = abap_false.
    MESSAGE e004(/mbtools/bc) WITH c_title.
    RETURN.
  ENDIF.

  CREATE OBJECT go_app.

  go_tool   = /mbtools/cl_tools=>factory( c_title ).
  go_screen = /mbtools/cl_screen=>factory( c_title ).

  go_screen->init(
    IMPORTING
      ev_text      = scr_t001
      ev_about     = scr_tab9
      ev_title     = scr_t900
      ev_version   = scr_t901
      ev_copyright = scr_t902
      ev_docu      = b_docu
      ev_tool      = b_tool
      ev_home      = b_home ).

  scr_tab2 = go_screen->header(
    iv_icon = icon_selection
    iv_text = 'Selection'(001) ).

  scr_t200 = 'Select which objects to view and set how you'(200).
  scr_t201 = 'want the results sorted and displayed'(201).

  " Is this BW4?
  SELECT SINGLE release FROM cvers INTO sy-lisel
    WHERE component = 'DW4CORE'.
  IF sy-subrc = 0.
    p_bw = p_b4h = abap_false.
    p_bw4 = abap_true.
  ENDIF.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN.

  go_app->screen( ).

  go_screen->ucomm( sscrfields-ucomm ).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.

  go_screen->banner( ).

  go_app->screen( ).

*-----------------------------------------------------------------------

START-OF-SELECTION.

  LOG-POINT ID /mbtools/bc SUBKEY c_title FIELDS sy-datum sy-uzeit sy-uname.

  " Setup tree
  go_app->initialize(
    ir_tlogos = s_tlogo[]
    iv_bw     = p_bw
    iv_b4h    = p_b4h
    iv_bw4    = p_bw4
    iv_prop   = p_prop
    iv_bytext = p_bytext
    iv_byname = p_byname
    iv_bysequ = p_bysequ
    iv_cache  = p_cache
    iv_subobj = p_subobj
    iv_bpc    = p_bpc ).

  " Output as ALV tree control
  CALL SCREEN 100.
