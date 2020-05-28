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
  p_bw  RADIOBUTTON GROUP g3 DEFAULT 'X',
  p_b4h RADIOBUTTON GROUP g3,
  p_bw4 RADIOBUTTON GROUP g3.
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

INCLUDE /mbtools/bc_screen_about_tab.

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

INCLUDE /mbtools/bc_screen_data.

DATA: gr_app TYPE REF TO /mbtools/cl_bw_tlogo_lister.

*-----------------------------------------------------------------------

INCLUDE /mbtools/bc_screen_pbo_pai.

*-----------------------------------------------------------------------

INITIALIZATION.

  INCLUDE /mbtools/bc_screen_init.

  scr_tab2 = /mbtools/cl_screen=>header(
    iv_icon = icon_selection
    iv_text = 'Selection' ).

  scr_t200 = 'Select which objects to view and set how you'.
  scr_t201 = 'want the results sorted and displayed'.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN.

  INCLUDE /mbtools/bc_screen_at_select.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.

  INCLUDE /mbtools/bc_screen_at_output.

*-----------------------------------------------------------------------

START-OF-SELECTION.

  LOG-POINT ID /mbtools/bc
    SUBKEY gr_tool->get_title( )
    FIELDS sy-datum sy-uzeit sy-uname.

  " Setup tree
  gr_app->initialize(
    i_tlogos = s_tlogo[]
    i_bw     = p_bw
    i_b4h    = p_b4h
    i_bw4    = p_bw4
    i_prop   = p_prop
    i_bytext = p_bytext
    i_byname = p_byname
    i_bysequ = p_bysequ
    i_cache  = p_cache
    i_subobj = p_subobj
    i_bpc    = p_bpc ).

  " Output as ALV tree control
  CALL SCREEN 100.
