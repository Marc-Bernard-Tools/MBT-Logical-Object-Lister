************************************************************************
* /MBTOOLS/CL_SCREEN
* MBT Screen
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_screen DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS cndp .

    TYPES:
      ty_screen_field TYPE c LENGTH 83 .

    CLASS-DATA gv_copyright TYPE string .
    CLASS-DATA gv_about TYPE string .
    CLASS-DATA gv_documentation TYPE string .
    CLASS-DATA gv_tool_page TYPE string .
    CLASS-DATA gv_website TYPE string .
    CLASS-DATA gv_terms TYPE string .
    CLASS-DATA gv_version TYPE string .

    CLASS-METHODS class_constructor .
    CLASS-METHODS icon
      IMPORTING
        VALUE(iv_icon)   TYPE icon_d
        VALUE(iv_text)   TYPE csequence OPTIONAL
        VALUE(iv_quick)  TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE ty_screen_field .
    CLASS-METHODS header
      IMPORTING
        VALUE(iv_icon)   TYPE icon_d
        VALUE(iv_text)   TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE ty_screen_field .
    CLASS-METHODS logo
      IMPORTING
        VALUE(iv_show) TYPE abap_bool DEFAULT abap_true
        VALUE(iv_top)  TYPE i OPTIONAL
        VALUE(iv_left) TYPE i OPTIONAL .
    CLASS-METHODS banner
      IMPORTING
        VALUE(iv_tool) TYPE string OPTIONAL
        VALUE(iv_show) TYPE abap_bool DEFAULT abap_true
        VALUE(iv_top)  TYPE i OPTIONAL
        VALUE(iv_left) TYPE i OPTIONAL .
    CLASS-METHODS init
      IMPORTING
        !ir_tool      TYPE REF TO /mbtools/cl_tools
      EXPORTING
        !ev_text      TYPE ty_screen_field
        !ev_about     TYPE ty_screen_field
        !ev_title     TYPE ty_screen_field
        !ev_version   TYPE ty_screen_field
        !ev_copyright TYPE ty_screen_field
        !ev_docu      TYPE ty_screen_field
        !ev_tool      TYPE ty_screen_field
        !ev_home      TYPE ty_screen_field .
    CLASS-METHODS ucomm
      IMPORTING
        VALUE(iv_ok_code)  TYPE sy-ucomm
        VALUE(iv_url_docs) TYPE string
        VALUE(iv_url_tool) TYPE string.
  PROTECTED SECTION.
ENDCLASS.
CLASS /mbtools/cl_screen IMPLEMENTATION.
  METHOD header.
  ENDMETHOD.
  METHOD icon.
  ENDMETHOD.
  METHOD class_constructor.
  ENDMETHOD.
  METHOD logo.
  ENDMETHOD.
  METHOD banner.
  ENDMETHOD.
  METHOD init.
  ENDMETHOD.
  METHOD ucomm.
  ENDMETHOD.
ENDCLASS.
