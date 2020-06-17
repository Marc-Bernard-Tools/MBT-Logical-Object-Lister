************************************************************************
* /MBTOOLS/CL_TREE
* MBT Tree
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_tree DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS icon .
    TYPE-POOLS rsrqt .

    DATA mv_container_name TYPE char25 VALUE 'GO_TREE_CONTAINER' ##NO_TEXT.
    DATA mo_custom_container TYPE REF TO cl_gui_custom_container .
    DATA mo_tree TYPE REF TO cl_gui_alv_tree .

    METHODS handle_node_double_click
        FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING
        !node_key .
    METHODS handle_item_double_click
        FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING
        !node_key
        !fieldname .
    METHODS constructor .
    METHODS pbo .
    METHODS pai
      IMPORTING
        !iv_ok_code TYPE sy-ucomm .
    METHODS display .
    METHODS download .
    METHODS print .
    METHODS destroy .
    METHODS expand
      IMPORTING
        VALUE(iv_level) TYPE i .
    METHODS expand_all .
    METHODS add_top_node
      IMPORTING
        VALUE(iv_title) TYPE csequence
        VALUE(iv_icon)  TYPE icon_d OPTIONAL
        VALUE(iv_text)  TYPE any OPTIONAL
        VALUE(iv_value) TYPE any OPTIONAL
        VALUE(iv_type)  TYPE csequence OPTIONAL .
    METHODS add_sub_node
      IMPORTING
        VALUE(iv_title) TYPE csequence
        VALUE(iv_icon)  TYPE icon_d OPTIONAL
        VALUE(iv_text)  TYPE any OPTIONAL
        VALUE(iv_value) TYPE any OPTIONAL
        VALUE(iv_type)  TYPE csequence OPTIONAL .
    METHODS add_detail
      IMPORTING
        VALUE(iv_title)  TYPE csequence
        VALUE(iv_icon)   TYPE icon_d OPTIONAL
        VALUE(iv_text)   TYPE any OPTIONAL
        VALUE(iv_value)  TYPE any OPTIONAL
        VALUE(iv_level)  TYPE i OPTIONAL
        VALUE(iv_sign)   TYPE abap_bool DEFAULT abap_false
        VALUE(iv_hidden) TYPE abap_bool DEFAULT abap_false
        VALUE(iv_type)   TYPE csequence OPTIONAL .
    METHODS pick_node .
    METHODS find_node .
    METHODS set_key
      IMPORTING
        !iv_key TYPE lvc_nkey .
    METHODS get_key
      RETURNING
        VALUE(rv_key) TYPE lvc_nkey .
    METHODS next_key .
  PROTECTED SECTION.

ENDCLASS.
CLASS /mbtools/cl_tree IMPLEMENTATION.
  METHOD handle_item_double_click.
  ENDMETHOD.
  METHOD handle_node_double_click.
  ENDMETHOD.
  METHOD destroy.
  ENDMETHOD.
  METHOD display.
  ENDMETHOD.
  METHOD download.
  ENDMETHOD.
  METHOD find_node.
  ENDMETHOD.
  METHOD pick_node.
  ENDMETHOD.
  METHOD print.
  ENDMETHOD.
  METHOD get_key.
  ENDMETHOD.
  METHOD set_key.
  ENDMETHOD.
  METHOD next_key.
  ENDMETHOD.
  METHOD add_detail.
  ENDMETHOD.
  METHOD add_sub_node.
  ENDMETHOD.
  METHOD add_top_node.
  ENDMETHOD.
  METHOD constructor.
  ENDMETHOD.
  METHOD expand.
  ENDMETHOD.
  METHOD expand_all.
  ENDMETHOD.
  METHOD pai.
  ENDMETHOD.
  METHOD pbo.
  ENDMETHOD.
ENDCLASS.
