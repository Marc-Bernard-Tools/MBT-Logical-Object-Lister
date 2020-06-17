************************************************************************
* /MBTOOLS/CL_TREE_LEVEL
* MBT Tree Level
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_tree_level DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA level TYPE i READ-ONLY .
    DATA icon TYPE icon_d .
    DATA value TYPE /mbtools/tree_control-value .
    DATA text TYPE /mbtools/tree_control-text .

    METHODS constructor
      IMPORTING
        !io_tree  TYPE REF TO /mbtools/cl_tree
        !iv_level TYPE i.
    METHODS next .
    METHODS back .
  PROTECTED SECTION.
ENDCLASS.
CLASS /mbtools/cl_tree_level IMPLEMENTATION.
  METHOD back.
  ENDMETHOD.
  METHOD constructor.
  ENDMETHOD.
  METHOD next.
  ENDMETHOD.
ENDCLASS.
