CLASS /mbtools/cl_tool_bw_lol DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Logical Object Lister
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  PUBLIC SECTION.
    TYPE-POOLS icon .

    INTERFACES /mbtools/if_manifest .

    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    CONSTANTS:
      BEGIN OF c_tool,
        version     TYPE string VALUE '1.0.0' ##NO_TEXT,
        title       TYPE string VALUE 'MBT Logical Object Lister' ##NO_TEXT,
        bundle_id   TYPE i VALUE 0,
        download_id TYPE i VALUE 3635,
        description TYPE string
        VALUE 'Display the metadata of SAP BW, SAP BPC, or SAP BW/4HANA object models' ##NO_TEXT,
        has_launch  TYPE abap_bool VALUE abap_true,
      END OF c_tool.

    METHODS constructor .

    METHODS launch.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_tool TYPE REF TO /mbtools/cl_tools .

ENDCLASS.



CLASS /MBTOOLS/CL_TOOL_BW_LOL IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    mbt_manifest   = mo_tool->mbt_manifest.
  ENDMETHOD.


  METHOD launch.
    /mbtools/cl_sap=>run_program( '/MBTOOLS/TLOGO_LISTER' ).
  ENDMETHOD.
ENDCLASS.
