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

    CONSTANTS:
      BEGIN OF c_tool,
        version     TYPE string VALUE '1.0.0' ##NO_TEXT,
        title       TYPE string VALUE 'MBT Logical Object Lister' ##NO_TEXT,
        bundle_id   TYPE i VALUE 0,
        download_id TYPE i VALUE 3635,
        description TYPE string
        VALUE 'Display the metadata of SAP BW, SAP BPC, or SAP BW/4HANA object models' ##NO_TEXT,
      END OF c_tool.

    METHODS constructor .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    DATA mo_tool TYPE REF TO /mbtools/cl_tools .
ENDCLASS.



CLASS /MBTOOLS/CL_TOOL_BW_LOL IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    mbt_manifest   = mo_tool->mbt_manifest.
  ENDMETHOD.
ENDCLASS.
