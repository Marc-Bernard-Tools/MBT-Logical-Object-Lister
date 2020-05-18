************************************************************************
* /MBTOOLS/CL_BW_TLOGO_LISTER
* MBT Logical Object Lister
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_bw_tlogo_lister DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apack_manifest .
    INTERFACES /mbtools/if_manifest .

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.
    CONSTANTS c_title TYPE string VALUE 'MBT Logical Object Lister' ##NO_TEXT.
    CONSTANTS c_description TYPE string VALUE 'Display the metadata of SAP BW, SAP BPC, or SAP BW/4HANA object models' ##NO_TEXT.
    CONSTANTS c_download_id TYPE i VALUE 3635.

    METHODS constructor .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES apack_manifest
      FOR if_apack_manifest~descriptor .
    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

ENDCLASS.



CLASS /MBTOOLS/CL_BW_TLOGO_LISTER IMPLEMENTATION.


  METHOD constructor.
    apack_manifest = /mbtools/cl_tools=>build_apack_manifest( me ).
    mbt_manifest = /mbtools/cl_tools=>build_mbt_manifest( me ).
  ENDMETHOD.
ENDCLASS.
