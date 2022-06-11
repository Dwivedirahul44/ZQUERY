CONSTANTS :
  BEGIN OF c,
    X                    type char1  value 'X'     ,
  END OF c.

TYPES:
  BEGIN OF ty_header,
    heading(600)         TYPE c,
  END OF ty_header,
  BEGIN OF ty_qrep_node.
    INCLUDE TYPE treev_node.
    types:
      text(100)          TYPE c,
      edit(1)            TYPE c,
      queryid            TYPE GUID_32,
  END OF ty_qrep_node,

tty_header               TYPE STANDARD TABLE OF ty_header   ,
tty_qrep_node            TYPE STANDARD TABLE OF ty_qrep_node.

DATA w_okcode            TYPE okcode.
CONTROLS w_tabstrip      TYPE TABSTRIP.

CLASS:
  lcl_scr                DEFINITION DEFERRED,
  lcl_app                DEFINITION DEFERRED.

DATA:
BEGIN OF w,

  BEGIN OF tab_title,
    tab1                 TYPE string VALUE 'Tab 1',                  "#EC NOTEXT
    tab2                 TYPE string VALUE 'Tab 2',                  "#EC NOTEXT
    tab3                 TYPE string VALUE 'Tab 3',                  "#EC NOTEXT
    tab4                 TYPE string VALUE 'Tab 4',                  "#EC NOTEXT
    tab5                 TYPE string VALUE 'Tab 5',                  "#EC NOTEXT
    tab6                 TYPE string VALUE 'Tab 6',                  "#EC NOTEXT
    tab7                 TYPE string VALUE 'Tab 7',                  "#EC NOTEXT
    tab8                 TYPE string VALUE 'Tab 8',                  "#EC NOTEXT
    tab9                 TYPE string VALUE 'Tab 9',                  "#EC NOTEXT
    tab10                TYPE string VALUE 'Tab 10',                 "#EC NOTEXT
    tab11                TYPE string VALUE 'Tab 11',                 "#EC NOTEXT
    tab12                TYPE string VALUE 'Tab 12',                 "#EC NOTEXT
    tab13                TYPE string VALUE 'Tab 13',                 "#EC NOTEXT
    tab14                TYPE string VALUE 'Tab 14',                 "#EC NOTEXT
    tab15                TYPE string VALUE 'Tab 15',                 "#EC NOTEXT
    tab16                TYPE string VALUE 'Tab 16',                 "#EC NOTEXT
    tab17                TYPE string VALUE 'Tab 17',                 "#EC NOTEXT
    tab18                TYPE string VALUE 'Tab 18',                 "#EC NOTEXT
    tab19                TYPE string VALUE 'Tab 19',                 "#EC NOTEXT
    tab20                TYPE string VALUE 'Tab 20',                 "#EC NOTEXT
    tab21                TYPE string VALUE 'Tab 21',                 "#EC NOTEXT
    tab22                TYPE string VALUE 'Tab 22',                 "#EC NOTEXT
    tab23                TYPE string VALUE 'Tab 23',                 "#EC NOTEXT
    tab24                TYPE string VALUE 'Tab 24',                 "#EC NOTEXT
    tab25                TYPE string VALUE 'Tab 25',                 "#EC NOTEXT
    tab26                TYPE string VALUE 'Tab 26',                 "#EC NOTEXT
    tab27                TYPE string VALUE 'Tab 27',                 "#EC NOTEXT
    tab28                TYPE string VALUE 'Tab 28',                 "#EC NOTEXT
    tab29                TYPE string VALUE 'Tab 29',                 "#EC NOTEXT
    tab30                TYPE string VALUE 'Tab 30',                 "#EC NOTEXT
  END OF tab_title,
    max_row                TYPE SMP_DYNTXT,
    qvisibility            TYPE char1,
    visibilitygrp          TYPE xuclass,
    vis_users              TYPE syuname,
    vuser_sel_more         TYPE char20,
    UGROUP_SEL_MORE        TYPE char20,
    qname                  TYPE char30,
    o_container_options    TYPE REF TO cl_gui_custom_container,
    o_splitter_ddic_full   TYPE REF TO cl_rsawb_splitter_for_toolbar,
    o_container_ddic       TYPE REF TO cl_gui_container,
    o_toolbar              TYPE REF TO cl_gui_toolbar,
    o_container            TYPE REF TO cl_gui_custom_container,
    o_splitter             TYPE REF TO cl_gui_splitter_container,
    o_container_ddic_full  TYPE REF TO cl_gui_container,
    o_container_qrep       TYPE REF TO cl_gui_container,
    o_qcontainer           TYPE REF TO cl_gui_container,
    o_sqlfld_cont          TYPE REF TO cl_gui_custom_container,
    o_tabrel_cont          TYPE REF TO cl_gui_custom_container,
    o_sqlfld_alv           TYPE REF TO cl_gui_alv_grid,
    o_tabrel_alv           TYPE REF TO cl_gui_alv_grid,
    sel_tabname            TYPE tabname,
    s1                     TYPE string,

END OF w.

  PARAMETERS: p_query TYPE string NO-DISPLAY.
  PARAMETERS: p_upto TYPE i NO-DISPLAY .
