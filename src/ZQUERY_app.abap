

CLASS lcl_error DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    DATA: v_text     TYPE string READ-ONLY.

    METHODS:
      constructor      IMPORTING iv_text         TYPE string OPTIONAL .
ENDCLASS.
CLASS lcl_error IMPLEMENTATION .

  METHOD constructor.

    super->constructor( ).

    v_text = iv_text.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES :
      BEGIN OF ty_dd03vt,
        tabname    TYPE tabname,
        fieldname  TYPE fieldname,
        position   TYPE tabfdpos,
        rollname   TYPE rollname,
        checktable TYPE checktable,
        ddtext     TYPE as4text,
      END OF ty_dd03vt,
      BEGIN OF ty_sqlfields,
        tabname   TYPE tabname,
        fieldname TYPE fieldname,
        ddtext    TYPE as4text,
        input     TYPE abap_bool,
      END OF ty_sqlfields,
      BEGIN OF ty_tabrel,
        tabname1 TYPE tabname,
        rel_txt  TYPE char30,
        tabname2 TYPE tabname,
      END OF ty_tabrel,
      BEGIN OF ty_ddic_fields,
        tabname   TYPE dd03l-tabname,
        fieldname TYPE dd03l-fieldname,
        position  TYPE dd03l-position,
        keyflag   TYPE dd03l-keyflag,
        ddtext1   TYPE char40,
      END OF ty_ddic_fields,
      BEGIN OF ty_joinfld,
        tab1     TYPE tabname,
        fldname  TYPE fieldname,
        tab2     TYPE tabname,
        position TYPE tabfdpos,
        key      TYPE keyflag,
        key2     TYPE keyflag,
      END OF ty_joinfld,
      BEGIN OF ty_list,
        ref TYPE REF TO lcl_scr,
      END OF ty_list,
      BEGIN OF ty_tabfield,
        tabname   TYPE tabname,
        fieldname TYPE fieldname,
      END OF ty_tabfield    ,
      tty_joinfld     TYPE STANDARD TABLE OF ty_joinfld     WITH DEFAULT KEY,
      tty_ddic_fields TYPE STANDARD TABLE OF ty_ddic_fields WITH DEFAULT KEY,
      tty_fields      TYPE STANDARD TABLE OF ty_sqlfields   WITH DEFAULT KEY,
      tty_dd03vt      TYPE STANDARD TABLE OF ty_dd03vt WITH DEFAULT KEY,
      tty_tabfield    TYPE STANDARD TABLE OF ty_tabfield WITH DEFAULT KEY.

    CLASS-DATA:
      t_list       TYPE STANDARD TABLE OF ty_list    READ-ONLY,
      t_sqlfld     TYPE STANDARD TABLE OF ty_sqlfields,
      t_tabrel     TYPE STANDARD TABLE OF ty_tabrel,
      paste_break  TYPE abap_bool,
      techname     TYPE abap_bool,
      conversion   TYPE abap_bool,
      default_rows TYPE i                      VALUE 100.

    CLASS-METHODS:

      str_2_range             IMPORTING iv_str                       TYPE string
                        RETURNING VALUE(rt_rnge)                     TYPE rsis_t_range,

      save_query              IMPORTING iv_query                     TYPE string
                               CHANGING cs_ztquery                   TYPE ztquery OPTIONAL,

      call_multisel           IMPORTING iv_excl_opt                  TYPE rsoptions
                                        iv_input                     TYPE rstabfield
                                        iv_hlpname                   TYPE shlpname OPTIONAL
                               CHANGING ct_rettab                    TYPE STANDARD TABLE,

      rfrsh_qwiz_data,

      popup_get_values  EXPORTING VALUE(ev_exit)                     TYPE abap_bool
                               CHANGING cs_sval                      TYPE sval ,

      ret_ora_whr_str         IMPORTING iv_whr                       TYPE string
                                        iv_upto                      TYPE i
                        RETURNING VALUE(rv_whr_str)                  TYPE string,

      call_sel_screen         EXPORTING et_where                     TYPE rsds_twhere
                               CHANGING ct_fld                       TYPE rsdsfields_t,

      ret_query         RETURNING VALUE(rt_string)                   TYPE string_table ,

      handle_sqlfld           IMPORTING iv_tabname                   TYPE tabname
                                RAISING lcl_error,

      chk_tabnm_empt_vald    IMPORTING iv_tabname                    TYPE tabname
                               RAISING lcl_error,

      settings_output          RAISING lcl_error,

      init_settings_cont       RAISING lcl_error,

      init_settings_obj        RAISING lcl_error,

      handle_settings         IMPORTING iv_ucomm                     TYPE okcode,

      get_q_template    RETURNING VALUE(rt_query)                    TYPE string_table,

      col_exp_repo,

      handle_bg_alv           IMPORTING iv_query                     TYPE string
                                        iv_upto                      TYPE i,

      check_tab_view_exists   IMPORTING iv_name                      TYPE tabname
                                RAISING lcl_error,
      ret_tabname       RETURNING VALUE(rv_tabname)                  TYPE tabname,

      push_list               IMPORTING iv_ref                       TYPE REF TO lcl_scr,

      modif_list              IMPORTING iv_ref                       TYPE REF TO lcl_scr
                                        iv_index                     TYPE sytabix,

      read_list               IMPORTING iv_index                     TYPE syst-tabix
                        RETURNING VALUE(rv_ref)                      TYPE REF TO lcl_scr,

      pop_list                IMPORTING iv_index                     TYPE syst-tabix,

      exec_query              IMPORTING it_query                     TYPE soli_tab
                                        iv_upto                      TYPE i
                        RETURNING VALUE(rt_data)                     TYPE REF TO data,

      qtab_to_string          IMPORTING it_query                     TYPE soli_tab
                                        iv_com                       TYPE abap_bool OPTIONAL
                                        iv_sep                       TYPE char2 OPTIONAL
                        RETURNING VALUE(rv_query)                    TYPE string,

      exec_query_string       IMPORTING iv_query                     TYPE string
                                        iv_upto                      TYPE i
                        RETURNING VALUE(rt_data)                     TYPE REF TO data,

      chk_limit,


      qwiz_output               RAISING lcl_error,

      ret_query_comp          IMPORTING iv_query                     TYPE string
                        EXPORTING VALUE(ev_select)                   TYPE string
                                  VALUE(ev_from)                     TYPE string
                                  VALUE(ev_where)                    TYPE string
                                  VALUE(ev_upto)                     TYPE i
                                  VALUE(ev_order)                    TYPE string
                                  VALUE(ev_group)                    TYPE string,

      get_est_rows            IMPORTING iv_query                     TYPE string
                        RETURNING VALUE(rv_rows)                     TYPE char32,

      get_ddicfields          IMPORTING iv_val                       TYPE pvarfield
                              EXPORTING et_data                      TYPE tty_ddic_fields
                                RAISING lcl_error,

      ret_whr                 IMPORTING iv_join                      TYPE abap_bool
                                        it_fld                       TYPE rsdsfields_t
                         	     CHANGING ct_whr                       TYPE rsds_twhere,

      set_visdropdown.

    CLASS-METHODS:
      init_ddic_toolbar,
      set_ddic_repo_width      IMPORTING iv_width                     TYPE i,

      set_query_repo_width     IMPORTING iv_width                     TYPE i.
    CLASS-DATA:
          v_repo_width     TYPE i.
  PRIVATE SECTION.

    CLASS-DATA:

      o_settings       TYPE REF TO cl_wdy_wb_property_box.

    CLASS-METHODS:
      get_new_guid      RETURNING VALUE(rv_guid)                     TYPE guid_32,
      set_settings,

      get_settings,

      chk_ext_joinfld_loop    IMPORTING is_joinfld     TYPE ty_joinfld
                                        iv_tab1        TYPE tabname
                                        iv_tab2        TYPE tabname
                              EXPORTING VALUE(ev_exit) TYPE abap_bool
                              CHANGING  cv_keymatch    TYPE abap_bool,

      prep_query              IMPORTING it_whr           TYPE rsds_twhere
                                        it_fld           TYPE rsdsfields_t
                              RETURNING VALUE(rt_string) TYPE string_table,

      get_tabfields           IMPORTING iv_tabname TYPE tabname
                              EXPORTING et_data    TYPE tty_dd03vt
                              RAISING   lcl_error,

      fld_sel_popup           IMPORTING it_data   TYPE tty_dd03vt
                              CHANGING  ct_fields TYPE tty_fields ,

      fill_tabrel,

      sqlfld_fcat       RETURNING VALUE(rt_fcat)                     TYPE lvc_t_fcat,

      tabrel_fcat       RETURNING VALUE(rt_fcat)                     TYPE lvc_t_fcat,

      sqlfld_disp               RAISING lcl_error,

      init_sqlfld_cont          RAISING lcl_error,

      sqlfld_alv_init           RAISING lcl_error,

      tabrel_disp               RAISING lcl_error,

      tabrel_alv_init           RAISING lcl_error,

      init_tabrel_cont          RAISING lcl_error,

      ret_fld                 IMPORTING iv_fld         TYPE string
                                        tab1           TYPE abap_component_tab
                                        tab2           TYPE abap_component_tab
                              EXPORTING VALUE(ev_skip) TYPE abap_bool
                                        es_tab         TYPE abap_componentdescr,

      ret_from_tabref         IMPORTING iv_select      TYPE string
                                        iv_from        TYPE string
                              EXPORTING VALUE(ev_tab)  TYPE abap_component_tab
                                        VALUE(ev_tab1) TYPE abap_component_tab,

      chk_all_wid_join        IMPORTING iv_select TYPE string
                                        iv_from   TYPE string,

      ret_query_tabref        IMPORTING iv_select      TYPE string
                                        iv_from        TYPE string
                              EXPORTING VALUE(rt_data) TYPE REF TO data
                                        et_fields      TYPE string_table,

      exec_select             IMPORTING iv_single   TYPE abap_bool
                                        iv_distinct TYPE abap_bool
                                        it_fields   TYPE string_table
                                        iv_from     TYPE string
                                        iv_where    TYPE string
                                        iv_group    TYPE string
                                        iv_order    TYPE string
                                        iv_upto     TYPE i
                              CHANGING  co_ref      TYPE REF TO data,

      ret_tabfields_from_fields IMPORTING it_fields           TYPE string_table
                                RETURNING VALUE(rt_tabfields) TYPE tty_tabfield,

      ret_tabfields_of_from     IMPORTING iv_from             TYPE string
                                RETURNING VALUE(rt_tabfields) TYPE tty_tabfield,
      ret_tabs_of_from          IMPORTING iv_from           TYPE string
                                RETURNING VALUE(rt_tablist) TYPE string_table,

      from_has_cluster_table    IMPORTING iv_from       TYPE string
                                RETURNING VALUE(rv_yes) TYPE abap_bool,

      is_cluster_tab            IMPORTING iv_name       TYPE tabname
                                RETURNING VALUE(rv_yes) TYPE abap_bool,


      get_sel_string    IMPORTING VALUE(iv_join)   TYPE abap_bool
                        RETURNING VALUE(rt_string) TYPE string_table,

      ret_from                IMPORTING iv_join   TYPE abap_bool
                              CHANGING  ct_string TYPE string_table,

      ret_join_from_str       IMPORTING iv_tab1    TYPE tabname
                                        iv_tab2    TYPE tabname
                                        it_joinfld TYPE tty_joinfld
                              CHANGING  ct_string  TYPE string_table,

      ret_joinfld_data         EXPORTING et_joinfld                   TYPE tty_joinfld,

      ret_tab_comp             IMPORTING iv_name        TYPE string
                               RETURNING VALUE(rt_comp) TYPE abap_component_tab,


      get_type_comp            IMPORTING iv_name        TYPE string
                               RETURNING VALUE(rt_comp) TYPE abap_component_tab.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD str_2_range.
    DATA:
      ls_strtab TYPE string,
      lr_range  TYPE rsis_s_range.

    DATA:
      lt_tab         TYPE string_table.

    DATA:
      lv_str         TYPE string.

    CHECK iv_str IS NOT INITIAL.

    lv_str = iv_str.

    CONDENSE lv_str .

    SPLIT lv_str
       AT space
       INTO TABLE lt_tab.

    lr_range-sign = 'I'.
    lr_range-option = 'EQ'.

    LOOP AT lt_tab INTO ls_strtab.

      lr_range-low = ls_strtab.
      APPEND lr_range TO rt_rnge.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_new_guid.
    DO 50 TIMES.

      TRY.

          rv_guid = cl_system_uuid=>create_uuid_c32_static( ).

          SELECT COUNT(*)
          FROM ztquery
          UP TO 1 ROWS
          WHERE queryid = rv_guid.

          IF syst-dbcnt = 0.
            EXIT.
          ENDIF.

        CATCH cx_uuid_error.

      ENDTRY.

    ENDDO.

  ENDMETHOD.
  METHOD save_query.
    DATA:
      ls_ztquery        TYPE ztquery.

    ls_ztquery = cs_ztquery.

    IF cs_ztquery-queryid IS INITIAL.
      ls_ztquery-queryid = lcl_app=>get_new_guid( ).
      ls_ztquery-owner = syst-uname.
      ls_ztquery-mandt = syst-mandt.
    ENDIF.

    ls_ztquery-query = iv_query.
    ls_ztquery-text  = w-qname.
    ls_ztquery-timestamp = |{ syst-datum }{ syst-uzeit }|.
    ls_ztquery-visibility = w-qvisibility.

    MODIFY ztquery FROM ls_ztquery.

    IF syst-subrc IS INITIAL.
      MESSAGE s000(db) WITH 'Query Saved'.
      cs_ztquery = ls_ztquery.
    ENDIF.

  ENDMETHOD.
  METHOD call_multisel.

    CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
      EXPORTING
        title            = 'Multiple Selection'
        signed           = 'X'
        just_display     = ''
        just_incl        = 'X'
        excluded_options = iv_excl_opt
        search_help      = iv_hlpname
        tab_and_field    = iv_input
      TABLES
        range            = ct_rettab
      EXCEPTIONS
        cancelled        = 0
        OTHERS           = 1.

    IF sy-subrc <> 0.
      REFRESH ct_rettab.
    ENDIF.

  ENDMETHOD.
  METHOD rfrsh_qwiz_data.

    CLEAR :
       lcl_app=>t_sqlfld[],
       lcl_app=>t_tabrel[].

  ENDMETHOD.
  METHOD popup_get_values.

    DATA:
      lt_sval        TYPE ty_sval.
    DATA:
      lv_returncode  TYPE c.

    APPEND cs_sval TO lt_sval.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = space
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_sval
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc NE 0
    OR lv_returncode NE space.
      ev_exit = abap_true.
    ENDIF.

    READ TABLE lt_sval INTO cs_sval INDEX 1.

    IF cs_sval-value = space.
      ev_exit = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD ret_ora_whr_str.

    DATA:
      lv_whr  TYPE string,
      lv_upto TYPE i.

    CHECK iv_whr  IS NOT INITIAL
      OR  iv_upto IS NOT INITIAL.

    lv_whr  = iv_whr.
    lv_upto = iv_upto.

    IF  lv_upto IS INITIAL
    AND lcl_app=>default_rows IS NOT INITIAL.

      lv_upto = lcl_app=>default_rows.

    ENDIF.

    rv_whr_str = 'WHERE'.

    IF lv_whr IS NOT INITIAL.

      REPLACE ALL OCCURRENCES OF ' NE ' IN lv_whr WITH '<>'.
      REPLACE ALL OCCURRENCES OF ' EQ ' IN lv_whr WITH '='.
      REPLACE ALL OCCURRENCES OF ' GE ' IN lv_whr WITH '=>'.
      REPLACE ALL OCCURRENCES OF ' LE ' IN lv_whr WITH '<='.

      rv_whr_str = |{ rv_whr_str } { lv_whr } |.

      IF lv_upto IS NOT INITIAL.
        rv_whr_str = |{ rv_whr_str } AND|.
      ENDIF.

    ENDIF.

    IF lv_upto IS NOT INITIAL.
      rv_whr_str = |{ rv_whr_str } ROWNUM <= { rv_whr_str } |.
    ENDIF.

  ENDMETHOD.
  METHOD call_sel_screen.
    DATA:
      lv_selid TYPE rsdynsel-selid.

    CLEAR:
      et_where[].

    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind         = 'F'
      IMPORTING
        selection_id = lv_selid
      TABLES
        fields_tab   = ct_fld
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0 .
      RETURN.

    ENDIF.

    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id  = lv_selid
        title         = 'Select data'
        as_window     = 'X'
        tree_visible  = ' '
      IMPORTING
        where_clauses = et_where
      TABLES
        fields_tab    = ct_fld
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.
  METHOD ret_query.

    DATA:
      ls_sqlfld TYPE ty_sqlfields,
      ls_fld    TYPE rsdsfields.

    DATA:
      lt_fld   TYPE STANDARD TABLE OF rsdsfields,
      lt_where TYPE rsds_twhere.

    w-o_sqlfld_alv->check_changed_data( ).

    w-o_tabrel_alv->check_changed_data( ).

    READ TABLE t_sqlfld INTO ls_sqlfld WITH KEY input = abap_true.

    IF syst-subrc IS INITIAL.

      LOOP AT t_sqlfld INTO ls_sqlfld WHERE input = abap_true.
        ls_fld-tablename = ls_sqlfld-tabname.
        ls_fld-fieldname = ls_sqlfld-fieldname.
        APPEND ls_fld TO lt_fld.
      ENDLOOP.

      lcl_app=>call_sel_screen( IMPORTING et_where = lt_where
                                 CHANGING ct_fld   = lt_fld
                              ).

    ENDIF.

    rt_string = lcl_app=>prep_query( it_whr    = lt_where
                                     it_fld    = lt_fld
                                   ).
    CLEAR:
      lcl_app=>t_tabrel,
      lcl_app=>t_sqlfld.

  ENDMETHOD.

  METHOD chk_tabnm_empt_vald.
    DATA:
      lv_tabname   TYPE tabname.

    IF iv_tabname IS INITIAL.

      RAISE EXCEPTION TYPE lcl_error
        EXPORTING
          iv_text = 'Table Name Empty!'.

    ELSE.

      lv_tabname = iv_tabname.

      CONDENSE lv_tabname.

      lcl_app=>check_tab_view_exists( lv_tabname ).

    ENDIF.

  ENDMETHOD.
  METHOD handle_sqlfld.
    DATA :
      lt_dd03vt     TYPE STANDARD TABLE OF ty_dd03vt .

    DATA :
      lv_tabname    TYPE tabname.

    lv_tabname = iv_tabname.

    CONDENSE lv_tabname.

    lcl_app=>get_tabfields( EXPORTING iv_tabname = lv_tabname
                            IMPORTING et_data    = lt_dd03vt
                          ).

    lcl_app=>fld_sel_popup( EXPORTING it_data   = lt_dd03vt
                             CHANGING ct_fields = t_sqlfld
                          ).

    IF t_sqlfld IS INITIAL.

      RETURN.

    ENDIF.

    lcl_app=>fill_tabrel( ).

  ENDMETHOD.
  METHOD fill_tabrel.
    DATA:
      lt_sqlfields  TYPE STANDARD TABLE OF ty_sqlfields .

    DATA:
      ls_tabrel TYPE ty_tabrel,
      ls_sqlfld TYPE ty_sqlfields.

    lt_sqlfields = t_sqlfld.

    SORT lt_sqlfields  BY tabname.

    DELETE ADJACENT DUPLICATES FROM lt_sqlfields COMPARING tabname.

    CLEAR:
      t_tabrel[].

    LOOP AT lt_sqlfields INTO ls_sqlfld.

      IF ls_tabrel-tabname1 IS INITIAL.
        ls_tabrel-tabname1 = ls_sqlfld-tabname.
      ENDIF.

      IF ls_tabrel-tabname1  <> ls_sqlfld-tabname.
        ls_tabrel-rel_txt = 'INNER JOIN'.
        ls_tabrel-tabname2 = ls_sqlfld-tabname.
        APPEND ls_tabrel TO t_tabrel.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD check_tab_view_exists.

    SELECT COUNT(*)
           FROM dd02l
           UP TO 1 ROWS
           WHERE tabname = iv_name
             AND as4local = 'A'
             AND tabclass IN ('TRANSP' , 'VIEW' , 'CLUSTER').

    IF syst-dbcnt <> 1.
      RAISE EXCEPTION TYPE lcl_error
        EXPORTING
          iv_text = 'Table/View does not exists'.
    ENDIF.
  ENDMETHOD.
  METHOD set_settings.

    DATA:
      lt_ptab  TYPE wdy_wb_property_tab.

    DATA:
      ls_ptab  TYPE wdy_wb_property.

    CHECK lcl_app=>o_settings IS NOT INITIAL.

    lt_ptab = lcl_app=>o_settings->get_properties( ).

    LOOP AT lt_ptab INTO ls_ptab.

      CASE ls_ptab-name.

        WHEN 'PB'.
          ls_ptab-value = lcl_app=>paste_break.

        WHEN 'MAXROWS'.
          ls_ptab-value = lcl_app=>default_rows.

        WHEN 'TECH'.
          ls_ptab-value = lcl_app=>techname.

        WHEN 'CONVERSION'.
          ls_ptab-value = lcl_app=>conversion.

      ENDCASE.

    ENDLOOP.

    lcl_app=>o_settings->set_properties( properties = lt_ptab
                                         refresh    = abap_true
                                       ).

  ENDMETHOD.

  METHOD get_settings.

    DATA:
      lt_ptab  TYPE wdy_wb_property_tab.

    DATA:
      ls_ptab  TYPE wdy_wb_property.

    lt_ptab = lcl_app=>o_settings->get_properties( ).

    LOOP AT lt_ptab INTO ls_ptab.
      CASE ls_ptab-name.
        WHEN 'PB'.
          lcl_app=>paste_break = ls_ptab-value.
        WHEN 'MAXROWS'.
          lcl_app=>default_rows = ls_ptab-value.
        WHEN 'TECH'.
          lcl_app=>techname = ls_ptab-value.
        WHEN 'CONVERSION'.
          lcl_app=>conversion = ls_ptab-value .
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_settings.

    DATA:
   	  lv_ucomm TYPE syucomm.

    lcl_app=>set_settings( ).
    CALL SCREEN 9200 STARTING AT 60 10
    ENDING AT 90 16.

    IF w_okcode NE 'OK_SET'.
      RETURN.
    ENDIF.
    lv_ucomm = iv_ucomm.
    lcl_app=>o_settings->dispatch(  EXPORTING cargo             = lv_ucomm
                                              eventid           = 18
                                              is_shellevent     = space
                                              is_systemdispatch = space
                                   EXCEPTIONS OTHERS            = 0
                                 ).

    lcl_app=>get_settings( ).
  ENDMETHOD.
  METHOD init_settings_cont.

    IF w-o_container_options IS INITIAL.

      CREATE OBJECT w-o_container_options
        EXPORTING
          container_name = 'CUSTCONT2'
        EXCEPTIONS
          OTHERS         = 6.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcl_error
          EXPORTING
            iv_text = 'Error Initialising Settings Container.'.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD init_settings_obj.
    DATA:
      lt_ptab TYPE wdy_wb_property_tab,
      ls_ptab TYPE wdy_wb_property.:

    IF lcl_app=>o_settings IS INITIAL.
      CREATE OBJECT lcl_app=>o_settings
        EXPORTING
          parent = w-o_container_options
        EXCEPTIONS
          OTHERS = 4.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcl_error
          EXPORTING
            iv_text = 'Error Initialising Settings.'.
      ENDIF.

      lcl_app=>o_settings->initialize( property_column_title = 'Property'
                                       value_column_title    = 'Value'
                                       focus_row             = 1
                                       scrollable            = abap_true
                                      ).

      lcl_app=>o_settings->set_enabled( abap_true ).

      ls_ptab-name    = 'PB'.
      ls_ptab-type    = cl_wdy_wb_property_box=>property_type_boolean.
      ls_ptab-enabled = abap_true.
      ls_ptab-value   = lcl_app=>paste_break.

      CONCATENATE '@74\Q'
      'Add break line after pasting ddic field into sql editor'
      '@' 'Line Break' INTO ls_ptab-display_name.

      APPEND ls_ptab TO lt_ptab.

      ls_ptab-name    = 'MAXROWS'.
      ls_ptab-type    = cl_wdy_wb_property_box=>property_type_integer.
      ls_ptab-enabled = abap_true.
      ls_ptab-value   = lcl_app=>default_rows.

      CONCATENATE '@3W\Q'
      'Default max number of displayed lines for SELECT'
      '@' 'Max Rows' INTO ls_ptab-display_name.
      APPEND ls_ptab TO lt_ptab.

      ls_ptab-name    = 'TECH'.
      ls_ptab-type    = cl_wdy_wb_property_box=>property_type_boolean.
      ls_ptab-enabled = abap_true.
      ls_ptab-value   = lcl_app=>techname.

      CONCATENATE '@AJ\Q'
      'Display technical name in query result display'
      '@' 'Technical name' INTO ls_ptab-display_name.

      APPEND ls_ptab TO lt_ptab.

      ls_ptab-name    = 'CONVERSION'.
      ls_ptab-type    = cl_wdy_wb_property_box=>property_type_boolean.
      ls_ptab-enabled = abap_true.
      ls_ptab-value   = lcl_app=>conversion.

      CONCATENATE '@AJ\Q'
      'Display data with Conversion'
      '@' 'Conversion Exit' INTO ls_ptab-display_name.

      APPEND ls_ptab TO lt_ptab.

      lcl_app=>o_settings->set_properties( properties = lt_ptab
                                           refresh    = abap_true
                                         ).

    ENDIF.

  ENDMETHOD.
  METHOD settings_output.

    lcl_app=>init_settings_cont( ).

    lcl_app=>init_settings_obj( ).

  ENDMETHOD.
  METHOD get_q_template.

    APPEND '' TO rt_query.
    APPEND 'SELECT *' TO rt_query.                          "#EC NOTEXT
    APPEND 'FROM <table_name>' TO rt_query.                 "#EC NOTEXT

    APPEND 'WHERE <conditions>' TO rt_query.                "#EC NOTEXT
    APPEND '.' TO rt_query.                                 "#EC NOTEXT

  ENDMETHOD.
  METHOD set_visdropdown.

    DATA:
      lt_visibility TYPE vrm_values,
      ls_visibility TYPE vrm_value.

    ls_visibility-key  = '0'.
    ls_visibility-text = 'Personal'.
    APPEND ls_visibility TO lt_visibility.

    ls_visibility-key  = '1'.
    ls_visibility-text = 'User group'.
    APPEND ls_visibility TO lt_visibility.

    ls_visibility-key  = '2'.
    ls_visibility-text = 'All'.
    APPEND ls_visibility TO lt_visibility.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'W-QVISIBILITY'
        values = lt_visibility.

  ENDMETHOD.
  METHOD col_exp_repo.

    IF lcl_app=>v_repo_width > 0.
      lcl_app=>v_repo_width = 0.
    ELSE.
      lcl_app=>v_repo_width = 20.
    ENDIF.

    lcl_app=>set_ddic_repo_width( lcl_app=>v_repo_width ).
    lcl_app=>set_query_repo_width( lcl_app=>v_repo_width ).

  ENDMETHOD.
  METHOD get_est_rows.
    DATA ora_par TYPE STANDARD TABLE OF   oraexplpar .
    DATA values TYPE STANDARD TABLE OF  expl_value .
    DATA subrc TYPE sysubrc.
    DATA lv_len TYPE i.
    DATA lv_string      TYPE string.
    DATA :
                card_p(16)         TYPE p.
    FIELD-SYMBOLS:<lf_plan> TYPE any.
    FIELD-SYMBOLS:<lf_rows> TYPE any.
    FIELD-SYMBOLS:
    <lfs_plan>         TYPE STANDARD TABLE.
    lv_string =  iv_query.

    CONDENSE lv_string.

    lv_len = strlen( lv_string ).

    PERFORM before_explain_plan IN PROGRAM rsxplora
                                           TABLES ora_par.

    PERFORM db_explain_plan IN PROGRAM rsxplora  TABLES values ora_par USING lv_string lv_len
                           CHANGING subrc.

    IF subrc IS NOT INITIAL.
      RETURN.
    ENDIF.
    PERFORM read_plan_table_ora9 IN PROGRAM rsxplora USING lv_string.

    ASSIGN ('(RSXPLORA)_plan_table_tab[]') TO <lfs_plan>.

    DATA:id TYPE string VALUE 'ID'.

    IF <lfs_plan> IS NOT ASSIGNED
    OR <lfs_plan> IS INITIAL .

      RETURN.

    ENDIF.
    READ TABLE <lfs_plan> ASSIGNING <lf_plan> WITH KEY (id) = 0.

    IF syst-subrc <> 0
    OR <lf_plan> IS NOT ASSIGNED.
      RETURN.

    ENDIF.

    ASSIGN COMPONENT 'CARDINALITY' OF STRUCTURE <lf_plan> TO <lf_rows>.

    IF <lf_rows> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    card_p = <lf_rows>.
    WRITE card_p TO rv_rows LEFT-JUSTIFIED.

    REFRESH <lfs_plan>.

  ENDMETHOD.
  METHOD get_ddicfields.

    SELECT dd03l~tabname
           dd03l~fieldname
           dd03l~position
           dd03l~keyflag
           dd03m~scrtext_l
           INTO TABLE et_data
                 FROM dd03l LEFT OUTER JOIN dd03m
                   ON dd03l~tabname    = dd03m~tabname
                  AND dd03l~fieldname  = dd03m~fieldname
                  AND dd03m~ddlanguage = sy-langu
                WHERE dd03l~tabname    = iv_val
                  AND dd03l~as4local   = 'A'
                  AND dd03l~as4vers    = space
                  AND ( dd03l~comptype = 'E'
                     OR dd03l~comptype = space ).

    IF syst-subrc  IS NOT INITIAL.
      RAISE EXCEPTION TYPE lcl_error
        EXPORTING
          iv_text = 'No Data Found for Input Table!'.
    ENDIF.

  ENDMETHOD.
  METHOD set_ddic_repo_width.

    w-o_splitter->set_column_width(  EXPORTING id                = 3
                                               width             = iv_width
                                    EXCEPTIONS OTHERS            = 0
                                  ).


  ENDMETHOD.
  METHOD set_query_repo_width.

    w-o_splitter->set_column_width( EXPORTING id                = 1
                                              width             = iv_width
                                   EXCEPTIONS OTHERS            = 0
                                  ).


  ENDMETHOD.
  METHOD handle_bg_alv.

    DATA:
      lo_data TYPE REF TO data,
      lo_salv TYPE REF TO cl_salv_table.

    FIELD-SYMBOLS:
      <lfs_data> TYPE STANDARD TABLE.

    lo_data = lcl_app=>exec_query_string( iv_query = iv_query
                                          iv_upto  = iv_upto
                                        ).

    ASSIGN lo_data->* TO <lfs_data>.

    IF <lfs_data> IS NOT ASSIGNED
    OR <lfs_data> IS  INITIAL.

      RETURN.

    ENDIF.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table   =   lo_salv
                                 CHANGING t_table        = <lfs_data>
                              ).

        lo_salv->display( ).
      CATCH cx_salv_msg.

    ENDTRY.
  ENDMETHOD.
  METHOD init_ddic_toolbar.

    DATA:
      lt_button TYPE ttb_button,
      ls_button TYPE stb_button,
      lt_events TYPE cntl_simple_events,
      ls_events TYPE cntl_simple_event.

    w-o_toolbar = w-o_splitter_ddic_full->get_toolbar( ).

    IF w-o_toolbar IS  INITIAL.

      RETURN.

    ENDIF.

    CLEAR ls_button.
    ls_button-function   = 'RFRS_DD_TRE'.
    ls_button-icon       = icon_refresh.
    ls_button-quickinfo  = 'Refresh DDIC tree'.
    ls_button-text       = 'Refresh'.
    ls_button-butn_type  = 0.

    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function   = 'FIND'.
    ls_button-icon       = icon_search.
    ls_button-quickinfo  = 'Search in DDIC tree'.
    ls_button-text       = 'Find'.
    ls_button-butn_type  = 0.

    APPEND ls_button TO lt_button.

    CLEAR ls_button.
    ls_button-function   = 'SET_FLD_IP'.
    ls_button-icon       = icon_biw_report.
    ls_button-quickinfo  = 'Push As Input'.
    ls_button-text       = 'Set Input'.
    ls_button-butn_type  = 0.
    APPEND ls_button TO lt_button.

    ls_button-function   = 'ADD'.
    ls_button-quickinfo  = 'Add Table'.
    ls_button-text       = 'ADD'.
    ls_button-butn_type  = 0.
    APPEND ls_button TO lt_button.


    w-o_toolbar->add_button_group( EXPORTING data_table       = lt_button
                                  EXCEPTIONS OTHERS           = 1
                                 ).

    IF syst-subrc <> 0.
    ENDIF.

    ls_events-eventid = cl_gui_toolbar=>m_id_function_selected.
    APPEND ls_events TO lt_events.

    w-o_toolbar->set_registered_events( EXPORTING events                    = lt_events
                                       EXCEPTIONS OTHERS                    = 1
      ).

    IF syst-subrc <> 0.
    ENDIF.

  ENDMETHOD.
  METHOD tabrel_fcat.

    DATA:
      ls_fcat   TYPE lvc_s_fcat,
      ls_result TYPE abap_componentdescr.

    DATA:
      lt_result TYPE abap_component_tab.

    lt_result = lcl_app=>get_type_comp( 'TY_TABREL' ).

    LOOP AT lt_result INTO ls_result.

      CLEAR ls_fcat.
      ls_fcat-fieldname = to_upper( ls_result-name ).
      ls_fcat-scrtext_m = to_upper( ls_result-name ).

      IF ls_fcat-fieldname = 'REL_TXT'.

        ls_fcat-edit      = 'X'.
        ls_fcat-drdn_hndl = 1.

      ENDIF.

      APPEND ls_fcat TO rt_fcat.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_type_comp.

    DATA:
      lo_type  TYPE REF TO cl_abap_typedescr,
      lo_struc TYPE REF TO cl_abap_structdescr.

    cl_abap_structdescr=>describe_by_name( EXPORTING p_name         = iv_name
                                           RECEIVING p_descr_ref    = lo_type
                                           EXCEPTIONS OTHERS         = 2
                                          ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_struc ?= lo_type.

    rt_comp   =  lo_struc->get_components( ).

  ENDMETHOD.
  METHOD sqlfld_fcat.

    DATA:
      ls_fcat   TYPE lvc_s_fcat,
      ls_result TYPE abap_componentdescr.

    DATA:
      lt_result TYPE abap_component_tab.

    lt_result = lcl_app=>get_type_comp( 'TY_SQLFIELDS' ).

    LOOP AT lt_result INTO ls_result.

      CLEAR ls_fcat.

      ls_fcat-fieldname = to_upper( ls_result-name ).
      ls_fcat-scrtext_m = to_upper( ls_result-name ).

      IF ls_fcat-fieldname = 'INPUT'.

        ls_fcat-checkbox = c-x.
        ls_fcat-edit     = c-x.

      ENDIF.

      APPEND ls_fcat TO rt_fcat.

    ENDLOOP.

  ENDMETHOD.
  METHOD init_sqlfld_cont.

    IF w-o_sqlfld_cont IS INITIAL.

      CREATE OBJECT w-o_sqlfld_cont
        EXPORTING
          container_name = 'CONT_SQLFIELDS'
        EXCEPTIONS
          OTHERS         = 1.

      IF syst-subrc <> 0.

        RAISE EXCEPTION TYPE lcl_error
          EXPORTING
            iv_text = 'Error Initialising SQL Fields Container.'.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD sqlfld_disp.

    lcl_app=>init_sqlfld_cont( ).

    IF w-o_sqlfld_alv IS INITIAL.

      lcl_app=>sqlfld_alv_init( ).

    ELSE.

      w-o_sqlfld_alv->refresh_table_display( EXCEPTIONS OTHERS      = 1 ).

      IF sy-subrc <> 0.

        RAISE EXCEPTION TYPE lcl_error
          EXPORTING
            iv_text = 'Error Refreshing Data.'.

      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD sqlfld_alv_init.

    DATA:
      lt_fcat TYPE lvc_t_fcat.

    DATA:
      ls_layo TYPE lvc_s_layo.

    CREATE OBJECT w-o_sqlfld_alv
      EXPORTING
        i_parent = w-o_sqlfld_cont
      EXCEPTIONS
        OTHERS   = 1.

    IF  syst-subrc <> 0.

      RAISE EXCEPTION TYPE lcl_error
        EXPORTING
          iv_text = 'Error Initialising SQL Fields ALV'.

    ENDIF.

    lt_fcat = lcl_app=>sqlfld_fcat( ).

    ls_layo-no_toolbar = c-x.

    w-o_sqlfld_alv->set_table_for_first_display(  EXPORTING is_layout                     = ls_layo
                                                   CHANGING it_outtab                     = t_sqlfld
                                                            it_fieldcatalog               = lt_fcat
                                                 EXCEPTIONS OTHERS                        = 1
                                               ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcl_error
        EXPORTING
          iv_text = 'Error Initialising SQL Fields ALV'.
    ENDIF.

  ENDMETHOD.
  METHOD tabrel_alv_init.
    DATA:
      lt_fcat TYPE lvc_t_fcat,
      lt_drdn TYPE lvc_t_drop.

    DATA:
      ls_layo TYPE lvc_s_layo,
      ls_drdn TYPE lvc_s_drop.

    CREATE OBJECT w-o_tabrel_alv
      EXPORTING
        i_parent = w-o_tabrel_cont
      EXCEPTIONS
        OTHERS   = 1.

    IF w-o_tabrel_alv IS INITIAL.

      RAISE EXCEPTION TYPE lcl_error
        EXPORTING
          iv_text = 'Error Initialising Table Relations ALV'.

    ENDIF.

    lt_fcat = lcl_app=>tabrel_fcat( ).

    ls_layo-no_toolbar = c-x.

    ls_drdn-handle     = 1.

    ls_drdn-value   = 'INNER JOIN'.
    APPEND ls_drdn TO lt_drdn.

    ls_drdn-value   = 'LEFT OUTER JOIN'.
    APPEND ls_drdn TO lt_drdn.

    w-o_tabrel_alv->set_drop_down_table( it_drop_down = lt_drdn  ).

    w-o_tabrel_alv->set_table_for_first_display( EXPORTING is_layout                     =  ls_layo
                                                  CHANGING it_outtab                     =  t_tabrel
                                                           it_fieldcatalog               =  lt_fcat
                                                EXCEPTIONS OTHERS                        = 1
                                               ).

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE lcl_error
        EXPORTING
          iv_text = 'Error Initialising Table Relations ALV'.

    ENDIF.

  ENDMETHOD.
  METHOD tabrel_disp.

    lcl_app=>init_tabrel_cont( ).

    IF w-o_tabrel_alv IS INITIAL.

      lcl_app=>tabrel_alv_init( ).

    ELSE.

      w-o_tabrel_alv->refresh_table_display( EXCEPTIONS OTHERS         = 1      ).

      IF sy-subrc <> 0.

        RAISE EXCEPTION TYPE lcl_error
          EXPORTING
            iv_text = 'Error Refreshing Data.'.

      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD init_tabrel_cont.

    IF w-o_tabrel_cont IS INITIAL.

      CREATE OBJECT w-o_tabrel_cont
        EXPORTING
          container_name = 'CONT_TABREL'
        EXCEPTIONS
          OTHERS         = 1.

      IF syst-subrc <> 0.

        RAISE EXCEPTION TYPE lcl_error
          EXPORTING
            iv_text = 'Error Initialising Table Relation Container.'.

      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD qwiz_output.

    lcl_app=>sqlfld_disp( ).

    lcl_app=>tabrel_disp( ).

  ENDMETHOD.
  METHOD get_sel_string.

    DATA :
      lv_sel      TYPE string.

    DATA:
      ls_sqlfld TYPE ty_sqlfields.

    lv_sel = 'SELECT'.

    LOOP AT t_sqlfld INTO ls_sqlfld.
      IF iv_join = abap_true.

        lv_sel = |{ lv_sel } { ls_sqlfld-tabname }~{ ls_sqlfld-fieldname }|.
      ELSE.
        lv_sel = |{ lv_sel }  { ls_sqlfld-fieldname }|.
      ENDIF.
      APPEND lv_sel TO rt_string.
      CLEAR lv_sel.
    ENDLOOP.

  ENDMETHOD.
  METHOD ret_joinfld_data.
    CHECK t_tabrel IS NOT INITIAL.
    SELECT dd03l~tabname
           dd03l~fieldname
           dd03m~tabname
           dd03l~position
           dd03l~keyflag
           dd03m~keyflag
    FROM dd03l INNER JOIN dd03m
    ON dd03l~fieldname = dd03m~fieldname
    INTO TABLE et_joinfld
    FOR ALL ENTRIES IN t_tabrel
    WHERE dd03l~tabname = t_tabrel-tabname1
    AND dd03m~tabname = t_tabrel-tabname2
    AND dd03m~ddlanguage = 'E' .

  ENDMETHOD.
  METHOD ret_from.
    DATA:
      ls_tabrel TYPE ty_tabrel,
      ls_sqlfld TYPE ty_sqlfields.

    DATA:
      lv_from     TYPE string.

    DATA:
      lt_joinfld  TYPE STANDARD TABLE OF ty_joinfld.

    lv_from = 'FROM'.

    IF iv_join = abap_true.

      lcl_app=>ret_joinfld_data( IMPORTING et_joinfld = lt_joinfld ).

      SORT lt_joinfld BY tab1
                         tab2
                         position.

      LOOP AT t_tabrel INTO ls_tabrel.

        lv_from = |{ lv_from } { ls_tabrel-tabname1 } { ls_tabrel-rel_txt } { ls_tabrel-tabname2 } ON |.

        APPEND lv_from TO ct_string.

        CLEAR lv_from.

        lcl_app=>ret_join_from_str(
          EXPORTING
            iv_tab1    = ls_tabrel-tabname1
            iv_tab2    = ls_tabrel-tabname2
            it_joinfld = lt_joinfld
          CHANGING
            ct_string  = ct_string
          ).

      ENDLOOP.

    ELSE.

      READ TABLE t_sqlfld INTO ls_sqlfld INDEX 1.
      IF syst-subrc <> 0.

        RETURN.

      ENDIF.
      lv_from = |{ lv_from } { ls_sqlfld-tabname }|.

      APPEND lv_from TO ct_string.

    ENDIF.

  ENDMETHOD.
  METHOD ret_join_from_str.

    DATA:
      ls_joinfld  TYPE ty_joinfld.

    DATA:
      lv_keymatch TYPE abap_bool,
      lv_from     TYPE string,
      lv_tabix    TYPE sytabix,
      lv_exit     TYPE abap_bool.

    READ TABLE it_joinfld TRANSPORTING NO FIELDS WITH KEY tab1 = iv_tab1
                                                          tab2 = iv_tab2
                                                          BINARY SEARCH.

    IF syst-subrc <> 0.

      RETURN.

    ENDIF.

    lv_tabix = syst-tabix.

    LOOP AT it_joinfld INTO ls_joinfld FROM lv_tabix.

      lcl_app=>chk_ext_joinfld_loop( EXPORTING is_joinfld  = ls_joinfld
                                               iv_tab1     = iv_tab1
                                               iv_tab2     = iv_tab2
                                     IMPORTING ev_exit     = lv_exit
                                      CHANGING cv_keymatch = lv_keymatch
                                   ).

      IF lv_exit = abap_true.
        EXIT.
      ENDIF.

      lv_from = |         { lv_from } { ls_joinfld-tab1 }~{ ls_joinfld-fldname } = { ls_joinfld-tab2 }~{ ls_joinfld-fldname } |.

      APPEND lv_from TO ct_string.

      lv_from = 'AND'.

    ENDLOOP.

  ENDMETHOD.
  METHOD chk_ext_joinfld_loop.

    CHECK is_joinfld-fldname <> 'MANDT'.

    IF is_joinfld-tab1  <> iv_tab1
    OR is_joinfld-tab2  <> iv_tab2.

      ev_exit = abap_true.

    ENDIF.

    IF is_joinfld-key = 'X'
    OR is_joinfld-key2 = 'X'.

      cv_keymatch = abap_true.

    ENDIF.

    IF  cv_keymatch = abap_true
    AND is_joinfld-key  <> 'X'
    AND is_joinfld-key2 <> 'X'.

      ev_exit = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD ret_whr.

    DATA:
      ls_fld      TYPE rsdsfields.

    FIELD-SYMBOLS:
      <lfs_whr>  TYPE rsdswhere,
      <lfs_whr1> TYPE rsds_where.

    DATA:
      lv_reg TYPE string,
      lv_rep TYPE string.

    CHECK iv_join = abap_true.

    SORT ct_whr BY tablename.

    LOOP AT it_fld INTO ls_fld.

      READ TABLE ct_whr ASSIGNING <lfs_whr1>
                         WITH KEY tablename = ls_fld-tablename
                                  BINARY SEARCH.

      CHECK syst-subrc IS INITIAL.

      LOOP AT <lfs_whr1>-where_tab ASSIGNING <lfs_whr> .

        lv_reg = '\b' && ls_fld-fieldname && '\b'.

        lv_rep = ls_fld-tablename && '~' && ls_fld-fieldname.

        <lfs_whr>-line = replace( val   = <lfs_whr>-line
                                  regex = lv_reg
                                  with  = lv_rep ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
  METHOD prep_query.

    DATA:
      lt_where   TYPE rsds_twhere      .

    DATA:
      lv_txt  TYPE string,
      lv_join TYPE abap_bool.

    DATA:
      ls_where    TYPE rsds_where,
      ls_whr_data TYPE rsdswhere.

    IF t_tabrel IS NOT INITIAL.
      lv_join = abap_true.
    ENDIF.

    rt_string = lcl_app=>get_sel_string( lv_join ).

    lcl_app=>ret_from(  EXPORTING iv_join  = lv_join
                        CHANGING ct_string = rt_string
                      ).

    IF it_whr IS  INITIAL
    OR it_fld IS  INITIAL.
      RETURN.
    ENDIF.

    lt_where = it_whr.
    lcl_app=>ret_whr( EXPORTING iv_join  = lv_join
                                it_fld   = it_fld
                      CHANGING  ct_whr   = lt_where ).

    lv_txt = 'WHERE'.
    LOOP AT lt_where INTO ls_where.

      LOOP AT ls_where-where_tab INTO ls_whr_data.

        CONDENSE ls_whr_data-line.
        lv_txt = |{ lv_txt } { ls_whr_data-line }|.
        APPEND lv_txt TO rt_string.
        CLEAR lv_txt.
      ENDLOOP.
      lv_txt = 'AND'.
    ENDLOOP.

  ENDMETHOD.
  METHOD fld_sel_popup.

    TYPES:
      BEGIN OF lty_fieldtab,
        tf   TYPE dd03p,
        mark TYPE char1,
      END OF lty_fieldtab.

    DATA :
      lt_tabfields TYPE STANDARD TABLE OF lty_fieldtab.

    DATA :
      ls_tabfields TYPE lty_fieldtab,
      ls_data      TYPE ty_dd03vt,
      ls_fields    TYPE ty_sqlfields.

    CHECK it_data IS NOT INITIAL.

    LOOP AT it_data INTO ls_data.
      ls_tabfields-tf-fieldname = ls_data-fieldname.
      ls_tabfields-tf-tabname  = ls_data-tabname.
      ls_tabfields-tf-rollname = ls_data-rollname.
      ls_tabfields-tf-ddtext   = ls_data-ddtext.

      READ TABLE ct_fields TRANSPORTING NO FIELDS
                                         WITH KEY tabname = ls_data-tabname
                                                  fieldname = ls_data-fieldname.
      IF  syst-subrc IS INITIAL.

        ls_tabfields-mark  = 'X'.
      ELSE.
        CLEAR ls_tabfields-mark.
      ENDIF.

      APPEND ls_tabfields TO lt_tabfields.

    ENDLOOP.

    CALL FUNCTION 'DD_LIST_TABFIELDS'
      TABLES
        fieldtab = lt_tabfields
      EXCEPTIONS
        OTHERS   = 2.

    IF sy-subrc <> 0.

      RETURN.

    ENDIF.

    DELETE lt_tabfields WHERE mark IS INITIAL.

    DELETE ct_fields WHERE tabname = ls_tabfields-tf-tabname.

    LOOP AT lt_tabfields INTO ls_tabfields.
      ls_fields-fieldname = ls_tabfields-tf-fieldname.
      ls_fields-tabname   = ls_tabfields-tf-tabname.
      ls_fields-ddtext  = ls_tabfields-tf-ddtext.
      APPEND ls_fields TO ct_fields.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_tabfields.

    SELECT  tabname
            fieldname
            position
            rollname
            checktable
            ddtext
            FROM  dd03vt
            INTO TABLE et_data
            WHERE tabname    = iv_tabname
            AND as4local   = 'A'
            AND ddlanguage = 'E'.

    IF syst-subrc <> 0.

      RAISE EXCEPTION TYPE lcl_error
        EXPORTING
          iv_text = 'No Data found for Input Table'.
    ENDIF.
    SORT et_data BY position ASCENDING.


  ENDMETHOD.
  METHOD ret_tabname.

    DATA:
      lv_ret     TYPE char1.
    DATA:
      ls_fields  TYPE sval.

    DATA:
      lt_fields  TYPE STANDARD TABLE OF sval.

    ls_fields-tabname   = 'DD03VT'.
    ls_fields-fieldname = 'TABNAME'.
    ls_fields-field_obl = 'X'.

    APPEND ls_fields TO lt_fields.

    CALL FUNCTION 'POPUP_GET_VALUES_DB_CHECKED'
      EXPORTING
        check_existence = 'X'
        popup_title     = 'Input Table Name'
      IMPORTING
        returncode      = lv_ret
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0
    OR lv_ret IS NOT INITIAL.

      MESSAGE s000(db) WITH 'Incorrect Table Name'.

    ELSE.

      READ TABLE lt_fields INTO ls_fields INDEX 1.

      IF syst-subrc IS INITIAL.
        rv_tabname = ls_fields-value.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD push_list.

    DATA :
      ls_list   TYPE ty_list.

    ls_list-ref = iv_ref.

    APPEND ls_list TO t_list.

  ENDMETHOD.

  METHOD modif_list.

    DATA :
      ls_list   TYPE ty_list.

    ls_list-ref = iv_ref.

    MODIFY t_list FROM ls_list INDEX iv_index.

  ENDMETHOD.

  METHOD read_list.
    DATA:
      ls_list TYPE ty_list.

    READ TABLE t_list INTO ls_list INDEX iv_index.

    IF syst-subrc IS INITIAL.

      rv_ref = ls_list-ref.

    ENDIF.

  ENDMETHOD.
  METHOD pop_list .

    DELETE t_list INDEX iv_index.

  ENDMETHOD.
  METHOD chk_limit.

    IF lines( t_list ) >= 30.
      MESSAGE 'You cannot open more than 30 tabs'(m64)
      TYPE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.
  METHOD qtab_to_string.
    DATA :
      lt_query         TYPE soli_tab.
    DATA:
      ls_query        TYPE soli,
      ls_find_comment TYPE match_result.

    lt_query = it_query.

    LOOP AT lt_query INTO ls_query.
      IF iv_com = abap_false.
        CONDENSE ls_query-line.
        CHECK ls_query IS NOT INITIAL
          AND ls_query-line+0(1) <> '*'
          AND ls_query-line+0(1) <> '"'.

        FIND FIRST OCCURRENCE OF '"'
                              IN ls_query-line
                         RESULTS ls_find_comment IGNORING CASE.

        IF sy-subrc = 0.
          ls_query-line = ls_query-line+0(ls_find_comment-offset).
        ENDIF.

        SHIFT ls_query-line LEFT DELETING LEADING space.
      ENDIF.
      IF rv_query IS INITIAL.
        rv_query =  ls_query-line.
        CONTINUE.
      ENDIF.
      CONCATENATE rv_query ls_query-line INTO rv_query SEPARATED BY iv_sep.

    ENDLOOP.

  ENDMETHOD.
  METHOD ret_query_comp.

    DATA:
      ls_find_upto TYPE match_result,
      ls_sub       TYPE submatch_result,
      ls_sql       TYPE sqls_stmt_t.

    DATA:
      lo_regex       TYPE REF TO cl_abap_regex.

    DATA:
      lv_query       TYPE string.

    lv_query = iv_query.

    lv_query = replace(
                 val   = lv_query
                 sub   = '.'
                 occ   = -1
                 with  = space
                 ).

    CREATE OBJECT lo_regex
      EXPORTING
        pattern     = 'UP TO ([0-9]+) ROWS'
        ignore_case = abap_true.

    FIND FIRST OCCURRENCE OF REGEX lo_regex
                                IN lv_query
                           RESULTS ls_find_upto.
    IF sy-subrc = 0.
      READ TABLE ls_find_upto-submatches INTO ls_sub INDEX 1.
      IF sy-subrc = 0.
        ev_upto = lv_query+ls_sub-offset(ls_sub-length).
      ENDIF.

      REPLACE FIRST OCCURRENCE OF REGEX lo_regex
                                     IN lv_query WITH ''.

    ENDIF.

    CALL FUNCTION 'DB_SQL_PARSE'
      EXPORTING
        stmt_str = lv_query
      IMPORTING
        sql_stmt = ls_sql.

    IF ls_sql-operation <> 'SELECT'.

      MESSAGE e000(db) WITH 'ERROR in Query'.

    ENDIF.

    ev_select = lv_query+ls_sql-select_list-off(ls_sql-select_list-len).
    ev_where  = lv_query+ls_sql-where_clause-off(ls_sql-where_clause-len).
    ev_from   = lv_query+ls_sql-table_clause-off(ls_sql-table_clause-len).
    ev_order  = lv_query+ls_sql-order_by_clause-off(ls_sql-order_by_clause-len).
    ev_group  = lv_query+ls_sql-group_by_clause-off(ls_sql-group_by_clause-len).

  ENDMETHOD.
  METHOD ret_tab_comp.

    DATA:
      lo_type  TYPE REF TO cl_abap_typedescr,
      lo_struc TYPE REF TO cl_abap_structdescr.

    DATA:
      lt_comps    TYPE abap_component_view_tab.

    DATA:
      ls_comps TYPE abap_simple_componentdescr,
      ls_comp  TYPE abap_componentdescr.

    cl_abap_structdescr=>describe_by_name(  EXPORTING    p_name         = iv_name
                                            RECEIVING    p_descr_ref    = lo_type
                                            EXCEPTIONS   OTHERS         = 2
                                         ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_struc ?= lo_type.

    rt_comp =  lo_struc->get_components( ).

    READ TABLE rt_comp TRANSPORTING NO FIELDS WITH KEY as_include = 'X'.

    IF syst-subrc IS INITIAL.

      REFRESH rt_comp.

      lt_comps =   lo_struc->get_included_view(  ).

      LOOP AT lt_comps INTO ls_comps.

        ls_comp-name = ls_comps-name.
        ls_comp-type = ls_comps-type.
        APPEND ls_comp TO rt_comp.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD chk_all_wid_join.

    IF  iv_from CS 'JOIN'
    AND iv_select CS '*'.
      MESSAGE e000(db) WITH '* not possible with join'.
    ENDIF.

  ENDMETHOD.
  METHOD ret_from_tabref.

    DATA:
      ls_from        TYPE string.

    DATA:
      lt_from_tab    TYPE string_table.

    DATA:
      lv_tabname     TYPE tabname.

    SPLIT iv_from   AT space
            INTO TABLE lt_from_tab.

    LOOP AT lt_from_tab INTO ls_from.

      TRANSLATE ls_from TO UPPER CASE.
      TRANSLATE ls_from USING '. '.
      CONDENSE ls_from.

      CASE ls_from.

        WHEN 'INNER'
          OR 'JOIN'
          OR 'OUTER'
          OR 'ON'
          OR '='
          OR '~'
          OR space.

          CONTINUE.

      ENDCASE.

      TRY.

          lv_tabname = ls_from.

          lcl_app=>chk_tabnm_empt_vald( lv_tabname ).

          IF  iv_select CS '*'
          AND iv_select NS '(*)'.

            APPEND LINES OF lcl_app=>get_type_comp( ls_from ) TO ev_tab1.
          ELSE.
            APPEND LINES OF lcl_app=>ret_tab_comp( ls_from ) TO ev_tab.

          ENDIF.

        CATCH lcl_error.

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.
  METHOD ret_query_tabref.

    DATA:
      lt_fields      TYPE string_table.

    DATA:
      ls_fields      TYPE string.

    DATA:
      lo_comp_tab1 TYPE cl_abap_structdescr=>component_table,
      lo_comp_tab  TYPE cl_abap_structdescr=>component_table.
    DATA:
      ls_comp_tab    TYPE abap_componentdescr.

    DATA:
      lv_skip        TYPE abap_bool.

    DATA:
      lo_new_tab  TYPE REF TO cl_abap_tabledescr.

    REFRESH:
      et_fields[].

    SPLIT iv_select AT space
            INTO TABLE lt_fields.

    lcl_app=>chk_all_wid_join(
      iv_select = iv_select
      iv_from   = iv_from
      ).

    lcl_app=>ret_from_tabref(
      EXPORTING
        iv_select = iv_select
        iv_from   = iv_from
      IMPORTING
        ev_tab    = lo_comp_tab
        ev_tab1   = lo_comp_tab1
      ).

    LOOP AT lt_fields INTO ls_fields.

      TRANSLATE ls_fields TO UPPER CASE.

      lcl_app=>ret_fld(
        EXPORTING
          iv_fld  = ls_fields
          tab1    = lo_comp_tab
          tab2    = lo_comp_tab1
        IMPORTING
          ev_skip = lv_skip
          es_tab  = ls_comp_tab
        ).

      CHECK lv_skip  = abap_false.

      APPEND ls_comp_tab TO lo_comp_tab1.
      APPEND ls_fields TO et_fields.

    ENDLOOP.

    IF lo_comp_tab1 IS INITIAL.

      RETURN.

    ENDIF.
    TRY.
        lo_new_tab = cl_abap_tabledescr=>create(
                       p_line_type  = cl_abap_structdescr=>create( lo_comp_tab1 )
                       p_table_kind = cl_abap_tabledescr=>tablekind_std
                       p_unique     = abap_false
                       ).

        CREATE DATA rt_data TYPE HANDLE lo_new_tab.

      CATCH
        cx_sy_table_creation
        cx_sy_create_data_error.

        RETURN.
    ENDTRY.

  ENDMETHOD.
  METHOD ret_fld.
    DATA:
      lv_tab   TYPE tabname,
      lv_fld   TYPE fieldname,
      lv_fldnm TYPE fieldname.

    SPLIT iv_fld AT '~'
               INTO lv_tab
                    lv_fld.

    IF lv_fld IS INITIAL.

      lv_fld = lv_tab.
      CLEAR lv_tab.

    ENDIF.

    lv_fldnm = iv_fld.

    TRANSLATE lv_fldnm USING '~_'.

    IF lv_fldnm = 'COUNT(*)'.
      es_tab-type ?= cl_abap_elemdescr=>describe_by_name( 'I' ).
      es_tab-name = 'COL_COUNT'.
      RETURN.
    ENDIF.
    READ TABLE tab1 INTO es_tab WITH KEY name = lv_fld.

    IF syst-subrc IS INITIAL.

      READ TABLE tab2 TRANSPORTING NO FIELDS WITH KEY name = lv_fldnm.

      IF syst-subrc IS INITIAL.
        ev_skip = abap_true.
      ELSE.
        es_tab-name = lv_fldnm.
      ENDIF.

    ELSE.
      ev_skip = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD exec_query_string.

    DATA:
      lt_fields      TYPE string_table.

    DATA:
      lv_select   TYPE string,
      lv_from     TYPE string,
      lv_where    TYPE string,
      lv_order    TYPE string,
      lv_group    TYPE string,
      lv_upto     TYPE i,
      lv_sel_len  TYPE i,
      lv_single   TYPE abap_bool,
      lv_distinct TYPE abap_bool.

    lcl_app=>ret_query_comp(
      EXPORTING
        iv_query  = iv_query
      IMPORTING
        ev_select = lv_select
        ev_from   = lv_from
        ev_where  = lv_where
        ev_upto   = lv_upto
        ev_order  = lv_order
        ev_group  = lv_group
        ).

    IF lv_from IS INITIAL.

      RETURN.

    ENDIF.

    IF lv_upto IS INITIAL.
      lv_upto = iv_upto.
    ENDIF.

    TRANSLATE lv_select TO UPPER CASE.
    CONDENSE lv_select.

    lv_sel_len  = strlen( lv_select ).

    IF lv_sel_len > 7.

      IF lv_select(6) = 'SINGLE' .

        lv_single = abap_true.
        lv_sel_len = lv_sel_len - 6.
        lv_select = lv_select+6(lv_sel_len).

      ELSEIF lv_select(8) = 'DISTINCT' .

        lv_distinct = abap_true.
        lv_sel_len = lv_sel_len - 8.
        lv_select = lv_select+8(lv_sel_len).

      ENDIF.
    ENDIF.

    CONDENSE lv_select.

    lcl_app=>ret_query_tabref(
      EXPORTING
        iv_select = lv_select
        iv_from   = lv_from
      IMPORTING
        rt_data   = rt_data
        et_fields = lt_fields
      ).

    DELETE lt_fields WHERE table_line IS INITIAL.

    lcl_app=>exec_select(
      EXPORTING
        iv_single   = lv_single
        iv_distinct = lv_distinct
        it_fields   = lt_fields
        iv_from     = lv_from
        iv_where    = lv_where
        iv_upto     = lv_upto
        iv_group    = lv_group
        iv_order    = lv_order
      CHANGING
        co_ref      = rt_data
      ).

  ENDMETHOD.
  METHOD exec_select.

    FIELD-SYMBOLS:
      <lft_data> TYPE STANDARD TABLE,
      <lfs_data> TYPE any.

    DATA :
      lo_root   TYPE REF TO cx_root.

    DATA :
      lv_msg    TYPE string.

    ASSIGN co_ref->* TO <lft_data>.

    IF <lft_data> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    TRY.
        CASE abap_true.

          WHEN iv_single.
            APPEND INITIAL LINE TO <lft_data> ASSIGNING <lfs_data>.

            SELECT SINGLE (it_fields)
                     FROM (iv_from)
                     INTO <lfs_data>
                    WHERE (iv_where)
                 GROUP BY (iv_group)
            .

          WHEN iv_distinct.

            SELECT DISTINCT (it_fields)
                       FROM (iv_from)
                      UP TO iv_upto ROWS
                 INTO TABLE <lft_data>
                      WHERE (iv_where)
                   GROUP BY (iv_group)
                   ORDER BY (iv_order)
            .

          WHEN OTHERS.










            SELECT (it_fields)
              FROM (iv_from)
             UP TO iv_upto ROWS
              INTO TABLE <lft_data>
             WHERE (iv_where)
          GROUP BY (iv_group)
          ORDER BY (iv_order)
            .

        ENDCASE.

        IF syst-subrc <> 0.
          MESSAGE e000(db) WITH 'Error fetching data'.
        ENDIF.

      CATCH cx_sy_dynamic_osql_syntax
            cx_sy_dynamic_osql_semantics
            cx_sy_open_sql_db
       INTO lo_root.

        lv_msg = lo_root->get_text( ).
        MESSAGE e000(db) WITH lv_msg.

    ENDTRY.
    DATA lv_msg1 TYPE char255.
    lv_msg1 = |{ lines( <lft_data> ) } rows fetched|.
    MESSAGE s000(db) WITH lv_msg1.

  ENDMETHOD.
  METHOD ret_tabfields_of_from.
    DATA:
      ls_from        TYPE string.

    DATA:
      lt_from_tab    TYPE string_table.

    DATA:
      ls_tabfields TYPE ty_tabfield.
    SPLIT iv_from   AT space
            INTO TABLE lt_from_tab.

    LOOP AT lt_from_tab INTO ls_from.

      TRANSLATE ls_from TO UPPER CASE.
      TRANSLATE ls_from USING '. '.
      CONDENSE ls_from.

      CASE ls_from.

        WHEN 'INNER'
          OR 'JOIN'
          OR 'OUTER'
          OR 'ON'
          OR '='
          OR space.

          CONTINUE.

      ENDCASE.

      IF ls_from CS '~'.
        SPLIT ls_from AT '~'
                    INTO ls_tabfields-tabname
                         ls_tabfields-fieldname.
        APPEND ls_tabfields TO rt_tabfields.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD ret_tabfields_from_fields.
    DATA:
      ls_tabfields TYPE ty_tabfield.
    DATA:
      lv_string TYPE string,
      lv_tab    TYPE tabname,
      lv_fld    TYPE fieldname.

    LOOP AT it_fields INTO lv_string.
      SPLIT lv_string  AT '~'
                     INTO lv_tab
                          lv_fld.

      ls_tabfields-tabname = lv_tab.
      ls_tabfields-fieldname = lv_fld.
      APPEND ls_tabfields TO rt_tabfields.
    ENDLOOP.

  ENDMETHOD.
  METHOD ret_tabs_of_from.
    DATA:
      ls_from        TYPE string.

    DATA:
      lt_from_tab    TYPE string_table.

    DATA:
      lv_tabname     TYPE tabname.

    SPLIT iv_from   AT space
            INTO TABLE lt_from_tab.

    LOOP AT lt_from_tab INTO ls_from.

      TRANSLATE ls_from TO UPPER CASE.
      TRANSLATE ls_from USING '. '.
      CONDENSE ls_from.

      CASE ls_from.

        WHEN 'INNER'
          OR 'JOIN'
          OR 'OUTER'
          OR 'ON'
          OR '='
          OR '~'
          OR space.

          CONTINUE.

      ENDCASE.

      TRY.

          lv_tabname = ls_from.

          lcl_app=>chk_tabnm_empt_vald( lv_tabname ).

          APPEND lv_tabname TO rt_tablist.

        CATCH lcl_error.

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.
  METHOD from_has_cluster_table.
    DATA:
      ls_from        TYPE string.

    DATA:
      lt_from_tab    TYPE string_table.

    DATA:
      lv_tabname     TYPE tabname.

    SPLIT iv_from   AT space
            INTO TABLE lt_from_tab.

    LOOP AT lt_from_tab INTO ls_from.

      TRANSLATE ls_from TO UPPER CASE.
      TRANSLATE ls_from USING '. '.
      CONDENSE ls_from.

      CASE ls_from.

        WHEN 'INNER'
          OR 'JOIN'
          OR 'OUTER'
          OR 'ON'
          OR '='
          OR '~'
          OR space.

          CONTINUE.

      ENDCASE.

      TRY.

          lv_tabname = ls_from.

          IF  abap_true = lcl_app=>is_cluster_tab( lv_tabname ).
            rv_yes = abap_true.
            RETURN.

          ENDIF.
        CATCH lcl_error.

      ENDTRY.

    ENDLOOP.
  ENDMETHOD.
  METHOD is_cluster_tab.

    SELECT COUNT(*)
               FROM dd02l
               UP TO 1 ROWS
               WHERE tabname = iv_name
                 AND as4local = 'A'
                 AND tabclass = 'CLUSTER'.

    IF syst-dbcnt = 1.
      rv_yes = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD exec_query.
    DATA: lv_query TYPE string.

    CHECK it_query IS NOT INITIAL.

    lv_query = lcl_app=>qtab_to_string( it_query ).

    IF lv_query IS NOT INITIAL.

      rt_data = lcl_app=>exec_query_string( iv_query = lv_query
                                            iv_upto  = iv_upto
                                        ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
                 "lcl_error IMPLEMENTATION
