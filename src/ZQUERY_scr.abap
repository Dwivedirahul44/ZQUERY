CLASS lcl_drag_object DEFINITION FINAL.
  PUBLIC SECTION.
    DATA:
      fields TYPE soli_tab.
ENDCLASS.
CLASS lcl_scr_static_objects_helper DEFINITION FINAL.

  PUBLIC SECTION.

  class-METHODs:
    init_screen_objects  IMPORTING io_container TYPE REF TO cl_gui_custom_container.

ENDCLASS.

CLASS lcl_scr_static_objects_helper IMPLEMENTATION.

  METHOD init_screen_objects .

 CHECK io_container IS NOT INITIAL.
  CREATE OBJECT w-o_splitter
    EXPORTING
      parent  = io_container
      rows    = 1
      columns = 3
    EXCEPTIONS
      others  = 3.

  IF syst-subrc <> 0.

    RETURN.

  ENDIF.

  w-o_qcontainer = w-o_splitter->get_container( row       = 1
                                                column    = 2
                                               ).

  w-o_container_ddic_full = w-o_splitter->get_container( row       = 1
                                                         column    = 3
                                                       ).

  CREATE OBJECT w-o_splitter_ddic_full
    EXPORTING
      i_r_container = w-o_container_ddic_full.

  w-o_container_ddic = w-o_splitter_ddic_full->get_controlcontainer( ).

  lcl_app=>init_ddic_toolbar( ).

  w-o_container_qrep = w-o_splitter->get_container( row       = 1
                                                    column    = 1
                                                  ).

  lcl_app=>v_repo_width = 20.

  lcl_app=>set_ddic_repo_width( lcl_app=>v_repo_width ).

  lcl_app=>set_query_repo_width( lcl_app=>v_repo_width ).

  ENDMETHOD.

ENDCLASS.
CLASS lcl_scr DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_local_vers,
        node_key             TYPE num6,
        txt(100)             TYPE c,
        query                TYPE string,
      END OF ty_local_vers.
    DATA:
      o_tree_ddic            TYPE REF TO cl_gui_column_tree,
      o_tree_qrep            TYPE REF TO cl_gui_simple_tree,
      o_dock                 TYPE REF TO cl_gui_docking_container,
      o_textedit             TYPE REF TO cl_gui_abapedit,
      o_alv                  TYPE REF TO cl_salv_table.

    CLASS-METHODS:
      init_screen_objects    IMPORTING io_container TYPE REF TO cl_gui_custom_container
                             RETURNING VALUE(ro_self)    TYPE REF TO lcl_scr.
    METHODS:
      constructor,

      handle_output          IMPORTING iv_dynnr          TYPE sydynnr,

      handle_ucomm           IMPORTING iv_ucomm          TYPE okcode
                             RETURNING value(er_app_ref)       TYPE REF TO lcl_scr.

  PRIVATE SECTION.

    DATA:
      t_node_ddic            TYPE treev_ntab,
      t_item_ddic            TYPE STANDARD TABLE OF mtreeitm,
      t_node_qrep            TYPE tty_qrep_node,
      t_local_vers           TYPE STANDARD TABLE OF ty_local_vers.

    DATA:
      o_tab                  TYPE REF TO data.

    DATA:
      v_ddrop_hndl_tree      TYPE i,
      v_node_num(6)          TYPE n,
      v_qrep_num(6)          TYPE n,
      v_qovrwrt              TYPE boolean,
      v_editable             TYPE boolean VALUE abap_true.

    DATA:
      s_ztquery              TYPE ztquery.

    METHODS:
      hndl_ugroup_multi,

      hndl_vuser_multi,

      handle_qsave,

      init_save_query RAISING lcl_error,

      check_query ,

      exit_app,

      set_focus_on_editor,

      report_estimated_qrows,

      ddic_init,

      query_repo_init,

      editor_init,

      dock_init,

      leave_current_tab,

      handle_visibility      IMPORTING iv_ref            TYPE REF TO lcl_scr
                                       iv_vis            TYPE abap_bool,

      ret_tab_obj            IMPORTING iv_index          TYPE sytabix OPTIONAL
                       RETURNING value(app_ref)          TYPE REF TO lcl_scr,

      exec2pc,

      ret_tab_columns        EXPORTING et_header         TYPE tty_header
                              CHANGING ct_data           TYPE STANDARD TABLE,

      get_file_path    RETURNING value(rv_filepath)      TYPE string,

      exec2bg,

      gen_query,

      set_tab_visible,

      alv_disp,

      alv_set_columns        IMPORTING io_alv            TYPE REF TO cl_salv_table,

      alv_init                CHANGING it_data           TYPE ANY TABLE,

      execute,

      handle_dtoolbar_add    IMPORTING iv_val            TYPE pvarfield,

      is_tab_current_ddic_itm IMPORTING iv_tabname       TYPE tabname
                             RETURNING value(rv_yes)    TYPE abap_bool,

      add_new_ddic_tabfolder IMPORTING iv_tabname        TYPE tabname,

      add_new_ddic_nodeitm   IMPORTING is_ddic_fields    TYPE lcl_app=>ty_ddic_fields
                                       iv_parent_node    TYPE tv_nodekey,

      itrative_ddic_srch     IMPORTING it_search         TYPE string_table
                                       it_node_key       TYPE tv_nodekeys
                             EXPORTING ev_exit           TYPE abap_bool
                              CHANGING cs_sval           TYPE sval
                               RAISING lcl_error,

      hnd_ddic_tbar_find       RAISING lcl_error,

      sel_node_post_srch     IMPORTING iv_ok             TYPE sysubrc
                                       iv_srch_line      TYPE i
                                       it_node_key       TYPE tv_nodekeys
                               RAISING lcl_error ,

      rfrsh_ddic_tree,

      scr_8000_output         RAISING  lcl_error,

      scr_9300_output         RAISING  lcl_error,

      hnd_ddic_inp             RAISING lcl_error,

      ddic_get_selfld        EXPORTING et_fld            TYPE rsdsfields_t
                              RAISING lcl_error,

      ddic_ret_selnode RETURNING value(rt_node_key)      TYPE treev_nks
                               RAISING lcl_error,

      get_whr_4_inp          IMPORTING it_fld            TYPE rsdsfields_t
                             EXPORTING et_query          TYPE soli_tab
                              CHANGING rsds_twhere       TYPE rsds_twhere,

      set_txt_at_cursor      IMPORTING it_query          TYPE soli_tab,

      set_txt_at_line        IMPORTING iv_line_start     TYPE i
                                       iv_pos_start      TYPE i
                                       iv_break          TYPE abap_bool OPTIONAL
                                       it_query          TYPE soli_tab,
      set_tab_title,

      load_repo,

      add_repo_local,

      fnd_add_tble           IMPORTING is_query          TYPE soli
                                       iv_pos_start      TYPE i,

      get_fld_frm_ddic_node  IMPORTING iv_node_key       TYPE tv_nodekey
                                       iv_relat_key      TYPE tv_nodekey
                       RETURNING value(rv_text)          TYPE string.

    METHODS:

      hnd_ddic_toolbar_clic   FOR EVENT function_selected OF cl_gui_toolbar
                              IMPORTING fcode,

      hnd_ddic_item_dblclick  FOR EVENT item_double_click OF cl_gui_column_tree
                              IMPORTING node_key,

      hnd_ddic_drag           FOR EVENT on_drag_multiple  OF cl_gui_column_tree
                              IMPORTING node_key_table
                                        drag_drop_object,

      hnd_editor_dblclk       FOR EVENT dblclick          OF cl_gui_abapedit,

      hnd_editor_drop         FOR EVENT on_drop           OF cl_gui_abapedit
                              IMPORTING line
                                        pos
                                        dragdrop_object,

      on_user_command         FOR EVENT added_function    OF cl_salv_events
                              IMPORTING e_salv_function,

      hnd_repo_context_menu   FOR EVENT node_context_menu_request  OF cl_gui_simple_tree
                              IMPORTING menu,
      hnd_repo_context_menu_sel  FOR EVENT node_context_menu_select OF cl_gui_simple_tree
                                IMPORTING fcode,
      hnd_repo_dblclick       FOR EVENT node_double_click OF cl_gui_simple_tree
                              IMPORTING node_key.
    .

ENDCLASS.

CLASS lcl_scr IMPLEMENTATION.
  METHOD init_screen_objects.

  CREATE OBJECT ro_self.
  ENDMETHOD.
  METHOD hndl_vuser_multi.

    DATA:
      lrt_vuser      TYPE trgr_user_name.

    DATA:
      lr_vuser       TYPE trgs_user_name,
      ls_vuser_str   TYPE string,
      ls_restrict    TYPE rsoptions,
      ls_inp         TYPE rstabfield.

    ls_restrict-nb = 'X'.
    ls_restrict-bt = 'X'.
    ls_restrict-cp = 'X'.
    ls_restrict-ge = 'X'.
    ls_restrict-gt = 'X'.
    ls_restrict-le = 'X'.
    ls_restrict-lt = 'X'.
    ls_restrict-nb = 'X'.
    ls_restrict-ne = 'X'.
    ls_restrict-np = 'X'.

    IF w-vis_users IS NOT INITIAL.

      ls_vuser_str = me->s_ztquery-visibility_users.

      lrt_vuser = lcl_app=>str_2_range( ls_vuser_str ).

      lr_vuser-low    = w-vis_users.

      IF lrt_vuser IS NOT INITIAL.

        MODIFY lrt_vuser FROM lr_vuser INDEX 1.

      ELSE.

        APPEND lr_vuser TO lrt_vuser.

      ENDIF.

    ENDIF.

    CLEAR me->s_ztquery-visibility_users.

    ls_inp-tablename = 'USR02'.
    ls_inp-fieldname = 'BNAME'.

    lcl_app=>call_multisel( EXPORTING iv_excl_opt = ls_restrict
                                      iv_input    = ls_inp
                            CHANGING  ct_rettab   = lrt_vuser
                           ).

    IF lrt_vuser IS NOT INITIAL.

      LOOP AT lrt_vuser INTO lr_vuser.
        IF syst-tabix = 1.
          w-vis_users = lr_vuser-low.
        ENDIF.

        me->s_ztquery-visibility_users = |{ me->s_ztquery-visibility_users } { lr_vuser-low }|.
      ENDLOOP.

    ELSE.

      CLEAR w-vis_users.

    ENDIF.

    CONDENSE me->s_ztquery-visibility_users.

  ENDMETHOD.
  METHOD hndl_ugroup_multi.

    DATA:
      lrt_ugroup     TYPE RANGE OF xuclass.

    DATA:
      ls_restrict    TYPE rsoptions,
      ls_inp         TYPE rstabfield,
      ls_ugrp_str    TYPE string,
      lr_ugroup      LIKE LINE OF lrt_ugroup.

    ls_restrict-nb = 'X'.
    ls_restrict-bt = 'X'.
    ls_restrict-cp = 'X'.
    ls_restrict-ge = 'X'.
    ls_restrict-gt = 'X'.
    ls_restrict-le = 'X'.
    ls_restrict-lt = 'X'.
    ls_restrict-nb = 'X'.
    ls_restrict-ne = 'X'.
    ls_restrict-np = 'X'.

    IF w-visibilitygrp IS NOT INITIAL.

      ls_ugrp_str = me->s_ztquery-visibility_group.

      lrt_ugroup = lcl_app=>str_2_range( ls_ugrp_str  ).

      lr_ugroup-low = w-visibilitygrp.

      IF lrt_ugroup IS NOT INITIAL.

        MODIFY lrt_ugroup FROM lr_ugroup INDEX 1.

      ELSE.

        APPEND lr_ugroup TO lrt_ugroup.

      ENDIF.

    ENDIF.

    CLEAR me->s_ztquery-visibility_group.

    ls_inp-tablename = 'USR02'.
    ls_inp-fieldname = 'CLASS'.

    lcl_app=>call_multisel( EXPORTING iv_excl_opt = ls_restrict
                                      iv_input    = ls_inp
                             CHANGING ct_rettab   = lrt_ugroup
                           ).

    IF lrt_ugroup IS NOT INITIAL.

      LOOP AT lrt_ugroup INTO lr_ugroup.
        IF syst-tabix = 1.
          w-visibilitygrp = lr_ugroup-low.
        ENDIF.
        me->s_ztquery-visibility_group = |{ me->s_ztquery-visibility_group } { lr_ugroup-low }|.
      ENDLOOP.

    ELSE.

      CLEAR:
        w-visibilitygrp.

    ENDIF.

    CONDENSE me->s_ztquery-visibility_group.

  ENDMETHOD.
  METHOD hnd_editor_drop.

    DATA:
      lo_drag_object TYPE REF TO lcl_drag_object.

    lo_drag_object ?= dragdrop_object->object.

    IF lo_drag_object IS INITIAL
    OR lo_drag_object->fields IS INITIAL.
      RETURN.
    ENDIF.

    me->set_txt_at_line( iv_line_start = line
                         iv_pos_start  = pos
                         iv_break      = lcl_app=>paste_break
                         it_query      = lo_drag_object->fields
                       ).

  ENDMETHOD.
  METHOD hnd_editor_dblclk.

    DATA:
      lv_line_start  TYPE i,
      lv_pos_start   TYPE i.

    DATA:
      ls_query       TYPE soli.

    DATA:
      lt_query       TYPE soli_tab.

    me->o_textedit->get_selection_pos( IMPORTINg from_line              =  lv_line_start
                                                 from_pos               =  lv_pos_start
                                      EXCEPTIONS OTHERS                 = 1
                                     ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    me->o_textedit->get_text(
      IMPORTING
        table                  =  lt_query
      EXCEPTIONS
        OTHERS                 = 3
        ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE lt_query INTO ls_query INDEX lv_line_start.

    IF syst-subrc <> 0.

      RETURN.

    ENDIF.

    me->fnd_add_tble(
      is_query      = ls_query
      iv_pos_start  = lv_pos_start
      ).

  ENDMETHOD.
  METHOD fnd_add_tble.

    DATA:
      lv_pos         TYPE i VALUE 1,
      lv_pos_end     TYPE i,
      lv_val         TYPE pvarfield .

    DATA:
      lt_string      TYPE string_table.

    DATA:
      ls_string      TYPE string.

    SPLIT is_query AT space INTO TABLE lt_string.

    LOOP AT lt_string INTO ls_string.

      lv_pos_end =  lv_pos + strlen( ls_string ).

      IF  lv_pos_end >= iv_pos_start .

        lv_val = to_upper( ls_string ).

        me->handle_dtoolbar_add( lv_val ).

        EXIT.

      ELSE.

        lv_pos = lv_pos_end + 1.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD hnd_ddic_drag.
    DATA :
      lo_drag_object TYPE REF TO lcl_drag_object.

    DATA:
      ls_node_key    TYPE tv_nodekey,
      ls_node        TYPE treev_node.

    DATA:
      lv_text        TYPE string.

    CREATE OBJECT lo_drag_object.

    LOOP AT node_key_table INTO ls_node_key.

      READ TABLE me->t_node_ddic INTO ls_node
                             WITH KEY node_key = ls_node_key.

      IF sy-subrc NE 0
      OR ls_node-isfolder = abap_true.
        MESSAGE 'Only fields can be drag&drop to editor' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      lv_text = me->get_fld_frm_ddic_node(
                  iv_node_key  = ls_node_key
                  iv_relat_key = ls_node-relatkey
                  ).

      APPEND lv_text TO lo_drag_object->fields.

    ENDLOOP.
    drag_drop_object->object = lo_drag_object.

  ENDMETHOD.
  METHOD get_fld_frm_ddic_node.
    DATA :
      ls_item        TYPE mtreeitm,
      ls_item_parent TYPE mtreeitm.

    DATA:
      lv_table       TYPE string,
      lv_alias       TYPE string.

    READ TABLE me->t_item_ddic INTO ls_item
                           WITH KEY node_key = iv_node_key
                                   item_name = 'col1'.

    READ TABLE me->t_item_ddic INTO ls_item_parent
                           WITH KEY node_key = iv_relat_key
                                   item_name = 'col1'.

    SPLIT ls_item_parent-text AT ' AS ' INTO lv_table lv_alias.
    IF NOT lv_alias IS INITIAL.
      lv_table = lv_alias.
    ENDIF.

    rv_text = ls_item-text.
    CONCATENATE space rv_text space INTO rv_text RESPECTING BLANKS.

  ENDMETHOD.
  METHOD hnd_ddic_item_dblclick.
    DATA :
      ls_node        TYPE treev_node,
      ls_query       TYPE soli,
      ls_item        TYPE mtreeitm,
      ls_item_parent TYPE mtreeitm,
      ls_txt         TYPE string .

    DATA:
      lv_join        TYPE abap_bool.

    DATA:
      lt_query       TYPE soli_tab,
      lt_txt         TYPE soli_tab.

    READ TABLE me->t_node_ddic INTO ls_node
                           WITH KEY node_key = node_key.
    IF sy-subrc <> 0
    OR ls_node-isfolder = abap_true.
      RETURN.
    ENDIF.

    READ TABLE me->t_item_ddic INTO ls_item
                           WITH KEY node_key = node_key
                                    item_name = 'col1'.

    READ TABLE me->t_item_ddic INTO ls_item_parent
                           WITH KEY node_key  = ls_node-relatkey
                                    item_name = 'col1'.

    me->o_textedit->get_text(
      IMPORTING
        table  = lt_query[]
      EXCEPTIONS
        OTHERS = 1
      ).

    DELETE lt_query WHERE line+0(1) = '*'.

    LOOP AT lt_query INTO ls_query.

      CHECK ls_query-line CS 'JOIN'.
      lv_join = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_join = abap_true.
      ls_txt = | { ls_item_parent-text }~{ ls_item-text } |.
    ELSE.
      ls_txt = | { ls_item-text } |.
    ENDIF.

    APPEND ls_txt TO lt_txt.

    me->set_txt_at_cursor(  lt_txt  ).

  ENDMETHOD.

  METHOD constructor.

    SET HANDLER me->hnd_ddic_toolbar_clic FOR w-o_toolbar .
    me->ddic_init( ).

    me->editor_init( ).

    me->query_repo_init( ).
    lcl_app=>push_list( iv_ref = me ).

  ENDMETHOD.
  METHOD get_file_path.

    DATA:
      lv_path       TYPE string,
      lv_file       TYPE string.

    cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      prompt_on_overwrite       = 'X'
    CHANGING
      filename                  =  lv_file
      path                      =  lv_path
      fullpath                  =  rv_filepath
    EXCEPTIONS
      cntl_error                = 1
      error_no_gui              = 2
      not_supported_by_gui      = 3
      invalid_default_file_name = 4
      OTHERS                    = 5
      ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.
  METHOD ret_tab_columns.

    DATA:
      lo_columns    TYPE REF TO cl_salv_columns_table,
      lo_salv       TYPE REF TO cl_salv_table.

    DATA:
      lt_column_ref TYPE salv_t_column_ref.

    DATA:
      ls_column_ref TYPE salv_s_column_ref,
      ls_header     TYPE ty_header.

    REFRESH et_header.

    TRY.

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   =  lo_salv
          CHANGING
            t_table        = ct_data
        ).

      CATCH cx_salv_msg.

        RETURN.

    ENDTRY.

    lo_columns = lo_salv->get_columns( ).

    lt_column_ref = lo_columns->get( ).

    LOOP AT lt_column_ref INTO ls_column_ref.

      ls_header = ls_column_ref-r_column->get_long_text( ).

      APPEND ls_header TO et_header.
    ENDLOOP.

  ENDMETHOD.
  METHOD exec2pc.

    FIELD-SYMBOLS:
      <lt_data>     TYPE STANDARD TABLE.

    DATA:
      lt_content    TYPE STANDARD TABLE OF ty_header.

    DATA:
      lv_fullpath   TYPE string.

    lv_fullpath = me->get_file_path( ).

    IF lv_fullpath IS INITIAL.

      RETURN.

    ENDIF.
    me->execute( ).

    ASSIGN me->o_tab->* TO <lt_data>.

    IF <lt_data> IS NOT ASSIGNED
    OR <lt_data> IS INITIAL.

      RETURN.
    ENDIF.

    me->ret_tab_columns(
      IMPORTING
        et_header = lt_content
      CHANGING
        ct_data   = <lt_data>
    ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  =  lv_fullpath
        write_field_separator     = 'X'
        fieldnames                =  lt_content
      CHANGING
        data_tab                  =  <lt_data>
      EXCEPTIONS
        OTHERS                    = 1
        ).

    IF sy-subrc <> 0.
    ENDIF.

    CLEAR me->o_tab.

  ENDMETHOD.

  METHOD exec2bg.

    DATA:
      lv_jobcount          TYPE btcjobcnt,
      lv_valid_flag        TYPE c           LENGTH 1,
      lv_query             TYPE string.

    DATA :
      ls_print_parameters  TYPE pri_params,
      ls_archi_parameters  TYPE arc_params.

    DATA:
      lt_query             TYPE soli_tab.

    me->o_textedit->get_text(
      IMPORTING
        table  = lt_query[]
      EXCEPTIONS
        OTHERS = 1
        ).

    IF syst-subrc <> 0 .
      RETURN.
    ENDIF.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = 'ZQUERY'
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        expiration             = '2'
        immediately            = ' '
        line_size              = 120
        line_count             = 44
        new_list_id            = 'X'
        no_dialog              = 'X'
        release                = 'X'
        sap_cover_page         = ' '
      IMPORTING
        out_parameters         = ls_print_parameters
        out_archive_parameters = ls_archi_parameters
        valid                  = lv_valid_flag
      EXCEPTIONS
        OTHERS                 = 4.

    IF lv_valid_flag = 'X'
    AND syst-subrc    = 0.

      lv_query = lcl_app=>qtab_to_string( lt_query ).

      SUBMIT zrd_query TO SAP-SPOOL
                       SPOOL   PARAMETERS ls_print_parameters
                       ARCHIVE PARAMETERS ls_archi_parameters
                       WITHOUT SPOOL DYNPRO
                       VIA JOB 'ZQUERY' NUMBER lv_jobcount
                       WITH p_query = lv_query
                       WITH p_upto  = lcl_app=>default_rows
                       AND RETURN.

    ENDIF.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount  = lv_jobcount
        jobname   = 'ZQUERY'
        strtimmed = 'X'
      EXCEPTIONS
        OTHERS    = 9.
    IF sy-subrc = 0.
      MESSAGE s000(db) WITH 'Query Submitted with Background Job Name: ZQUERY' .
    ENDIF.

  ENDMETHOD.
  METHOD check_query.
    DATA:
      lt_query          TYPE soli_tab,
      lt_prog           TYPE string_table.

    DATA:
      lv_select         TYPE string,
      lv_from           TYPE string,
      lv_where          TYPE string,
      lv_msg            TYPE char255,
      lv_string         TYPE string,
      lv_line           TYPE i,
      lv_word           TYPE char30,
      lv_upto           TYPE i,
      lv_order          TYPE string,
      lv_group          TYPE string.

    CLEAR lv_line.

    me->o_textedit->get_text(
      IMPORTING
        table  = lt_query[]
      EXCEPTIONS
        OTHERS = 1
      ).

    lcl_app=>ret_query_comp(
      EXPORTING
        iv_query  = lcl_app=>qtab_to_string( lt_query )
      IMPORTING
        ev_from   = lv_from
        ev_select = lv_select
        ev_where  = lv_where
        ev_upto   = lv_upto
        ev_order  = lv_order
        ev_group  = lv_group
      ).

    IF lv_upto IS INITIAL.
      lv_upto = lcl_app=>default_rows.
    ENDIF.

    APPEND 'REPORT ZRD_QUERY.' TO lt_prog.

    APPEND '    FIELD-SYMBOLS:      <lfs_data>         TYPE STANDARD TABLE.' TO lt_prog.

    APPEND 'START-OF-SELECTION.' TO lt_prog.

    REFRESH lt_query.

    lv_string = |SELECT { lv_select }|.
    APPEND lv_string TO lt_prog.

    lv_string = |FROM { lv_from }|.
    APPEND lv_string TO lt_prog.

    IF lv_upto IS NOT INITIAL.
      lv_string = |UP TO { lv_upto } ROWS|.

      APPEND lv_string TO lt_prog.
    ENDIF.

    lv_string = |INTO TABLE <lfs_Data>|.
    APPEND lv_string TO lt_prog.

    IF lv_where IS NOT INITIAL.
      lv_string = |WHERE { lv_where }|.
      APPEND lv_string TO lt_prog.

    ENDIF.

    IF lv_order IS NOT INITIAL.
      lv_string = |ORDER BY { lv_order }|.
      APPEND lv_string TO lt_prog.
    ENDIF.

    IF lv_group IS NOT INITIAL.
      lv_string = |GROUP BY { lv_group }|.
      APPEND lv_string TO lt_prog.
    ENDIF.

    APPEND '.' TO lt_prog.

    SYNTAX-CHECK FOR lt_prog
    MESSAGE lv_msg LINE lv_line WORD lv_word.

    IF lv_msg IS NOT INITIAL.

      MESSAGE lv_msg TYPE 'E'.
      RETURN.

    ENDIF.

  ENDMETHOD.
  METHOD report_estimated_qrows.
    DATA:
      lt_query   TYPE soli_tab.

    DATA:
      lv_select      TYPE string,
      lv_from        TYPE string,
      lv_where       TYPE string,
      lv_query       TYPE string,
      lv_est_row     TYPE char32,
      lv_upto        TYPE i.

    me->o_textedit->get_text( IMPORTING table  = lt_query[]
                             EXCEPTIONS OTHERS = 1
      ).

    lcl_app=>ret_query_comp( EXPORTING iv_query  = lcl_app=>qtab_to_string( lt_query )
                             IMPORTING ev_from   = lv_from
                                       ev_select = lv_select
                                       ev_where  = lv_where
                                       ev_upto   = lv_upto
                            ).


    lv_query = | SELECT { lv_select } FROM { lv_from } { lcl_app=>ret_ora_whr_str( iv_whr = lv_where iv_upto = lv_upto ) } |.

    lv_query = replace( val   = lv_query
                        sub   = '~'
                        occ   = 0
                        with  = '.'
               ).
    lv_est_row = lcl_app=>get_est_rows( lv_query ).

    IF lv_est_row IS NOT INITIAL.

      MESSAGE s000(db) WITH 'Query Is Correct ,Estimated result rows are:' lv_est_row.
    ENDIF.
  ENDMETHOD.
  METHOD exit_app.
    DATA :
      lv_ans TYPE char1.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question = 'Are You Sure to Exit?'
        text_button_1 = 'Yes'(001)
        text_button_2 = 'No'(002)
      IMPORTING
        answer        = lv_ans
      EXCEPTIONS
        OTHERS        = 2.
    IF  sy-subrc = 0
    AND lv_ans = '1'.

      LEAVE TO SCREEN 0.

    ENDIF.

  ENDMETHOD.
  METHOD handle_qsave.
    DATA:
      lv_ans    TYPE char1,
      lv_string TYPE string.

    DATA:
      lr_ugroup TYPE usselgroup,
      lr_vuser  TYPE trgs_user_name.

    DATA:
      lrt_ugroup TYPE RANGE OF xuclass,
      lrt_vuser  TYPE trgr_user_name.

    CLEAR:
      w-vis_users,
      w-qname,
      w-qvisibility,
      me->v_qovrwrt.

    IF w-visibilitygrp IS INITIAL.

      SELECT SINGLE class
             INTO w-visibilitygrp
             FROM usr02
             WHERE bname = sy-uname.

    ENDIF.

    IF  me->s_ztquery-queryid IS NOT INITIAL
    AND me->s_ztquery-owner  = syst-uname.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
        text_question               = 'OVER Write Query'
        text_button_1               = 'Yes'(001)
        text_button_2               = 'No'(002)
      IMPORTING
        answer                      = lv_ans
      EXCEPTIONS
        text_not_found              = 1
        OTHERS                      = 2
        .

      IF sy-subrc <> 0
      OR lv_ans <> '1'
     AND lv_ans <> '2'.

        RETURN.

      ENDIF.


      IF lv_ans = '1'.

        me->v_qovrwrt = abap_true.

        CLEAR:
          w-visibilitygrp.

        lv_string = me->s_ztquery-visibility_group.

        lrt_ugroup[] = lcl_app=>str_2_range( lv_string ).

        READ TABLE lrt_ugroup INTO lr_ugroup INDEX 1.

        IF lr_ugroup IS NOT INITIAL.

          w-visibilitygrp = lr_ugroup-low.

          WRITE icon_display_more AS ICON TO w-ugroup_sel_more.

        ELSE.

          WRITE icon_enter_more AS ICON TO w-ugroup_sel_more.

        ENDIF.

        lv_string = me->s_ztquery-visibility_users.

        lrt_vuser[] = lcl_app=>str_2_range( lv_string ) .

        READ TABLE lrt_vuser INTO lr_vuser INDEX 1.

        IF lr_vuser IS NOT INITIAL.

          w-vis_users = lr_vuser-low.

          WRITE icon_display_more AS ICON TO w-vuser_sel_more.

        ELSE.

          WRITE icon_enter_more AS ICON TO w-vuser_sel_more.

        ENDIF.

        w-qname       = me->s_ztquery-text.
        w-qvisibility = me->s_ztquery-visibility.
      ENDIF.

    ENDIF.

    CALL SCREEN 9300 STARTING AT 10 5
    ENDING AT 60 7.

  ENDMETHOD.
  METHOD handle_ucomm.

    DATA:
      lv_index TYPE sytabix.

    DATA:
      lo_err   TYPE REF TO lcl_error.

    er_app_ref = me.

    TRY.

        CASE w_okcode.
          WHEN 'EXP'.

            lcl_app=>col_exp_repo( ).

          WHEN 'SAVE'.

            me->handle_qsave( ).

          WHEN 'MSUGROUP'.

            IF  w-qvisibility <> '1'.

              MESSAGE s000(db) WITH 'Visibility User Group needed for Shared queries' DISPLAY LIKE 'E'.
            ELSE.

              me->hndl_ugroup_multi( ).
            ENDIF.

          WHEN 'MSVUSER'.

            IF  w-qvisibility <> '1'.

              MESSAGE s000(db) WITH 'Visibility User Group needed for Shared queries' DISPLAY LIKE 'E'.
            ELSE.

              me->hndl_vuser_multi( ).
            ENDIF.

          WHEN 'CHECK'.

            me->check_query( ).

            MESSAGE s000(db) WITH 'Query is syntacticly correct'.

            me->report_estimated_qrows( ).

          WHEN 'EXIT'.

            me->exit_app( ).

          WHEN 'EXECUTE'.

            me->check_query( ).

            me->execute( ).

            me->set_tab_title( ).

            me->add_repo_local( ).

          WHEN 'NEW'.

            lcl_app=>chk_limit( ).

            er_app_ref = me->ret_tab_obj( ).

          WHEN 'DEL_TAB'.

            lv_index = w_tabstrip-activetab+3 - 1.

            er_app_ref = me->ret_tab_obj( lv_index  ).

            lcl_app=>pop_list( lv_index + 1 ).

            w_tabstrip-activetab = |TAB{ lv_index }|.

          WHEN 'QWIZ'.

            CALL SCREEN 9100 STARTING AT 5 5.

          WHEN 'CLOSE_QWIZ'.

            LEAVE TO SCREEN 0.

          WHEN 'OK_QWIZ'.

            me->gen_query( ).

            LEAVE TO SCREEN 0.

          WHEN 'GET_FLD'.

            lcl_app=>chk_tabnm_empt_vald( w-sel_tabname ).

            lcl_app=>handle_sqlfld( w-sel_tabname ).

          WHEN 'REFRESH'.

            lcl_app=>rfrsh_qwiz_data( ).

          WHEN 'EXEC2PC'.

            me->check_query( ).

            me->exec2pc( ).

            me->set_tab_title( ).


          WHEN 'SETTINGS'.

            lcl_app=>handle_settings( iv_ucomm ).

          WHEN 'CLOSE_SET'
            OR 'OK_SET'
            OR 'CLOSE'.

            LEAVE TO SCREEN 0.

          WHEN 'OK_SAVE'.

            IF  w-qvisibility = '1'
            AND w-visibilitygrp IS INITIAL
            AND w-vis_users IS INITIAL.

              MESSAGE s000(db) WITH 'Visibility User/User Group needed for Shared queries' DISPLAY LIKE 'E'.

            ELSEIF w-qvisibility <> '1'
            AND ( w-visibilitygrp IS NOT INITIAL
                OR me->s_ztquery-visibility_group IS NOT INITIAL
                OR w-vis_users IS NOT INITIAL
                OR me->s_ztquery-visibility_users IS NOT INITIAL ) .

              MESSAGE s000(db) WITH 'Visibility User/User Group needed only for Shared queries' DISPLAY LIKE 'E'.

            ELSE.

              me->init_save_query( ).

              LEAVE TO SCREEN 0.

            ENDIF.

          WHEN 'EXEC2BG'.

            me->check_query( ).

            me->exec2bg( ).

            me->set_tab_title( ).

          WHEN OTHERS.

            IF w_okcode(3) = 'TAB'
            AND w_tabstrip-activetab NE iv_ucomm.

              lv_index = iv_ucomm+3.

              er_app_ref = me->ret_tab_obj( lv_index ).

              w_tabstrip-activetab = iv_ucomm.

            ENDIF.

        ENDCASE.

      CATCH lcl_error INTO lo_err.

        MESSAGE s000(db) WITH lo_err->v_text DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.
  METHOD init_save_query.
    DATA:
      lt_query TYPE soli_tab.

    me->o_textedit->get_text( IMPORTING table      =  lt_query
                             EXCEPTIONS OTHERS     = 3
                            ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcl_error
      EXPORTING
        iv_text = 'Error fetching query from editor.'.
    ENDIF.

    IF me->v_qovrwrt = abap_false.
      CLEAR:
        me->s_ztquery-queryid.
    ENDIF.

    lcl_app=>save_query( EXPORTING iv_query   = lcl_app=>qtab_to_string( it_query = lt_query
                                                                         iv_com   = abap_true
                                                                         iv_sep   = cl_abap_char_utilities=>cr_lf
                                                                        )
                          CHANGING cs_ztquery = me->s_ztquery
                       ).

    CLEAR:
       me->v_qovrwrt.

    me->load_repo( ).

  ENDMETHOD.
  METHOD gen_query.

    DATA:
      lt_query TYPE string_table.

    lt_query = lcl_app=>ret_query( ).

    IF lt_query IS NOT INITIAL.

      me->o_textedit->set_text(
        EXPORTING
          table           = lt_query
        EXCEPTIONS
          error_dp        = 1
          error_dp_create = 2
          error_code_page = 3
          OTHERS          = 4
          ).

      IF sy-subrc <> 0.
      ENDIF.

    ENDIF.

    me->set_focus_on_editor( ).

  ENDMETHOD.

  METHOD leave_current_tab.
    DATA:
      lv_index TYPE sytabix.

    me->handle_visibility( iv_ref = me
                           iv_vis = abap_false
                          ).

    CALL METHOD cl_gui_cfw=>flush.

    lv_index = w_tabstrip-activetab+3.

    lcl_app=>modif_list( iv_index = lv_index
                          iv_ref  = me
                       ).

  ENDMETHOD.
  METHOD handle_visibility.

    iv_ref->o_textedit->set_visible( EXPORTING visible = iv_vis ).

    IF iv_ref->o_dock IS NOT INITIAL.
      iv_ref->o_dock->set_visible( EXPORTING visible = iv_vis ).
    ENDIF.

    IF iv_ref->o_tree_ddic IS NOT INITIAL.
      iv_ref->o_tree_ddic->set_visible( EXPORTING visible           =  iv_vis
                                       EXCEPTIONS OTHERS            = 0
                                      ).

    ENDIF.

    IF iv_ref->o_tree_qrep IS NOT INITIAL.
      iv_ref->o_tree_qrep->set_visible( EXPORTING visible           =  iv_vis
                                       EXCEPTIONS OTHERS            = 0
                                      ).

    ENDIF.

    SET HANDLER iv_ref->hnd_ddic_toolbar_clic FOR w-o_toolbar ACTIVATION iv_vis.

  ENDMETHOD.
  METHOD ret_tab_obj.

    me->leave_current_tab( ).

    IF iv_index IS NOT INITIAL.

      app_ref = lcl_app=>read_list( iv_index ).

      IF app_ref IS NOT INITIAL.

        me->handle_visibility( iv_ref = app_ref
                               iv_vis = abap_true
                             ).

      ENDIF.

    ELSE.

      w_tabstrip-activetab = |TAB{ lines( lcl_app=>t_list ) + 1 } |.

      CREATE OBJECT app_ref.
      SET HANDLER app_ref->hnd_ddic_toolbar_clic FOR w-o_toolbar .
    ENDIF.

    w-o_toolbar->set_button_visible( EXPORTING visible = app_ref->v_editable
                                               fcode   = 'SET_FLD_IP'
                                    EXCEPTIONS OTHERS  = 0
    ).

  ENDMETHOD.
  METHOD dock_init.

    CREATE OBJECT o_dock
      EXPORTING
        side                        = cl_gui_docking_container=>dock_at_bottom
        style                       = cl_gui_control=>ws_child + cl_gui_control=>ws_thickframe + cl_gui_control=>ws_visible
        ratio                       = 40
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.
  METHOD editor_init.
    DATA:
      lo_completer TYPE REF TO cl_abap_parser,
      lo_dragdrop  TYPE REF TO cl_dragdrop.

    CHECK w-o_qcontainer IS NOT INITIAL.

    CREATE OBJECT me->o_textedit
      EXPORTING
        parent           = w-o_qcontainer
        max_number_chars = 72.

    IF me->o_textedit IS  INITIAL.

      RETURN.
    ENDIF.
    CALL METHOD me->o_textedit->('INIT_COMPLETER').

    CALL METHOD me->o_textedit->('GET_COMPLETER')
      RECEIVING
        m_parser = lo_completer.

    SET HANDLER lo_completer->handle_completion_request
            FOR me->o_textedit.

    SET HANDLER lo_completer->handle_insertion_request
            FOR me->o_textedit.

    SET HANDLER lo_completer->handle_quickinfo_request
            FOR me->o_textedit.

    SET HANDLER me->hnd_editor_dblclk
            FOR me->o_textedit.

    SET HANDLER me->hnd_editor_drop
           FOR me->o_textedit.

    me->o_textedit->register_event_completion( ).
    me->o_textedit->register_event_quick_info( ).
    me->o_textedit->register_event_insert_pattern( ).
    me->o_textedit->register_event_f1( ).

    me->o_textedit->register_event_dblclick(
      EXPORTING
        navigate_on_dblclick     =   0
      EXCEPTIONS
        error_regist_event       = 1
        error_unregist_event     = 2
        cntl_error               = 3
        event_already_registered = 4
        event_not_registered     = 5
        OTHERS                   = 6
        ).

    IF sy-subrc <> 0.
    ENDIF.

    CREATE OBJECT lo_dragdrop.

    lo_dragdrop->add(
      flavor     = 'EDIT_INSERT'
      dragsrc    = space
      droptarget = abap_true
      effect     = cl_dragdrop=>copy
      ).

    me->o_textedit->set_dragdrop(
      dragdrop = lo_dragdrop
      ).

    me->o_textedit->set_text(
      EXPORTING
        table           = lcl_app=>get_q_template( )
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2
        error_code_page = 3
        OTHERS          = 4
      ).

    IF sy-subrc <> 0.
    ENDIF.

    me->set_focus_on_editor( ).

  ENDMETHOD.
  METHOD set_focus_on_editor.

    cl_gui_control=>set_focus(
      control = me->o_textedit
      ).
  ENDMETHOD.

  METHOD set_tab_visible.

    DATA :
      lv_num TYPE i,
      lv_max TYPE i.

    lv_max = lines( lcl_app=>t_list ).

    LOOP AT SCREEN.

      CHECK screen-name(15) = 'W-TAB_TITLE-TAB'.

      lv_num = screen-name+15.

      IF lv_num > lv_max.
        screen-invisible = 1.
      ELSE.
        screen-invisible = 0.
      ENDIF.

      MODIFY SCREEN.

    ENDLOOP.

    me->set_focus_on_editor( ).

  ENDMETHOD.
  METHOD set_tab_title.
    DATA :
      lv_name(40) TYPE c,
      lt_query    TYPE soli_tab.

    FIELD-SYMBOLS:
      <fs_title>  TYPE any.

    IF w_tabstrip-activetab IS INITIAL.
      lv_name = 'W-TAB_TITLE-TAB1'.
    ELSE.
      CONCATENATE 'W-TAB_TITLE-TAB' w_tabstrip-activetab+3 INTO lv_name.
    ENDIF.

    ASSIGN (lv_name) TO <fs_title>.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CALL METHOD me->o_textedit->get_text
      IMPORTING
        table  = lt_query[]
      EXCEPTIONS
        OTHERS = 1.

    IF syst-subrc IS INITIAL.

      lcl_app=>ret_query_comp(
      EXPORTING
        iv_query  = lcl_app=>qtab_to_string( lt_query )
      IMPORTING
        ev_from   = <fs_title>

        ).

    ENDIF.
  ENDMETHOD.
  METHOD alv_init.

    DATA:
      lo_func   TYPE REF TO cl_salv_functions,
      lo_events TYPE REF TO cl_salv_events_table.

    TRY.

        cl_salv_table=>factory(
          EXPORTING
            r_container = me->o_dock
          IMPORTING
            r_salv_table = o_alv
          CHANGING
            t_table      = it_data
          ).

        lo_func =  me->o_alv->get_functions( ).

        lo_func->add_function(
          name     = 'CLOSE_GRID'
          icon     = |{ ICON_CLOSE }|
          text     = 'CLOSE'
          tooltip  = 'Close'
          position =  if_salv_c_function_position=>right_of_salv_functions
          ).

        lo_func->set_all( ).

        lo_func->set_function(
          name    =  'CLOSE_GRID'
          boolean =  c-x
          ).

        lo_events = me->o_alv->get_event( ).

        SET HANDLER me->on_user_command FOR lo_events.

        me->alv_set_columns( o_alv ).

        o_alv->display( ).

      CATCH cx_salv_error.

        RETURN.

    ENDTRY.

  ENDMETHOD.
  METHOD alv_set_columns.

    DATA:
      lo_columns    TYPE REF TO cl_salv_columns_table,
      lt_column_ref TYPE salv_t_column_ref,
      ls_column_ref TYPE salv_s_column_ref.      .

    DATA:
      lv_scrtxt_l     TYPE scrtext_l,
      lv_scrtxt_m     TYPE scrtext_m,
      lv_scrtxt_s     TYPE scrtext_s,
      lv_editmask     TYPE lvc_edtmsk,
      lv_internaltype TYPE inttype.

    lo_columns = io_alv->get_columns( ).

    lt_column_ref = lo_columns->get( ).

    LOOP AT lt_column_ref INTO ls_column_ref.

      IF lcl_app=>conversion = abap_false.

        lv_editmask = ls_column_ref-r_column->get_edit_mask( ).
        IF  lv_editmask(2) =  '=='.
          ls_column_ref-r_column->set_edit_mask( space ).
        ENDIF.

        lv_internaltype = ls_column_ref-r_column->get_ddic_inttype( ).

        IF lv_internaltype = 'C'
        OR lv_internaltype = 'N'.
          ls_column_ref-r_column->set_leading_zero( ).
        ENDIF.

      ENDIF.
      IF lcl_app=>techname = abap_true.
        lv_scrtxt_s = lv_scrtxt_m = lv_scrtxt_l = ls_column_ref-columnname.
        ls_column_ref-r_column->set_long_text( lv_scrtxt_l ).
        ls_column_ref-r_column->set_medium_text( lv_scrtxt_m ).
        ls_column_ref-r_column->set_short_text( lv_scrtxt_s ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD alv_disp.

    FIELD-SYMBOLS:
      <lfs_data> TYPE STANDARD TABLE.

    ASSIGN me->o_tab->* TO <lfs_data>.

    IF <lfs_data> IS NOT ASSIGNED
    OR <lfs_data> IS INITIAL.

      RETURN.
    ENDIF.

    IF me->o_dock IS INITIAL.

      me->dock_init( ).

    ENDIF.

    me->o_dock->set_visible(  EXPORTING visible           = c-x
                             EXCEPTIONS cntl_error        = 1
                                        cntl_system_error = 2
                                        OTHERS            = 3
                           ).

    IF sy-subrc <> 0.
    ENDIF.

    IF me->o_alv IS INITIAL.

      me->alv_init( CHANGING  it_data =  <lfs_data> ).

    ELSE.

      TRY.
          me->o_alv->set_data( CHANGING t_table =  <lfs_data> ).
          me->alv_set_columns( me->o_alv ).
          me->o_alv->refresh( refresh_mode = if_salv_c_refresh=>full  ).
        CATCH cx_root.

      ENDTRY.


    ENDIF.

  ENDMETHOD.

  METHOD scr_8000_output.

    DATA:
      lt_buttons TYPE STANDARD TABLE OF sy-ucomm.

    DATA:
      ls_buttons TYPE sy-ucomm.

    IF me->v_editable = abap_false.

      ls_buttons = 'QWIZ'.
      APPEND ls_buttons TO lt_buttons.

    ENDIF.

    SET PF-STATUS 'PF_9000' EXCLUDING lt_buttons.

    me->set_tab_visible( ).

    me->alv_disp( ).

  ENDMETHOD.
  METHOD scr_9300_output.

    DATA:
      lv_string TYPE string.

    lv_string = me->s_ztquery-visibility_group.

    IF lines( lcl_app=>str_2_range( lv_string ) ) > 1.
      WRITE icon_display_more AS ICON TO w-ugroup_sel_more.
    ELSE.
      WRITE icon_enter_more AS ICON TO w-ugroup_sel_more.
    ENDIF.

    lv_string = me->s_ztquery-visibility_users.

    IF lines( lcl_app=>str_2_range( lv_string ) ) > 1.
      WRITE icon_display_more AS ICON TO w-vuser_sel_more.
    ELSE.
      WRITE icon_enter_more AS ICON TO w-vuser_sel_more.
    ENDIF.

    lcl_app=>set_visdropdown( ).

  ENDMETHOD.

  METHOD handle_output.

    DATA:
      lo_err   TYPE REF TO lcl_error.

    TRY.

        CASE iv_dynnr.

          WHEN '8000'.

            IF lcl_app=>default_rows IS NOT INITIAL.
              w-max_row-text = |Max No. of Rows: { lcl_app=>default_rows }|.
            ELSE.
              CLEAR w-max_row.
            ENDIF.

            me->scr_8000_output( ).

          WHEN '9100'.

            lcl_app=>qwiz_output( ).

          WHEN '9200'.

            lcl_app=>settings_output( ).

          WHEN '9300'.

            me->scr_9300_output( ).

        ENDCASE.
      CATCH lcl_error INTO lo_err.

        MESSAGE s000(db) WITH lo_err->v_text DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.
  METHOD on_user_command.

    FIELD-SYMBOLS:
      <lfs_data> TYPE STANDARD TABLE.

    CASE e_salv_function.

      WHEN 'CLOSE_GRID'.

        me->o_dock->set_visible(
          EXPORTING
            visible           =  space
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2
            OTHERS            = 3
          ).

        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        ASSIGN o_tab->* TO <lfs_data>.

        IF <lfs_data> IS ASSIGNED.
          REFRESH <lfs_data>.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD execute.

    DATA:
          lt_query   TYPE soli_tab.

    CALL METHOD o_textedit->get_text
      IMPORTING
        table  = lt_query[]
      EXCEPTIONS
        OTHERS = 1.

    o_tab = lcl_app=>exec_query(
              it_query = lt_query
              iv_upto  = lcl_app=>default_rows
              ).

  ENDMETHOD.
  METHOD query_repo_init.

    DATA: lt_event TYPE cntl_simple_events,
          ls_event TYPE cntl_simple_event.

    CREATE OBJECT me->o_tree_qrep
      EXPORTING
        parent              = w-o_container_qrep
        node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single
      EXCEPTIONS
        lifetime_error      = 1
        cntl_system_error   = 2
        create_error        = 3
        failed              = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    ls_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_event.

    ls_event-eventid = cl_gui_simple_tree=>eventid_node_context_menu_req.
    APPEND ls_event TO lt_event.

    CALL METHOD me->o_tree_qrep->set_registered_events
      EXPORTING
        events                    = lt_event
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.
    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    SET HANDLER me->hnd_repo_dblclick
        FOR me->o_tree_qrep.
    SET HANDLER me->hnd_repo_context_menu
        FOR me->o_tree_qrep.
    SET HANDLER me->hnd_repo_context_menu_sel
        FOR me->o_tree_qrep.

    me->load_repo( ).
  ENDMETHOD.
  METHOD hnd_repo_dblclick.

    DATA:
      lt_string  TYPE string_table.

    DATA:
      ls_ztquery    TYPE ztquery,
      ls_qrep_node  TYPE ty_qrep_node,
      ls_local_vers TYPE ty_local_vers.

    DATA:
      lv_string     TYPE string,

      lv_readmode   TYPE i  VALUE 0.

    READ TABLE me->t_node_qrep INTO ls_qrep_node
                               WITH KEY node_key = node_key.

    IF sy-subrc = 0
    AND ls_qrep_node-relatkey IS NOT INITIAL.

      IF ls_qrep_node-relatkey = 'LOCAL'.

        READ TABLE me->t_local_vers INTO ls_local_vers WITH KEY node_key = ls_qrep_node-node_key.

        IF syst-subrc IS INITIAL.

          lv_string = ls_local_vers-query.
          SPLIT lv_string
             AT cl_abap_char_utilities=>cr_lf
           INTO TABLE lt_string.

        ENDIF.
      ELSE.
        SELECT SINGLE *
               FROM ztquery
               INTO ls_ztquery
              WHERE queryid = ls_qrep_node-queryid.
        IF syst-subrc <> 0 .
          RETURN.
        ENDIF.

        SPLIT ls_ztquery-query
           AT cl_abap_char_utilities=>cr_lf
           INTO TABLE lt_string.

        me->s_ztquery = ls_ztquery.

      ENDIF.

      me->o_textedit->set_text( EXPORTING table  = lt_string
                               EXCEPTIONS OTHERS = 0
                              ).

      IF ls_qrep_node-edit = abap_true.
        lv_readmode = 0.
        me->v_editable = abap_true.
      ELSE.
        lv_readmode = 1.
        me->v_editable = abap_false.
      ENDIF.

      me->o_textedit->set_readonly_mode( EXPORTING readonly_mode          = lv_readmode
                                        EXCEPTIONS OTHERS                 = 0
                                       ).

      w-o_toolbar->set_button_visible( EXPORTING visible = me->v_editable
                                                 fcode   = 'SET_FLD_IP'
                                      EXCEPTIONS OTHERS  = 1
                                      ).

      me->set_focus_on_editor( ).

    ENDIF.
  ENDMETHOD.
  METHOD hnd_repo_context_menu_sel.
    DATA:
      ls_node_key TYPE tv_nodekey.

    CASE fcode.
      WHEN 'DELETE_QUERY'.
        me->o_tree_qrep->get_selected_node( IMPORTING node_key = ls_node_key
                                          ).

      WHEN 'DELETE_LOCAL'.
    ENDCASE.
  ENDMETHOD.
  METHOD hnd_repo_context_menu.
    DATA:
      ls_node_key TYPE tv_nodekey,
      ls_node_qrep TYPE ty_qrep_node.

    CALL METHOD me->o_tree_qrep->get_selected_node
      IMPORTING
        node_key = ls_node_key.
    IF ls_node_key = 'LOCAL'.

      READ TABLE me->t_node_qrep TRANSPORTING NO FIELDS
                                 WITH KEY relatkey = 'LOCAL'.
      IF sy-subrc = 0.

        CALL METHOD menu->add_function
          EXPORTING
            text  = 'Delete All'(m36)
            icon  = ICON_INCOMPLETE
            fcode = 'DELETE_LOCAL'.

      ENDIF.

      RETURN.

    ENDIF.

    READ TABLE me->t_node_qrep INTO ls_node_qrep
               WITH KEY node_key = ls_node_key.
    IF sy-subrc <> 0
    OR ls_node_qrep-edit = space.
      RETURN.
    ENDIF.

    CALL METHOD menu->add_function
      EXPORTING
        text  = 'Delete'(m01)
        icon  = ICON_INCOMPLETE
        fcode = 'DELETE_QUERY'.
  ENDMETHOD.
  METHOD load_repo.
    DATA:
      ls_node_qrep TYPE ty_qrep_node.
    DATA:
      ls_local_vers TYPE ty_local_vers.
    DATA:
      lt_ztquery TYPE STANDARD TABLE OF ztquery,
      ls_ztquery TYPE ztquery.
    DATA:
      lv_xuclass TYPE xuclass.

    SELECT *
           FROM ztquery
           INTO TABLE lt_ztquery.

    IF syst-subrc IS NOT INITIAL.
    ENDIF.

    SELECT SINGLE class
           FROM usr02
           INTO lv_xuclass
           WHERE bname = syst-uname.

    REFRESH t_node_qrep.

    CALL METHOD me->o_tree_qrep->delete_all_nodes.

    CLEAR ls_node_qrep.
    ls_node_qrep-node_key = 'ME'.
    ls_node_qrep-isfolder = abap_true.
    ls_node_qrep-text = 'My Private queries'.
    ls_node_qrep-n_image = ICON_SEGMENTED_DATA_INA.
    ls_node_qrep-exp_image = ICON_SEGMENTED_DATA_ACT.
    APPEND ls_node_qrep TO t_node_qrep.

    CLEAR ls_node_qrep.
    ls_node_qrep-node_key = 'ME_SHARED'.
    ls_node_qrep-isfolder = abap_true.
    ls_node_qrep-text = 'My Shared queries'.
    ls_node_qrep-n_image = ICON_SEGMENTED_DATA_INA.
    ls_node_qrep-exp_image = ICON_SEGMENTED_DATA_ACT.
    APPEND ls_node_qrep TO t_node_qrep.

    CLEAR ls_node_qrep.
    ls_node_qrep-node_key = 'SHARED_ME'.
    ls_node_qrep-isfolder = abap_true.
    ls_node_qrep-text = 'Shared to Me Queries'.
    ls_node_qrep-n_image = ICON_SEGMENTED_DATA_INA.
    ls_node_qrep-exp_image = ICON_SEGMENTED_DATA_ACT.
    APPEND ls_node_qrep TO t_node_qrep.

    CLEAR ls_node_qrep.
    ls_node_qrep-node_key = 'LOCAL'.
    ls_node_qrep-isfolder = abap_true.
    ls_node_qrep-text = 'Unsaved Query Versions'.
    ls_node_qrep-n_image = ICON_SEGMENTED_DATA_INA.
    ls_node_qrep-exp_image = ICON_SEGMENTED_DATA_ACT.
    APPEND ls_node_qrep TO t_node_qrep.

    LOOP AT me->t_local_vers INTO ls_local_vers.
      CLEAR ls_node_qrep.
      ls_node_qrep-node_key = ls_local_vers-node_key.
      ls_node_qrep-relatkey = 'LOCAL'.
      ls_node_qrep-relatship = cl_gui_simple_tree=>relat_last_child.
      ls_node_qrep-n_image = ls_node_qrep-exp_image = ICON_SUBCLAIM_PERSONAL_INJURY.
      ls_node_qrep-text = ls_local_vers-txt.
      ls_node_qrep-edit = abap_true.
      APPEND ls_node_qrep TO t_node_qrep.
    ENDLOOP.

    LOOP AT lt_ztquery INTO ls_ztquery.
      me->v_qrep_num = me->v_qrep_num + 1.
      CLEAR ls_node_qrep.

      ls_node_qrep-node_key = me->v_qrep_num.
      ls_node_qrep-relatship = cl_gui_simple_tree=>relat_last_child.
      ls_node_qrep-isfolder = abap_false.
      ls_node_qrep-text = ls_ztquery-text.
      ls_node_qrep-queryid = ls_ztquery-queryid.

      IF ls_ztquery-owner = syst-uname.
        ls_node_qrep-edit = abap_true.
        IF ls_ztquery-visibility = '0'.
          ls_node_qrep-relatkey = 'ME'.
          ls_node_qrep-n_image = ls_node_qrep-exp_image = ICON_ADMINISTRATIVE_DATA.
        ELSE.
          ls_node_qrep-relatkey = 'ME_SHARED'.
          ls_node_qrep-n_image = ls_node_qrep-exp_image = ICON_PERSONAL_SETTINGS.

        ENDIF.

        ls_node_qrep-edit = abap_true.
        APPEND ls_node_qrep TO t_node_qrep.

      ELSE.
        IF  ls_ztquery-visibility = '1'
        AND ls_ztquery-visibility_group CS lv_xuclass
        OR ls_ztquery-visibility_users CS syst-uname .
          ls_node_qrep-relatkey = 'SHARED_ME'.
          ls_node_qrep-n_image = ls_node_qrep-exp_image = ICON_PERSONAL_SETTINGS.
          APPEND ls_node_qrep TO t_node_qrep.
        ENDIF.
      ENDIF.

    ENDLOOP.

    me->o_tree_qrep->add_nodes(
      EXPORTING
        table_structure_name           = 'MTREESNODE'
        node_table                     = t_node_qrep
      EXCEPTIONS
        OTHERS                         = 5
      ).

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

  ENDMETHOD.
  METHOD add_repo_local.

    DATA:
      ls_node_qrep TYPE ty_qrep_node,
      lt_node_qrep TYPE tty_qrep_node.
    DATA:
          ls_local_vers TYPE ty_local_vers.
    DATA:
          lt_query TYPE soli_tab.

    me->o_textedit->get_text( IMPORTING table                  = lt_query
                             EXCEPTIONS OTHERS                 = 1
      ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ls_local_vers-node_key = lines( me->t_local_vers ) + 1.
    ls_local_vers-txt      = |Saved at { syst-datum } { syst-uzeit }|.
    ls_local_vers-query    = lcl_app=>qtab_to_string( it_query =  lt_query iv_sep = cl_abap_char_utilities=>cr_lf ).

    APPEND ls_local_vers TO me->t_local_vers.

    ls_node_qrep-node_key  = ls_local_vers-node_key.
    ls_node_qrep-relatkey  = 'LOCAL'.
    ls_node_qrep-relatship = cl_gui_simple_tree=>relat_last_child.
    ls_node_qrep-n_image   = ls_node_qrep-exp_image = ICON_SUBCLAIM_PERSONAL_INJURY.
    ls_node_qrep-text      = ls_local_vers-txt.
    ls_node_qrep-edit      = abap_true.

    APPEND ls_node_qrep TO lt_node_qrep.

    APPEND ls_node_qrep TO me->t_node_qrep.

    me->load_repo( ).

    me->o_tree_qrep->set_selected_node(
      EXPORTING
        node_key = ls_node_qrep-node_key
      ) .

  ENDMETHOD.
  METHOD ddic_init.

    DATA:
      lv_mode     TYPE i.

    DATA:
      lt_events   TYPE cntl_simple_events.

    DATA :
      ls_header   TYPE treev_hhdr,
      ls_event    TYPE cntl_simple_event.

    DATA:
      lo_dragdrop TYPE REF TO cl_dragdrop.

    CHECK w-o_container_ddic IS NOT INITIAL.

    ls_header-heading = 'SAP Table/Fields'.
    ls_header-width   = 30.

    lv_mode = cl_gui_column_tree=>node_sel_mode_multiple.

    CREATE OBJECT me->o_tree_ddic
      EXPORTING
        parent                      = w-o_container_ddic
        node_selection_mode         = lv_mode
        item_selection              = abap_true
        hierarchy_column_name       = 'col1'
        hierarchy_header            = ls_header
      EXCEPTIONS
        cntl_system_error           = 1
        create_error                = 2
        failed                      = 3
        illegal_node_selection_mode = 4
        illegal_column_name         = 5
        lifetime_error              = 6.
    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    me->o_tree_ddic->add_column(
      EXPORTING
        name                         = 'col2'
        width                        = 41
        header_text                  = 'Description'
      EXCEPTIONS
        column_exists                = 1
        illegal_column_name          = 2
        too_many_columns             = 3
        illegal_alignment            = 4
        different_column_types       = 5
        cntl_system_error            = 6
        failed                       = 7
        predecessor_column_not_found = 8
      ).

    ls_event-eventid    = cl_gui_column_tree=>eventid_item_double_click.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    me->o_tree_ddic->set_registered_events(
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
      ).

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT lo_dragdrop.

    lo_dragdrop->add(
      flavor     = 'EDIT_INSERT'
      dragsrc    = abap_true
      droptarget = space
      effect     = cl_dragdrop=>copy
      ).

    lo_dragdrop->get_handle(
      IMPORTING
        handle = me->v_ddrop_hndl_tree
      ).

    SET HANDLER me->hnd_ddic_item_dblclick
            FOR me->o_tree_ddic.

    SET HANDLER me->hnd_ddic_drag
            FOR me->o_tree_ddic.

  ENDMETHOD.
  METHOD is_tab_current_ddic_itm.
    DATA:
      ls_node_ddic       TYPE treev_node.

    LOOP AT me->t_node_ddic INTO ls_node_ddic.

      READ TABLE t_item_ddic TRANSPORTING NO FIELDS WITH KEY
                                                    node_key = ls_node_ddic-node_key
                                                    text     = iv_tabname .

      CHECK syst-subrc IS INITIAL.

      rv_yes = abap_true.

      RETURN.

    ENDLOOP.

  ENDMETHOD.

  METHOD handle_dtoolbar_add.
    DATA:
      lt_ddic_fields  TYPE STANDARD TABLE OF lcl_app=>ty_ddic_fields.

    DATA:
      lv_parent_node  TYPE tv_nodekey,
      lv_tabname      TYPE tabname.

    DATA:
      ls_ddic_fields  TYPE lcl_app=>ty_ddic_fields.

    lv_tabname = to_upper( iv_val ).

    TRANSLATE lv_tabname USING '. '.

    TRY.
        lcl_app=>chk_tabnm_empt_vald( lv_tabname ).

      CATCH lcl_error.
        RETURN.
    ENDTRY.

    IF abap_true = me->is_tab_current_ddic_itm( lv_tabname ).
      RETURN.
    ENDIF.

    TRY.
        lcl_app=>get_ddicfields(
          EXPORTING
            iv_val  = lv_tabname
          IMPORTING
            et_data = lt_ddic_fields
          ).

      CATCH lcl_error.
        RETURN.
    ENDTRY.

    CLEAR lv_tabname.
    SORT lt_ddic_fields BY position ASCENDING.

    LOOP AT lt_ddic_fields INTO ls_ddic_fields.

      IF lv_tabname <> ls_ddic_fields-tabname.

        me->add_new_ddic_tabfolder(
          ls_ddic_fields-tabname
          ).

        lv_parent_node = me->v_node_num.
        lv_tabname     = ls_ddic_fields-tabname.

      ENDIF.

      me->add_new_ddic_nodeitm(
        EXPORTING
          is_ddic_fields = ls_ddic_fields
          iv_parent_node = lv_parent_node
        ).

    ENDLOOP.

    me->rfrsh_ddic_tree( ).

  ENDMETHOD.
  METHOD add_new_ddic_nodeitm.

    DATA:
      ls_node         TYPE treev_node,
      ls_item         TYPE mtreeitm.

    me->v_node_num      = me->v_node_num + 1.

    ls_node-node_key    = me->v_node_num.
    ls_node-relatkey    = iv_parent_node.
    ls_node-relatship   = cl_gui_column_tree=>relat_last_child.

    IF is_ddic_fields-keyflag = space.
      ls_node-n_image   = ICON_LIST.
      ls_node-exp_image = ICON_LIST.
    ELSE.
      ls_node-n_image   = ICON_FOREIGN_KEY.
      ls_node-exp_image = ICON_FOREIGN_KEY.
    ENDIF.

    ls_node-dragdropid  = me->v_ddrop_hndl_tree.

    APPEND ls_node TO me->t_node_ddic.

    ls_item-node_key    = me->v_node_num.
    ls_item-class       = cl_gui_column_tree=>item_class_text.
    ls_item-item_name   = 'col1'.
    ls_item-text        = is_ddic_fields-fieldname.

    APPEND ls_item TO me->t_item_ddic.

    ls_item-item_name   = 'col2'.

    IF is_ddic_fields-ddtext1 IS NOT INITIAL.
      ls_item-text      = is_ddic_fields-ddtext1.
    ELSE.
      ls_item-text      = is_ddic_fields-fieldname.
    ENDIF.

    APPEND ls_item TO me->t_item_ddic.

  ENDMETHOD.

  METHOD add_new_ddic_tabfolder.
    DATA:
      ls_node         TYPE treev_node,
      ls_item         TYPE mtreeitm.

    me->v_node_num = me->v_node_num + 1.

    CLEAR ls_node.
    ls_node-node_key  = v_node_num.
    ls_node-isfolder  = abap_true.
    ls_node-n_image   = ICON_DATABASE_TABLE.
    ls_node-exp_image = ICON_DATABASE_TABLE.
    ls_node-expander  = abap_true.

    APPEND ls_node TO me->t_node_ddic.

    CLEAR ls_item.
    ls_item-node_key  = me->v_node_num.
    ls_item-class     = cl_gui_column_tree=>item_class_text.
    ls_item-item_name = 'col1'.
    ls_item-text      = iv_tabname.

    APPEND ls_item TO me->t_item_ddic.

    ls_item-item_name = 'col2' .

    SELECT SINGLE ddtext
             INTO ls_item-text
             FROM dd02t
            WHERE tabname = iv_tabname
              AND ddlanguage = sy-langu
              AND as4local = 'A'
              AND as4vers = space.

    IF sy-subrc NE 0.
      ls_item-text = iv_tabname.
    ENDIF.

    APPEND ls_item TO me->t_item_ddic.

  ENDMETHOD.
  METHOD rfrsh_ddic_tree.

    CHECK me->t_item_ddic IS NOT INITIAL.

    me->o_tree_ddic->delete_all_nodes( ).

    o_tree_ddic->add_nodes_and_items(
      EXPORTING
        node_table                     = me->t_node_ddic
        item_table                     = me->t_item_ddic
        item_table_structure_name      = 'MTREEITM'
      EXCEPTIONS
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6
      ).

    IF sy-subrc <> 0.
    ENDIF.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.
  METHOD hnd_ddic_tbar_find.
    DATA :
      ls_item_ddic   TYPE mtreeitm,
      ls_sval        TYPE sval.

    DATA:
      lt_search      TYPE string_table,
      lt_nodekey     TYPE tv_nodekeys.

    DATA:
      lv_exit        TYPE abap_bool.

    CHECK me->t_item_ddic IS NOT INITIAL.

    LOOP AT me->t_item_ddic INTO ls_item_ddic.

      APPEND ls_item_ddic-text TO lt_search.
      APPEND ls_item_ddic-node_key TO lt_nodekey.

    ENDLOOP.

    ls_sval-tabname   = 'RSDXX'.
    ls_sval-fieldname = 'FINDSTR'.
    ls_sval-value     = space.

    DO.

      me->itrative_ddic_srch(
        EXPORTING
          it_search   = lt_search
          it_node_key = lt_nodekey
        IMPORTING
          ev_exit     = lv_exit
        CHANGING
          cs_sval     = ls_sval
        ).

      CHECK lv_exit  = abap_true.

      EXIT.

    ENDDO.

  ENDMETHOD.
  METHOD itrative_ddic_srch.

    DATA:
      lv_search_term TYPE string,
      lv_search_line TYPE i.

    lcl_app=>popup_get_values(
      IMPORTING
        ev_exit = ev_exit
      CHANGING
        cs_sval = cs_sval
      ).

    IF ev_exit = abap_true.

      ev_exit = abap_true.

      RETURN.

    ENDIF.
    IF lv_search_term <> cs_sval-value.
      lv_search_term = cs_sval-value.
      lv_search_line = 1.
    ELSE.
      lv_search_line = lv_search_line + 1 + lv_search_line MOD 2 .
    ENDIF.

    FIND FIRST OCCURRENCE OF cs_sval-value
                    IN TABLE it_search
                        FROM lv_search_line IN CHARACTER MODE IGNORING CASE
                  MATCH LINE lv_search_line.

    me->sel_node_post_srch(
      iv_ok        = syst-subrc
      iv_srch_line = lv_search_line
      it_node_key  = it_node_key  ).

  ENDMETHOD.

  METHOD sel_node_post_srch.
    DATA:
      ls_node_key    TYPE tv_nodekey .

    DATA:
      lt_nodesel     TYPE STANDARD TABLE OF tv_nodekey.

    DATA:
      lv_er_txt      TYPE string.

    IF iv_srch_line <= 1.
      lv_er_txt = 'No Such Entry Exists.'.
    ELSE.
      lv_er_txt = 'No Further Entries Found.'.
    ENDIF.

    IF iv_ok = 0.

      READ TABLE it_node_key INTO ls_node_key INDEX iv_srch_line.

      IF syst-subrc IS INITIAL.

        APPEND ls_node_key TO lt_nodesel.

        me->o_tree_ddic->select_nodes(
          node_key_table = lt_nodesel
          ).

        me->o_tree_ddic->ensure_visible(
          node_key = ls_node_key
          ).

        RETURN.

      ENDIF.

    ENDIF.

    RAISE EXCEPTION TYPE lcl_error
      EXPORTING
        iv_text = lv_er_txt.

  ENDMETHOD.

  METHOD ddic_ret_selnode.

    DATA:
      ls_node_key    TYPE tv_nodekey.

    me->o_tree_ddic->get_selected_nodes(
     CHANGING
       node_key_table   =   rt_node_key
     EXCEPTIONS
       OTHERS           = 5
     ).

    IF  sy-subrc = 0
    AND rt_node_key IS NOT INITIAL.

      RETURN.

    ENDIF.

    me->o_tree_ddic->get_selected_node(
      IMPORTING
        node_key    = ls_node_key
      EXCEPTIONS
        OTHERS      = 4
      ).

    IF sy-subrc = 0
    AND ls_node_key IS NOT INITIAL.

      RETURN.

    ENDIF.

    me->o_tree_ddic->get_selected_item(
      IMPORTING
        node_key          = ls_node_key
      EXCEPTIONS
        OTHERS            = 4
      ).

    IF sy-subrc <> 0
    OR ls_node_key IS INITIAL.

      RAISE EXCEPTION TYPE lcl_error
        EXPORTING
          iv_text = 'Could not find selected node'.

    ENDIF.

    APPEND ls_node_key TO rt_node_key.

  ENDMETHOD.
  METHOD ddic_get_selfld.

    DATA:
      lt_node_key    TYPE treev_nks .

    DATA:
      ls_node_key    TYPE tv_nodekey,
      ls_node        TYPE treev_node,
      ls_item        TYPE mtreeitm,
      ls_item_parent TYPE mtreeitm,
      ls_fld         TYPE rsdsfields.

    REFRESH:
      et_fld[].

    lt_node_key = me->ddic_ret_selnode( ).

    LOOP AT lt_node_key INTO ls_node_key.

      READ TABLE me->t_node_ddic INTO ls_node
                             WITH KEY node_key = ls_node_key.

      IF sy-subrc <> 0
      OR ls_node-isfolder = abap_true.

        RETURN.

      ENDIF.

      READ TABLE me->t_item_ddic INTO ls_item
                             WITH KEY node_key  = ls_node_key
                                      item_name = 'col1'.

      CHECK syst-subrc IS INITIAL.

      READ TABLE me->t_item_ddic INTO ls_item_parent
                             WITH KEY node_key = ls_node-relatkey
                                      item_name = 'col1'.

      CHECK syst-subrc IS INITIAL.

      ls_fld-tablename = ls_item_parent-text.
      ls_fld-fieldname = ls_item-text.

      APPEND ls_fld TO et_fld.

    ENDLOOP.

  ENDMETHOD.

  METHOD hnd_ddic_inp.

    DATA:
      lt_where       TYPE rsds_twhere,
      lt_fld         TYPE rsdsfields_t,
      lt_query       TYPE soli_tab.

    me->ddic_get_selfld(
      IMPORTING
        et_fld = lt_fld
      ).

    IF lt_fld IS NOT INITIAL.

      lcl_app=>call_sel_screen(
        IMPORTING
          et_where = lt_where
        CHANGING
          ct_fld   = lt_fld
        ).

      me->get_whr_4_inp(
        EXPORTING
          it_fld      = lt_fld
        IMPORTING
          et_query    = lt_query
        CHANGING
          rsds_twhere = lt_where
      ).

      me->set_txt_at_cursor(
        it_query = lt_query
        ).

    ENDIF.

  ENDMETHOD.
  METHOD get_whr_4_inp.

    DATA:
      ls_query   TYPE soli,
      ls_whr     TYPE rsds_where,
      ls_whrline TYPE rsdswhere.

    DATA:
      lv_join    TYPE abap_bool.

    me->o_textedit->get_text(
      IMPORTING
        table  = et_query[]
      EXCEPTIONS
        OTHERS = 1
      ).

    DELETE et_query WHERE line+0(1) = '*'.

    LOOP AT et_query INTO ls_query.

      CHECK ls_query-line CS 'JOIN'.

      lv_join = abap_true.
      EXIT.

    ENDLOOP.

    REFRESH:
      et_query.

    lcl_app=>ret_whr(
      EXPORTING
        iv_join  = lv_join
        it_fld   = it_fld
      CHANGING
        ct_whr   = rsds_twhere
      ).

    LOOP AT rsds_twhere INTO ls_whr.

      LOOP AT ls_whr-where_tab INTO ls_whrline.

        CONDENSE ls_whrline-line.

        APPEND ls_whrline-line TO et_query.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
  METHOD set_txt_at_cursor.
    DATA :
      lv_line_start  TYPE i,
      lv_pos_start   TYPE i,
      lv_line_end    TYPE i,
      lv_pos_end     TYPE i.

    me->o_textedit->get_selection_pos(
      IMPORTING
        from_line = lv_line_start
        from_pos  = lv_pos_start
        to_line   = lv_line_end
        to_pos    = lv_pos_end
      EXCEPTIONS
        error_cntl_call_method = 1
        OTHERS                 = 4
      ).

    IF sy-subrc NE 0.
      MESSAGE 'Cannot get cursor position' TYPE 'E'.
    ENDIF.

    IF lv_line_start NE lv_line_end
    OR lv_pos_start  NE lv_pos_end.

      me->o_textedit->delete_text(
        from_line = lv_line_start
        from_pos  = lv_pos_start
        to_line   = lv_line_end
        to_pos    = lv_pos_end
        ).

    ENDIF.

    me->set_txt_at_line(
        iv_line_start = lv_line_start
        iv_pos_start  = lv_pos_start
        iv_break      = lcl_app=>paste_break
        it_query      = it_query
    ).


  ENDMETHOD.
  METHOD set_txt_at_line.
    DATA :
      lv_line_start  TYPE i,
      lv_pos_start   TYPE i.

    DATA:
      ls_query       TYPE soli.

    DATA:
      lt_query       TYPE soli_tab.

    lt_query[] = it_query[].

    IF iv_break = abap_true.
      APPEND ' ' TO lt_query.
    ENDIF.

    lv_line_start = iv_line_start.
    lv_pos_start  = iv_pos_start.

    me->o_textedit->insert_block_at_position(
      EXPORTING
        line     = iv_line_start
        pos      = iv_pos_start
        text_tab = lt_query[]
      EXCEPTIONS
        error_dp = 1
        OTHERS   = 2
      ).

    IF iv_break = abap_true.
      lv_line_start = lv_line_start + 1.
      lv_pos_start  = 1.
    ELSE.

      LOOP AT it_query INTO ls_query .
        lv_pos_start = lv_pos_start + strlen( ls_query ).
      ENDLOOP.

    ENDIF.

    me->o_textedit->set_selection_pos_in_line(
      EXPORTING
        line   = lv_line_start
        pos    = lv_pos_start
      EXCEPTIONS
        error_cntl_call_method = 1
        OTHERS                 = 2
      ).

    me->set_focus_on_editor( ).

  ENDMETHOD.
  METHOD hnd_ddic_toolbar_clic.

    DATA:
      lv_val   TYPE pvarfield.

    DATA:
      lo_error TYPE REF TO lcl_error.

    TRY.

        CASE fcode.

          WHEN 'ADD'.

            lv_val = lcl_app=>ret_tabname( ).

            IF lv_val IS NOT INITIAL.

              me->handle_dtoolbar_add( lv_val ).

            ENDIF.

          WHEN 'FIND'.

            me->hnd_ddic_tbar_find( ).

          WHEN 'RFRS_DD_TRE'.

            me->o_tree_ddic->delete_all_nodes( ).

            REFRESH:
              me->t_item_ddic,
              me->t_node_ddic.

            CLEAR:
              me->v_node_num.

          WHEN 'SET_FLD_IP'.

            me->hnd_ddic_inp( ).

        ENDCASE.

      CATCH lcl_error INTO lo_error.

        MESSAGE s000(db) WITH lo_error->v_text DISPLAY LIKE 'E'.

    ENDTRY.


  ENDMETHOD.
ENDCLASS.
