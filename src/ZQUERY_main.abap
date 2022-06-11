
CLASS lcl_main DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-DATA:
      o_self                        TYPE REF TO lcl_main READ-ONLY.
    CLASS-METHODS:
      init                          RAISING lcl_error,
      handle_initialization.
    METHODS:
      handle_start                  IMPORTING iv_query                     TYPE string
                                              iv_upto                      TYPE i,
      handle_pbo                    IMPORTING iv_dynnr                     TYPE sydynnr,
      handle_pai                    IMPORTING iv_okcode                    TYPE okcode
                                    RETURNING VALUE(ro_ref_scr)            TYPE REF TO lcl_scr .
  PRIVATE SECTION.
    DATA:
      o_ref_scr                     TYPE REF TO lcl_scr.
    METHODS:

      initiate_frontend_objects     RAISING lcl_error,
      init_custom_container         RAISING lcl_error.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD init_custom_container.
  CREATE OBJECT w-o_container
    EXPORTING
      container_name = 'CUSTCONT'
    EXCEPTIONS
      others         = 6.

  IF syst-subrc <> 0.

    RAISE EXCEPTION TYPE lcl_error
    EXPORTING iv_text = 'Error starting container.'.

  ENDIF.
  ENDMETHOD.
  METHOD handle_pai.

  me->o_ref_scr =  me->o_ref_scr->handle_ucomm( iv_okcode ).

  ENDMETHOD.
  METHOD handle_pbo.

    me->o_ref_scr->handle_output( iv_dynnr ).

  ENDMETHOD.
  METHOD handle_start.

    IF  sy-batch IS NOT INITIAL
    AND iv_query  IS NOT INITIAL.

      lcl_app=>handle_bg_alv( iv_query = iv_query
                              iv_upto  = iv_upto
                             ).

      RETURN.

    ENDIF.

    CALL SCREEN 8000.

  ENDMETHOD.
  METHOD handle_initialization .
    TRY.

        lcl_main=>init( ).

        IF syst-batch IS INITIAL.

          lcl_main=>o_self->initiate_frontend_objects( ).

        ENDIF.
      CATCH lcl_error.

        MESSAGE e000(db) WITH 'Error Initializing Components'.

    ENDTRY.

  ENDMETHOD.
  METHOD init.

    CHECK lcl_main=>o_self IS INITIAL.

      CREATE OBJECT lcl_main=>o_self.

      IF lcl_main=>o_self IS NOT BOUND.
        RAISE EXCEPTION TYPE lcl_error.
      ENDIF.

  ENDMETHOD.
  METHOD initiate_frontend_objects.
    IF me->o_ref_scr IS INITIAL.
      me->init_custom_container( ).
      lcl_scr_static_objects_helper=>init_screen_objects( W-o_container ).
      me->o_ref_scr = lcl_Scr=>init_screen_objects( W-o_container ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
