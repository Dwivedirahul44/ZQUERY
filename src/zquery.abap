

REPORT  zquery    NO STANDARD PAGE HEADING.

INCLUDE zquery_top.
INCLUDE zquery_app.
INCLUDE zquery_scr.
INCLUDE zquery_main.

INITIALIZATION.

  lcl_main=>handle_initialization( ).

START-OF-SELECTION.

  lcl_main=>o_self->handle_start( iv_query = p_query
                                  iv_upto  = p_upto ).

MODULE status_8000 OUTPUT.

  lcl_main=>o_self->handle_pbo( syst-dynnr ).

ENDMODULE.
MODULE user_command_8000 INPUT.

  lcl_main=>o_self->handle_pai( w_okcode ).

  CLEAR w_okcode.

ENDMODULE.

MODULE status_9100 OUTPUT.

  SET PF-STATUS 'PF_9100'.
  lcl_main=>o_self->handle_pbo( syst-dynnr ).

ENDMODULE.
MODULE user_command_9100 INPUT.

  lcl_main=>o_self->handle_pai( w_okcode ).
  CLEAR w_okcode.

ENDMODULE.
MODULE status_9200 OUTPUT.
  SET PF-STATUS 'PF_9200'.
    lcl_main=>o_self->handle_pbo( syst-dynnr ).
ENDMODULE.
MODULE user_command_9200 INPUT.
  lcl_main=>o_self->handle_pai( w_okcode ).
  CLEAR w_okcode.
ENDMODULE.
MODULE status_9300 OUTPUT.
  SET PF-STATUS 'STATUS9300'.
    lcl_main=>o_self->handle_pbo( syst-dynnr ).
ENDMODULE.
MODULE user_command_9300 INPUT.
  lcl_main=>o_self->handle_pai( w_okcode ).
  CLEAR w_okcode.
ENDMODULE.
