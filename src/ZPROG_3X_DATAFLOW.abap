*&---------------------------------------------------------------------*
*& Report ZPROG_3X_DATAFLOW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprog_3x_dataflow.

TABLES: rsdcubeiobj, rsisfield, rsoltpsourcefie, rsdodsoiobj.

TYPES: BEGIN OF _ty_src_tgt_map_3x,
         no                     TYPE i,
         src_field              TYPE rstsfield-fieldnm,
         transfer_feldregel(20) TYPE c,
         aggregation(13)        TYPE c,
         tgt_field              TYPE rsupdkey-iciobjnm,
         tgt_iobjtp             TYPE rsiobjtp,
         tgt_txtlg              TYPE rstxtlg,
       END OF _ty_src_tgt_map_3x .
DATA lt_output_compressed TYPE STANDARD TABLE OF _ty_src_tgt_map_3x WITH EMPTY KEY.
DATA lv_no TYPE i VALUE 1.
DATA gr_alv TYPE REF TO cl_salv_table.



SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE tblock1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar1 FOR FIELD par1.
PARAMETERS par1 TYPE rsoltpsourcefie-oltpsource.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar2 FOR FIELD par2.
PARAMETERS par2 TYPE rsisfield-isource.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar3 FOR FIELD par3.
PARAMETERS par3 TYPE rsdcubeiobj-infocube.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block1.
SELECTION-SCREEN SKIP 1.


SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE tblock2.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar4 FOR FIELD par4.
PARAMETERS par4 TYPE rsdodsoiobj-odsobject.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar5 FOR FIELD par5.
PARAMETERS par5 TYPE rsdcubeiobj-infocube.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block2.

INITIALIZATION.
  tblock1 = 'DataSource -> InfoSource -> InfoProvider'.
  tblock2 = 'InfoProvider -> InfoProvider'.
  tpar1 = 'Quelle (DataSource)'.
  tpar2 = 'InfoSource'.
  tpar3 = 'Ziel (InfoProvider)'.
  tpar4 = 'Quelle (InfoProvider)'.
  tpar5 = 'Ziel (InfoProvider)'.

AT SELECTION-SCREEN.

  DATA lv_target_type TYPE string.


  IF par2 IS INITIAL. "wenn keine InfoSource angegeben wurde
    DATA(lt_output) = zcl_3x_dataflow=>display_source_target_map_3x(
                                          i_source_provider = CONV #( par4 )
                                          i_target_provider = CONV #( par5 )
                                          i_execution_mode = '1' ). "Provider-Provider
    MOVE-CORRESPONDING lt_output TO lt_output_compressed.
  ELSE.
    DATA(lt_output2) = zcl_3x_dataflow=>display_source_target_map_3x( i_infosource = par2
                                                               i_target_provider = CONV #( par3 )
                                                               i_datasource = par1
                                                               i_execution_mode = '0' ). "DataSource->InfoSource->InfoProvider
  ENDIF.

  IF lt_output IS INITIAL.
    cl_salv_table=>factory( IMPORTING r_salv_table = gr_alv
                            CHANGING t_table      = lt_output2 ).
  ELSE.
    cl_salv_table=>factory( IMPORTING r_salv_table = gr_alv
                          CHANGING t_table      = lt_output_compressed ).
  ENDIF.

  gr_alv->get_functions( )->set_all( abap_true ). "allgemeines Setup (mandatory)
  gr_alv->get_columns( )->set_optimize( abap_true ). "Spaltenbreite
  gr_alv->get_display_settings( )->set_list_header( 'Source-to-Target Mapping 3.x Datenfluss' ).
  gr_alv->get_display_settings( )->set_striped_pattern( abap_false ). "Zeilenspalten farblich abheben
  gr_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ). "Zeilen markieren können

  LOOP AT gr_alv->get_columns( )->get( ) ASSIGNING FIELD-SYMBOL(<so>).
    DATA(o_col_o) = <so>-r_column.
    o_col_o->set_short_text( || ).
    o_col_o->set_medium_text( || ).
    o_col_o->set_long_text( |{ o_col_o->get_columnname( ) }| ).
  ENDLOOP.

  gr_alv->display( ).
