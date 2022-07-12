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
SELECTION-SCREEN COMMENT 1(8) tpar88 FOR FIELD par88.
PARAMETERS par88 RADIOBUTTON GROUP rgb1 DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
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
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(8) tpar99 FOR FIELD par99.
PARAMETERS par99 RADIOBUTTON GROUP rgb1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar4 FOR FIELD par4.
PARAMETERS par4 TYPE rsdodsoiobj-odsobject.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar5 FOR FIELD par5.
PARAMETERS par5 TYPE rsdcubeiobj-infocube.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block1.

INITIALIZATION.
  tblock1 = ''.
  tpar1 = 'Quelle (DataSource)'.
  tpar2 = 'InfoSource'.
  tpar3 = 'Ziel (InfoProvider)'.
  tpar4 = 'Quelle (InfoProvider)'.
  tpar5 = 'Ziel (InfoProvider)'.
  tpar88 = 'Option 1'.
  tpar99 = 'Option 2'.

AT SELECTION-SCREEN.

  DATA lv_target_type TYPE string.

  CASE abap_true.
    WHEN par88. "verarbeite DataSource->InfoSource->InfoProvider
      DATA(lt_output2) = zcl_3x_dataflow=>display_source_target_map_3x( i_infosource = par2
                                                                 i_target_provider = CONV #( par3 )
                                                                 i_datasource = par1
                                                                 i_execution_mode = '0' ). "DataSource->InfoSource->InfoProvider
    WHEN par99.
      DATA(lt_output) = zcl_3x_dataflow=>display_source_target_map_3x(
                                      i_source_provider = CONV #( par4 )
                                      i_target_provider = CONV #( par5 )
                                      i_execution_mode = '1' ). "Provider-Provider
      MOVE-CORRESPONDING lt_output TO lt_output_compressed.
  ENDCASE.


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
