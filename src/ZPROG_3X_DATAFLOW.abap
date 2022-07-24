*&---------------------------------------------------------------------*
*& Report ZPROG_3X_DATAFLOW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprog_3x_dataflow.

TABLES: rsdcubeiobj, rsisfield, rsoltpsourcefie, rsdodsoiobj, rsdbchatr.

TYPES: BEGIN OF _ty_output_only_update_rules,
         no                     TYPE i,
         src_field              TYPE rstsfield-fieldnm,
         update_rule(20) TYPE c,
         aggregation(13)        TYPE c,
         tgt_field              TYPE rsupdkey-iciobjnm,
         tgt_iobjtp             TYPE rsiobjtp,
         tgt_txtlg              TYPE rstxtlg,
       END OF _ty_output_only_update_rules.
TYPES: BEGIN OF _ty_output_only_transfer_rules,
         no                TYPE i,
         src_field         TYPE rstsfield-fieldnm,
         transfer_rule(20) TYPE c,
         tgt_field         TYPE rsupdkey-iciobjnm,
         tgt_iobjtp        TYPE rsiobjtp,
         tgt_txtlg         TYPE rstxtlg,
       END OF _ty_output_only_transfer_rules.
DATA lt_output_compressed TYPE STANDARD TABLE OF _ty_output_only_update_rules WITH EMPTY KEY.
DATA lt_output_only_transfer_rules TYPE STANDARD TABLE OF _ty_output_only_transfer_rules WITH EMPTY KEY.
DATA lv_no TYPE i VALUE 1.
DATA gr_alv TYPE REF TO cl_salv_table.


SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE tblock1.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS par11 RADIOBUTTON GROUP rgb1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(50) tpar11 FOR FIELD par11.
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
PARAMETERS par22 RADIOBUTTON GROUP rgb1.
SELECTION-SCREEN COMMENT 20(50) tpar22 FOR FIELD par22.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar4 FOR FIELD par4.
PARAMETERS par4 TYPE rsdodsoiobj-odsobject.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar5 FOR FIELD par5.
PARAMETERS par5 TYPE rsdcubeiobj-infocube.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS par33 RADIOBUTTON GROUP rgb1.
SELECTION-SCREEN COMMENT 20(50) tpar33 FOR FIELD par33.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar6 FOR FIELD par6.
PARAMETERS par6 TYPE rsoltpsourcefie-oltpsource.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) tpar7 FOR FIELD par7.
PARAMETERS par7 TYPE rsdbchatr-chabasnm.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block1.

INITIALIZATION.
  tblock1 = ''.
  tpar1 = 'Quelle (DataSource)'.
  tpar2 = 'InfoSource'.
  tpar3 = 'Ziel (InfoProvider)'.
  tpar4 = 'Quelle (InfoProvider)'.
  tpar5 = 'Ziel (InfoProvider)'.
  tpar6 = 'Quelle (DataSource)'.
  tpar7 = 'Ziel (InfoObject)'.
  tpar11 = 'DataSource -> InfoSource -> Provider'.
  tpar22 = 'Provider -> Provider'.
  tpar33 = 'DataSource -> InfoObject (Stammdaten-Attribute)'.

AT SELECTION-SCREEN.

  DATA lv_target_type TYPE string.

  CASE abap_true.
    WHEN par11. "verarbeite DataSource->InfoSource->InfoProvider
      DATA(lt_output) = zcl_3x_dataflow=>display_source_target_map_3x( i_infosource = par2
                                                                 i_target_provider = CONV #( par3 )
                                                                 i_datasource = par1
                                                                 i_execution_mode = '0' ). "DataSource->InfoSource->InfoProvider

      cl_salv_table=>factory( IMPORTING r_salv_table = gr_alv
                        CHANGING t_table      = lt_output ).

    WHEN par22.
      DATA(lt_output2) = zcl_3x_dataflow=>display_source_target_map_3x(
                                      i_source_provider = CONV #( par4 )
                                      i_target_provider = CONV #( par5 )
                                      i_execution_mode = '1' ). "Provider-Provider
      MOVE-CORRESPONDING lt_output2 TO lt_output_compressed.

      cl_salv_table=>factory( IMPORTING r_salv_table = gr_alv
                      CHANGING t_table      = lt_output_compressed ).

    WHEN par33.
      DATA(lt_output3) = zcl_3x_dataflow=>display_source_target_map_3x(
                                      i_datasource = CONV #( par6 )
                                      i_target_provider = CONV #( par7 )
                                      i_execution_mode = '2' ). "DataSource-InfoObject
      MOVE-CORRESPONDING lt_output3 TO lt_output_only_transfer_rules.

      cl_salv_table=>factory( IMPORTING r_salv_table = gr_alv
                CHANGING t_table      = lt_output_only_transfer_rules ).

  ENDCASE.


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
